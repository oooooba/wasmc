use std::fs::File;
use std::io::{BufReader, Read};

use wasmir::{Elemtype, Functype, Global, Globaltype, Limits, Memtype, Module, Mut, Tabletype, Typeidx, Valtype};
use wasmir::instructions::{Const, Expr, WasmInstr};

#[derive(PartialEq, Eq, Debug)]
pub enum ParserErrorKind {
    FileNotFound(String),
    UnexpectedFileTermination,
    InvalidFormat(String),
    IllegalFunctypeFormat,
    IllegalValtypeFormat(u8),
    IllegalLimits(u8),
    IllegalElemtypeFormat(u8),
    IllegalMutFormat(u8),
}

fn read_one_byte(reader: &mut Read) -> Result<(u8, usize), ParserErrorKind> {
    let mut buf = [0];
    match reader.read(&mut buf) {
        Ok(1) => Ok((buf[0], 1)),
        _ => Err(ParserErrorKind::UnexpectedFileTermination),
    }
}

fn decode_by_unsigned_leb128(reader: &mut Read, mut nbits: usize) -> Result<(usize, usize), ParserErrorKind> {
    assert!(nbits == 32 || nbits == 64);
    let num_iters = (nbits as f64 / 7.0).ceil() as usize;
    let mut result: usize = 0;
    let mut consumed = 0;
    for i in 0..num_iters {
        let (n, c) = read_one_byte(reader)?;
        consumed += c;
        result |= (n as usize & 0x7f) << (i * 7);
        if n & 0x80 == 0 {
            break
        }
        nbits -= 8;
    }
    Ok((result, consumed))
}

#[test]
fn test_integer_decoder() {
    let mut buf: &[u8] = &[0x02];
    assert_eq!(decode_by_unsigned_leb128(buf.by_ref(), 32), Ok((2, 1)));
    let mut buf: &[u8] = &[0x7f];
    assert_eq!(decode_by_unsigned_leb128(buf.by_ref(), 32), Ok((127, 1)));
    let mut buf: &[u8] = &[0x80, 0x01];
    assert_eq!(decode_by_unsigned_leb128(buf.by_ref(), 32), Ok((128, 2)));
    let mut buf: &[u8] = &[0x81, 0x01];
    assert_eq!(decode_by_unsigned_leb128(buf.by_ref(), 32), Ok((129, 2)));
    let mut buf: &[u8] = &[0x82, 0x01];
    assert_eq!(decode_by_unsigned_leb128(buf.by_ref(), 32), Ok((130, 2)));
    let mut buf: &[u8] = &[0xb9, 0x64];
    assert_eq!(decode_by_unsigned_leb128(buf.by_ref(), 32), Ok((12857, 2)));
}

fn parse_vector<T, F>(reader: &mut Read, f: F) -> Result<(Vec<T>, usize), ParserErrorKind>
    where F: Fn(&mut Read) -> Result<(T, usize), ParserErrorKind> {
    let (num_items, mut consumed) = decode_by_unsigned_leb128(reader, 32)?;
    let mut items = vec![];
    for _ in 0..num_items {
        let (item, c) = f(reader)?;
        items.push(item);
        consumed += c;
    }
    Ok((items, consumed))
}

fn parse_valtype(reader: &mut Read) -> Result<(Valtype, usize), ParserErrorKind> {
    read_one_byte(reader).and_then(|(b, c)| match b {
        0x7F => Ok((Valtype::I32, c)),
        0x7E => Ok((Valtype::I64, c)),
        b => Err(ParserErrorKind::IllegalValtypeFormat(b)),
    })
}

fn parse_functype(reader: &mut Read) -> Result<(Functype, usize), ParserErrorKind> {
    let c = read_one_byte(reader).and_then(|(b, c)| match b {
        0x60 => Ok(c),
        _ => Err(ParserErrorKind::IllegalFunctypeFormat),
    })?;
    let (input_type, c_i) = parse_vector(reader, parse_valtype)?;
    let (output_type, c_o) = parse_vector(reader, parse_valtype)?;
    Ok((Functype::new(input_type, output_type), c + c_i + c_o))
}

fn parse_typeidx(reader: &mut Read) -> Result<(Typeidx, usize), ParserErrorKind> {
    let (idx, consumed) = decode_by_unsigned_leb128(reader, 32)?;
    Ok((Typeidx::new(idx as u32), consumed))
}

fn parse_elemtype(reader: &mut Read) -> Result<(Elemtype, usize), ParserErrorKind> {
    read_one_byte(reader).and_then(|(b, c)| match b {
        0x70 => Ok((Elemtype::Anyfunc, c)),
        b => Err(ParserErrorKind::IllegalElemtypeFormat(b)),
    })
}

fn parse_limits(reader: &mut Read) -> Result<(Limits, usize), ParserErrorKind> {
    let (b, c_b) = read_one_byte(reader)?;
    let (min, max, c_r) = match b {
        0x00 => {
            let (min, c) = decode_by_unsigned_leb128(reader, 32)?;
            (min as u32, None, c)
        }
        0x01 => {
            let (min, c_min) = decode_by_unsigned_leb128(reader, 32)?;
            let (max, c_max) = decode_by_unsigned_leb128(reader, 32)?;
            (min as u32, Some(max as u32), c_min + c_max)
        }
        b => return Err(ParserErrorKind::IllegalLimits(b)),
    };
    Ok((Limits::new(min, max), c_b + c_r))
}

fn parse_memtype(reader: &mut Read) -> Result<(Memtype, usize), ParserErrorKind> {
    let (lim, c) = parse_limits(reader)?;
    Ok((Memtype::new(lim), c))
}

fn parse_tabletype(reader: &mut Read) -> Result<(Tabletype, usize), ParserErrorKind> {
    let (elemtype, c_e) = parse_elemtype(reader)?;
    let (limits, c_l) = parse_limits(reader)?;
    Ok((Tabletype::new(limits, elemtype), c_e + c_l))
}

fn parse_mut(reader: &mut Read) -> Result<(Mut, usize), ParserErrorKind> {
    read_one_byte(reader).and_then(|(b, c)| match b {
        0x00 => Ok((Mut::Const, c)),
        0x01 => Ok((Mut::Var, c)),
        b => Err(ParserErrorKind::IllegalMutFormat(b)),
    })
}

fn parse_globaltype(reader: &mut Read) -> Result<(Globaltype, usize), ParserErrorKind> {
    let (valtype, c_v) = parse_valtype(reader)?;
    let (mutability, c_m) = parse_mut(reader)?;
    Ok((Globaltype::new(mutability, valtype), c_v + c_m))
}

fn parse_expr(reader: &mut Read) -> Result<(Expr, usize), ParserErrorKind> {
    let mut instrs = vec![];
    let mut consumed = 0;
    loop {
        let (b, c) = read_one_byte(reader)?;
        consumed += c;
        if b == 0x0B {
            break;
        }
        let (instr, c) = match b {
            0x41 => {
                let (n, c) = decode_by_unsigned_leb128(reader, 32)?;
                (WasmInstr::Const(Const::I32(n as u32)), c)
            }
            _ => unimplemented!(),
        };
        instrs.push(instr);
        consumed += c;
    }
    Ok((Expr::new(instrs), consumed))
}

fn parse_global(reader: &mut Read) -> Result<(Global, usize), ParserErrorKind> {
    let (typ, c_g) = parse_globaltype(reader)?;
    let (init, c_e) = parse_expr(reader)?;
    Ok((Global::new(typ, init), c_g + c_e))
}

fn parse_type_section(reader: &mut Read, size: usize) -> Result<(Vec<Functype>, usize), ParserErrorKind> {
    let (functypes, consumed) = parse_vector(reader, parse_functype)?;
    assert_eq!(size, consumed);
    Ok((functypes, consumed))
}

fn parse_function_section(reader: &mut Read, size: usize) -> Result<(Vec<Typeidx>, usize), ParserErrorKind> {
    let (typeidxes, consumed) = parse_vector(reader, parse_typeidx)?;
    assert_eq!(size, consumed);
    Ok((typeidxes, consumed))
}

fn parse_table_section(reader: &mut Read, size: usize) -> Result<(Vec<Tabletype>, usize), ParserErrorKind> {
    let (tabletypes, consumed) = parse_vector(reader, parse_tabletype)?;
    assert_eq!(size, consumed);
    Ok((tabletypes, consumed))
}

fn parse_memory_section(reader: &mut Read, size: usize) -> Result<(Vec<Memtype>, usize), ParserErrorKind> {
    let (memtypes, consumed) = parse_vector(reader, parse_memtype)?;
    assert_eq!(size, consumed);
    Ok((memtypes, consumed))
}

fn parse_global_section(reader: &mut Read, size: usize) -> Result<(Vec<Global>, usize), ParserErrorKind> {
    let (globals, consumed) = parse_vector(reader, parse_global)?;
    assert_eq!(size, consumed);
    Ok((globals, consumed))
}

fn parse_section_header(reader: &mut Read) -> Result<Option<(u8, usize, usize)>, ParserErrorKind> {
    let (section_id, c_i) = match read_one_byte(reader) {
        Ok((n, c)) => (n, c),
        Err(ParserErrorKind::UnexpectedFileTermination) => return Ok(None),
        Err(e) => return Err(e),
    };
    let (size, c_s) = decode_by_unsigned_leb128(reader, 32)?;
    Ok(Some((section_id, size, c_s + c_i)))
}

pub fn parse(file_name: String) -> Result<Module, ParserErrorKind> {
    let file = File::open(&file_name).map_err(|_| ParserErrorKind::FileNotFound(file_name))?;
    let mut reader = BufReader::new(file);
    {
        let mut buf = [0; 4];
        match reader.read(&mut buf) {
            Ok(4) if buf[0] == 0x00 && buf[1] == 0x61 && buf[2] == 0x73 && buf[3] == 0x6d => (),
            _ => return Err(ParserErrorKind::InvalidFormat("field 'magic' is broken".to_string())),
        };
    }
    {
        let mut buf = [0; 4];
        match reader.read(&mut buf) {
            Ok(4) if buf[0] == 0x01 && buf[1] == 0x00 && buf[2] == 0x00 && buf[3] == 0x00 => (),
            _ => return Err(ParserErrorKind::InvalidFormat("field 'version' is broken".to_string())),
        };
    }
    let _type_section = match parse_section_header(&mut reader)? {
        Some((1, section_size, _)) => parse_type_section(&mut reader, section_size)?.0,
        Some(_) => return Err(ParserErrorKind::InvalidFormat("expected type section".to_string())),
        None => return Err(ParserErrorKind::UnexpectedFileTermination),
    };
    let _function_section = match parse_section_header(&mut reader)? {
        Some((3, section_size, _)) => parse_function_section(&mut reader, section_size)?.0,
        Some(_) => return Err(ParserErrorKind::InvalidFormat("expected function section".to_string())),
        None => return Err(ParserErrorKind::UnexpectedFileTermination),
    };
    let _table_section = match parse_section_header(&mut reader)? {
        Some((4, section_size, _)) => parse_table_section(&mut reader, section_size)?.0,
        Some(_) => return Err(ParserErrorKind::InvalidFormat("expected table section".to_string())),
        None => return Err(ParserErrorKind::UnexpectedFileTermination),
    };
    let _memory_section = match parse_section_header(&mut reader)? {
        Some((5, section_size, _)) => parse_memory_section(&mut reader, section_size)?.0,
        Some(_) => return Err(ParserErrorKind::InvalidFormat("expected memory section".to_string())),
        None => return Err(ParserErrorKind::UnexpectedFileTermination),
    };
    let _global_section = match parse_section_header(&mut reader)? {
        Some((6, section_size, _)) => parse_global_section(&mut reader, section_size)?.0,
        Some(_) => return Err(ParserErrorKind::InvalidFormat("expected global section".to_string())),
        None => return Err(ParserErrorKind::UnexpectedFileTermination),
    };
    unimplemented!()
}