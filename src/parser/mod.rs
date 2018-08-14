use std::str;
use std::fs::File;
use std::io::{BufReader, Read};

use wasmir::{Export, Exportdesc, Funcidx, Global, Globalidx, Memidx, Module, Table, Tableidx, Typeidx};
use wasmir::instructions::{Const, Expr, WasmInstr};
use wasmir::types::{Elemtype, Functype, Globaltype, Limits, Memtype, Mut, Tabletype, Valtype};

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
    IllegalExportdescFormat(u8),
    InvalidUTF8Encode,
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

fn parse_u32(reader: &mut Read) -> Result<(u32, usize), ParserErrorKind> {
    let (n, c) = decode_by_unsigned_leb128(reader, 32)?;
    assert!(n < 2 << 32);
    Ok((n as u32, c))
}

fn parse_vector<T, F>(reader: &mut Read, f: F) -> Result<(Vec<T>, usize), ParserErrorKind>
    where F: Fn(&mut Read) -> Result<(T, usize), ParserErrorKind> {
    let (num_items, mut consumed) = parse_u32(reader)?;
    let mut items = vec![];
    for _ in 0..num_items as usize {
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
    parse_index(reader).map(|p| (Typeidx::new(p.0), p.1))
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
            let (min, c) = parse_u32(reader)?;
            (min, None, c)
        }
        0x01 => {
            let (min, c_min) = parse_u32(reader)?;
            let (max, c_max) = parse_u32(reader)?;
            (min, Some(max), c_min + c_max)
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

fn parse_table(reader: &mut Read) -> Result<(Table, usize), ParserErrorKind> {
    parse_tabletype(reader).map(|p| (Table::new(p.0), p.1))
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
            0x41 => parse_u32(reader).map(|p| (WasmInstr::Const(Const::I32(p.0)), p.1))?,
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

fn parse_index(reader: &mut Read) -> Result<(u32, usize), ParserErrorKind> {
    parse_u32(reader)
}

fn parse_exportdesc(reader: &mut Read) -> Result<(Exportdesc, usize), ParserErrorKind> {
    read_one_byte(reader).and_then(|(b, c)| match b {
        0x00 => {
            let (index, c_i) = parse_index(reader)?;
            Ok((Exportdesc::Func(Funcidx::new(index)), c + c_i))
        }
        0x01 => {
            let (index, c_i) = parse_index(reader)?;
            Ok((Exportdesc::Table(Tableidx::new(index)), c + c_i))
        }
        0x02 => {
            let (index, c_i) = parse_index(reader)?;
            Ok((Exportdesc::Mem(Memidx::new(index)), c + c_i))
        }
        0x03 => {
            let (index, c_i) = parse_index(reader)?;
            Ok((Exportdesc::Global(Globalidx::new(index)), c + c_i))
        }
        b => Err(ParserErrorKind::IllegalExportdescFormat(b)),
    })
}

fn parse_name(reader: &mut Read) -> Result<(String, usize), ParserErrorKind> {
    let (size, c_s) = parse_u32(reader)?;
    let size = size as usize;
    let mut buf = Vec::with_capacity(size);
    unsafe {
        buf.set_len(size);
    }
    reader.read_exact(&mut buf).map_err(|_| ParserErrorKind::UnexpectedFileTermination)?;
    let s = str::from_utf8(&buf).map_err(|_| ParserErrorKind::InvalidUTF8Encode)?;
    Ok((s.to_string(), c_s + size))
}

fn parse_export(reader: &mut Read) -> Result<(Export, usize), ParserErrorKind> {
    let (name, c_n) = parse_name(reader)?;
    let (desc, c_d) = parse_exportdesc(reader)?;
    Ok((Export::new(name, desc), c_n + c_d))
}

#[derive(Debug)]
struct Code {
    size: u32,
    locals: Vec<Valtype>,
    expr: Expr,
}

fn parse_locals(reader: &mut Read) -> Result<((u32, Valtype), usize), ParserErrorKind> {
    let (n, c_n) = parse_u32(reader)?;
    let (t, c_t) = parse_valtype(reader)?;
    Ok(((n, t), c_n + c_t))
}

fn parse_code(reader: &mut Read) -> Result<(Code, usize), ParserErrorKind> {
    let (size, c_s) = parse_u32(reader)?;
    let (local_pairs, c_l) = parse_vector(reader, parse_locals)?;
    let (expr, c_e) = parse_expr(reader)?;
    let mut locals = vec![];
    for (n, t) in local_pairs.into_iter() {
        let n = n as usize;
        let mut ts = Vec::with_capacity(n);
        ts.resize(n, t);
        locals.append(&mut ts);
    }
    assert_eq!(size as usize, c_l + c_e);
    assert!(locals.len() < 2 << 32);
    Ok((Code { size, locals, expr }, c_s + c_l + c_e))
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

fn parse_table_section(reader: &mut Read, size: usize) -> Result<(Vec<Table>, usize), ParserErrorKind> {
    let (tabletypes, consumed) = parse_vector(reader, parse_table)?;
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

fn parse_export_section(reader: &mut Read, size: usize) -> Result<(Vec<Export>, usize), ParserErrorKind> {
    let (exports, consumed) = parse_vector(reader, parse_export)?;
    assert_eq!(size, consumed);
    Ok((exports, consumed))
}

fn parse_code_section(reader: &mut Read, size: usize) -> Result<(Vec<Code>, usize), ParserErrorKind> {
    let (code, consumed) = parse_vector(reader, parse_code)?;
    assert_eq!(size, consumed);
    Ok((code, consumed))
}

fn discard_until_next_semantic_section(reader: &mut Read) -> Result<Option<(u8, usize, usize)>, ParserErrorKind> {
    let mut consumed = 0;
    loop {
        let (section_id, c_i) = match read_one_byte(reader) {
            Ok((n, c)) => (n, c),
            Err(ParserErrorKind::UnexpectedFileTermination) => return Ok(None),
            Err(e) => return Err(e),
        };
        let (section_size, c_s) = parse_u32(reader)?;
        let section_size = section_size as usize;
        consumed += c_i + c_s;
        if section_id != 0 {
            return Ok(Some((section_id, section_size, consumed)));
        }
        let mut buf = Vec::with_capacity(section_size);
        unsafe {
            buf.set_len(section_size);
        }
        reader.read_exact(&mut buf).map_err(|_| ParserErrorKind::UnexpectedFileTermination)?;
    }
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
    let _type_section = match discard_until_next_semantic_section(&mut reader)? {
        Some((1, section_size, _)) => parse_type_section(&mut reader, section_size)?.0,
        Some(_) => return Err(ParserErrorKind::InvalidFormat("expected type section".to_string())),
        None => return Err(ParserErrorKind::UnexpectedFileTermination),
    };
    let _function_section = match discard_until_next_semantic_section(&mut reader)? {
        Some((3, section_size, _)) => parse_function_section(&mut reader, section_size)?.0,
        Some(_) => return Err(ParserErrorKind::InvalidFormat("expected function section".to_string())),
        None => return Err(ParserErrorKind::UnexpectedFileTermination),
    };
    let _table_section = match discard_until_next_semantic_section(&mut reader)? {
        Some((4, section_size, _)) => parse_table_section(&mut reader, section_size)?.0,
        Some(_) => return Err(ParserErrorKind::InvalidFormat("expected table section".to_string())),
        None => return Err(ParserErrorKind::UnexpectedFileTermination),
    };
    let _memory_section = match discard_until_next_semantic_section(&mut reader)? {
        Some((5, section_size, _)) => parse_memory_section(&mut reader, section_size)?.0,
        Some(_) => return Err(ParserErrorKind::InvalidFormat("expected memory section".to_string())),
        None => return Err(ParserErrorKind::UnexpectedFileTermination),
    };
    let _global_section = match discard_until_next_semantic_section(&mut reader)? {
        Some((6, section_size, _)) => parse_global_section(&mut reader, section_size)?.0,
        Some(_) => return Err(ParserErrorKind::InvalidFormat("expected global section".to_string())),
        None => return Err(ParserErrorKind::UnexpectedFileTermination),
    };
    let _export_section = match discard_until_next_semantic_section(&mut reader)? {
        Some((7, section_size, _)) => parse_export_section(&mut reader, section_size)?.0,
        Some(_) => return Err(ParserErrorKind::InvalidFormat("expected export section".to_string())),
        None => return Err(ParserErrorKind::UnexpectedFileTermination),
    };
    let _code_section = match discard_until_next_semantic_section(&mut reader)? {
        Some((10, section_size, _)) => parse_code_section(&mut reader, section_size)?.0,
        Some(_) => return Err(ParserErrorKind::InvalidFormat("expected code section".to_string())),
        None => return Err(ParserErrorKind::UnexpectedFileTermination),
    };
    assert_eq!(discard_until_next_semantic_section(&mut reader), Ok(None));
    unimplemented!()
}