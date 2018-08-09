use std::fs::File;
use std::io::{BufReader, Read};

use wasmir::{Functype, Module, Typeidx, Valtype};

#[derive(PartialEq, Eq, Debug)]
pub enum ParserErrorKind {
    FileNotFound(String),
    UnexpectedFileTermination,
    InvalidFormat(String),
    IllegalFunctypeFormat,
    IllegalValtypeFormat(u8),
}

fn read_one_byte(reader: &mut Read) -> Option<u8> {
    let mut buf = [0];
    match reader.read(&mut buf) {
        Ok(1) => Some(buf[0]),
        _ => None,
    }
}

fn decode_by_unsigned_leb128(reader: &mut Read, mut nbits: usize) -> Result<(usize, usize), ParserErrorKind> {
    assert!(nbits == 32 || nbits == 64);
    let num_iters = (nbits as f64 / 7.0).ceil() as usize;
    let mut result: usize = 0;
    let mut consumed = 0;
    for i in 0..num_iters {
        let n = read_one_byte(reader).ok_or(ParserErrorKind::UnexpectedFileTermination)?;
        consumed += 1;
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

fn parse_valtype(reader: &mut Read) -> Result<(Valtype, usize), ParserErrorKind> {
    match read_one_byte(reader) {
        Some(0x7F) => Ok((Valtype::I32, 1)),
        Some(0x7E) => Ok((Valtype::I64, 1)),
        Some(n) => Err(ParserErrorKind::IllegalValtypeFormat(n)),
        None => Err(ParserErrorKind::UnexpectedFileTermination),
    }
}

fn parse_valtypes(reader: &mut Read) -> Result<(Vec<Valtype>, usize), ParserErrorKind> {
    let (size, mut consumed) = decode_by_unsigned_leb128(reader, 32)?;
    let mut valtypes = vec![];
    for _ in 0..size {
        let (valtype, c) = parse_valtype(reader)?;
        valtypes.push(valtype);
        consumed += c;
    }
    Ok((valtypes, consumed))
}

fn parse_type_section(reader: &mut Read, size: usize) -> Result<(Vec<Functype>, usize), ParserErrorKind> {
    let (num_functypes, mut consumed) = decode_by_unsigned_leb128(reader, 32)?;
    let mut functypes = vec![];
    for _ in 0..num_functypes {
        match read_one_byte(reader) {
            Some(0x60) => consumed += 1,
            _ => return Err(ParserErrorKind::IllegalFunctypeFormat),
        }
        let (input_type, c) = parse_valtypes(reader)?;
        consumed += c;
        let (output_type, c) = parse_valtypes(reader)?;
        consumed += c;
        functypes.push(Functype::new(input_type, output_type));
    }
    assert_eq!(size, consumed);
    Ok((functypes, consumed))
}

fn parse_function_section(reader: &mut Read, size: usize) -> Result<(Vec<Typeidx>, usize), ParserErrorKind> {
    let (num_typeidxes, mut consumed) = decode_by_unsigned_leb128(reader, 32)?;
    let mut typeidxes = vec![];
    for _ in 0..num_typeidxes {
        let (idx, c) = decode_by_unsigned_leb128(reader, 32)?;
        consumed += c;
        typeidxes.push(Typeidx::new(idx as u32));
    }
    assert_eq!(size, consumed);
    Ok((typeidxes, consumed))
}

fn parse_section_header(reader: &mut Read) -> Result<Option<(u8, usize, usize)>, ParserErrorKind> {
    let section_id = match read_one_byte(reader) {
        Some(n) => n,
        None => return Ok(None),
    };
    let (size, consumed) = decode_by_unsigned_leb128(reader, 32)?;
    Ok(Some((section_id, size, consumed)))
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
    unimplemented!()
}