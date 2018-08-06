use std::fs::File;
use std::io::{BufReader, Read};

use wasmir::Module;

#[derive(PartialEq, Eq, Debug)]
pub enum ParserErrorKind {
    FileNotFound(String),
    FileTerminated,
    InvalidFormat(String),
}

fn decode_by_unsigned_leb128(reader: &mut Read, mut nbits: usize) -> Result<usize, ParserErrorKind> {
    assert!(nbits == 32 || nbits == 64);
    let num_iters = (nbits as f64 / 7.0).ceil() as usize;
    let mut result: usize = 0;
    for i in 0..num_iters {
        let mut buf = [0];
        let n = match reader.read(&mut buf) {
            Ok(1) => buf[0],
            _ => return Err(ParserErrorKind::FileTerminated),
        };
        result |= (n as usize & 0x7f) << (i * 7);
        if n & 0x80 == 0 {
            break
        }
        nbits -= 8;
    }
    Ok(result)
}

#[test]
fn test_integer_decoder() {
    let mut buf: &[u8] = &[0x02];
    assert_eq!(decode_by_unsigned_leb128(buf.by_ref(), 32), Ok(2));
    let mut buf: &[u8] = &[0x7f];
    assert_eq!(decode_by_unsigned_leb128(buf.by_ref(), 32), Ok(127));
    let mut buf: &[u8] = &[0x80, 0x01];
    assert_eq!(decode_by_unsigned_leb128(buf.by_ref(), 32), Ok(128));
    let mut buf: &[u8] = &[0x81, 0x01];
    assert_eq!(decode_by_unsigned_leb128(buf.by_ref(), 32), Ok(129));
    let mut buf: &[u8] = &[0x82, 0x01];
    assert_eq!(decode_by_unsigned_leb128(buf.by_ref(), 32), Ok(130));
    let mut buf: &[u8] = &[0xb9, 0x64];
    assert_eq!(decode_by_unsigned_leb128(buf.by_ref(), 32), Ok(12857));
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
    unimplemented!()
}