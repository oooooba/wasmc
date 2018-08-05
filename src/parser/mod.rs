use std::fs::File;
use std::io::{BufReader, Read};

use wasmir::Module;

#[derive(Debug)]
pub enum ParserErrorKind {
    FileNotFound(String),
    InvalidFormat(String),
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