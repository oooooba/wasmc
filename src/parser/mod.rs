use std::str;
use std::io::Read;

use wasmir::{Elem, Export, Exportdesc, Func, Funcidx, Global, Globalidx, Import, Importdesc, Labelidx, Localidx, Mem, Memidx, Module, Table, Tableidx, Typeidx};
use wasmir::instructions::{Const, Expr, Ibinop, Irelop, WasmInstr};
use wasmir::types::{Elemtype, Functype, Globaltype, Limits, Memtype, Mut, Resulttype, Tabletype, Valtype};

#[derive(PartialEq, Eq, Debug)]
pub enum ParserErrorKind {
    UnexpectedFileTermination,
    InvalidFormat(String),
    IllegalFunctypeFormat,
    IllegalValtypeFormat(u8),
    IllegalLimits(u8),
    IllegalElemtypeFormat(u8),
    IllegalMutFormat(u8),
    IllegalImportdescFormat(u8),
    IllegalExportdescFormat(u8),
    InvalidUTF8Encode,
}

#[derive(PartialEq, Eq, Debug, Clone, Copy)]
enum BinaryOpcode {
    Block = 0x02,
    Loop = 0x03,
    End = 0x0B,
    BrIf = 0x0D,
    Call = 0x10,
    Drop = 0x1A,
    GetLocal = 0x20,
    SetLocal = 0x21,
    TeeLocal = 0x22,
    I32Const = 0x41,
    I32LtS = 0x48,
    I32Add = 0x60,
    I32Shl = 0x74,
}

struct InstructionEntry {
    opcode: BinaryOpcode,
}

static INSTRUCTION_TABLE: &'static [Option<InstructionEntry>] = &[
    // 0x00 - 0x07
    None,
    None,
    Some(InstructionEntry { opcode: BinaryOpcode::Block }),
    Some(InstructionEntry { opcode: BinaryOpcode::Loop }),
    None,
    None,
    None,
    None,
    // 0x08 - 0x0F
    None,
    None,
    None,
    Some(InstructionEntry { opcode: BinaryOpcode::End }),
    None,
    Some(InstructionEntry { opcode: BinaryOpcode::BrIf }),
    None,
    None,
    // 0x10 - 0x17
    Some(InstructionEntry { opcode: BinaryOpcode::Call }),
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    // 0x18 - 0x1F
    None,
    None,
    Some(InstructionEntry { opcode: BinaryOpcode::Drop }),
    None,
    None,
    None,
    None,
    None,
    // 0x20 - 0x27
    Some(InstructionEntry { opcode: BinaryOpcode::GetLocal }),
    Some(InstructionEntry { opcode: BinaryOpcode::SetLocal }),
    Some(InstructionEntry { opcode: BinaryOpcode::TeeLocal }),
    None,
    None,
    None,
    None,
    None,
    // 0x28 - 0x2F
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    // 0x30 - 0x37
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    // 0x38 - 0x3F
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    // 0x40 - 0x47
    None,
    Some(InstructionEntry { opcode: BinaryOpcode::I32Const }),
    None,
    None,
    None,
    None,
    None,
    None,
    // 0x48 - 0x4F
    Some(InstructionEntry { opcode: BinaryOpcode::I32LtS }),
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    // 0x50 - 0x57
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    // 0x58 - 0x5F
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    // 0x60 - 0x67
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    // 0x68 - 0x6F
    None,
    None,
    Some(InstructionEntry { opcode: BinaryOpcode::I32Add }),
    None,
    None,
    None,
    None,
    None,
    // 0x70 - 0x77
    None,
    None,
    None,
    None,
    Some(InstructionEntry { opcode: BinaryOpcode::I32Shl }),
    None,
    None,
    None,
];

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

fn parse_typeidx(reader: &mut Read) -> Result<(Typeidx, usize), ParserErrorKind> {
    parse_u32(reader).map(|p| (Typeidx::new(p.0), p.1))
}

fn parse_funcidx(reader: &mut Read) -> Result<(Funcidx, usize), ParserErrorKind> {
    parse_u32(reader).map(|p| (Funcidx::new(p.0), p.1))
}

fn parse_tableidx(reader: &mut Read) -> Result<(Tableidx, usize), ParserErrorKind> {
    parse_u32(reader).map(|p| (Tableidx::new(p.0), p.1))
}

fn parse_memidx(reader: &mut Read) -> Result<(Memidx, usize), ParserErrorKind> {
    parse_u32(reader).map(|p| (Memidx::new(p.0), p.1))
}

fn parse_globalidx(reader: &mut Read) -> Result<(Globalidx, usize), ParserErrorKind> {
    parse_u32(reader).map(|p| (Globalidx::new(p.0), p.1))
}

fn parse_localidx(reader: &mut Read) -> Result<(Localidx, usize), ParserErrorKind> {
    parse_u32(reader).map(|p| (Localidx::new(p.0), p.1))
}

fn parse_labelidx(reader: &mut Read) -> Result<(Labelidx, usize), ParserErrorKind> {
    parse_u32(reader).map(|p| (Labelidx::new(p.0), p.1))
}

fn parse_valtype(reader: &mut Read) -> Result<(Valtype, usize), ParserErrorKind> {
    read_one_byte(reader).and_then(|(b, c)| match b {
        0x7F => Ok((Valtype::I32, c)),
        0x7E => Ok((Valtype::I64, c)),
        b => Err(ParserErrorKind::IllegalValtypeFormat(b)),
    })
}

fn parse_blocktype(reader: &mut Read) -> Result<(Resulttype, usize), ParserErrorKind> {
    let (b, c) = read_one_byte(reader)?;
    if b == 0x40 {
        Ok((Resulttype::new(None), c))
    } else {
        let mut buf: &[u8] = &[b];
        parse_valtype(buf.by_ref()).map(|p| (Resulttype::new(Some(vec![p.0])), p.1))
    }
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
    parse_limits(reader).map(|p| (Memtype::new(p.0), p.1))
}

fn parse_mem(reader: &mut Read) -> Result<(Mem, usize), ParserErrorKind> {
    parse_memtype(reader).map(|p| (Mem::new(p.0), p.1))
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

fn parse_instrs(reader: &mut Read, terminal_opcode: BinaryOpcode) -> Result<(Vec<WasmInstr>, usize), ParserErrorKind> {
    let mut instrs = vec![];
    let mut consumed = 0;
    loop {
        use self::BinaryOpcode::*;
        let (b, c) = read_one_byte(reader)?;
        consumed += c;
        let opcode = match INSTRUCTION_TABLE[b as usize].as_ref() {
            Some(entry) => entry.opcode,
            None => unimplemented!("opcode: 0x{:02X}", b),
        };
        let (instr, c) = match opcode {
            Block => {
                let (resulttype, c_r) = parse_blocktype(reader)?;
                let (instrs, c_i) = parse_instrs(reader, BinaryOpcode::End)?;
                (WasmInstr::Block(resulttype, instrs), c_r + c_i)
            }
            Loop => {
                let (resulttype, c_r) = parse_blocktype(reader)?;
                let (instrs, c_i) = parse_instrs(reader, BinaryOpcode::End)?;
                (WasmInstr::Loop(resulttype, instrs), c_r + c_i)
            }
            End => if terminal_opcode == End { break; } else { panic!("unexpected terminal opcode") },
            BrIf => parse_labelidx(reader).map(|p| (WasmInstr::BrIf(p.0), p.1))?,
            Call => parse_funcidx(reader).map(|p| (WasmInstr::Call(p.0), p.1))?,
            Drop => (WasmInstr::Drop, 0),
            GetLocal => parse_localidx(reader).map(|p| (WasmInstr::GetLocal(p.0), p.1))?,
            SetLocal => parse_localidx(reader).map(|p| (WasmInstr::SetLocal(p.0), p.1))?,
            TeeLocal => parse_localidx(reader).map(|p| (WasmInstr::TeeLocal(p.0), p.1))?,
            I32Const => parse_u32(reader).map(|p| (WasmInstr::Const(Const::I32(p.0)), p.1))?,
            I32LtS => (WasmInstr::Irelop(Irelop::LtS32), 0),
            I32Add => (WasmInstr::Ibinop(Ibinop::Add32), 0),
            I32Shl => (WasmInstr::Ibinop(Ibinop::Shl32), 0),
        };
        instrs.push(instr);
        consumed += c;
    }
    Ok((instrs, consumed))
}

fn parse_expr(reader: &mut Read) -> Result<(Expr, usize), ParserErrorKind> {
    parse_instrs(reader, BinaryOpcode::End).map(|p| (Expr::new(p.0), p.1))
}

fn parse_global(reader: &mut Read) -> Result<(Global, usize), ParserErrorKind> {
    let (typ, c_g) = parse_globaltype(reader)?;
    let (init, c_e) = parse_expr(reader)?;
    Ok((Global::new(typ, init), c_g + c_e))
}

fn parse_exportdesc(reader: &mut Read) -> Result<(Exportdesc, usize), ParserErrorKind> {
    read_one_byte(reader).and_then(|(b, c)| match b {
        0x00 => parse_funcidx(reader).map(|p| (Exportdesc::Func(p.0), c + p.1)),
        0x01 => parse_tableidx(reader).map(|p| (Exportdesc::Table(p.0), c + p.1)),
        0x02 => parse_memidx(reader).map(|p| (Exportdesc::Mem(p.0), c + p.1)),
        0x03 => parse_globalidx(reader).map(|p| (Exportdesc::Global(p.0), c + p.1)),
        b => Err(ParserErrorKind::IllegalExportdescFormat(b)),
    })
}

fn parse_importdesc(reader: &mut Read) -> Result<(Importdesc, usize), ParserErrorKind> {
    read_one_byte(reader).and_then(|(b, c)| match b {
        0x00 => parse_typeidx(reader).map(|p| (Importdesc::Func(p.0), c + p.1)),
        0x01 => parse_tabletype(reader).map(|p| (Importdesc::Table(p.0), c + p.1)),
        0x02 => parse_memtype(reader).map(|p| (Importdesc::Mem(p.0), c + p.1)),
        0x03 => parse_globaltype(reader).map(|p| (Importdesc::Global(p.0), c + p.1)),
        b => Err(ParserErrorKind::IllegalImportdescFormat(b)),
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

fn parse_import(reader: &mut Read) -> Result<(Import, usize), ParserErrorKind> {
    let (module, c_m) = parse_name(reader)?;
    let (name, c_n) = parse_name(reader)?;
    let (desc, c_d) = parse_importdesc(reader)?;
    Ok((Import::new(module, name, desc), c_m + c_n + c_d))
}

fn parse_export(reader: &mut Read) -> Result<(Export, usize), ParserErrorKind> {
    let (name, c_n) = parse_name(reader)?;
    let (desc, c_d) = parse_exportdesc(reader)?;
    Ok((Export::new(name, desc), c_n + c_d))
}

fn parse_elem(reader: &mut Read) -> Result<(Elem, usize), ParserErrorKind> {
    let (table, c_t) = parse_tableidx(reader)?;
    let (offset, c_o) = parse_expr(reader)?;
    let (init, c_i) = parse_vector(reader, parse_funcidx)?;
    Ok((Elem::new(table, offset, init), c_t + c_o + c_i))
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

fn parse_type_section(reader: &mut Read, _size: usize) -> Result<(Vec<Functype>, usize), ParserErrorKind> {
    parse_vector(reader, parse_functype)
}

fn parse_import_section(reader: &mut Read, _size: usize) -> Result<(Vec<Import>, usize), ParserErrorKind> {
    parse_vector(reader, parse_import)
}

fn parse_function_section(reader: &mut Read, _size: usize) -> Result<(Vec<Typeidx>, usize), ParserErrorKind> {
    parse_vector(reader, parse_typeidx)
}

fn parse_table_section(reader: &mut Read, _size: usize) -> Result<(Vec<Table>, usize), ParserErrorKind> {
    parse_vector(reader, parse_table)
}

fn parse_memory_section(reader: &mut Read, _size: usize) -> Result<(Vec<Mem>, usize), ParserErrorKind> {
    parse_vector(reader, parse_mem)
}

fn parse_global_section(reader: &mut Read, _size: usize) -> Result<(Vec<Global>, usize), ParserErrorKind> {
    parse_vector(reader, parse_global)
}

fn parse_export_section(reader: &mut Read, _size: usize) -> Result<(Vec<Export>, usize), ParserErrorKind> {
    parse_vector(reader, parse_export)
}

fn parse_element_section(reader: &mut Read, _size: usize) -> Result<(Vec<Elem>, usize), ParserErrorKind> {
    parse_vector(reader, parse_elem)
}

fn parse_code_section(reader: &mut Read, _size: usize) -> Result<(Vec<Code>, usize), ParserErrorKind> {
    parse_vector(reader, parse_code)
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

fn section_parser_driver<T, P>(reader: &mut Read, parser: P, section_size: usize, storage: &mut Vec<T>) -> Result<usize, ParserErrorKind>
    where P: Fn(&mut Read, usize) -> Result<(Vec<T>, usize), ParserErrorKind> {
    assert_eq!(storage.len(), 0);
    let (mut result, consumed) = parser(reader, section_size)?;
    storage.append(&mut result);
    Ok(consumed)
}

fn parse_module(reader: &mut Read) -> Result<Module, ParserErrorKind> {
    let mut type_section = vec![];
    let mut import_section = vec![];
    let mut function_section = vec![];
    let mut table_section = vec![];
    let mut memory_section = vec![];
    let mut global_section = vec![];
    let mut export_section = vec![];
    let mut element_section = vec![];
    let mut code_section = vec![];

    let mut last_parsed_section_id = 0;
    while let Some((section_id, section_size, _)) = discard_until_next_semantic_section(reader)? {
        if section_id <= last_parsed_section_id {
            return Err(ParserErrorKind::InvalidFormat(
                format!("invalid section order, {} after {}", section_id, last_parsed_section_id)));
        }
        let consumed = match section_id {
            0 => unreachable!(),
            1 => section_parser_driver(reader, parse_type_section, section_size, &mut type_section)?,
            2 => section_parser_driver(reader, parse_import_section, section_size, &mut import_section)?,
            3 => section_parser_driver(reader, parse_function_section, section_size, &mut function_section)?,
            4 => section_parser_driver(reader, parse_table_section, section_size, &mut table_section)?,
            5 => section_parser_driver(reader, parse_memory_section, section_size, &mut memory_section)?,
            6 => section_parser_driver(reader, parse_global_section, section_size, &mut global_section)?,
            7 => section_parser_driver(reader, parse_export_section, section_size, &mut export_section)?,
            8 => unimplemented!(),
            9 => section_parser_driver(reader, parse_element_section, section_size, &mut element_section)?,
            10 => section_parser_driver(reader, parse_code_section, section_size, &mut code_section)?,
            11 => unimplemented!(),
            _ => return Err(ParserErrorKind::InvalidFormat(format!("invalid section id {}", section_id))),
        };
        assert_eq!(section_size, consumed);
        last_parsed_section_id = section_id;
    }
    assert_eq!(function_section.len(), code_section.len());
    assert_eq!(discard_until_next_semantic_section(reader), Ok(None));

    let funcs = function_section
        .into_iter()
        .zip(code_section.into_iter())
        .map(|(f, c)| Func::new(f, c.locals, c.expr))
        .collect();
    Ok(Module::new(
        type_section,
        funcs,
        table_section,
        memory_section,
        global_section,
        export_section,
        import_section,
    ))
}

pub fn parse(reader: &mut Read) -> Result<Module, ParserErrorKind> {
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
    parse_module(reader)
}