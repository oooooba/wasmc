use std::io::Read;
use std::str;

use wasmir::instructions::{
    Const, Cvtop, Expr, Ibinop, Irelop, Itestop, Iunop, Loadattr, Memarg, Storeattr, WasmInstr,
};
use wasmir::types::{
    Elemtype, Functype, Globaltype, Limits, Memtype, Mut, Resulttype, Tabletype, Valtype,
};
use wasmir::{
    Data, Elem, Export, Exportdesc, Func, Funcidx, Global, Globalidx, Import, Importdesc, Labelidx,
    Localidx, Mem, Memidx, Module, Table, Tableidx, Typeidx,
};

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
    Unreachable = 0x00,
    Block = 0x02,
    Loop = 0x03,
    End = 0x0B,
    Br = 0x0C,
    BrIf = 0x0D,
    BrTable = 0x0E,
    Return = 0x0F,
    Call = 0x10,
    CallIndirect = 0x11,
    Drop = 0x1A,
    Select = 0x1B,
    GetLocal = 0x20,
    SetLocal = 0x21,
    TeeLocal = 0x22,
    GetGlobal = 0x23,
    SetGlobal = 0x24,
    I32Load = 0x28,
    I64Load = 0x29,
    I32Load8S = 0x2C,
    I32Load8U = 0x2D,
    I32Store = 0x36,
    I64Store = 0x37,
    I32Store8 = 0x3A,
    I32Const = 0x41,
    I64Const = 0x42,
    I32Eqz = 0x45,
    I32Eq = 0x46,
    I32Ne = 0x47,
    I32LtS = 0x48,
    I32LtU = 0x49,
    I32GtS = 0x4A,
    I32GtU = 0x4B,
    I32LeS = 0x4C,
    I32LeU = 0x4D,
    I32GeS = 0x4E,
    I32GeU = 0x4F,
    I32Clz = 0x67,
    I32Ctz = 0x68,
    I32Add = 0x6A,
    I32Sub = 0x6B,
    I32Mul = 0x6C,
    I32DivU = 0x6E,
    I32And = 0x71,
    I32Or = 0x72,
    I32Xor = 0x73,
    I32Shl = 0x74,
    I32ShrS = 0x75,
    I32ShrU = 0x76,
    I64Or = 0x84,
    I64Shl = 0x86,
    I64ShrU = 0x88,
    I32WrapI64 = 0xA7,
    I64ExtendUI32 = 0xAD,
}

struct InstructionEntry {
    opcode: BinaryOpcode,
}

static INSTRUCTION_TABLE: &'static [Option<InstructionEntry>] = &[
    // 0x00 - 0x07
    Some(InstructionEntry {
        opcode: BinaryOpcode::Unreachable,
    }),
    None,
    Some(InstructionEntry {
        opcode: BinaryOpcode::Block,
    }),
    Some(InstructionEntry {
        opcode: BinaryOpcode::Loop,
    }),
    None,
    None,
    None,
    None,
    // 0x08 - 0x0F
    None,
    None,
    None,
    Some(InstructionEntry {
        opcode: BinaryOpcode::End,
    }),
    Some(InstructionEntry {
        opcode: BinaryOpcode::Br,
    }),
    Some(InstructionEntry {
        opcode: BinaryOpcode::BrIf,
    }),
    Some(InstructionEntry {
        opcode: BinaryOpcode::BrTable,
    }),
    Some(InstructionEntry {
        opcode: BinaryOpcode::Return,
    }),
    // 0x10 - 0x17
    Some(InstructionEntry {
        opcode: BinaryOpcode::Call,
    }),
    Some(InstructionEntry {
        opcode: BinaryOpcode::CallIndirect,
    }),
    None,
    None,
    None,
    None,
    None,
    None,
    // 0x18 - 0x1F
    None,
    None,
    Some(InstructionEntry {
        opcode: BinaryOpcode::Drop,
    }),
    Some(InstructionEntry {
        opcode: BinaryOpcode::Select,
    }),
    None,
    None,
    None,
    None,
    // 0x20 - 0x27
    Some(InstructionEntry {
        opcode: BinaryOpcode::GetLocal,
    }),
    Some(InstructionEntry {
        opcode: BinaryOpcode::SetLocal,
    }),
    Some(InstructionEntry {
        opcode: BinaryOpcode::TeeLocal,
    }),
    Some(InstructionEntry {
        opcode: BinaryOpcode::GetGlobal,
    }),
    Some(InstructionEntry {
        opcode: BinaryOpcode::SetGlobal,
    }),
    None,
    None,
    None,
    // 0x28 - 0x2F
    Some(InstructionEntry {
        opcode: BinaryOpcode::I32Load,
    }),
    Some(InstructionEntry {
        opcode: BinaryOpcode::I64Load,
    }),
    None,
    None,
    Some(InstructionEntry {
        opcode: BinaryOpcode::I32Load8S,
    }),
    Some(InstructionEntry {
        opcode: BinaryOpcode::I32Load8U,
    }),
    None,
    None,
    // 0x30 - 0x37
    None,
    None,
    None,
    None,
    None,
    None,
    Some(InstructionEntry {
        opcode: BinaryOpcode::I32Store,
    }),
    Some(InstructionEntry {
        opcode: BinaryOpcode::I64Store,
    }),
    // 0x38 - 0x3F
    None,
    None,
    Some(InstructionEntry {
        opcode: BinaryOpcode::I32Store8,
    }),
    None,
    None,
    None,
    None,
    None,
    // 0x40 - 0x47
    None,
    Some(InstructionEntry {
        opcode: BinaryOpcode::I32Const,
    }),
    Some(InstructionEntry {
        opcode: BinaryOpcode::I64Const,
    }),
    None,
    None,
    Some(InstructionEntry {
        opcode: BinaryOpcode::I32Eqz,
    }),
    Some(InstructionEntry {
        opcode: BinaryOpcode::I32Eq,
    }),
    Some(InstructionEntry {
        opcode: BinaryOpcode::I32Ne,
    }),
    // 0x48 - 0x4F
    Some(InstructionEntry {
        opcode: BinaryOpcode::I32LtS,
    }),
    Some(InstructionEntry {
        opcode: BinaryOpcode::I32LtU,
    }),
    Some(InstructionEntry {
        opcode: BinaryOpcode::I32GtS,
    }),
    Some(InstructionEntry {
        opcode: BinaryOpcode::I32GtU,
    }),
    Some(InstructionEntry {
        opcode: BinaryOpcode::I32LeS,
    }),
    Some(InstructionEntry {
        opcode: BinaryOpcode::I32LeU,
    }),
    Some(InstructionEntry {
        opcode: BinaryOpcode::I32GeS,
    }),
    Some(InstructionEntry {
        opcode: BinaryOpcode::I32GeU,
    }),
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
    Some(InstructionEntry {
        opcode: BinaryOpcode::I32Clz,
    }),
    // 0x68 - 0x6F
    Some(InstructionEntry {
        opcode: BinaryOpcode::I32Ctz,
    }),
    None,
    Some(InstructionEntry {
        opcode: BinaryOpcode::I32Add,
    }),
    Some(InstructionEntry {
        opcode: BinaryOpcode::I32Sub,
    }),
    Some(InstructionEntry {
        opcode: BinaryOpcode::I32Mul,
    }),
    None,
    Some(InstructionEntry {
        opcode: BinaryOpcode::I32DivU,
    }),
    None,
    // 0x70 - 0x77
    None,
    Some(InstructionEntry {
        opcode: BinaryOpcode::I32And,
    }),
    Some(InstructionEntry {
        opcode: BinaryOpcode::I32Or,
    }),
    Some(InstructionEntry {
        opcode: BinaryOpcode::I32Xor,
    }),
    Some(InstructionEntry {
        opcode: BinaryOpcode::I32Shl,
    }),
    Some(InstructionEntry {
        opcode: BinaryOpcode::I32ShrS,
    }),
    Some(InstructionEntry {
        opcode: BinaryOpcode::I32ShrU,
    }),
    None,
    // 0x78 - 0x7F
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    // 0x80 - 0x87
    None,
    None,
    None,
    None,
    Some(InstructionEntry {
        opcode: BinaryOpcode::I64Or,
    }),
    None,
    Some(InstructionEntry {
        opcode: BinaryOpcode::I64Shl,
    }),
    None,
    // 0x88 - 0x8F
    Some(InstructionEntry {
        opcode: BinaryOpcode::I64ShrU,
    }),
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    // 0x90 - 0x97
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    // 0x98 - 0x9F
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    // 0xA0 - 0xA7
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    Some(InstructionEntry {
        opcode: BinaryOpcode::I32WrapI64,
    }),
    // 0xA8 - 0xAF
    None,
    None,
    None,
    None,
    None,
    Some(InstructionEntry {
        opcode: BinaryOpcode::I64ExtendUI32,
    }),
    None,
    None,
];

fn decode_by_unsigned_leb128(
    reader: &mut Read,
    nbits: usize,
) -> Result<(usize, usize), ParserErrorKind> {
    assert!(nbits == 32 || nbits == 64);
    let num_iters = (nbits as f64 / 7.0).ceil() as usize;
    let mut result: usize = 0;
    let mut consumed = 0;
    for i in 0..num_iters {
        let (n, c) = parse_byte(reader)?;
        consumed += c;
        result |= (n as usize & 0x7f) << (i * 7);
        if n & 0x80 == 0 {
            break;
        }
    }
    Ok((result, consumed))
}

fn decode_by_signed_leb128(
    reader: &mut Read,
    nbits: usize,
) -> Result<(isize, usize), ParserErrorKind> {
    assert!(nbits == 32 || nbits == 64);
    let (n, consumed) = decode_by_unsigned_leb128(reader, nbits)?;
    let n = n as i128;
    let sign_bit = 1i128 << (consumed * 7 - 1);
    let mask = (sign_bit << 1) - 1;
    let masked_n = n & mask;
    let sign_extended_masked_n = if n & sign_bit != 0 {
        masked_n | !mask
    } else {
        masked_n
    };
    Ok((sign_extended_masked_n as isize, consumed))
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

    let mut buf: &[u8] = &[0x02];
    assert_eq!(decode_by_signed_leb128(buf.by_ref(), 32), Ok((2, 1)));
    let mut buf: &[u8] = &[0x7e];
    assert_eq!(decode_by_signed_leb128(buf.by_ref(), 32), Ok((-2, 1)));
    let mut buf: &[u8] = &[0xff, 0x00];
    assert_eq!(decode_by_signed_leb128(buf.by_ref(), 32), Ok((127, 2)));
    let mut buf: &[u8] = &[0x81, 0x7f];
    assert_eq!(decode_by_signed_leb128(buf.by_ref(), 32), Ok((-127, 2)));
    let mut buf: &[u8] = &[0x80, 0x01];
    assert_eq!(decode_by_signed_leb128(buf.by_ref(), 32), Ok((128, 2)));
    let mut buf: &[u8] = &[0x80, 0x7f];
    assert_eq!(decode_by_signed_leb128(buf.by_ref(), 32), Ok((-128, 2)));
    let mut buf: &[u8] = &[0x81, 0x01];
    assert_eq!(decode_by_signed_leb128(buf.by_ref(), 32), Ok((129, 2)));
    let mut buf: &[u8] = &[0xff, 0x7e];
    assert_eq!(decode_by_signed_leb128(buf.by_ref(), 32), Ok((-129, 2)));
}

fn parse_byte(reader: &mut Read) -> Result<(u8, usize), ParserErrorKind> {
    let mut buf = [0];
    match reader.read(&mut buf) {
        Ok(1) => Ok((buf[0], 1)),
        _ => Err(ParserErrorKind::UnexpectedFileTermination),
    }
}

fn parse_u32(reader: &mut Read) -> Result<(u32, usize), ParserErrorKind> {
    let (n, c) = decode_by_unsigned_leb128(reader, 32)?;
    assert!(n < 2 << 32);
    Ok((n as u32, c))
}

fn parse_s32(reader: &mut Read) -> Result<(i32, usize), ParserErrorKind> {
    let (n, c) = decode_by_signed_leb128(reader, 32)?;
    assert!(n < 2 << 32);
    Ok((n as i32, c))
}

fn parse_s64(reader: &mut Read) -> Result<(i64, usize), ParserErrorKind> {
    let (n, c) = decode_by_signed_leb128(reader, 64)?;
    Ok((n as i64, c))
}

fn parse_i32(reader: &mut Read) -> Result<(i32, usize), ParserErrorKind> {
    parse_s32(reader)
}

fn parse_i64(reader: &mut Read) -> Result<(i64, usize), ParserErrorKind> {
    parse_s64(reader)
}

fn parse_vector<T, F>(reader: &mut Read, f: F) -> Result<(Vec<T>, usize), ParserErrorKind>
where
    F: Fn(&mut Read) -> Result<(T, usize), ParserErrorKind>,
{
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
    parse_byte(reader).and_then(|(b, c)| match b {
        0x7F => Ok((Valtype::I32, c)),
        0x7E => Ok((Valtype::I64, c)),
        b => Err(ParserErrorKind::IllegalValtypeFormat(b)),
    })
}

fn parse_blocktype(reader: &mut Read) -> Result<(Resulttype, usize), ParserErrorKind> {
    let (b, c) = parse_byte(reader)?;
    if b == 0x40 {
        Ok((Resulttype::new(None), c))
    } else {
        let mut buf: &[u8] = &[b];
        parse_valtype(buf.by_ref()).map(|p| (Resulttype::new(Some(vec![p.0])), p.1))
    }
}

fn parse_functype(reader: &mut Read) -> Result<(Functype, usize), ParserErrorKind> {
    let c = parse_byte(reader).and_then(|(b, c)| match b {
        0x60 => Ok(c),
        _ => Err(ParserErrorKind::IllegalFunctypeFormat),
    })?;
    let (input_type, c_i) = parse_vector(reader, parse_valtype)?;
    let (output_type, c_o) = parse_vector(reader, parse_valtype)?;
    Ok((Functype::new(input_type, output_type), c + c_i + c_o))
}

fn parse_elemtype(reader: &mut Read) -> Result<(Elemtype, usize), ParserErrorKind> {
    parse_byte(reader).and_then(|(b, c)| match b {
        0x70 => Ok((Elemtype::Anyfunc, c)),
        b => Err(ParserErrorKind::IllegalElemtypeFormat(b)),
    })
}

fn parse_limits(reader: &mut Read) -> Result<(Limits, usize), ParserErrorKind> {
    let (b, c_b) = parse_byte(reader)?;
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
    parse_byte(reader).and_then(|(b, c)| match b {
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

fn parse_memarg(reader: &mut Read) -> Result<(Memarg, usize), ParserErrorKind> {
    let (align, c_a) = parse_u32(reader)?;
    let (offset, c_o) = parse_u32(reader)?;
    Ok((Memarg::new(offset, align), c_a + c_o))
}

fn parse_instrs(
    reader: &mut Read,
    terminal_opcode: BinaryOpcode,
) -> Result<(Vec<WasmInstr>, usize), ParserErrorKind> {
    let mut instrs = vec![];
    let mut consumed = 0;
    loop {
        use self::BinaryOpcode::*;
        let (b, c) = parse_byte(reader)?;
        consumed += c;
        let opcode = match INSTRUCTION_TABLE[b as usize].as_ref() {
            Some(entry) => entry.opcode,
            None => unimplemented!("opcode: 0x{:02X}", b),
        };
        let (instr, c) = match opcode {
            Unreachable => (WasmInstr::Unreachable, 0),
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
            End => if terminal_opcode == End {
                break;
            } else {
                panic!("unexpected terminal opcode")
            },
            Br => parse_labelidx(reader).map(|p| (WasmInstr::Br(p.0), p.1))?,
            BrIf => parse_labelidx(reader).map(|p| (WasmInstr::BrIf(p.0), p.1))?,
            BrTable => {
                let (table, c_t) = parse_vector(reader, parse_labelidx)?;
                let (default, c_d) = parse_labelidx(reader)?;
                (WasmInstr::BrTable { table, default }, c_t + c_d)
            }
            Return => (WasmInstr::Return, 0),
            Call => parse_funcidx(reader).map(|p| (WasmInstr::Call(p.0), p.1))?,
            CallIndirect => {
                let (typeidx, c_t) = parse_typeidx(reader)?;
                let (zero, c_z) = parse_byte(reader)?;
                if zero != 0x00 {
                    return Err(ParserErrorKind::InvalidFormat(format!(
                        "instruction call_indirect must be terminated 0x00, but actual {}",
                        zero
                    )));
                }
                (WasmInstr::CallIndirect(typeidx), c_t + c_z)
            }
            Drop => (WasmInstr::Drop, 0),
            Select => (WasmInstr::Select, 0),
            GetLocal => parse_localidx(reader).map(|p| (WasmInstr::GetLocal(p.0), p.1))?,
            SetLocal => parse_localidx(reader).map(|p| (WasmInstr::SetLocal(p.0), p.1))?,
            TeeLocal => parse_localidx(reader).map(|p| (WasmInstr::TeeLocal(p.0), p.1))?,
            GetGlobal => parse_globalidx(reader).map(|p| (WasmInstr::GetGlobal(p.0), p.1))?,
            SetGlobal => parse_globalidx(reader).map(|p| (WasmInstr::SetGlobal(p.0), p.1))?,
            I32Load => parse_memarg(reader).map(|p| {
                (
                    WasmInstr::Load {
                        attr: Loadattr::I32,
                        arg: p.0,
                    },
                    p.1,
                )
            })?,
            I64Load => parse_memarg(reader).map(|p| {
                (
                    WasmInstr::Load {
                        attr: Loadattr::I64,
                        arg: p.0,
                    },
                    p.1,
                )
            })?,
            I32Load8S => parse_memarg(reader).map(|p| {
                (
                    WasmInstr::Load {
                        attr: Loadattr::I32x8S,
                        arg: p.0,
                    },
                    p.1,
                )
            })?,
            I32Load8U => parse_memarg(reader).map(|p| {
                (
                    WasmInstr::Load {
                        attr: Loadattr::I32x8U,
                        arg: p.0,
                    },
                    p.1,
                )
            })?,
            I32Store => parse_memarg(reader).map(|p| {
                (
                    WasmInstr::Store {
                        attr: Storeattr::I32,
                        arg: p.0,
                    },
                    p.1,
                )
            })?,
            I64Store => parse_memarg(reader).map(|p| {
                (
                    WasmInstr::Store {
                        attr: Storeattr::I64,
                        arg: p.0,
                    },
                    p.1,
                )
            })?,
            I32Store8 => parse_memarg(reader).map(|p| {
                (
                    WasmInstr::Store {
                        attr: Storeattr::I32x8,
                        arg: p.0,
                    },
                    p.1,
                )
            })?,
            I32Const => {
                parse_i32(reader).map(|p| (WasmInstr::Const(Const::I32(p.0 as u32)), p.1))?
            }
            I64Const => {
                parse_i64(reader).map(|p| (WasmInstr::Const(Const::I64(p.0 as u64)), p.1))?
            }
            I32Eqz => (WasmInstr::Itestop(Itestop::Eqz32), 0),
            I32Eq => (WasmInstr::Irelop(Irelop::Eq32), 0),
            I32Ne => (WasmInstr::Irelop(Irelop::Ne32), 0),
            I32LtS => (WasmInstr::Irelop(Irelop::LtS32), 0),
            I32LtU => (WasmInstr::Irelop(Irelop::LtU32), 0),
            I32GtS => (WasmInstr::Irelop(Irelop::GtS32), 0),
            I32GtU => (WasmInstr::Irelop(Irelop::GtU32), 0),
            I32LeS => (WasmInstr::Irelop(Irelop::LeS32), 0),
            I32LeU => (WasmInstr::Irelop(Irelop::LeU32), 0),
            I32GeS => (WasmInstr::Irelop(Irelop::GeS32), 0),
            I32GeU => (WasmInstr::Irelop(Irelop::GeU32), 0),
            I32Clz => (WasmInstr::Iunop(Iunop::Clz32), 0),
            I32Ctz => (WasmInstr::Iunop(Iunop::Ctz32), 0),
            I32Add => (WasmInstr::Ibinop(Ibinop::Add32), 0),
            I32Sub => (WasmInstr::Ibinop(Ibinop::Sub32), 0),
            I32Mul => (WasmInstr::Ibinop(Ibinop::Mul32), 0),
            I32DivU => (WasmInstr::Ibinop(Ibinop::DivU32), 0),
            I32And => (WasmInstr::Ibinop(Ibinop::And32), 0),
            I32Or => (WasmInstr::Ibinop(Ibinop::Or32), 0),
            I32Xor => (WasmInstr::Ibinop(Ibinop::Xor32), 0),
            I32Shl => (WasmInstr::Ibinop(Ibinop::Shl32), 0),
            I32ShrS => (WasmInstr::Ibinop(Ibinop::ShrS32), 0),
            I32ShrU => (WasmInstr::Ibinop(Ibinop::ShrU32), 0),
            I64Or => (WasmInstr::Ibinop(Ibinop::Or64), 0),
            I64Shl => (WasmInstr::Ibinop(Ibinop::Shl64), 0),
            I64ShrU => (WasmInstr::Ibinop(Ibinop::ShrU64), 0),
            I32WrapI64 => (
                WasmInstr::Cvtop {
                    op: Cvtop::Wrap,
                    dst_type: Valtype::I32,
                    src_type: Valtype::I64,
                },
                0,
            ),
            I64ExtendUI32 => (
                WasmInstr::Cvtop {
                    op: Cvtop::ExtendU,
                    dst_type: Valtype::I64,
                    src_type: Valtype::I32,
                },
                0,
            ),
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
    parse_byte(reader).and_then(|(b, c)| match b {
        0x00 => parse_funcidx(reader).map(|p| (Exportdesc::Func(p.0), c + p.1)),
        0x01 => parse_tableidx(reader).map(|p| (Exportdesc::Table(p.0), c + p.1)),
        0x02 => parse_memidx(reader).map(|p| (Exportdesc::Mem(p.0), c + p.1)),
        0x03 => parse_globalidx(reader).map(|p| (Exportdesc::Global(p.0), c + p.1)),
        b => Err(ParserErrorKind::IllegalExportdescFormat(b)),
    })
}

fn parse_importdesc(reader: &mut Read) -> Result<(Importdesc, usize), ParserErrorKind> {
    parse_byte(reader).and_then(|(b, c)| match b {
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
    reader
        .read_exact(&mut buf)
        .map_err(|_| ParserErrorKind::UnexpectedFileTermination)?;
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

fn parse_data(reader: &mut Read) -> Result<(Data, usize), ParserErrorKind> {
    let (data, c_d) = parse_memidx(reader)?;
    let (offset, c_o) = parse_expr(reader)?;
    let (init, c_i) = parse_vector(reader, parse_byte)?;
    Ok((Data::new(data, offset, init), c_d + c_o + c_i))
}

fn parse_type_section(
    reader: &mut Read,
    _size: usize,
) -> Result<(Vec<Functype>, usize), ParserErrorKind> {
    parse_vector(reader, parse_functype)
}

fn parse_import_section(
    reader: &mut Read,
    _size: usize,
) -> Result<(Vec<Import>, usize), ParserErrorKind> {
    parse_vector(reader, parse_import)
}

fn parse_function_section(
    reader: &mut Read,
    _size: usize,
) -> Result<(Vec<Typeidx>, usize), ParserErrorKind> {
    parse_vector(reader, parse_typeidx)
}

fn parse_table_section(
    reader: &mut Read,
    _size: usize,
) -> Result<(Vec<Table>, usize), ParserErrorKind> {
    parse_vector(reader, parse_table)
}

fn parse_memory_section(
    reader: &mut Read,
    _size: usize,
) -> Result<(Vec<Mem>, usize), ParserErrorKind> {
    parse_vector(reader, parse_mem)
}

fn parse_global_section(
    reader: &mut Read,
    _size: usize,
) -> Result<(Vec<Global>, usize), ParserErrorKind> {
    parse_vector(reader, parse_global)
}

fn parse_export_section(
    reader: &mut Read,
    _size: usize,
) -> Result<(Vec<Export>, usize), ParserErrorKind> {
    parse_vector(reader, parse_export)
}

fn parse_element_section(
    reader: &mut Read,
    _size: usize,
) -> Result<(Vec<Elem>, usize), ParserErrorKind> {
    parse_vector(reader, parse_elem)
}

fn parse_code_section(
    reader: &mut Read,
    _size: usize,
) -> Result<(Vec<Code>, usize), ParserErrorKind> {
    parse_vector(reader, parse_code)
}

fn parse_data_section(
    reader: &mut Read,
    _size: usize,
) -> Result<(Vec<Data>, usize), ParserErrorKind> {
    parse_vector(reader, parse_data)
}

fn discard_until_next_semantic_section(
    reader: &mut Read,
) -> Result<Option<(u8, usize, usize)>, ParserErrorKind> {
    let mut consumed = 0;
    loop {
        let (section_id, c_i) = match parse_byte(reader) {
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
        reader
            .read_exact(&mut buf)
            .map_err(|_| ParserErrorKind::UnexpectedFileTermination)?;
    }
}

fn section_parser_driver<T, P>(
    reader: &mut Read,
    parser: P,
    section_size: usize,
    storage: &mut Vec<T>,
) -> Result<usize, ParserErrorKind>
where
    P: Fn(&mut Read, usize) -> Result<(Vec<T>, usize), ParserErrorKind>,
{
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
    let mut data_section = vec![];

    let mut last_parsed_section_id = 0;
    while let Some((section_id, section_size, _)) = discard_until_next_semantic_section(reader)? {
        if section_id <= last_parsed_section_id {
            return Err(ParserErrorKind::InvalidFormat(format!(
                "invalid section order, {} after {}",
                section_id, last_parsed_section_id
            )));
        }
        let consumed = match section_id {
            0 => unreachable!(),
            1 => {
                section_parser_driver(reader, parse_type_section, section_size, &mut type_section)?
            }
            2 => section_parser_driver(
                reader,
                parse_import_section,
                section_size,
                &mut import_section,
            )?,
            3 => section_parser_driver(
                reader,
                parse_function_section,
                section_size,
                &mut function_section,
            )?,
            4 => section_parser_driver(
                reader,
                parse_table_section,
                section_size,
                &mut table_section,
            )?,
            5 => section_parser_driver(
                reader,
                parse_memory_section,
                section_size,
                &mut memory_section,
            )?,
            6 => section_parser_driver(
                reader,
                parse_global_section,
                section_size,
                &mut global_section,
            )?,
            7 => section_parser_driver(
                reader,
                parse_export_section,
                section_size,
                &mut export_section,
            )?,
            8 => unimplemented!(),
            9 => section_parser_driver(
                reader,
                parse_element_section,
                section_size,
                &mut element_section,
            )?,
            10 => {
                section_parser_driver(reader, parse_code_section, section_size, &mut code_section)?
            }
            11 => {
                section_parser_driver(reader, parse_data_section, section_size, &mut data_section)?
            }
            _ => {
                return Err(ParserErrorKind::InvalidFormat(format!(
                    "invalid section id {}",
                    section_id
                )))
            }
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
        data_section,
    ))
}

pub fn parse(reader: &mut Read) -> Result<Module, ParserErrorKind> {
    {
        let mut buf = [0; 4];
        match reader.read(&mut buf) {
            Ok(4) if buf[0] == 0x00 && buf[1] == 0x61 && buf[2] == 0x73 && buf[3] == 0x6d => (),
            _ => {
                return Err(ParserErrorKind::InvalidFormat(
                    "field 'magic' is broken".to_string(),
                ))
            }
        };
    }
    {
        let mut buf = [0; 4];
        match reader.read(&mut buf) {
            Ok(4) if buf[0] == 0x01 && buf[1] == 0x00 && buf[2] == 0x00 && buf[3] == 0x00 => (),
            _ => {
                return Err(ParserErrorKind::InvalidFormat(
                    "field 'version' is broken".to_string(),
                ))
            }
        };
    }
    parse_module(reader)
}
