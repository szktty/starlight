// bytecode file

use opcode::{BitstrEndian, BitstrSign, BitstrType, BlockTag, Opcode};
use serde_json;
use serde_json::Result;
use std::fs::File;
use std::io::prelude::*;
use std::sync::Arc;
use value;
use value::{Bitstr, CompiledCode, List, Value};

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct Bytecode {
    pub version: usize,
    pub module: String,
    pub main: BcCompiledCode,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct BcCompiledCode {
    pub name: Option<String>,
    pub arity: usize,
    pub consts: Vec<BcConst>,
    pub opcodes: Vec<Opcode>,
    pub locals: usize,
    pub frame: usize,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub enum BcConst {
    Atom(String),
    Int(String),
    String(String),
    Function(BcCompiledCode),
    Bitstr(u32, u64, BitstrType, BitstrSign, BitstrEndian, Option<u8>),
    Block(BlockTag, Vec<BcConst>),
}

pub fn from_file(file: &str) -> Result<Bytecode> {
    let mut file = File::open(file).expect("cannot open");
    let mut buf = String::new();
    file.read_to_string(&mut buf).unwrap();
    serde_json::from_str(&buf)
}

impl BcConst {
    pub fn to_value(&self) -> Value {
        match self {
            BcConst::Atom(value) => Value::Atom(Arc::new(value.clone())),
            BcConst::Int(value) => Value::Int(value.parse().unwrap()),
            BcConst::String(value) => Value::String(Arc::new(value.clone())),
            BcConst::Function(value) => {
                let code = value.to_value_code();
                Value::CompiledCode(Arc::new(code.clone()))
            }
            BcConst::Bitstr(size, value, _ty, _sign, _endian, _unit) => {
                // TODO
                // Bitstr.from_spec(size,value,ty,sign,endian,unit)
                Value::Bitstr(Arc::new(Bitstr {
                    size: *size,
                    value: *value,
                }))
            }
            BcConst::Block(BlockTag::Binary, vals) => {
                Value::Binary(Arc::new(vals.iter().map(|val| val.to_value()).collect()))
            }
            BcConst::Block(BlockTag::List, vals) => {
                let elts = vals.iter().map(|val| val.to_value()).collect();
                Value::List(Arc::new(List::from_list(&elts)))
            }
            _ => panic!("# not impl {:?}", self),
        }
    }
}

impl BcCompiledCode {
    pub fn to_value_code(&self) -> value::CompiledCode {
        let consts: Vec<Value> = self
            .consts
            .iter()
            .map(|cons| BcConst::to_value(cons))
            .collect();
        CompiledCode {
            name: self.name.clone(),
            arity: self.arity,
            consts,
            ops: self.opcodes.clone(),
            locals: self.locals,
            frame: self.frame,
        }
    }
}
