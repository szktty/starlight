use arglist::ArgList;
use heap::{Heap, ObjectId};
use interp::Interp;
use list::{BrList, Cell};
use opcode::Opcode;
use process::Process;
use result::Result;
use std::fmt;
use std::sync::Arc;

// TODO: 1 word
#[derive(Debug, Clone)]
pub enum Value {
    Atom(Arc<String>),
    Bool(bool),
    Int(i64),
    String(Arc<String>),
    Nil,
    List(ObjectId),
    Tuple(Arc<Vec<Value>>),
    CompiledCode(Arc<CompiledCode>),
    Nif(Arc<Nif>),
    Closure(Arc<Closure>),
    Module(ObjectId),
    Binary(Arc<Vec<Value>>),
    Array(Arc<Vec<Value>>),
    Bitstr32(u8, u32),
    Bitstr(Arc<Bitstr>),
    ListGenerator(ObjectId),
}

#[derive(Debug, Clone)]
pub struct Bitstr {
    // TODO: Vec<u8>
    pub value: u64,
    pub size: u32,
}

pub trait Callable {
    fn call();
}

#[derive(Debug, Clone)]
pub struct Closure {
    pub fun: Arc<CompiledCode>,
}

#[derive(Clone)]
// TODO: exception table
pub struct CompiledCode {
    pub name: Option<String>,
    pub arity: usize,
    pub consts: Vec<Value>,
    pub ops: Vec<Opcode>,
    pub locals: usize,
    pub frame: usize,
}

impl fmt::Debug for CompiledCode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "CompiledCode({:?})", self.name)
    }
}

pub type NifFun = fn(interp: &Arc<Interp>, proc: &Arc<Process>, args: &ArgList) -> Result<Value>;

#[derive(Clone)]
pub struct Nif {
    pub arity: usize,
    pub fun: NifFun,
}

impl fmt::Debug for Nif {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Nif/{}", self.arity)
    }
}

impl Value {
    pub fn nif(arity: usize, fun: NifFun) -> Value {
        Value::Nif(Arc::new(Nif { arity, fun }))
    }

    pub fn callable(&self) -> bool {
        match self {
            Value::CompiledCode(_) | Value::Nif(_) => true,
            _ => false,
        }
    }

    pub fn get_string(&self) -> Option<&String> {
        match self {
            Value::Atom(s) => Some(s),
            Value::String(s) => Some(s),
            _ => None,
        }
    }

    pub fn get_tuple(&self) -> Option<&Vec<Value>> {
        match self {
            Value::Tuple(list) => Some(list),
            _ => None,
        }
    }

    pub fn eq(&self, other: &Value) -> bool {
        match (self, other) {
            (Value::Int(a), Value::Int(b)) => a == b,
            _ => false,
        }
    }

    pub fn ne(&self, other: &Value) -> bool {
        !self.eq(other)
    }

    pub fn to_string(&self, heap: &Arc<Heap>) -> String {
        match self {
            Value::Atom(s) => format!("{}", s),
            Value::Bool(v) => format!("{}", v),
            Value::Int(v) => format!("{}", v),
            Value::String(s) => format!("{:?}", s),
            Value::List(_) => {
                let br = BrList::from_value(heap.clone(), self).unwrap();
                let mut buf: Vec<String> = Vec::new();
                for cell in BrList::iter(&br) {
                    match cell {
                        Cell::Proper(e) => buf.push(e.to_string(heap)),
                        Cell::Improper(hd, tl) => {
                            buf.push(format!("{}|{}", hd.to_string(heap), tl.to_string(heap)))
                        }
                    }
                }
                format!("[{}]", buf.join(", "))
            }
            _ => format!("{:?}", self),
        }
    }
}

impl CompiledCode {
    #[inline]
    pub fn get_const(&self, i: usize) -> Option<&Value> {
        self.consts.get(i)
    }
}

impl Bitstr {
    /*
    #[inline]
    pub fn new(size)
    */
}
