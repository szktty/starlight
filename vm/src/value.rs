use module::Module;
use opcode::Opcode;
use std::boxed::Box;
use std::fmt;
use std::rc::Rc;
use std::result::Result;

// TODO: 1 word
#[derive(Debug, Clone)]
pub enum Value {
    Atom(Rc<String>),
    Bool(bool),
    Int(i64),
    String(Rc<String>),
    Nil,
    List(Box<List>),
    CompiledCode(Rc<CompiledCode>),
    Nif(Rc<Nif>),
    Closure(Rc<Closure>),
    Module(Rc<Module>),
    ModuleName(Rc<String>),
    Binary(Rc<Vec<Value>>),
    Array(Rc<Vec<Value>>),
    Bitstr32(u8, u32),
    Bitstr(Rc<Bitstr>),
}

#[derive(Debug, Clone)]
pub enum List {
    Cons { value: Value, next: Value },
    Nil,
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
    pub fun: Rc<CompiledCode>,
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

pub type NifFun = fn(args: &Vec<Value>) -> Result<Value, String>;

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
        Value::Nif(Rc::new(Nif { arity, fun }))
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
    pub fn eq(&self, other: &Value) -> bool {
        match (self, other) {
            (Value::Int(a), Value::Int(b)) => a == b,
            _ => false,
        }
    }
    pub fn ne(&self, other: &Value) -> bool {
        !self.eq(other)
    }

    pub fn to_string(&self) -> String {
        match self {
            Value::Atom(s) => format!("{}", s),
            Value::Bool(v) => format!("{}", v),
            Value::Int(v) => format!("{}", v),
            Value::String(s) => format!("{:?}", s),
            Value::List(list) => list.to_string(),
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

impl List {
    pub fn nil_value() -> Value {
        Value::List(Box::new(List::Nil))
    }

    pub fn new(value: Value, next: Value) -> List {
        List::Cons { value, next }
    }

    pub fn from_list(values: &Vec<Value>) -> List {
        values
            .iter()
            .rev()
            .fold(List::Nil, |next, value| List::Cons {
                value: value.clone(),
                next: next.to_value(),
            })
    }

    pub fn to_value(self) -> Value {
        Value::List(Box::new(self))
    }

    pub fn length(&self) -> Option<usize> {
        let mut length: usize = 0;
        let mut e = self;
        loop {
            match e {
                List::Cons { next, .. } => match next {
                    Value::List(list) => {
                        length += 1;
                        e = list;
                    }
                    _ => return None,
                },
                List::Nil => return Some(length),
            }
        }
    }

    pub fn get(&self, i: usize) -> Option<Value> {
        let mut j: usize = 0;
        let mut e = self;
        loop {
            match e {
                List::Cons { value, next } => match next {
                    Value::List(list) => {
                        if i == j {
                            return Some(value.clone());
                        }
                        j += 1;
                        e = list;
                    }
                    _ => return None,
                },
                List::Nil => return None,
            }
        }
    }

    pub fn to_string(&self) -> String {
        let mut elts: Vec<String> = Vec::new();
        let mut last: Option<String> = None;
        let mut e = self;
        loop {
            match e {
                List::Cons { value, next } => {
                    elts.push(value.to_string());
                    match next {
                        Value::List(list) => {
                            e = list;
                        }
                        _ => {
                            last = Some(next.to_string());
                            break;
                        }
                    }
                }
                List::Nil => break,
            }
        }
        format!(
            "[{}{}]",
            elts.join(", "),
            match last {
                None => "".to_string(),
                Some(e) => format!("|{}", e),
            }
        )
    }
}
