use heap::ObjectId;
use module::Module;
use opcode::Opcode;
use std::cell::RefCell;
use std::fmt;
use std::iter::Iterator;
use std::result::Result;
use std::sync::{Arc, Mutex};

// TODO: 1 word
#[derive(Debug, Clone)]
pub enum Value {
    Atom(Arc<String>),
    Bool(bool),
    Int(i64),
    String(Arc<String>),
    Nil,
    List(Arc<List>),
    Tuple(Arc<Vec<Value>>),
    CompiledCode(Arc<CompiledCode>),
    Nif(Arc<Nif>),
    Closure(Arc<Closure>),
    Module(ObjectId),
    Binary(Arc<Vec<Value>>),
    Array(Arc<Vec<Value>>),
    Bitstr32(u8, u32),
    Bitstr(Arc<Bitstr>),
    ListGenerator(Arc<Mutex<RefCell<ListGenerator>>>),
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
        Value::List(Arc::new(List::Nil))
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
        Value::List(Arc::new(self))
    }

    pub fn to_vec(&self) -> Option<Vec<Value>> {
        let mut vec = Vec::new();
        let mut e = self;
        loop {
            match e {
                List::Cons { value, next } => match next {
                    Value::List(list) => {
                        vec.push(value.clone());
                        e = list;
                    }
                    _ => return None,
                },
                List::Nil => return Some(vec),
            }
        }
    }

    pub fn iter(&self) -> ListIter {
        ListIter::new(Value::List(Arc::new(self.clone())))
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

pub struct ListIter {
    cur: Value,
}

impl ListIter {
    fn new(list: Value) -> ListIter {
        ListIter { cur: list }
    }
}

impl Iterator for ListIter {
    type Item = Value;

    fn next(&mut self) -> Option<Value> {
        let (value, next) = {
            match &self.cur {
                Value::List(list) => match &(**list) {
                    List::Nil => return None,
                    List::Cons { value, next } => (value.clone(), next.clone()),
                },
                _ => return None,
            }
        };
        self.cur = next;
        Some(value)
    }
}

#[derive(Debug, Clone)]
pub struct ListGenerator {
    lists: Vec<List>,
    sum: usize,
    i: usize,
    accu: Vec<Value>,
}

impl ListGenerator {
    pub fn new(lists: Vec<List>) -> ListGenerator {
        let mut sum = 0;
        for list in lists.iter() {
            match list.length() {
                None => {
                    sum = 0;
                    break;
                }
                Some(len) => sum += len,
            }
        }
        ListGenerator {
            lists,
            sum,
            i: 0,
            accu: Vec::new(),
        }
    }

    pub fn next(&mut self) -> Option<Vec<Value>> {
        if self.i >= self.sum {
            return None;
        }
        let mut i = self.i;
        let mut elts: Vec<Value> = Vec::with_capacity(self.lists.len());
        for list in self.lists.iter() {
            match list.length() {
                None => return None,
                Some(0) => return None,
                Some(len) => {
                    let j = if i == 0 { 0 } else { i - (i / len) * len };
                    match list.get(j) {
                        None => return None,
                        Some(elt) => {
                            elts.push(elt.clone());
                            i = if i == 0 { 0 } else { len / i };
                        }
                    }
                }
            }
        }
        self.i += 1;
        Some(elts)
    }

    pub fn add(&mut self, value: Value) {
        self.accu.push(value)
    }

    pub fn collect(&self) -> Vec<Value> {
        self.accu.clone()
    }
}
