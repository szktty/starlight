use error::{Error, ErrorKind};
use heap::{Content, Heap};
use list::{List, ListGenerator};
use result::Result;
use std::cell::RefCell;
use std::sync::Arc;
use value::Value;

pub struct ArgList {
    heap: Arc<Heap>,
    pub args: Vec<Value>,
}

impl ArgList {
    pub fn new(heap: Arc<Heap>, arity: usize, args: Vec<Value>) -> Result<ArgList> {
        if arity != args.len() {
            Err(Error::exception(
                ErrorKind::InvalidType,
                format!("takes {} but {}", arity, args.len()),
            ))
        } else {
            Ok(ArgList { heap, args })
        }
    }

    pub fn get(&self, i: usize) -> &Value {
        self.args.get(i).unwrap()
    }

    pub fn get_string(&self, i: usize) -> Result<&String> {
        match self.get(i) {
            Value::String(s) => Ok(s),
            arg => {
                return Err(Error::exception(
                    ErrorKind::InvalidType,
                    format!("not string {:?}", arg),
                ))
            }
        }
    }

    pub fn get_list<'a>(&self, i: usize) -> Result<Arc<List>> {
        match self.get(i) {
            Value::List(id) => match self.heap.get_content(*id).unwrap() {
                Content::List(list) => Ok(list.clone()),
                _ => panic!("content is not list"),
            },
            arg => Err(Error::exception(
                ErrorKind::InvalidType,
                format!("not list {:?}", arg),
            )),
        }
    }

    pub fn get_tuple(&self, i: usize) -> Result<&Vec<Value>> {
        match self.get(i) {
            Value::Tuple(elts) => Ok(elts),
            arg => {
                return Err(Error::exception(
                    ErrorKind::InvalidType,
                    format!("not tuple {:?}", arg),
                ))
            }
        }
    }

    pub fn get_listgen<'a>(&self, heap: &'a Heap, i: usize) -> Result<Arc<RefCell<ListGenerator>>> {
        match self.get(i) {
            Value::ListGenerator(id) => heap.get_listgen(*id),
            _ => None,
        }
        .ok_or(Error::exception(
            ErrorKind::InvalidType,
            format!("not listgen"),
        ))
    }
}
