use error::{Error, ErrorKind};
use heap::{Content, Heap, Object, ObjectId};
use result::Result;
use std::iter::Iterator;
use std::sync::Arc;
use value::Value;

#[derive(Debug, Clone)]
pub enum List {
    Cons {
        id: ObjectId,
        value: Value,
        next: Value,
    },
    Nil,
}

pub struct Iter {
    heap: Arc<Heap>,
    cur: Arc<List>,
}

pub enum Cell {
    Proper(Value),
    Improper(Value, Value),
}

#[derive(Debug, Clone)]
pub struct ListGenerator {
    heap: Arc<Heap>,
    lists: Vec<List>,
    sum: usize,
    i: usize,
    accu: Vec<Value>,
}

impl List {
    pub fn new_value(heap: &Heap, value: Value, next: Value) -> Value {
        let id = heap.create(|id| {
            let list = List::Cons {
                id,
                value: value.clone(),
                next,
            };
            Content::List(Arc::new(list))
        });
        Value::List(id)
    }

    pub fn from_list(heap: &Heap, values: Vec<Value>) -> Arc<List> {
        let list = List::value_from_list(heap, values);
        heap.get_list(&list).unwrap().clone()
    }

    pub fn value_from_list(heap: &Heap, values: Vec<Value>) -> Value {
        values
            .iter()
            .rev()
            .fold(heap.get_list_nil().1, |next, value| {
                let id = heap.create(|id| {
                    let list = List::Cons {
                        id,
                        value: value.clone(),
                        next,
                    };
                    Content::List(Arc::new(list))
                });
                Value::List(id)
            })
    }

    pub fn get_id(&self, heap: &Heap) -> ObjectId {
        match self {
            List::Cons { id, .. } => *id,
            List::Nil => heap.get_list_nil().0,
        }
    }

    pub fn to_vec(&self, heap: &Heap) -> Option<Vec<Value>> {
        match self {
            List::Nil => None,
            List::Cons { value, next, .. } => {
                let mut vec = Vec::new();
                vec.push(value.clone());
                let mut e = next.clone();
                loop {
                    match e {
                        Value::List(id) => match heap.get_content(id).unwrap() {
                            Content::List(ref list) => match *list.clone() {
                                List::Cons {
                                    ref value,
                                    ref next,
                                    ..
                                } => {
                                    vec.push(value.clone());
                                    e = next.clone();
                                }
                                List::Nil => return Some(vec),
                            },
                            _ => return None,
                        },
                        _ => return None,
                    }
                }
            }
        }
    }

    pub fn iter(&self, heap: Arc<Heap>) -> Iter {
        Iter::new(heap, Arc::new(self.clone()))
    }

    pub fn len(&self, heap: Arc<Heap>) -> Option<usize> {
        let mut len = 0;
        for cell in self.iter(heap) {
            match cell {
                Cell::Proper(..) => {
                    len += 1;
                }
                Cell::Improper(..) => return None,
            }
        }
        Some(len)
    }

    pub fn get(&self, heap: Arc<Heap>, i: usize) -> Option<Value> {
        let mut j = 0;
        for cell in self.iter(heap) {
            if i == j {
                return Some(match cell {
                    Cell::Proper(value) => value,
                    Cell::Improper(value, _) => value,
                });
            }
            j += 1
        }
        None
    }

    /*
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

    */
}

impl Iter {
    fn new(heap: Arc<Heap>, list: Arc<List>) -> Iter {
        Iter { heap, cur: list }
    }
}

impl Iterator for Iter {
    type Item = Cell;

    fn next(&mut self) -> Option<Cell> {
        match (*self.cur).clone() {
            List::Nil => None,
            List::Cons { value, next, .. } => match self.heap.get_list(&next) {
                Some(next) => {
                    self.cur = next.clone();
                    Some(Cell::Proper(value.clone()))
                }
                None => Some(Cell::Improper(value.clone(), next.clone())),
            },
            _ => panic!("not list"),
        }
    }
}

impl ListGenerator {
    pub fn new(heap: Arc<Heap>, lists: Vec<List>) -> ListGenerator {
        let mut sum = 0;
        for list in lists.iter() {
            match list.len(heap.clone()) {
                None => {
                    sum = 0;
                    break;
                }
                Some(len) => sum += len,
            }
        }
        ListGenerator {
            heap,
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
            match list.len(self.heap.clone()) {
                None => return None,
                Some(0) => return None,
                Some(len) => {
                    let j = if i == 0 { 0 } else { i - (i / len) * len };
                    match list.get(self.heap.clone(), j) {
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
