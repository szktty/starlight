use heap::{Br, Content, Heap, ObjectId, ToValue};
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

pub enum BrList {}

pub enum Cell {
    Proper(Value),
    Improper(Value, Value),
}

pub struct Iter {
    heap: Arc<Heap>,
    cur: Option<Value>,
}

#[derive(Debug, Clone)]
pub struct ListGenerator {
    lists: Vec<Br<List>>,
    sum: usize,
    i: usize,
    accu: Vec<Value>,
}

impl List {
    pub fn get_content(heap: &Heap, value: &Value) -> Option<Arc<List>> {
        match value {
            Value::List(id) => heap.get_content(*id).and_then(|c| match c {
                Content::List(list) => Some(list.clone()),
                _ => None,
            }),
            _ => None,
        }
    }

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
        List::get_content(heap, &list).unwrap().clone()
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
            List::Nil => Some(vec![heap.get_list_nil().1]),
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
}

impl ToValue for List {
    fn to_value(&self, heap: &Heap) -> Value {
        Value::List(self.get_id(heap))
    }
}

impl BrList {
    pub fn from_value(heap: Arc<Heap>, value: &Value) -> Option<Br<List>> {
        Some(Br::new(heap.clone(), List::get_content(&*heap, value)?))
    }

    pub fn id(list: &Br<List>) -> ObjectId {
        list.data().get_id(list.heap())
    }

    pub fn to_value(list: &Br<List>) -> Value {
        Value::List(BrList::id(list))
    }

    pub fn iter(list: &Br<List>) -> Iter {
        Iter {
            heap: list.heap_ref().clone(),
            cur: Some(list.to_value()),
        }
    }

    pub fn len(list: &Br<List>) -> Option<usize> {
        let mut len = 0;
        for cell in BrList::iter(list) {
            match cell {
                Cell::Proper(..) => {
                    len += 1;
                }
                Cell::Improper(..) => return None,
            }
        }
        Some(len)
    }

    pub fn get(list: &Br<List>, i: usize) -> Option<Value> {
        let mut j = 0;
        for cell in BrList::iter(list) {
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
}

impl Iterator for Iter {
    type Item = Cell;

    fn next(&mut self) -> Option<Cell> {
        match self.cur.clone() {
            None => None,
            Some(cur) => match List::get_content(&*self.heap, &cur) {
                None => None,
                Some(cur) => match *cur.clone() {
                    List::Nil => None,
                    List::Cons {
                        ref value,
                        ref next,
                        ..
                    } => match List::get_content(&*self.heap, &next) {
                        Some(_) => {
                            self.cur = Some(next.clone());
                            Some(Cell::Proper(value.clone()))
                        }
                        None => {
                            self.cur = None;
                            Some(Cell::Improper(value.clone(), next.clone()))
                        }
                    },
                },
            },
        }
    }
}

impl ListGenerator {
    pub fn new(lists: Vec<Br<List>>) -> ListGenerator {
        let mut sum = 0;
        for list in lists.iter() {
            match BrList::len(list) {
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
            match BrList::len(&list) {
                None => return None,
                Some(0) => return None,
                Some(len) => {
                    let j = if i == 0 { 0 } else { i - (i / len) * len };
                    match BrList::get(&list, j) {
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
