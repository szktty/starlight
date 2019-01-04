use arglist::ArgList;
use error::{Error, ErrorKind};
use heap::{Content, Heap};
use interp::Interp;
use list::{List, ListGenerator};
use module::Module;
use result::Result;
use std::cell::RefCell;
use std::sync::{Arc, Mutex};
use value::Value;

pub fn new() -> Module {
    let mut m = Module::with_name("strl_genlists");
    m.fields
        .insert("create".to_string(), Value::nif(1, nif_create));
    m.fields.insert("next".to_string(), Value::nif(1, nif_next));
    m.fields.insert("add".to_string(), Value::nif(2, nif_add));
    m.fields
        .insert("collect".to_string(), Value::nif(1, nif_collect));
    m
}

fn nif_create(interp: Arc<Interp>, args: &ArgList) -> Result<Value> {
    let tuple = try!(args.get_tuple(0));
    let mut lists: Vec<List> = Vec::new();
    for e in tuple.iter() {
        match e {
            Value::List(id) => {
                let list = interp.heap.get_content_list(*id).unwrap();
                lists.push((*list).clone());
            }
            _ => {
                return Err(Error::exception(
                    ErrorKind::InvalidType,
                    format!("not list {:?}", e),
                ))
            }
        }
    }
    let gen = ListGenerator::new(interp.heap.clone(), lists);
    let id = interp
        .heap
        .create(|id| Content::ListGenerator(Arc::new(RefCell::new(gen))));
    Ok(Value::ListGenerator(id))
}

fn nif_next(interp: Arc<Interp>, args: &ArgList) -> Result<Value> {
    let mut gen = try!(args.get_listgen(&interp.heap, 0));
    let mut genref = (*gen).borrow_mut();
    match genref.next() {
        None => Ok(Value::Nil),
        Some(values) => Ok(Value::Tuple(Arc::new(values))),
    }
}

fn nif_add(interp: Arc<Interp>, args: &ArgList) -> Result<Value> {
    let mut gen = try!(args.get_listgen(&interp.heap, 0));
    let value = args.get(1);
    let mut genref = (*gen).borrow_mut();
    genref.add(value.clone());
    Ok(Value::Nil)
}

fn nif_collect(interp: Arc<Interp>, args: &ArgList) -> Result<Value> {
    let mut gen = try!(args.get_listgen(&interp.heap, 0));
    let mut genref = (*gen).borrow_mut();
    Ok(List::value_from_list(&interp.heap, genref.collect()))
}
