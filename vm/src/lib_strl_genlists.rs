use interp::Interp;
use module::Module;
use std::cell::RefCell;
use std::result::Result;
use std::sync::{Arc, Mutex};
use value::{List, ListGenerator, Value};

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

fn nif_create(_interp: Arc<Interp>, args: &Vec<Value>) -> Result<Value, String> {
    let arg = args.get(0).unwrap();
    let mut lists: Vec<List> = Vec::new();
    match arg {
        Value::Tuple(tuple) => {
            for e in tuple.iter() {
                match e {
                    Value::List(e) => lists.push((**e).clone()),
                    _ => return Err(format!("not list {:?}", e)),
                }
            }
        }
        _ => return Err(format!("not tuple {:?}", arg)),
    }
    let body = ListGenerator::new(lists);
    Ok(Value::ListGenerator(Arc::new(Mutex::new(RefCell::new(
        body,
    )))))
}

fn nif_next(_interp: Arc<Interp>, args: &Vec<Value>) -> Result<Value, String> {
    let gen = match args.get(0).unwrap() {
        Value::ListGenerator(gen) => gen,
        arg => return Err(format!("not list generator {:?}", arg)),
    };
    let lock = gen.lock().unwrap();
    let mut genref = lock.borrow_mut();
    match genref.next() {
        None => Ok(Value::Nil),
        Some(values) => Ok(Value::Tuple(Arc::new(values))),
    }
}

fn nif_add(_interp: Arc<Interp>, args: &Vec<Value>) -> Result<Value, String> {
    let gen = match args.get(0).unwrap() {
        Value::ListGenerator(gen) => gen,
        arg => return Err(format!("not list generator {:?}", arg)),
    };
    let value = args.get(1).unwrap();
    let lock = gen.lock().unwrap();
    let mut genref = lock.borrow_mut();
    genref.add(value.clone());
    Ok(Value::Nil)
}

fn nif_collect(_interp: Arc<Interp>, args: &Vec<Value>) -> Result<Value, String> {
    let gen = match args.get(0).unwrap() {
        Value::ListGenerator(gen) => gen,
        arg => return Err(format!("not list generator {:?}", arg)),
    };
    let lock = gen.lock().unwrap();
    let value = lock.borrow().collect();
    Ok(Value::List(Arc::new(List::from_list(&value))))
}
