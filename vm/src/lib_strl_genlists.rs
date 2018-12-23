use module::Module;
use std::cell::RefCell;
use std::rc::Rc;
use std::result::Result;
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

fn nif_create(args: &Vec<Value>) -> Result<Value, String> {
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
    Ok(Value::ListGenerator(Rc::new(RefCell::new(body))))
}

fn nif_next(args: &Vec<Value>) -> Result<Value, String> {
    let gen = match args.get(0).unwrap() {
        Value::ListGenerator(gen) => gen,
        arg => return Err(format!("not list generator {:?}", arg)),
    };
    let mut genref = gen.borrow_mut();
    match genref.next() {
        None => Ok(Value::Nil),
        Some(values) => Ok(Value::Tuple(Rc::new(values))),
    }
}

fn nif_add(args: &Vec<Value>) -> Result<Value, String> {
    let gen = match args.get(0).unwrap() {
        Value::ListGenerator(gen) => gen,
        arg => return Err(format!("not list generator {:?}", arg)),
    };
    let value = args.get(1).unwrap();
    let mut genref = gen.borrow_mut();
    genref.add(value.clone());
    Ok(Value::Nil)
}

fn nif_collect(args: &Vec<Value>) -> Result<Value, String> {
    let gen = match args.get(0).unwrap() {
        Value::ListGenerator(gen) => gen,
        arg => return Err(format!("not list generator {:?}", arg)),
    };
    let value = gen.borrow().collect();
    Ok(Value::List(Rc::new(List::from_list(&value))))
}
