use arglist::ArgList;
use error::{Error, ErrorKind};
use heap::{Br, Content};
use interp::Interp;
use list::{BrList, List, ListGenerator};
use module::{ModuleBuilder, ModuleDesc};
use process::Process;
use result::Result;
use std::cell::RefCell;
use std::sync::Arc;
use value::Value;

pub fn new() -> ModuleDesc {
    let mut build = ModuleBuilder::new("strl_genlists");
    build.add_nif("create", nif_create, 1);
    build.add_nif("next", nif_next, 1);
    build.add_nif("add", nif_add, 2);
    build.add_nif("collect", nif_collect, 1);
    build.to_desc()
}

fn nif_create(interp: &Arc<Interp>, proc: &Arc<Process>, args: &ArgList) -> Result<Value> {
    let tuple = try!(args.get_tuple(0));
    let mut lists: Vec<Br<List>> = Vec::new();
    for e in tuple.iter() {
        match BrList::from_value(proc.heap.clone(), e) {
            Some(br) => {
                lists.push(br);
            }
            _ => {
                return Err(Error::exception(
                    ErrorKind::InvalidType,
                    format!("not list {:?}", e),
                ))
            }
        }
    }
    let gen = ListGenerator::new(lists);
    let id = interp
        .heap
        .create(|_| Content::ListGenerator(Arc::new(RefCell::new(gen))));
    Ok(Value::ListGenerator(id))
}

fn nif_next(_interp: &Arc<Interp>, proc: &Arc<Process>, args: &ArgList) -> Result<Value> {
    let gen = try!(args.get_listgen(&proc.heap, 0));
    let mut genref = (*gen).borrow_mut();
    match genref.next() {
        None => Ok(Value::Nil),
        Some(values) => Ok(Value::Tuple(Arc::new(values))),
    }
}

fn nif_add(_interp: &Arc<Interp>, proc: &Arc<Process>, args: &ArgList) -> Result<Value> {
    let gen = try!(args.get_listgen(&proc.heap, 0));
    let value = args.get(1);
    let mut genref = (*gen).borrow_mut();
    genref.add(value.clone());
    Ok(Value::Nil)
}

fn nif_collect(_interp: &Arc<Interp>, proc: &Arc<Process>, args: &ArgList) -> Result<Value> {
    let gen = try!(args.get_listgen(&proc.heap, 0));
    let genref = (*gen).borrow();
    Ok(List::value_from_list(&proc.heap, genref.collect()))
}
