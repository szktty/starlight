use arglist::ArgList;
use error::{Error, ErrorKind};
use heap::Content;
use interp::Interp;
use module::Module;
use process::Process;
use result::Result;
use std::sync::Arc;
use value::Value;

pub fn new() -> Module {
    let mut m = Module::with_name("strl_runtime");
    m.fields.insert(
        "create_module".to_string(),
        Value::nif(1, nif_create_module),
    );
    m
}

fn nif_create_module(interp: &Arc<Interp>, proc: &Arc<Process>, args: &ArgList) -> Result<Value> {
    let attrs = try!(args.get_tuple(0));
    let funs = args.get(1);
    let mut m = Module::new();
    for attr in attrs {
        let elts = attr.get_tuple().unwrap();
        let name = elts.get(0).unwrap().get_string().unwrap();
        let value = elts.get(1).unwrap();
        match &**name {
            "module" => {
                let mname = value.get_string().unwrap();
                m.set_name(mname);
            }
            _ => (),
        }
    }
    for fun in funs.get_tuple().unwrap() {
        let elts = fun.get_tuple().unwrap();
        let name = elts.get(0).unwrap().get_string().unwrap();
        let code = elts.get(1).unwrap();
        match code {
            Value::CompiledCode(_) | Value::Nif(_) => m.fields.insert(name.clone(), code.clone()),
            _ => {
                return Err(Error::exception(
                    ErrorKind::InvalidType,
                    "not function".to_string(),
                ))
            }
        };
    }
    Ok(Value::Module(
        proc.heap.create(|_| Content::Module(Arc::new(m))),
    ))
}
