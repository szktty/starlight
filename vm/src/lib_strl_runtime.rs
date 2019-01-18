use arglist::ArgList;
use error::{Error, ErrorKind};
use heap::Content;
use interp::Interp;
use module::{Module, ModuleBuilder, ModuleDesc};
use process::Process;
use result::Result;
use std::collections::HashMap;
use std::sync::Arc;
use value::Value;

pub fn new() -> ModuleDesc {
    let mut build = ModuleBuilder::new("strl_runtime");
    build.add_nif("create_module", nif_create_module, 1);
    build.to_desc()
}

fn nif_create_module(_interp: &Arc<Interp>, proc: &Arc<Process>, args: &ArgList) -> Result<Value> {
    let mut name = "";
    let attrs = try!(args.get_tuple(0));
    let mut fields = HashMap::new();
    let funs = args.get(1);
    for attr in attrs {
        let elts = attr.get_tuple().unwrap();
        let aname = elts.get(0).unwrap().get_string().unwrap();
        let value = elts.get(1).unwrap();
        match &**aname {
            "module" => {
                name = value.get_string().unwrap();
            }
            _ => (),
        }
    }
    for fun in funs.get_tuple().unwrap() {
        let elts = fun.get_tuple().unwrap();
        let name = elts.get(0).unwrap().get_string().unwrap();
        let code = elts.get(1).unwrap();
        match code {
            Value::CompiledCode(_) | Value::Nif(_) => {
                println!("# module fun {:?}",name);
                fields.insert(name.clone(), code.clone())
            }
            _ => {
                return Err(Error::exception(
                    ErrorKind::InvalidType,
                    "not function".to_string(),
                ))
            }
        };
    }
    Ok(Value::Module(proc.heap.create(|id| {
        let m = Module::new(id, ModuleDesc::new(&name, fields));
        Content::Module(Arc::new(m))
    })))
}
