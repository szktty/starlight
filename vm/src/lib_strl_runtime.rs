use heap::Content;
use interp::Interp;
use module::Module;
use std::result::Result;
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

fn nif_create_module(interp: Arc<Interp>, args: &Vec<Value>) -> Result<Value, String> {
    let attrs = args.get(0).unwrap();
    let funs = args.get(1).unwrap();
    let mut m = Module::new();
    for attr in attrs.get_tuple().unwrap() {
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
            _ => return Err("not function".to_string()),
        };
    }
    let obj = interp.heap.create(Content::Module(Arc::new(m)));
    Ok(Value::Module(obj.id))
}
