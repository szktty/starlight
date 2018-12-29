use interp::Interp;
use module::Module;
use std::result::Result;
use std::sync::Arc;
use value::Value;

pub fn new() -> Module {
    let mut m = Module::with_name("io");
    m.fields
        .insert("fwrite".to_string(), Value::nif(1, nif_fwrite));
    m
}

fn nif_fwrite(_interp: Arc<Interp>, args: &Vec<Value>) -> Result<Value, String> {
    let fmt_val = args.get(0).unwrap();
    match fmt_val.get_string() {
        None => {
            // TODO: bad arg
            println!("{}", fmt_val.to_string());
            return Ok(Value::Nil);
        }
        Some(fmt) => {
            let fmt = fmt.replace("\\n", "\n");
            print!("{}", fmt);
            Ok(Value::Nil)
        }
    }
}
