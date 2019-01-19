use arglist::ArgList;
use interp::Interp;
use module::{ModuleBuilder, ModuleDesc};
use process::Process;
use result::Result;
use std::sync::Arc;
use value::Value;

pub fn new() -> ModuleDesc {
    let mut build = ModuleBuilder::new("io");
    build.add_nif("fwrite", nif_fwrite, 1);
    build.to_desc()
}

fn nif_fwrite(_interp: &Arc<Interp>, _proc: &Arc<Process>, args: &ArgList) -> Result<Value> {
    let fmt_val = args.get(0);
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
