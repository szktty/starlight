use bytecode;
use clap::App;
use config::{set_global, Config};
use interp::Interp;
use interp_init;
use list::List;
use std::process;
use std::sync::Arc;
use value::Value;

pub struct Command {
    pub interp: Arc<Interp>,
    pub args: Vec<String>,
}

impl Command {
    pub fn new() -> Command {
        Command {
            interp: Arc::new(interp_init::init()),
            args: Vec::new(),
        }
    }

    pub fn run(&mut self) {
        let yaml = load_yaml!("cli.yml");
        let mut app = App::from_yaml(yaml);
        let matches = app.clone().get_matches();
        let mut input = match matches.values_of("INPUT") {
            Some(input) => input,
            None => {
                app.print_long_help().unwrap();
                process::exit(1)
            }
        };

        let config = Config {
            debug_mode: matches.is_present("debug"),
            verbose_mode: matches.is_present("verbose"),
        };
        set_global(config);

        let file = input.next().unwrap();
        let args: Vec<Value> = input
            .map(|arg| Value::String(Arc::new(arg.to_string())))
            .collect();

        match bytecode::from_file(file) {
            Err(msg) => panic!("Error: invalid bytecode format: {}", msg),
            Ok(bc) => {
                debug!("execute module initialization function");
                let interp = self.interp.clone();
                let proc = interp.procs.create();
                let init = Arc::new(bc.main.to_value_code(&proc.heap).clone());
                {
                    match Interp::eval(&interp, &proc, None, init.clone(), Vec::new()) {
                        Ok(_) => {}
                        Err(msg) => panic!("# error: {:?}", msg),
                    };
                }

                debug!("get module");
                let m = match interp.get_module(&bc.module) {
                    None => panic!("# main: module not found {}", bc.module),
                    Some(m) => m,
                };

                debug!("execute 'main' function");
                {
                    match m.desc.fields.get("main") {
                        Some(Value::CompiledCode(code)) => {
                            let arity = (*code).arity;
                            let res = if arity == 0 {
                                Interp::eval(&interp, &proc, None, code.clone(), Vec::new())
                            } else if arity == 1 {
                                let cmd_args = List::value_from_list(&proc.heap, args);
                                Interp::eval(&interp, &proc, None, code.clone(), vec![cmd_args])
                            } else {
                                panic!("main/0 or main/1 not found")
                            };
                            match res {
                                Ok(value) => debug!("# main => {:?}", value),
                                Err(e) => println!("# error: {:?}", e),
                            }
                        }
                        Some(value) => panic!("# main/0 {:?} must be a function", value),
                        None => debug!("# main() not found"),
                    }
                }
            }
        }
        self.interp.pool.wait_all();
    }
}

pub fn run() {
    let mut cmd = Command::new();
    cmd.run()
}
