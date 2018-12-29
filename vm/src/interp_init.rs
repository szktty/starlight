use interp::Interp;
use lib_io;
use lib_strl_genlists;
use lib_strl_runtime;

pub struct InterpInit {
    pub interp: Interp,
}

impl InterpInit {
    pub fn new() -> InterpInit {
        InterpInit {
            interp: Interp::new(),
        }
    }

    pub fn init_libs(&mut self) {
        self.interp.install_modules(vec![
            lib_io::new(),
            lib_strl_genlists::new(),
            lib_strl_runtime::new(),
        ])
    }
}

pub fn init() -> Interp {
    let mut init = InterpInit::new();
    init.init_libs();
    init.interp
}
