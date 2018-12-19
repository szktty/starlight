use interp::Interp;
use lib_io;

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
        self.interp.add_modules(vec![lib_io::new()])
    }
}

pub fn init() -> Interp {
    let mut init = InterpInit::new();
    init.init_libs();
    init.interp
}
