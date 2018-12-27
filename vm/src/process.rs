use value::Value;

pub struct Process {
    pid: usize,
}

impl Process {
    pub fn new() -> Process {
        Process { pid: 0 }
    }
}
