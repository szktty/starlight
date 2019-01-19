use heap::Heap;
use std::collections::HashMap;
use std::sync::Arc;
use sync::RecLock;

#[derive(Debug, Clone)]
pub struct Process {
    pub id: usize,
    pub heap: Arc<Heap>,
}

pub struct ProcessGroup {
    global: Arc<Heap>,
    procs: RecLock<HashMap<usize, Arc<Process>>>,
}

impl Process {
    #[inline]
    fn new(global: Option<Arc<Heap>>, id: usize) -> Process {
        Process {
            id,
            heap: Arc::new(Heap::new(global)),
        }
    }
}

impl ProcessGroup {
    pub fn new(global: Arc<Heap>) -> ProcessGroup {
        ProcessGroup {
            global,
            procs: RecLock::new(HashMap::new()),
        }
    }

    pub fn create(&self) -> Arc<Process> {
        match self.procs.lock() {
            Ok(_) => {
                let mut procs = self.procs.get_mut();
                let id = procs.len();
                //println!("# ProcessGroup: id {}", id);
                if id > 1000000 {
                    //panic!("ok {}", id)
                }
                let proc = Arc::new(Process::new(Some(self.global.clone()), id));
                procs.insert(id, proc.clone());
                proc
            }
            _ => unreachable!(),
        }
    }

    pub fn finish(&self, id: usize) {
        let mut procs = self.procs.lock().unwrap();
        procs.remove(&id);
    }
}
