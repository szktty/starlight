use heap::Heap;
use std::collections::HashMap;
use std::sync::{Arc, RwLock};
use sync::{RecLock, ThreadPool};

#[derive(Debug, Clone)]
pub struct Process {
    pub id: usize,
    pub heap: Arc<Heap>,
}

pub struct ProcessGroup {
    global: Arc<Heap>,
    pool: Arc<ThreadPool>,
    procs: RecLock<HashMap<usize, Arc<Process>>>,
}

impl Process {
    #[inline]
    fn new(global: Option<Arc<Heap>>, id: usize, pool: Arc<ThreadPool>) -> Process {
        Process {
            id,
            heap: Arc::new(Heap::new(global, pool.clone())),
        }
    }
}

impl ProcessGroup {
    pub fn new(global: Arc<Heap>, pool: Arc<ThreadPool>) -> ProcessGroup {
        ProcessGroup {
            global,
            pool,
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
                let proc = Arc::new(Process::new(
                    Some(self.global.clone()),
                    id,
                    self.pool.clone(),
                ));
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
