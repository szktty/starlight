use heap::Heap;
use std::collections::HashMap;
use std::sync::{Arc, RwLock};
use sync::ThreadPool;

#[derive(Debug, Clone)]
pub struct Process {
    pub id: usize,
    pub heap: Arc<Heap>,
}

pub struct ProcessGroup {
    pool: Arc<ThreadPool>,
    procs: RwLock<HashMap<usize, Arc<Process>>>,
}

impl Process {
    #[inline]
    fn new(id: usize, pool: Arc<ThreadPool>) -> Process {
        Process {
            id,
            heap: Arc::new(Heap::new(pool.clone())),
        }
    }
}

impl ProcessGroup {
    pub fn new(pool: Arc<ThreadPool>) -> ProcessGroup {
        ProcessGroup {
            pool,
            procs: RwLock::new(HashMap::new()),
        }
    }

    pub fn create(&self) -> Arc<Process> {
        match self.procs.write() {
            Result::Ok(mut procs) => {
                let id = procs.len();
                let proc = Arc::new(Process::new(id, self.pool.clone()));
                procs.insert(id, proc.clone());
                proc
            }
            _ => panic!("# ProcessGroup::create: lock failure"),
        }
    }

    pub fn finish(&self, id: usize) {
        match self.procs.write() {
            Result::Ok(mut procs) => {
                procs.remove(&id);
            }
            _ => (),
        };
    }
}
