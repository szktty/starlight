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
    pool: Arc<ThreadPool>,
    procs: RecLock<HashMap<usize, Arc<Process>>>,
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
            procs: RecLock::new(HashMap::new()),
        }
    }

    pub fn create(&self) -> Arc<Process> {
        self.procs.lock();
        let mut procs = self.procs.get_mut();
        let id = procs.len();
        let proc = Arc::new(Process::new(id, self.pool.clone()));
        procs.insert(id, proc.clone());
        self.procs.unlock();
        proc
    }

    pub fn finish(&self, id: usize) {
        self.procs.lock();
        let mut procs = self.procs.get_mut();
        procs.remove(&id);
        self.procs.unlock();
    }
}
