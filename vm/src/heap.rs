use dispatch::{Queue, SuspendGuard};
use module::{Module, ModuleGroup};
use std::cell::RefCell;
use std::collections::HashMap;
use std::sync::Arc;
use thread_pool::ThreadPool;

pub type ObjectId = usize;

pub struct Heap {
    pool: Arc<ThreadPool>,
    guards: RefCell<Vec<SuspendGuard>>,
    id: RefCell<usize>,
    store: RefCell<HashMap<ObjectId, Object>>,
}

#[derive(Debug, Clone)]
pub struct Object {
    pub id: ObjectId,
    pub content: Content,
}

#[derive(Debug, Clone)]
pub enum Content {
    Module(Arc<Module>),
    ModuleGroup(Arc<ModuleGroup>),
}

unsafe impl Sync for Heap {}
unsafe impl Send for Heap {}

impl Heap {
    pub fn new(pool: Arc<ThreadPool>) -> Heap {
        Heap {
            pool,
            guards: RefCell::new(Vec::new()),
            id: RefCell::new(0),
            store: RefCell::new(HashMap::new()),
        }
    }

    pub fn lock(&self) {
        let mut guards = self.guards.borrow_mut();
        if !guards.is_empty() {
            panic!("already locked")
        }
        guards.push(self.pool.user.suspend())
    }

    pub fn unlock(&self) {
        let guards = self.guards.replace(Vec::new());
        for mut guard in guards {
            guard.resume();
        }
    }

    pub fn get(&self, id: ObjectId) -> Option<Object> {
        match self.store.borrow().get(&id) {
            None => None,
            Some(obj) => Some(obj.clone()),
        }
    }

    pub fn get_content(&self, id: ObjectId) -> Option<Content> {
        self.get(id).map(|obj| obj.content)
    }

    pub fn create(&self, content: Content) -> Object {
        self.lock();

        let mut store = self.store.borrow_mut();
        let mut id = self.id.borrow_mut();
        let obj = Object { id: *id, content };
        store.insert(*id, obj.clone());
        *id += 1;

        self.unlock();
        obj
    }

    pub fn update<F>(&self, obj: Object) -> Option<Object> {
        self.lock();
        let mut store = self.store.borrow_mut();
        let old = store.insert(obj.id, obj);
        self.unlock();
        old
    }
}
