use heap::ObjectId;
use std::cell::{Ref, RefCell, RefMut};
use std::collections::HashMap;
use std::rc::Rc;
use std::sync::Arc;
use sync::RecLock;
use value::Value;

#[derive(Debug, Clone)]
pub struct Module {
    pub name: RefCell<Option<String>>,
    pub author: Option<String>,
    pub fields: HashMap<String, Value>,
}

#[derive(Debug)]
pub struct ModuleGroup {
    pub mods: RecLock<Arc<RefCell<HashMap<String, ObjectId>>>>,
}

impl Module {
    pub fn new() -> Module {
        Module {
            name: RefCell::new(None),
            author: None,
            fields: HashMap::new(),
        }
    }
    pub fn with_name(name: &str) -> Module {
        let m = Module::new();
        m.set_name(name);
        m
    }
    pub fn get_name(&self) -> Option<String> {
        self.name.borrow().clone()
    }
    pub fn set_name(&self, name: &str) {
        *self.name.borrow_mut() = Some(name.to_string());
    }
}

impl ModuleGroup {
    pub fn new() -> ModuleGroup {
        ModuleGroup {
            mods: RecLock::new(Arc::new(RefCell::new(HashMap::new()))),
        }
    }

    pub fn get(&self, name: &str) -> Option<ObjectId> {
        match self.mods.lock() {
            Ok(_) => self.mods.get().borrow().get(name).cloned(),
            _ => unreachable!(),
        }
    }

    pub fn add(&self, name: &str, m: ObjectId) -> Option<ObjectId> {
        match self.mods.lock() {
            Ok(_) => self.mods.get_mut().borrow_mut().insert(name.to_string(), m),
            _ => unreachable!(),
        }
    }
}

impl Clone for ModuleGroup {
    #[inline]
    fn clone(&self) -> ModuleGroup {
        ModuleGroup {
            mods: RecLock::new(self.mods.get().clone()),
        }
    }
}
