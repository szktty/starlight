use heap::ObjectId;
use std::cell::{Ref, RefCell, RefMut};
use std::collections::HashMap;
use std::rc::Rc;
use std::sync::Arc;
use value::{NifFun, Value};

#[derive(Debug, Clone)]
pub struct Module {
    pub id: ObjectId,
    pub desc: ModuleDesc,
}

#[derive(Debug, Clone)]
pub struct ModuleDesc {
    pub name: String,
    pub author: Option<String>,
    pub fields: HashMap<String, Value>,
}

#[derive(Debug)]
pub struct ModuleGroup {
    pub mods: RefCell<HashMap<String, Arc<Module>>>,
}

#[derive(Debug)]
pub struct ModuleBuilder {
    name: String,
    fields: HashMap<String, Value>,
}

impl Module {
    pub fn new(id: ObjectId, desc: ModuleDesc) -> Module {
        Module { id, desc }
    }

    pub fn get_prop(&self, name: &str) -> Option<&Value> {
        self.desc.fields.get(name)
    }
}

impl ModuleDesc {
    pub fn new(name: &str, fields: HashMap<String, Value>) -> ModuleDesc {
        ModuleDesc {
            name: name.to_string(),
            author: None,
            fields,
        }
    }
}

impl ModuleGroup {
    pub fn new() -> ModuleGroup {
        ModuleGroup {
            mods: RefCell::new(HashMap::new()),
        }
    }

    pub fn get(&self, name: &str) -> Option<Arc<Module>> {
        self.mods.borrow().get(name).cloned()
    }

    pub fn add(&self, name: &str, m: Arc<Module>) -> Option<Arc<Module>> {
        self.mods.borrow_mut().insert(name.to_string(), m)
    }
}

impl ModuleBuilder {
    pub fn new(name: &str) -> ModuleBuilder {
        ModuleBuilder {
            name: name.to_string(),
            fields: HashMap::new(),
        }
    }

    pub fn add_value(&mut self, name: &str, value: Value) {
        self.fields.insert(name.to_string(), value);
    }

    pub fn add_nif(&mut self, name: &str, f: NifFun, arity: usize) {
        self.add_value(name, Value::nif(arity, f))
    }

    pub fn to_desc(&self) -> ModuleDesc {
        ModuleDesc::new(&self.name, self.fields.clone())
    }
}
