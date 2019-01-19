use list::{List, ListGenerator};
use module::Module;
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::sync::Arc;
use sync::RecLock;
use value::Value;

#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub struct ObjectId {
    pub id: usize,
}

pub struct Heap {
    pub global: Option<Arc<Heap>>,
    store: RecLock<ObjectStore>,

    // shared variables
    list_nil: (ObjectId, Value),
}

pub struct ObjectStore {
    id: usize,
    store: HashMap<ObjectId, Object>,
}

#[derive(Debug, Clone)]
pub struct Object {
    pub id: ObjectId,
    pub content: Content,
}

#[derive(Debug, Clone)]
pub enum Content {
    Nil,
    Module(Arc<Module>),
    List(Arc<List>),
    ListGenerator(Arc<RefCell<ListGenerator>>),
    Global(ObjectId),
}

impl ObjectId {
    #[inline]
    pub fn new(id: usize) -> ObjectId {
        ObjectId { id }
    }
}

impl Object {
    #[inline]
    pub fn new(id: ObjectId, content: Content) -> Object {
        Object { id, content }
    }
}

unsafe impl Sync for Heap {}
unsafe impl Send for Heap {}

impl Heap {
    pub fn new(global: Option<Arc<Heap>>) -> Heap {
        // shared values
        let mut store = HashMap::new();
        let nil_id = ObjectId::new(0);
        let nil_obj = Object::new(nil_id, Content::List(Arc::new(List::Nil)));
        store.insert(nil_id, nil_obj);

        Heap {
            global,
            store: RecLock::new(ObjectStore { id: 1, store }),
            list_nil: (nil_id, Value::List(nil_id)),
        }
    }

    pub fn get(&self, id: ObjectId) -> Option<Object> {
        match self.store.get().store.get(&id) {
            None => None,
            Some(obj) => Some(obj.clone()),
        }
    }

    pub fn get_content(&self, id: ObjectId) -> Option<Content> {
        self.get(id).map(|obj| obj.content)
    }

    pub fn new_id(&self) -> ObjectId {
        self.create(|_| Content::Nil)
    }

    pub fn create<F>(&self, f: F) -> ObjectId
    where
        F: FnOnce(ObjectId) -> Content,
    {
        match self.store.lock() {
            Ok(_) => {
                let mut store = self.store.get_mut();
                let oid = ObjectId::new(store.id);
                let obj = Object {
                    id: oid,
                    content: f(oid),
                };
                store.store.insert(oid, obj.clone());
                store.id += 1;
                oid
            }
            _ => unreachable!(),
        }
    }

    pub fn update<F>(&self, obj: Object) -> Option<Object> {
        match self.store.lock() {
            Ok(_) => {
                let mut store = self.store.get_mut();
                let old = store.store.insert(obj.id, obj);
                old
            }
            _ => unreachable!(),
        }
    }

    // TODO: -> &(ObjectId, Value)
    pub fn get_list_nil(&self) -> (ObjectId, Value) {
        self.list_nil.clone()
    }

    pub fn get_listgen(&self, id: ObjectId) -> Option<Arc<RefCell<ListGenerator>>> {
        self.get_content(id).and_then(|c| match c {
            Content::ListGenerator(gen) => Some(gen.clone()),
            _ => None,
        })
    }
}

impl fmt::Debug for Heap {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Heap({})", self.store.get().id)
    }
}

// bridge (Value <-> T)
#[derive(Debug, Clone)]
pub struct Br<T: ToValue> {
    heap: Arc<Heap>,
    data: Arc<T>,
}

pub trait ToValue {
    fn to_value(&self, heap: &Heap) -> Value;
}

impl<T: ToValue> Br<T> {
    #[inline]
    pub fn new(heap: Arc<Heap>, data: Arc<T>) -> Br<T> {
        Br { heap, data }
    }

    #[inline]
    pub fn cloned(&self) -> Arc<T> {
        self.data.clone()
    }

    #[inline]
    pub fn heap(&self) -> &Heap {
        &*self.heap
    }

    #[inline]
    pub fn heap_ref(&self) -> &Arc<Heap> {
        &self.heap
    }

    #[inline]
    pub fn data(&self) -> &T {
        &*self.data
    }

    #[inline]
    pub fn data_ref(&self) -> &Arc<T> {
        &self.data
    }

    #[inline]
    pub fn to_value(&self) -> Value {
        (*self.data).to_value(&*self.heap)
    }
}
