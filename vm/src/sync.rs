use dispatch::ffi;
use dispatch::{Group, Queue, QueueAttribute, QueuePriority};
use rand::random;
use std::cell::{RefCell, UnsafeCell};
use std::mem;
use std::sync::Arc;

pub struct ThreadPool {
    pub group: Group,
    pub system: Queue,       // serial, high priority
    pub process: Vec<Queue>, // process queues, default priority
    pub bg: Queue,           // serial, background priority
}

// recursive lock
pub struct RecLock<T> {
    queue: Queue,
    inner: Arc<RecLockInner>,
    data: UnsafeCell<T>,
}

struct RecLockInner {
    is_alive: bool,
    count: RefCell<usize>,
    semaphore: ffi::dispatch_semaphore_t,
}

impl ThreadPool {
    pub fn new() -> ThreadPool {
        let group = Group::create();
        let pr_high = Queue::global(QueuePriority::High);
        let pr_default = Queue::global(QueuePriority::Default);
        let pr_bg = Queue::global(QueuePriority::Background);
        let system = Queue::with_target_queue("system", QueueAttribute::Serial, &pr_high);
        let bg = Queue::with_target_queue("background", QueueAttribute::Serial, &pr_bg);
        let mut process = Vec::new();
        for n in 0..12 {
            let queue = Queue::with_target_queue(
                &format!("process{:?}", n),
                QueueAttribute::Concurrent,
                &pr_default,
            );
            process.push(queue)
        }
        ThreadPool {
            group,
            system,
            process,
            bg,
        }
    }

    pub fn wait_all(&self) {
        self.group.wait()
    }

    pub fn process_async<F>(&self, work: F)
    where
        F: 'static + Send + FnOnce(),
    {
        let n = match random::<u8>() {
            0 => 0,
            n => n % self.process.len() as u8,
        };
        let queue = &self.process[n as usize];
        self.group.async(queue, work)
    }
}

impl<T> RecLock<T> {
    pub fn new(data: T) -> RecLock<T> {
        let target = Queue::global(QueuePriority::Default);
        let semaphore = unsafe { ffi::dispatch_semaphore_create(1) };
        RecLock {
            queue: Queue::with_target_queue("RecLock", QueueAttribute::Serial, &target),
            inner: Arc::new(RecLockInner {
                is_alive: true,
                count: RefCell::new(0),
                semaphore,
            }),
            data: UnsafeCell::new(data),
        }
    }

    pub fn get(&self) -> &T {
        unsafe { &*self.data.get() }
    }

    pub fn get_mut(&self) -> &mut T {
        unsafe { &mut *self.data.get() }
    }

    pub fn try_lock(&self) -> bool {
        let mut flag = false;
        let inner = self.inner.clone();
        self.queue.sync(|| {
            if inner.count() == 0 {
                inner.increment();
                flag = true;
            }
        });
        flag
    }

    pub fn lock(&self) {
        println!("# RecLock::lock");
        if self.inner.is_alive {
            let inner = self.inner.clone();
            self.queue.sync(|| {
                inner.increment();
            });
            self.inner.wait();
        }
    }

    pub fn unlock(&self) {
        if self.inner.is_alive {
            let inner = self.inner.clone();
            self.queue.sync(|| {
                inner.decrement();
            });
            inner.signal();
        }
    }
}

unsafe impl<T> Send for RecLock<T> {}
unsafe impl<T> Sync for RecLock<T> {}

impl RecLockInner {
    #[inline]
    pub fn count(&self) -> usize {
        *self.count.borrow()
    }

    #[inline]
    pub fn increment(&self) {
        *self.count.borrow_mut() += 1
    }

    #[inline]
    pub fn decrement(&self) {
        *self.count.borrow_mut() -= 1
    }

    #[inline]
    pub fn dispatch_object(&self) -> ffi::dispatch_object_t {
        unsafe {
            mem::transmute::<ffi::dispatch_semaphore_t, ffi::dispatch_object_t>(self.semaphore)
        }
    }

    pub fn wait(&self) {
        unsafe { ffi::dispatch_semaphore_wait(self.semaphore, ffi::DISPATCH_TIME_FOREVER) };
    }

    pub fn signal(&self) {
        unsafe { ffi::dispatch_semaphore_signal(self.dispatch_object()) };
    }
}

impl Drop for RecLockInner {
    fn drop(&mut self) {
        self.is_alive = false;
        unsafe {
            let obj =
                mem::transmute::<ffi::dispatch_semaphore_t, ffi::dispatch_object_t>(self.semaphore);
            ffi::dispatch_release(obj)
        };
    }
}

unsafe impl Send for RecLockInner {}
unsafe impl Sync for RecLockInner {}
