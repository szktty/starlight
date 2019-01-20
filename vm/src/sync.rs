use dispatch::ffi;
use dispatch::{Group, Queue, QueueAttribute, QueuePriority};
use rand::random;
use std::cell::UnsafeCell;
use std::fmt;
use std::mem;
use std::ops::{Deref, DerefMut};
use std::sync::{Arc, LockResult};

pub struct ThreadPool {
    pub group: Group,
    pub system: Queue,       // serial, high priority
    pub process: Vec<Queue>, // process queues, default priority
    pub bg: Queue,           // serial, background priority
}

// recursive lock
#[derive(Debug)]
pub struct RecLock<T: ?Sized> {
    inner: Arc<RecLockInner>,
    data: UnsafeCell<T>,
}

struct RecLockInner {
    queue: Queue,
    semaphore: ffi::dispatch_semaphore_t,
}

#[derive(Debug)]
pub struct RecLockGuard<'a, T: ?Sized + 'a> {
    lock: &'a RecLock<T>,
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
        RecLock {
            inner: Arc::new(RecLockInner::new()),
            data: UnsafeCell::new(data),
        }
    }

    pub fn get(&self) -> &T {
        unsafe { &*self.data.get() }
    }

    pub fn get_mut(&self) -> &mut T {
        unsafe { &mut *self.data.get() }
    }

    pub fn lock(&self) -> LockResult<RecLockGuard<T>> {
        self.inner.lock();
        Ok(RecLockGuard::new(self))
    }
}

unsafe impl<T> Send for RecLock<T> {}
unsafe impl<T> Sync for RecLock<T> {}

impl RecLockInner {
    #[inline]
    pub fn new() -> RecLockInner {
        let target = Queue::global(QueuePriority::Default);
        let queue = Queue::with_target_queue("RecLock", QueueAttribute::Serial, &target);
        let semaphore = unsafe { ffi::dispatch_semaphore_create(1) };
        RecLockInner { queue, semaphore }
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

    pub fn lock(&self) {
        self.wait();
    }
}

impl Drop for RecLockInner {
    fn drop(&mut self) {
        unsafe {
            let obj =
                mem::transmute::<ffi::dispatch_semaphore_t, ffi::dispatch_object_t>(self.semaphore);
            ffi::dispatch_release(obj)
        };
    }
}

impl fmt::Debug for RecLockInner {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "RecLockInner")
    }
}

unsafe impl Send for RecLockInner {}
unsafe impl Sync for RecLockInner {}

impl<'a, T: ?Sized> RecLockGuard<'a, T> {
    pub fn new(lock: &'a RecLock<T>) -> RecLockGuard<'a, T> {
        RecLockGuard { lock }
    }
}

impl<'a, T: ?Sized> Deref for RecLockGuard<'a, T> {
    type Target = T;

    fn deref(&self) -> &T {
        unsafe { &*self.lock.data.get() }
    }
}

impl<'a, T: ?Sized> DerefMut for RecLockGuard<'a, T> {
    fn deref_mut(&mut self) -> &mut T {
        unsafe { &mut *self.lock.data.get() }
    }
}

impl<'a, T: ?Sized> Drop for RecLockGuard<'a, T> {
    fn drop(&mut self) {
        let inner = self.lock.inner.clone();
        inner.queue.sync(|| {
            inner.signal();
        });
    }
}

// nightly only
// impl<'a, T: ?Sized> !Send for RecLockGuard<'a, T> {}

unsafe impl<'a, T: ?Sized + Sync> Sync for RecLockGuard<'a, T> {}
