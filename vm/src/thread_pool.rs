use dispatch::{Group, Queue, QueueAttribute, QueuePriority};

pub struct ThreadPool {
    pub group: Group,
    pub system: Queue, // serial, high priority
    pub user: Queue,   // concurrent, default priority
    pub bg: Queue,     // serial, background priority
}

impl ThreadPool {
    pub fn new() -> ThreadPool {
        let pr_high = Queue::global(QueuePriority::High);
        let pr_default = Queue::global(QueuePriority::Default);
        let pr_bg = Queue::global(QueuePriority::Background);
        let system = Queue::with_target_queue("system", QueueAttribute::Serial, &pr_high);
        let user = Queue::with_target_queue("user", QueueAttribute::Concurrent, &pr_default);
        let bg = Queue::with_target_queue("background", QueueAttribute::Serial, &pr_bg);
        ThreadPool {
            group: Group::create(),
            system,
            user,
            bg,
        }
    }

    pub fn wait_all(&self) {
        self.group.wait()
    }
}
