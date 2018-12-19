#[derive(Debug, Clone)]
pub struct Config {
    pub debug_mode: bool,
    pub verbose_mode: bool,
}

static mut G_CONFIG: Config = Config {
    debug_mode: false,
    verbose_mode: false,
};

pub fn get_global() -> &'static Config {
    unsafe { &G_CONFIG }
}

pub fn set_global(config: Config) {
    unsafe {
        G_CONFIG = config;
    }
}

impl Config {}
