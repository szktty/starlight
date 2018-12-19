macro_rules! debug {
    ($msg:expr) => {{
        use config::get_global;
        if get_global().debug_mode {
            println!("# {}: line {}: {}", file!(), line!(), $msg)
        }
    }};
    ($fmt:expr, $($arg:tt)+) => {{
        use config::get_global;
        if get_global().debug_mode {
            println!("# {}: line {}: {}", file!(), line!(), format_args!($fmt, $($arg)+))
        }
    }};
}
