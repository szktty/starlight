#[macro_use]
extern crate serde_derive;

extern crate serde;
extern crate serde_json;

#[macro_use]
extern crate clap;

extern crate dispatch;

#[macro_use]
pub mod macros;

pub mod bytecode;
pub mod cmd;
pub mod config;
pub mod error;
pub mod heap;
pub mod interp;
pub mod interp_init;
pub mod lib_io;
pub mod lib_strl_genlists;
pub mod lib_strl_runtime;
pub mod module;
pub mod opcode;
pub mod process;
pub mod value;
