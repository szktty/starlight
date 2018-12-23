#[macro_use]
extern crate serde_derive;

extern crate serde;
extern crate serde_json;

#[macro_use]
extern crate clap;

#[macro_use]
pub mod macros;

pub mod bytecode;
pub mod cmd;
pub mod config;
pub mod error;
pub mod interp;
pub mod interp_init;
pub mod lib_io;
pub mod lib_strl_genlists;
pub mod module;
pub mod opcode;
pub mod value;
