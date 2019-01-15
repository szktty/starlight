#![feature(optin_builtin_traits)]

#[macro_use]
extern crate serde_derive;

extern crate serde;
extern crate serde_json;

#[macro_use]
extern crate clap;

extern crate dispatch;
extern crate rand;

#[macro_use]
pub mod macros;

pub mod arglist;
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
pub mod list;
pub mod module;
pub mod opcode;
pub mod process;
pub mod result;
pub mod sync;
pub mod value;
