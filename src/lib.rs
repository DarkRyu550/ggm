#![feature(never_type)]

#[macro_use]
extern crate log;

/** Definitions useful for every format. */ 
pub mod song;
pub use song::Song;
pub use song::RemainingLength;

#[cfg(feature = "ast")]
pub mod ast;
#[cfg(feature = "organya")]
pub mod organya;
#[cfg(feature = "psf")]
mod psf;
#[cfg(feature = "usf")]
mod usf;
#[cfg(feature = "gsf")]
mod gsf;

