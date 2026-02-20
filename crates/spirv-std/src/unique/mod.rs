//! Provides [`UniqueIndex`] and [`UniqueId`] for safe buffer and shared memory writing limited to some [`Scope`].
//!
//! We recommend to start by reading into [`Scope`].

mod builtin;
mod disjoint_slice;
mod id;
mod index;
mod scalar;
mod scope;

pub use builtin::*;
pub use disjoint_slice::*;
pub use id::*;
pub use index::*;
pub use scalar::*;
pub use scope::*;
