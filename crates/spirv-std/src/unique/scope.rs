//! See [`Scope`]

/// A scope defines how many invocations share some property. Each individual type must declare themselves what the
/// scope describes. For [`UniqueIndex`] and [`UniqueId`], the scope describes for which invocations their index / id is
/// unique for.
///
/// Rust-GPU currently defines these concrete Scopes, ordered from small to large:
/// * [`ActiveInvocations`]: A scope containing only the active invocations **of a subgroup**, *at creation time*.
///   If you don't exit the scope this was created at, threads can only be masked out but never be added, making this a
///   potentially useful scope in some scenarios. Can be trivially created with subgroup ops.
/// * [`Subgroup`]: A scope sized like the subgroup, contains [`ActiveInvocations`]. Mostly used to interact
///   with subgroup operations found in [`arch::subgroup*`]([`crate::arch::subgroup_elect`]).
/// * [`Workgroup`]: A scope sized like the workgroup, contains [`Subgroup`] and [`ActiveInvocations`]. Usually
///   used for shared memory operations.
/// * [`Global`]: A global scope includes the entire execution, meaning all workgroups within this dispatch or draw cmd.
///   Contains both [`Workgroup`], [`Subgroup`] and [`ActiveInvocations`]. Usually used to write to buffers or
///   images in global memory.
///
/// Notice how each larger scope contains all smaller scopes. Next to these listed concrete scopes, implemented as
/// structs, there are also `AtLeast*` traits to declare that a scope is at least this size, like [`AtLeastSubgroup`]
/// or [`AtLeastWorkgroup`].
///
/// Implementations should consider on a case by case basis whether to explicitly require concrete scopes or allow
/// implicit downcasting using the `AtLeast*` traits. For example, [`UniqueIndex`] and [`UniqueId`] use the `AtLeast*`
/// traits to implement the explicitly downcasting to smaller scopes. But operations on shared memory may want to use
/// the explicit [`Workgroup`] scope, since it doesn't make sense to use something in [`Global`] scope for shared memory
/// access.
///
/// [`UniqueId`]: `crate::entry::UniqueId`
/// [`UniqueIndex`]: `crate::entry::UniqueIndex`
// TODO How does this interact with SPIR-V's ScopeId?
pub unsafe trait Scope {}

macro_rules! def_scope {
    ($trait_name:ident, $ty_name:ident$(:$($contains:ident),*)?) => {
        /// See [`Scope`]
        pub unsafe trait $trait_name: Scope {}
        /// See [`Scope`]
        pub struct $ty_name;
        unsafe impl Scope for $ty_name {}
        $($(
            unsafe impl $contains for $ty_name {}
        )*)?
    };
}

def_scope!(AtLeastActiveInvocations, ActiveInvocations);
def_scope!(AtLeastSubgroup, Subgroup: AtLeastActiveInvocations);
def_scope!(AtLeastWorkgroup, Workgroup: AtLeastActiveInvocations, AtLeastSubgroup);
def_scope!(AtLeastGlobal, Global: AtLeastActiveInvocations, AtLeastSubgroup, AtLeastWorkgroup);
