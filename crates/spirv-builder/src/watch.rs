use std::convert::Infallible;
use std::path::{Path, PathBuf};
use std::sync::mpsc::Receiver;
use std::thread::JoinHandle;
use std::{collections::HashSet, sync::mpsc::sync_channel};

use notify::{Event, RecommendedWatcher, RecursiveMode, Watcher as _};
use rustc_codegen_spirv_types::CompileResult;

use crate::{SpirvBuilder, SpirvBuilderError, leaf_deps};

impl SpirvBuilder {
    /// Watches the module for changes using [`notify`], rebuilding it upon changes.
    ///
    /// Calls `on_compilation_finishes` after each successful compilation.
    /// The second `Option<AcceptFirstCompile<T>>` param allows you to return some `T`
    /// on the first compile, which is then returned by this function
    /// in pair with [`JoinHandle`] to the watching thread.
    pub fn watch<T>(
        &self,
        mut on_compilation_finishes: impl FnMut(CompileResult, Option<AcceptFirstCompile<'_, T>>)
        + Send
        + 'static,
    ) -> Result<Watch<T>, SpirvBuilderError> {
        let path_to_crate = self
            .path_to_crate
            .as_ref()
            .ok_or(SpirvBuilderError::MissingCratePath)?;
        if !matches!(self.print_metadata, crate::MetadataPrintout::None) {
            return Err(SpirvBuilderError::WatchWithPrintMetadata);
        }

        let metadata_result = crate::invoke_rustc(self);
        // Load the dependencies of the thing
        let metadata_file = if let Ok(path) = metadata_result {
            path
        } else {
            // Fall back to watching from the crate root if the initial compilation fails
            // This is likely to notice changes in the `target` dir, however, given that `cargo watch` doesn't seem to handle that,
            let mut watcher = Watcher::new();
            watcher
                .watcher
                .watch(path_to_crate, RecursiveMode::Recursive)
                .expect("Could watch crate root");
            loop {
                watcher.recv();
                let metadata_file = crate::invoke_rustc(self);
                if let Ok(f) = metadata_file {
                    break f;
                }
            }
        };
        let metadata = self.parse_metadata_file(&metadata_file)?;
        let mut first_compile = None;
        on_compilation_finishes(metadata, Some(AcceptFirstCompile(&mut first_compile)));

        let builder = self.clone();
        let watch_thread = std::thread::spawn(move || {
            let mut watcher = Watcher::new();
            watcher.watch_leaf_deps(&metadata_file);

            loop {
                watcher.recv();
                let metadata_result = crate::invoke_rustc(&builder);
                if let Ok(file) = metadata_result {
                    let metadata = builder
                        .parse_metadata_file(&file)
                        .expect("Metadata file is correct");
                    watcher.watch_leaf_deps(&metadata_file);
                    on_compilation_finishes(metadata, None);
                }
            }
        });

        Ok(Watch {
            first_compile,
            watch_thread,
        })
    }
}

pub struct AcceptFirstCompile<'a, T>(&'a mut Option<T>);

impl<'a, T> AcceptFirstCompile<'a, T> {
    pub fn new(write: &'a mut Option<T>) -> Self {
        Self(write)
    }

    pub fn submit(self, t: T) {
        *self.0 = Some(t);
    }
}

/// Result of [watching](SpirvBuilder::watch) a module for changes.
#[must_use]
#[non_exhaustive]
pub struct Watch<T> {
    /// Result of the first compile, if any.
    pub first_compile: Option<T>,
    /// Join handle to the watching thread.
    ///
    /// You can drop it to detach the watching thread,
    /// or [`join()`](JoinHandle::join) it to block the current thread until shutdown of the program.
    pub watch_thread: JoinHandle<Infallible>,
}

struct Watcher {
    watcher: RecommendedWatcher,
    rx: Receiver<()>,
    watched_paths: HashSet<PathBuf>,
}

impl Watcher {
    fn new() -> Self {
        let (tx, rx) = sync_channel(0);
        let watcher =
            notify::recommended_watcher(move |event: notify::Result<Event>| match event {
                Ok(e) => match e.kind {
                    notify::EventKind::Access(_) => (),
                    notify::EventKind::Any
                    | notify::EventKind::Create(_)
                    | notify::EventKind::Modify(_)
                    | notify::EventKind::Remove(_)
                    | notify::EventKind::Other => {
                        let _ = tx.try_send(());
                    }
                },
                Err(e) => println!("notify error: {e:?}"),
            })
            .expect("Could create watcher");
        Self {
            watcher,
            rx,
            watched_paths: HashSet::new(),
        }
    }

    fn watch_leaf_deps(&mut self, metadata_file: &Path) {
        leaf_deps(metadata_file, |it| {
            let path = it.to_path().unwrap();
            if self.watched_paths.insert(path.to_owned()) {
                self.watcher
                    .watch(it.to_path().unwrap(), RecursiveMode::NonRecursive)
                    .expect("Cargo dependencies are valid files");
            }
        })
        .expect("Could read dependencies file");
    }

    fn recv(&self) {
        self.rx.recv().expect("Watcher still alive");
    }
}
