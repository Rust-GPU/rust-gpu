use crate::{SpirvBuilder, SpirvBuilderError};
use notify::{Event, RecommendedWatcher, RecursiveMode, Watcher};
use raw_string::RawStr;
use rustc_codegen_spirv_types::CompileResult;
use std::sync::mpsc::TryRecvError;
use std::{
    collections::HashSet,
    path::PathBuf,
    sync::mpsc::{Receiver, sync_channel},
};

impl SpirvBuilder {
    /// Watches the module for changes, rebuilding it upon them.
    pub fn watch(self) -> Result<SpirvWatcher, SpirvBuilderError> {
        SpirvWatcher::new(self)
    }
}

type WatchedPaths = HashSet<PathBuf>;

#[derive(Copy, Clone, Debug)]
enum WatcherState {
    /// Upcoming compile is the first compile:
    /// * always recompile regardless of file watches
    /// * success: go to [`Self::Watching`]
    /// * fail: go to [`Self::FirstFailed`]
    First,
    /// The first compile (and all consecutive ones) failed:
    /// * only recompile when watcher notifies us
    /// * the whole project dir is being watched, remove that watch
    /// * success: go to [`Self::Watching`]
    /// * fail: stay in [`Self::FirstFailed`]
    FirstFailed,
    /// At least one compile finished and has set up the proper file watches:
    /// * only recompile when watcher notifies us
    /// * always stays in [`Self::Watching`]
    Watching,
}

/// Watcher of a crate which rebuilds it on changes.
#[derive(Debug)]
pub struct SpirvWatcher {
    builder: SpirvBuilder,
    watcher: RecommendedWatcher,
    rx: Receiver<()>,
    path_to_crate: PathBuf,
    watched_paths: WatchedPaths,
    state: WatcherState,
}

impl SpirvWatcher {
    fn new(builder: SpirvBuilder) -> Result<Self, SpirvBuilderError> {
        let path_to_crate = builder
            .path_to_crate
            .as_ref()
            .ok_or(SpirvBuilderError::MissingCratePath)?
            .clone();

        let (tx, rx) = sync_channel(1);
        let watcher =
            notify::recommended_watcher(move |result: notify::Result<Event>| match result {
                Ok(event) => match event.kind {
                    notify::EventKind::Any
                    | notify::EventKind::Create(_)
                    | notify::EventKind::Modify(_)
                    | notify::EventKind::Remove(_)
                    | notify::EventKind::Other => {
                        // `Err(Disconnected)` is fine, SpirvWatcher is currently dropping
                        // `Err(Full)` is fine, we just need to send a single event anyway
                        tx.try_send(()).ok();
                    }
                    notify::EventKind::Access(_) => (),
                },
                Err(err) => log::error!("notify error: {err:?}"),
            })
            .map_err(SpirvWatcherError::NotifyFailed)?;

        Ok(Self {
            path_to_crate,
            builder,
            watcher,
            rx,
            watched_paths: HashSet::new(),
            state: WatcherState::First,
        })
    }

    /// Blocks the current thread until a change is detected, rebuilds the crate and returns the [`CompileResult`] or
    /// an [`SpirvBuilderError`]. Always builds once when called for the first time.
    ///
    /// See [`Self::try_recv`] for a non-blocking variant.
    pub fn recv(&mut self) -> Result<CompileResult, SpirvBuilderError> {
        self.recv_inner(|rx| rx.recv().map_err(TryRecvError::from))
            .map(|result| result.unwrap())
    }

    /// If a change is detected or this is the first invocation, builds the crate and returns the [`CompileResult`]
    /// (wrapped in `Some`) or an [`SpirvBuilderError`]. If no change has been detected, returns `Ok(None)` without
    /// blocking.
    ///
    /// See [`Self::recv`] for a blocking variant.
    pub fn try_recv(&mut self) -> Result<Option<CompileResult>, SpirvBuilderError> {
        self.recv_inner(Receiver::try_recv)
    }

    #[inline]
    fn recv_inner(
        &mut self,
        recv: impl FnOnce(&Receiver<()>) -> Result<(), TryRecvError>,
    ) -> Result<Option<CompileResult>, SpirvBuilderError> {
        let received = match self.state {
            // always compile on first invocation
            // file watches have yet to be setup, so recv channel is empty and must not be cleared
            WatcherState::First => Ok(()),
            WatcherState::FirstFailed | WatcherState::Watching => recv(&self.rx),
        };
        match received {
            Ok(_) => (),
            Err(TryRecvError::Empty) => return Ok(None),
            Err(TryRecvError::Disconnected) => return Err(SpirvWatcherError::WatcherDied.into()),
        }

        let result = (|| {
            let out = crate::invoke_rustc(&self.builder)?;
            self.watch_leaf_deps(out.deps.iter().map(|d| d.as_ref()));
            Ok(out.compile_result)
        })();
        match result {
            Ok(result) => {
                if matches!(self.state, WatcherState::FirstFailed) {
                    self.watcher
                        .unwatch(&self.path_to_crate)
                        .map_err(SpirvWatcherError::NotifyFailed)?;
                }
                self.state = WatcherState::Watching;
                Ok(Some(result))
            }
            Err(err) => {
                self.state = match self.state {
                    WatcherState::First => {
                        self.watcher
                            .watch(&self.path_to_crate, RecursiveMode::Recursive)
                            .map_err(SpirvWatcherError::NotifyFailed)?;
                        WatcherState::FirstFailed
                    }
                    WatcherState::FirstFailed => WatcherState::FirstFailed,
                    WatcherState::Watching => WatcherState::Watching,
                };
                Err(err)
            }
        }
    }

    fn watch_leaf_deps<'a>(&mut self, deps: impl Iterator<Item = &'a RawStr>) {
        for dep in deps {
            let path = dep.to_path().unwrap();
            if self.watched_paths.insert(path.to_owned())
                && let Err(err) = self.watcher.watch(path, RecursiveMode::NonRecursive)
            {
                log::error!("failed to watch `{}`: {err}", path.display());
            }
        }
    }
}

#[derive(Debug, thiserror::Error)]
pub enum SpirvWatcherError {
    #[error("could not notify for changes: {0}")]
    NotifyFailed(#[from] notify::Error),
    #[error("watcher died and closed channel")]
    WatcherDied,
}
