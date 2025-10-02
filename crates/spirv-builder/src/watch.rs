use crate::{SpirvBuilder, SpirvBuilderError, leaf_deps};
use notify::{Event, RecommendedWatcher, RecursiveMode, Watcher};
use rustc_codegen_spirv_types::CompileResult;
use std::sync::mpsc::TrySendError;
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

/// Watcher of a crate which rebuilds it on changes.
#[derive(Debug)]
pub struct SpirvWatcher {
    builder: SpirvBuilder,
    watcher: RecommendedWatcher,
    rx: Receiver<()>,
    /// `!first_result`: the path to the crate
    /// `first_result`: the path to our metadata file with entry point names and file paths
    watch_path: PathBuf,
    watched_paths: WatchedPaths,
    first_result: bool,
}

impl SpirvWatcher {
    fn new(builder: SpirvBuilder) -> Result<Self, SpirvBuilderError> {
        let path_to_crate = builder
            .path_to_crate
            .as_ref()
            .ok_or(SpirvBuilderError::MissingCratePath)?
            .clone();
        if !matches!(builder.print_metadata, crate::MetadataPrintout::None) {
            return Err(SpirvWatcherError::WatchWithPrintMetadata.into());
        }

        let (tx, rx) = sync_channel(1);
        let watcher =
            notify::recommended_watcher(move |result: notify::Result<Event>| match result {
                Ok(event) => match event.kind {
                    notify::EventKind::Any
                    | notify::EventKind::Create(_)
                    | notify::EventKind::Modify(_)
                    | notify::EventKind::Remove(_)
                    | notify::EventKind::Other => match tx.try_send(()) {
                        Ok(_) => (),
                        // disconnect is fine, SpirvWatcher is currently dropping
                        Err(TrySendError::Disconnected(_)) => (),
                        // full is fine, we just need to send a single event anyway
                        Err(TrySendError::Full(_)) => (),
                    },
                    notify::EventKind::Access(_) => {}
                },
                Err(err) => log::error!("notify error: {err:?}"),
            })
            .map_err(SpirvWatcherError::NotifyFailed)?;

        Ok(Self {
            watch_path: path_to_crate,
            builder,
            watcher,
            rx,
            watched_paths: HashSet::new(),
            first_result: false,
        })
    }

    /// Blocks the current thread until a change is detected
    /// and the crate is rebuilt.
    ///
    /// Result of rebuilding of the crate is then returned to the caller.
    pub fn recv(&mut self) -> Result<CompileResult, SpirvBuilderError> {
        if !self.first_result {
            return self.recv_first_result();
        }

        self.rx.recv().map_err(|_| SpirvWatcherError::WatcherDied)?;
        let metadata_file = crate::invoke_rustc(&self.builder)?;
        let result = self.builder.parse_metadata_file(&metadata_file)?;

        self.watch_leaf_deps()?;
        Ok(result)
    }

    fn recv_first_result(&mut self) -> Result<CompileResult, SpirvBuilderError> {
        let metadata_file = match crate::invoke_rustc(&self.builder) {
            Ok(path) => path,
            Err(err) => {
                log::error!("{err}");

                let watch_path = self.watch_path.as_ref();
                self.watcher
                    .watch(watch_path, RecursiveMode::Recursive)
                    .map_err(SpirvWatcherError::NotifyFailed)?;
                let path = loop {
                    self.rx.recv().map_err(|_| SpirvWatcherError::WatcherDied)?;
                    match crate::invoke_rustc(&self.builder) {
                        Ok(path) => break path,
                        Err(err) => log::error!("{err}"),
                    }
                };
                self.watcher
                    .unwatch(watch_path)
                    .map_err(SpirvWatcherError::NotifyFailed)?;
                path
            }
        };
        let result = self.builder.parse_metadata_file(&metadata_file)?;

        self.watch_path = metadata_file;
        self.first_result = true;
        self.watch_leaf_deps()?;
        Ok(result)
    }

    fn watch_leaf_deps(&mut self) -> Result<(), SpirvBuilderError> {
        leaf_deps(&self.watch_path, |artifact| {
            let path = artifact.to_path().unwrap();
            if self.watched_paths.insert(path.to_owned())
                && let Err(err) = self.watcher.watch(path, RecursiveMode::NonRecursive)
            {
                log::error!("files of cargo dependencies are not valid: {err}");
            }
        })
        .map_err(SpirvBuilderError::MetadataFileMissing)
    }
}

#[derive(Debug, thiserror::Error)]
pub enum SpirvWatcherError {
    #[error("watching within build scripts will prevent build completion")]
    WatchWithPrintMetadata,
    #[error("could not notify for changes: {0}")]
    NotifyFailed(#[from] notify::Error),
    #[error("watcher died and closed channel")]
    WatcherDied,
}
