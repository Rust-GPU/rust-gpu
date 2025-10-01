use std::path::{Path, PathBuf};
use std::sync::mpsc::Receiver;
use std::{collections::HashSet, sync::mpsc::sync_channel};

use notify::{Event, RecommendedWatcher, RecursiveMode, Watcher};
use rustc_codegen_spirv_types::CompileResult;

use crate::{SpirvBuilder, SpirvBuilderError, leaf_deps};

impl SpirvBuilder {
    /// Watches the module for changes, rebuilding it upon them.
    pub fn watch(&self) -> Result<SpirvWatcher<&Self>, SpirvBuilderError> {
        SpirvWatcher::new(self)
    }
}

#[derive(Debug)]
pub struct SpirvWatcher<B> {
    builder: B,
    watcher: RecommendedWatcher,
    rx: Receiver<()>,
    watch_path: PathBuf,
    watched_paths: HashSet<PathBuf>,
    first_result: bool,
}

impl<B> SpirvWatcher<B>
where
    B: AsRef<SpirvBuilder>,
{
    fn new(as_builder: B) -> Result<Self, SpirvBuilderError> {
        let builder = as_builder.as_ref();
        let path_to_crate = builder
            .path_to_crate
            .as_ref()
            .ok_or(SpirvBuilderError::MissingCratePath)?;
        if !matches!(builder.print_metadata, crate::MetadataPrintout::None) {
            return Err(SpirvWatcherError::WatchWithPrintMetadata.into());
        }

        let (tx, rx) = sync_channel(0);
        let watcher =
            notify::recommended_watcher(move |result: notify::Result<Event>| match result {
                Ok(event) => match event.kind {
                    notify::EventKind::Any
                    | notify::EventKind::Create(_)
                    | notify::EventKind::Modify(_)
                    | notify::EventKind::Remove(_)
                    | notify::EventKind::Other => {
                        if let Err(err) = tx.try_send(()) {
                            log::error!("send error: {err:?}");
                        }
                    }
                    notify::EventKind::Access(_) => {}
                },
                Err(err) => log::error!("notify error: {err:?}"),
            })
            .map_err(SpirvWatcherError::NotifyFailed)?;

        Ok(Self {
            watch_path: path_to_crate.clone(),
            builder: as_builder,
            watcher,
            rx,
            watched_paths: HashSet::new(),
            first_result: false,
        })
    }

    pub fn recv(&mut self) -> Result<CompileResult, SpirvBuilderError> {
        if !self.first_result {
            return self.recv_first_result();
        }

        self.rx.recv().expect("watcher should be alive");
        let builder = self.builder.as_ref();
        let metadata_file = crate::invoke_rustc(builder)?;
        let result = builder.parse_metadata_file(&metadata_file)?;

        self.watch_leaf_deps(&self.watch_path.clone())?;
        Ok(result)
    }

    fn recv_first_result(&mut self) -> Result<CompileResult, SpirvBuilderError> {
        let builder = self.builder.as_ref();
        let metadata_file = match crate::invoke_rustc(builder) {
            Ok(path) => path,
            Err(err) => {
                log::error!("{err}");

                self.watcher
                    .watch(&self.watch_path, RecursiveMode::Recursive)
                    .map_err(SpirvWatcherError::NotifyFailed)?;
                let path = loop {
                    self.rx.recv().expect("watcher should be alive");
                    match crate::invoke_rustc(builder) {
                        Ok(path) => break path,
                        Err(err) => log::error!("{err}"),
                    }
                };
                self.watcher
                    .unwatch(&self.watch_path)
                    .map_err(SpirvWatcherError::NotifyFailed)?;
                path
            }
        };
        let result = builder.parse_metadata_file(&metadata_file)?;

        self.watch_leaf_deps(&metadata_file)?;
        self.watch_path = metadata_file;
        self.first_result = true;
        Ok(result)
    }

    fn watch_leaf_deps(&mut self, metadata_file: &Path) -> Result<(), SpirvBuilderError> {
        leaf_deps(metadata_file, |it| {
            let path = it.to_path().unwrap();
            if self.watched_paths.insert(path.to_owned())
                && let Err(err) = self
                    .watcher
                    .watch(it.to_path().unwrap(), RecursiveMode::NonRecursive)
            {
                log::error!("files of cargo dependencies are not valid: {err}");
            }
        })
        .map_err(SpirvBuilderError::MetadataFileMissing)
    }
}

impl SpirvWatcher<&SpirvBuilder> {
    pub fn forget_lifetime(self) -> SpirvWatcher<SpirvBuilder> {
        SpirvWatcher {
            builder: self.builder.clone(),
            watcher: self.watcher,
            rx: self.rx,
            watch_path: self.watch_path,
            watched_paths: self.watched_paths,
            first_result: self.first_result,
        }
    }
}

#[derive(Debug, thiserror::Error)]
pub enum SpirvWatcherError {
    #[error("watching within build scripts will prevent build completion")]
    WatchWithPrintMetadata,
    #[error("could not notify for changes: {0}")]
    NotifyFailed(#[from] notify::Error),
}
