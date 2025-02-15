#![feature(trait_alias, let_chains)]

use std::{
    fmt,
    sync::Arc,
};

use async_trait::async_trait;
use bytes::Bytes;
use cab_error::Result;

mod fs;
pub use fs::*;

mod stdin;
pub use stdin::*;

#[async_trait]
pub trait Entry: fmt::Display + Send + Sync + 'static {
    fn name(&self) -> Option<&str> {
        None
    }

    fn parent(&self) -> Option<Arc<dyn Collection>> {
        None
    }

    async fn as_leaf(self: Arc<Self>) -> Option<Arc<dyn Leaf>> {
        None
    }

    async fn as_collection(self: Arc<Self>) -> Option<Arc<dyn Collection>> {
        None
    }
}

impl dyn Entry {
    pub fn to_display(self: Arc<Self>) -> impl fmt::Display {
        struct EntryDisplay(Arc<dyn Entry>);

        impl fmt::Display for EntryDisplay {
            fn fmt(&self, writer: &mut fmt::Formatter<'_>) -> fmt::Result {
                let mut entries = vec![self.0.clone()];

                while let Some(parent) = entries.last().unwrap().parent() {
                    entries.push(parent);
                }

                for (index, entry) in entries.iter().rev().enumerate() {
                    if index == 0 {
                        write!(writer, "<{entry}>")?;
                    } else {
                        write!(writer, "/{entry}")?;
                    }
                }

                Ok(())
            }
        }

        EntryDisplay(self)
    }
}

#[async_trait]
pub trait Leaf: Entry {
    async fn read(self: Arc<Self>) -> Result<Bytes>;
}

#[async_trait]
pub trait Collection: Entry {
    async fn entry(self: Arc<Self>, name: &str) -> Result<Option<Arc<dyn Entry>>>;
}

#[async_trait]
pub trait CollectionPeek: Collection {
    async fn list(self: Arc<Self>) -> Result<Arc<[Arc<dyn Entry>]>>;
}
