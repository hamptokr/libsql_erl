use std::future::Future;

use once_cell::sync::Lazy;
use tokio::runtime::{Builder, Runtime};
use tokio::task::JoinHandle;

static TOKIO: Lazy<Runtime> = Lazy::new(|| {
    Builder::new_multi_thread()
        .build()
        .expect("libsql_nif: Failed to start the tokio runtime")
});

pub fn spawn<T>(task: T) -> JoinHandle<T::Output>
where
    T: Future + Send + 'static,
    T::Output: Send + 'static,
{
    TOKIO.spawn(task)
}
