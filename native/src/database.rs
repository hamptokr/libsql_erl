use std::sync::Mutex;

use crate::{atoms::error, task};
use rustler::{Encoder, Env, LocalPid, OwnedEnv, ResourceArc};
use tokio::sync::mpsc::{Receiver, Sender};

pub enum DbMessage {
    Connect(LocalPid),
    Stop,
}

pub struct DatabaseRef(Mutex<Sender<DbMessage>>);

impl DatabaseRef {
    pub fn new(tx: Sender<DbMessage>) -> ResourceArc<DatabaseRef> {
        ResourceArc::new(DatabaseRef(Mutex::new(tx)))
    }
}

pub fn load(env: Env) -> bool {
    rustler::resource!(DatabaseRef, env);
    true
}

pub fn spawn_database(db: libsql::Database, mut rx: Receiver<DbMessage>) {
    task::spawn(async move {
        let mut env = OwnedEnv::new();

        loop {
            match rx.recv().await {
                Some(DbMessage::Connect(pid)) => match db.connect() {
                    Ok(_connection) => {}
                    Err(err) => {
                        env.send_and_clear(&pid, move |env| {
                            (error(), format!("{:?}", err)).encode(env)
                        })
                        .unwrap();
                    }
                },
                Some(DbMessage::Stop) => break,
                None => continue,
            }
        }
    });
}
