//! This module contains the nif code for managing a database connection to
//! libsql.
//!
//! The code below has a bit more going on than typical rustler examples so I'll
//! try to document the thinking here. After all, rustler is *criminally*
//! undocumented. I'm still not 100% clear on some concepts.
//!
//! Because the `libsql` client exposes an async interface, we need to connect
//! the concurrency models of rust and erlang. Luckily they're both somewhat
//! similar in that they both rely on a form of message passing.
//!
//! Upon calling `start` (or `libsql_nif:database_start().` on the erlang side)
//! we will spawn a tokio task that will listen for messages. The erlang side
//! receives a reference, (`DatabaseRef`) to the data we will need later on. The
//! erlang side can then use that reference as an argument to other functions,
//! e.g. `libsql_nif:connect_local(DbRef, ":memory:").`, the looping task will
//! receive a message to connect and will update the ref's connection. The
//! erlang side can now begin calling other functions that rely on that
//! connection.

use std::{path::PathBuf, sync::Mutex};

use crate::{atoms::ok, task};
use libsql::{Builder, Connection};
use rustler::{Atom, Encoder, Env, LocalPid, OwnedEnv, ResourceArc};
use tokio::sync::mpsc::{channel, Receiver, Sender};

pub enum DbMessage {
    ConnectLocal(LocalPid, PathBuf),
    Execute(LocalPid, String),
}

pub struct DatabaseRef {
    pub inner: Mutex<Sender<DbMessage>>,
    pub connection: Mutex<Option<Connection>>,
}

#[rustler::nif(name = "database_start")]
pub fn start() -> (Atom, ResourceArc<DatabaseRef>) {
    let (tx, rx) = channel::<DbMessage>(1000);
    let db_ref = ResourceArc::new(DatabaseRef {
        inner: Mutex::new(tx),
        connection: Mutex::new(None),
    });
    spawn_database(db_ref.clone(), rx);

    (ok(), db_ref)
}

#[rustler::nif(name = "database_connect_local")]
pub fn connect_local(
    env: Env,
    resource: ResourceArc<DatabaseRef>,
    path: &str,
) -> (Atom, ResourceArc<DatabaseRef>) {
    let path = PathBuf::from(path);
    send(resource.clone(), DbMessage::ConnectLocal(env.pid(), path));
    (ok(), resource)
}

#[rustler::nif(name = "database_execute")]
pub fn execute(env: Env, resource: ResourceArc<DatabaseRef>, sql: &str) -> Atom {
    send(
        resource.clone(),
        DbMessage::Execute(env.pid(), String::from(sql)),
    );
    ok()
}

fn send(resource: ResourceArc<DatabaseRef>, msg: DbMessage) {
    let lock = (*resource).inner.lock().expect("Failed to obtain a lock");
    let sender = lock.clone();

    task::spawn(async move {
        sender.send(msg).await.unwrap();
    });
}

pub fn load(env: Env) -> bool {
    rustler::resource!(DatabaseRef, env);
    true
}

fn spawn_database(db_ref: ResourceArc<DatabaseRef>, mut rx: Receiver<DbMessage>) {
    task::spawn(async move {
        let mut env = OwnedEnv::new();

        loop {
            match rx.recv().await {
                Some(DbMessage::ConnectLocal(pid, path)) => {
                    let db = Builder::new_local(path).build().await.unwrap();
                    let conn = db.connect().unwrap();
                    let mut db_ref = db_ref.connection.lock().unwrap();
                    *db_ref = Some(conn);
                    env.send_and_clear(&pid, |env| ok().encode(env)).unwrap();
                }
                Some(DbMessage::Execute(pid, sql)) => {
                    let conn = db_ref.connection.lock().unwrap().take().unwrap();
                    let rows_changed = conn.execute(&sql, ()).await.unwrap();

                    // For some reason we gotta put it back now
                    let mut db_ref = db_ref.connection.lock().unwrap();
                    *db_ref = Some(conn);

                    env.send_and_clear(&pid, |env| (ok(), rows_changed).encode(env))
                        .unwrap();
                }
                None => continue,
            }
        }
    });
}
