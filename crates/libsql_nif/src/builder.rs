use std::{path::PathBuf, str::FromStr, sync::Mutex};

use crate::{
    atoms::{error, ok},
    database::{spawn_database, DatabaseRef, DbMessage},
    task,
};
use rustler::{Atom, Encoder, Env, LocalPid, OwnedEnv, ResourceArc};
use tokio::sync::mpsc::{channel, Receiver, Sender};

pub enum BuilderMsg {
    BuildLocal(LocalPid, PathBuf),
    Stop,
}

struct BuilderRef(Mutex<Sender<BuilderMsg>>);

impl BuilderRef {
    pub fn new(tx: Sender<BuilderMsg>) -> ResourceArc<BuilderRef> {
        ResourceArc::new(BuilderRef(Mutex::new(tx)))
    }
}

#[rustler::nif(name = "start_builder")]
pub fn start() -> (Atom, ResourceArc<BuilderRef>) {
    let (tx, rx) = channel::<BuilderMsg>(1000);
    spawn_builder(rx);
    (ok(), BuilderRef::new(tx))
}

#[rustler::nif(name = "build_local")]
pub fn build_local(
    env: Env,
    resource: ResourceArc<BuilderRef>,
    path: &str,
) -> (Atom, ResourceArc<BuilderRef>) {
    send(
        resource.clone(),
        BuilderMsg::BuildLocal(env.pid(), PathBuf::from_str(path).unwrap()),
    );
    (ok(), resource)
}

#[rustler::nif(name = "stop_builder")]
pub fn stop(resource: ResourceArc<BuilderRef>) -> Atom {
    send(resource.clone(), BuilderMsg::Stop);
    ok()
}

fn send(resource: ResourceArc<BuilderRef>, msg: BuilderMsg) {
    let lock = resource.0.lock().expect("Failed to obtain a lock");
    let sender = lock.clone();

    task::spawn(async move {
        sender.send(msg).await.unwrap();
    });
}

pub fn load(env: Env) -> bool {
    rustler::resource!(BuilderRef, env);
    true
}

fn spawn_builder(mut rx: Receiver<BuilderMsg>) {
    task::spawn(async move {
        let mut env = OwnedEnv::new();

        loop {
            match rx.recv().await {
                Some(BuilderMsg::BuildLocal(pid, path)) => {
                    match libsql::Builder::new_local(path).build().await {
                        Ok(db) => {
                            let (db_tx, db_rx) = channel::<DbMessage>(1000);
                            spawn_database(db, db_rx);
                            (ok(), DatabaseRef::new(db_tx));
                        }

                        Err(err) => {
                            env.send_and_clear(&pid, move |env| {
                                (error(), format!("{:?}", err)).encode(env)
                            })
                            .unwrap();
                        }
                    }
                }
                Some(BuilderMsg::Stop) => break,
                None => continue,
            }
        }
    });
}
