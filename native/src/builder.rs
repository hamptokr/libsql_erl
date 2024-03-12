use std::{path::PathBuf, str::FromStr, sync::Mutex};

use crate::{
    atoms::{error, ok},
    task,
};
use rustler::{Atom, Encoder, Env, LocalPid, NifStruct, NifUnitEnum, OwnedEnv, ResourceArc};
use tokio::sync::mpsc::{channel, Receiver, Sender};

pub enum BuilderMsg {
    BuildLocal(LocalPid, PathBuf),
    Stop,
}

pub struct BuilderRef {
    pub inner: Mutex<Sender<BuilderMsg>>,
}

pub fn new(tx: Sender<BuilderMsg>) -> ResourceArc<BuilderRef> {
    ResourceArc::new(BuilderRef {
        inner: Mutex::new(tx),
    })
}

#[rustler::nif(name = "builder_start")]
pub fn start() -> (Atom, ResourceArc<BuilderRef>) {
    let (tx, rx) = channel::<BuilderMsg>(1000);
    spawn_builder(rx);
    (ok(), new(tx))
}

#[rustler::nif(name = "builder_stop")]
pub fn stop(resource: ResourceArc<BuilderRef>) -> Atom {
    send(resource.clone(), BuilderMsg::Stop);
    ok()
}

fn send(resource: ResourceArc<BuilderRef>, msg: BuilderMsg) {
    let lock = (*resource).inner.lock().expect("Failed to obtain a lock");
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
                Some(BuilderMsg::BuildLocal(pid, path)) => {}
                Some(BuilderMsg::Stop) => break,
                None => continue,
            }
        }
    });
}
