use rustler::{Env, Term};

mod atoms {
    rustler::atoms! {
        ok,
        error
    }
}

mod builder;
mod database;
mod task;

fn load(env: Env, _: Term) -> bool {
    builder::load(env);
    database::load(env);
    true
}

rustler::init!("libsql_nif", [], load = load);
