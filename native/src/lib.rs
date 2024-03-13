use rustler::{Env, Term};

mod atoms {
    rustler::atoms! {
        ok,
        error
    }
}

mod database;
mod task;

fn load(env: Env, _: Term) -> bool {
    database::load(env);
    true
}

rustler::init!(
    "libsql_nif",
    [database::start, database::connect_local, database::execute],
    load = load
);
