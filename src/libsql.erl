-module(libsql).

-export([start_builder/0]).

start_builder() ->
    err().

err() ->
    erlang:nif_error(libsql_nif_not_loaded).
