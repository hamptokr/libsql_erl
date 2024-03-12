-module(libsql_nif).

-export([init/0]).

-on_load(init/0).

init() -> 
    SoName = case code:priv_dir(libsql_erl) of
        {error, bad_name} ->
            case code:which(?MODULE) of
                Filename when is_list(Filename) ->
                    filename:join([filename:dirname(Filename),"../priv", "crates", "libsql_nif", "libsql_nif"]);
                _ ->
                    filename:join(["../priv", "crates", "libsql_nif", "libsql_nif"])
            end;
        Dir ->
            filename:join([Dir, "crates", "libsql_nif", "libsql_nif"])
    end,
    erlang:load_nif(SoName, 0).
