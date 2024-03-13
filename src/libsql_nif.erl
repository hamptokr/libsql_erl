-module(libsql_nif).

-export([init/0, database_start/0, database_connect_local/2, database_execute/2]).

-on_load init/0.

-define(APPNAME, ?MODULE).
-define(LIBNAME, "libsql_nif").

database_start() ->
    not_loaded(?LINE).

database_connect_local(_, _) ->
    not_loaded(?LINE).

database_execute(_, _) ->
    not_loaded(?LINE).

init() -> 
    SoName =
        case code:priv_dir(?APPNAME) of
            {error, bad_name} ->
                case filelib:is_dir(
                         filename:join(["..", priv]))
                of
                    true ->
                        filename:join(["..", priv, ?LIBNAME]);
                    _ ->
                        filename:join([priv, ?LIBNAME])
                end;
            Dir ->
                filename:join(Dir, ?LIBNAME)
        end,
    erlang:load_nif(SoName, 0).

not_loaded(Line) ->
    exit({not_loaded, [{module, ?MODULE}, {line, Line}]}).
