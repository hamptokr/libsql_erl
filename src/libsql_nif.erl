-module(libsql_nif).

-export([init/0, builder_start/0, builder_stop/1]).

-on_load init/0.

-define(APPNAME, ?MODULE).
-define(LIBNAME, "libsql_nif").

builder_start() ->
    not_loaded(?LINE).

builder_stop(_) ->
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
    io:format("~s~n", [SoName]),
    erlang:load_nif(SoName, 0).

not_loaded(Line) ->
    exit({not_loaded, [{module, ?MODULE}, {line, Line}]}).
