# libsql_erl

This is a **very early wip** NIF for [libsql](https://github.com/tursodatabase/libsql).

## What works?

1. Connecting to a local db
2. Executing SQL without any params


## Example

```erlang
{ok, Database} = libsql_nif:database_start().
libsql_nif:database_connect_local(Database, <<"./testing.db">>).

receive
  ok ->
    io:format("Connection successful~n")
end.

% Connection Successful

libsql_nif:database_execute(Database, <<"CREATE TABLE IF NOT EXISTS users (email TEXT)">>).

receive
  {ok, RowsChangedOnCreate} ->
    io:format("Rows changed: ~p~n", [RowsChangedOnCreate])
end.

% Rows changed: 0

libsql_nif:database_execute(Database, <<"INSERT INTO users (email) VALUES ('alice@example.org')">>).

receive
  {ok, RowsChangedOnInsert} ->
    io:format("Rows changed: ~p~n", [RowsChangedOnInsert])
end.

% Rows changed: 1
```

