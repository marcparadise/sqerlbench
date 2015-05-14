-module(sqerlbench_tools).
-export([cleanup/0,
         setup/0,
         load_config/0]).

% Different versions are named differently:
% defined in rebar.config based on currnet versions...
-ifndef(EPGSQL).
-define(EPGSQL, pgsql).
-endif.


-define(APPS, [crypto, asn1, public_key, ssl, ?EPGSQL, pooler]).
% Exports
%
setup() ->
    application:stop(sasl),
    % Start in a clean state.
    destroy_db(),
    create_db(),
    populate_db(),
    [ ok = application:set_env(sqerl, Key, Val) || {Key, Val} <- config(sqerl)],
    [ ok = application:set_env(pooler, Key, Val) || {Key, Val} <- config(pooler)],
    [ application:start(A) || A <- ?APPS],
    ok.

cleanup() ->
    % Kill all conns to DB before we clean up the DB.
    application:stop(sqerl),
    pooler_sup:rm_pool(sqerl),
    % wtf how can we wait for everything to stop? Shouldn't that be DONE
    % by the time we get here?
    % This crashes workers because they're still working when it's invoked:
    % pooler_sup:rm_pool(sqerl),
    case basho_bench_config:get(destroy_db_on_finish) of
        true ->
            destroy_db();
        _ ->
            ok
    end,
    [application:stop(A) || A <- lists:reverse(?APPS ++ [sqerl])].

% Helper for user from console
load_config() ->
    basho_bench_config:load([privpath("tests.config")]).

%% Internal
config() ->
    basho_bench_config:get(config).

config(Which) ->
    C = config(),
    proplists:get_value(Which, C).

config_value(Which, Key) ->
    PL = config(Which),
    proplists:get_value(Key, PL).

connect(DB, User, Pass) ->
    DBPort = config_value(sqerl, db_port),
    DBHost = config_value(sqerl, db_host),
    {ok, C} = ?EPGSQL:connect(DBHost, User, Pass, [
        {database, DB},
        {timeout, 10000},
        {port, DBPort}
    ]),
    C.

admin_connection() ->
    PrivUser = config_value(postgres, db_root_user),
    PrivPass = config_value(postgres, db_root_password),

    connect("postgres", PrivUser, PrivPass).

user_connection() ->
    DBUser = config_value(sqerl, db_user),
    DBPass = config_value(sqerl, db_pass),
    DB = config_value(sqerl, db_name),
    connect(DB, DBUser, DBPass).


create_db() ->
    C = admin_connection(),
    DBName = config_value(sqerl, db_name),
    DBUser = config_value(sqerl, db_user),
    DBPass = config_value(sqerl, db_pass),
    valid_setup_results(?EPGSQL:squery(C, io_lib:format("CREATE USER ~s PASSWORD '~s'", [DBUser, DBPass]))),
    valid_setup_results(?EPGSQL:squery(C, io_lib:format("CREATE DATABASE ~s OWNER ~s", [DBName, DBUser]))),
    valid_setup_results(?EPGSQL:squery(C, io_lib:format("GRANT ALL PRIVILEGES ON DATABASE ~s TO ~s",[DBName, DBUser]))),
    ok = ?EPGSQL:close(C),
    % TODO NAME
    F = privpath(config_value(postgres, schema)),
    {ok, Schema} = file:read_file(F),
    % Object creation we do as user
    C2 = user_connection(),
    Results = ?EPGSQL:squery(C2, Schema),
    [ {ok, _, _} = R || R <- Results ],
    ok = ?EPGSQL:close(C2).

populate_db() ->
    C = user_connection(),
    F = privpath(config_value(postgres, populate)),
    {ok, Populate} = file:read_file(F),
    valid_insert_results(?EPGSQL:squery(C, Populate)),
    ok = ?EPGSQL:close(C).

valid_setup_results([{ok, _, _}|T]) ->
    valid_setup_results(T);
valid_setup_results([]) ->
    ok;
valid_setup_results({ok, _, _}) ->
    ok.

valid_insert_results([{ok, _}|T]) ->
    valid_insert_results(T);
valid_insert_results([]) ->
    ok;
valid_insert_results({ok, _}) ->
    ok.


destroy_db() ->
    C = admin_connection(),
    DBName = config_value(sqerl, db_name),
    DBUser = config_value(sqerl, db_user),
    valid_setup_results(?EPGSQL:squery(C, io_lib:format("DROP DATABASE IF EXISTS ~s", [DBName]))),
    valid_setup_results(?EPGSQL:squery(C, io_lib:format("DROP USER IF EXISTS ~s", [DBUser]))),
    ok = ?EPGSQL:close(C).


privpath(File) ->
    case code:priv_dir(sqerlbench) of
        {error, _} ->
            {ok, WD} = file:get_cwd(),
            filename:join([WD, "priv", File]);
        Path ->
            filename:join(Path, File)
    end.

