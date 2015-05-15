-module(sqerlbench).


-export([new/1, run/4]).
-record(state, {id, last_key = 0 } ).

new(Id) ->
    {ok, #state{id = Id}}.

run(select_hit, _KeyGen, _ValueGen, State) ->
    % This record is set up in populate
    case sqerl:select(fetch_node, [<<"ABCDEFGH">>]) of
        {error, Any} ->
            {error, Any, State};
        _ ->
            {ok, State}
    end;
run(select_miss, _KeyGen, _ValueGen, State) ->
    case sqerl:select(fetch_node, [<<"NoSuchId">>]) of
        {error, Any} ->
            {error, Any, State};
        {ok, _}  ->
            {ok, State}
    end;
run(statement_update, _KeyGen, _ValueGen, #state{id = Id, last_key = LastKey} = State) ->
    UpdateKey = make_key(Id, LastKey),
    Args = [UpdateKey, <<"different_org_id">>],
    case sqerl:statement(update_node_org, Args) of
        {error, Any} ->
            {error, Any, State};
        _ ->
            {ok, State}
    end;

run(statement_ins, _KeyGen, _ValueGen, #state{id = Id, last_key = Key} = State) ->
    NewKey = make_key(Id, Key+1),
    AuthKey = make_key(Id, Key+2),
    NameKey = make_key(Id, Key+3),
    Args = [NewKey, AuthKey,  <<"anorgid">>, NameKey, <<"_default">>,
            <<"me">>],
    case sqerl:statement(insert_node, Args) of
        {error, Any} ->
            {error, Any, State};
        _ ->
            {ok, State#state{last_key = Key + 3}}
    end;
run(statement_del, _KeyGen, _ValueGen, #state{id = Id, last_key = LastKey} = State) ->
    DeleteKey = make_key(Id, LastKey),
    % Not guaranteed to hit if we do a del before we do an insert.
    case sqerl:statement(delete_node, [DeleteKey]) of
        {error, Any} ->
            {error, Any, State};
        {ok, _}  ->
            {ok, State}
    end.

make_key(Id, Key) ->
   list_to_binary(io_lib:format("~p-~p", [Id, Key])).

