-module(mnesia_tx_check).
-define(SERVER, ?MODULE).

-export([start/3]).
-export([start_link/0]).
-export([on_start/1]).
-export([do_start/1]).

-compile(export_all).

-record(employee, {name,
                   salary}).

on_start(Sut) ->
    Nodes = [Id || {Id, _Ref} <- systest:list_processes(Sut)],

    mnesia:stop(),
    Where = [node()|Nodes],
    mnesia:delete_schema(Where),

    systest:log("creating mnesia schema on nodes ~p~n", [Nodes]),
    do_start(Where).

do_start(Nodes) ->
    mnesia:create_schema(Nodes),
    mnesia:start(),
    [rpc:call(N, mnesia, start, []) || N <- Nodes],
    case mnesia:create_table(employee,
                             [{attributes, record_info(fields, employee)},
                              {disc_copies, Nodes}]) of
        {atomic, ok} ->
            ok;
        {aborted, _Reason}=Err ->
            throw(Err)
    end.

start(Node, _ClusterRef, _Siblins) ->
    Id = systest:process_data(id, Node),
    Result = rpc:call(Id, application, start, [?MODULE, permanent]),
    error_logger:info_msg("starting ~p on ~p: ~p~n", [?MODULE, Id, Result]),
    Result.

remote_stop() ->
    application:stop(?MODULE).

stop(Node) ->
    NodeId = systest:process_data(id, Node),
    rpc:call(NodeId, ?MODULE, remote_stop, []).

start_link() ->
    bootstrap_db(),
    Pid = spawn_link(fun() -> execute(go) end),
    {ok, Pid}.

bootstrap_db() ->
    case mnesia:system_info(is_running) of
        yes ->
            ok;
        _ ->
            mnesia:start(),
            timer:sleep(1000),
            bootstrap_db()
    end,
    Nodes = mnesia:system_info(db_nodes) -- [node()],
    {ok, _OldNodes} = mnesia:change_config(extra_db_nodes, Nodes),
%    mnesia:wait_for_tables([employee], 2000),
    mnesia:add_table_copy(schema, node(), disk_copies),
    mnesia:add_table_copy(employee, node(), disk),
    ok.

execute(go) ->
    next(write(1));
execute({go, Last}) ->
    next(write(Last));
execute({wait, Last}) ->
    receive X -> error_logger:info_msg("unexpected message: ~p~n", [X]),
                 exit({unexpected_message, X})
    after   0 -> next(write(Last))
    end.

next({atomic, Last}) ->
    if Last rem 2 =:= 0 -> execute({go, Last});
                   true -> execute({wait, Last})
    end;
next(Other) ->
    exit({internal_error, Other}).


write(Last) ->
    Next = Last + 1,
    mnesia:sync_transaction(
      fun() ->
              case Last rem 5 =:= 0 of
                  false ->
                      mnesia:write(
                        #employee{name=emp_name(Next),
                                  salary=5000}),
                      Next;
                  true ->
                      Id = round(max(1, Last / 2)),
                      [#employee{salary=Salary}=Rec] =
                          mnesia:read(employee, emp_name(Id)),
                      mnesia:write(Rec#employee{salary=Salary * 1.5}),
                      Next
              end
      end).

emp_name(Id) ->
    atom_to_list(node()) ++ "-emp" ++ integer_to_list(Id).

