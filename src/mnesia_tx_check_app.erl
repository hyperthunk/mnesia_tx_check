-module(mnesia_tx_check_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    mnesia_tx_check_sup:start_link().

stop(_State) ->
    error_logger:info_msg("table size = ~p~n",
                          [mnesia:table_info(employee, size)]),
    ok.
