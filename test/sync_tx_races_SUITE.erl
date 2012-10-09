%% -*- tab-width: 4;erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
%% ----------------------------------------------------------------------------
%%
%% Copyright (c) 2005 - 2012 Nebularis.
%% Copyright (c) 2010 Dave Smith (dizzyd@dizzyd.com).
%%
%% Some portions of the code taken from sh (c) 2005 - 2012 Nebularis
%% Some portions of the code taken from rebar (c) 2010 Dave Smith
%% Some portions of the code taken from retest (c) 2010 Dave Smith
%%
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
%% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
%% IN THE SOFTWARE.
%% ----------------------------------------------------------------------------
-module(sync_tx_races_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

suite() -> [{timetrap, {minutes, 45}}].

all() ->
    timer:start(),
    [interupted_sync_transactions].
%    systest_suite:export_all(?MODULE).

interupted_sync_transactions(Config) ->
    prep_send_stop(),
    Sut = systest:get_system_under_test(Config),
    Procs = systest:list_processes(Sut),

    perform_random_restarts(Sut, Procs),

    Size = mnesia:table_info(employee, size),
    ct:pal("mnesia table contains ~p records~n", [Size]).

multiple_running_sync_transactions(_Config) ->
    timer:sleep(time_to_ms({minutes, 5})),
    Size = mnesia:table_info(employee, size),
    ct:pal("mnesia table contains ~p records~n", [Size]).

prep_send_stop() ->
    {timetrap, Timeout} = lists:keyfind(timetrap, 1, suite()),
    StopGap = time_to_ms({minutes, 2}),
    After = time_to_ms(Timeout) - StopGap,
    erlang:send_after(After, self(), stop).

perform_random_restarts(Sut, Procs) ->
    try
        Indexes =
            [X || {_, X} <- lists:sort(
                              [{random:uniform(), N} ||
                                  N <- lists:seq(1, length(Procs))])],
        [begin
             maybe_stop({seconds, erlang:max(45, random:uniform(100))}),
             {_Id, Ref} = lists:nth(I, Procs),
             systest:restart_process(Sut, Ref)
         end || I <- Indexes],

        maybe_stop({seconds, erlang:max(45, random:uniform(200))}),
        perform_random_restarts(Sut, Procs)
    catch
        throw:stop -> ok
    end.

maybe_stop(When) ->
    Ms = time_to_ms(When),
    systest:log("maybe stopping in ~p ms~n", [Ms]),
    receive stop -> throw(stop)
    after Ms -> systest:log("no stop signal received - go!~n", []),
                ok
    end.

time_to_ms({UoM, Val}) ->
    erlang:apply(timer, UoM, [Val]).

