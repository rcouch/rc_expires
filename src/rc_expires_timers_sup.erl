%%% -*- erlang -*-
%%%
%%% This file is part of rc_expires released under the MIT license.
%%% See the NOTICE for more information.

-module(rc_expires_timers_sup).
-behaviour(supervisor).

-export([start_timer/1]).
-export([start_link/0, stop/1]).

-export([init/1]).

start_timer(DbName) ->
    supervisor:start_child(?MODULE, [DbName]).



start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

stop(_S) -> ok.

%% @private
init([]) ->
    {ok,
     {{simple_one_for_one, 10, 10},
      [{rc_expires_timer,
        {rc_expires_timer, start_link, []},
        permanent, 5000, worker, [rc_expires_timer]}]}}.
