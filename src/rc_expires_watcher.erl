%%% -*- erlang -*-
%%%
%%% This file is part of rc_expires released under the MIT license.
%%% See the NOTICE for more information.

-module(rc_expires_watcher).
-behaviour(gen_server).


-export([start_link/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-include_lib("couch/include/couch_db.hrl").

-record(state, {
        notifier_pid,
        timers}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    process_flag(trap_exit, true),
    %% start the db notifier used to start/stop db timers
    {ok, Pid} = start_db_notifier(),
    %% init timers for existing dbs
    {ok, Timers0} = init_timers(),
    {ok, #state{notifier_pid = Pid,
                timers = Timers0}}.

handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast({created, DbName}, #state{timers=Timers}=State) ->
    case can_handle_expires(DbName) of
        true ->
            {ok, Pid} = rc_expires_timers_sup:start_timer(DbName),
            {noreply, State#state{timers=dict:store(DbName, Pid, Timers)}};
        _ ->
            {noreply, State}
    end;

handle_cast({deleted, DbName}, #state{timers=Timers}=State) ->
    Timers1 = case dict:find(DbName, Timers) of
        {ok, Pid} ->
            supervisor:terminate_child(rc_expires_timers_sup, Pid),
            dict:erase(DbName, Timers);
        _ ->
            Timers
    end,
    {noreply, State#state{timers=Timers1}}.


handle_info({'EXIT', Pid, Reason}, #state{notifier_pid=Pid}=State) ->
    ?LOG_INFO("watcher notifier (pid ~p) unexpectedly crashed:~n~p~n",
              [Pid, Reason]),
    {ok, Pid} = start_db_notifier(),
    {ok, State#state{notifier_pid = Pid}};

handle_info(_Info, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, #state{notifier_pid=Pid}) ->
    catch couch_db_update_notifier:stop(Pid),
    ok.

start_db_notifier() ->
    Self = self(),
    couch_db_update_notifier:start_link(fun
            ({created, DbName}) ->
                gen_server:cast(Self, {created, DbName});
            ({deleted, DbName}) ->
                gen_server:cast(Self, {deleted, DbName});
            (_) ->
                ok
        end).


init_timers() ->
    couch_server:all_databases(fun(DbName, D) ->
                case can_handle_expires(DbName) of
                    true ->
                        {ok, Pid} = rc_expires_timers_sup:start_timer(DbName),
                        {ok, dict:store(DbName, Pid, D)};
                    _ ->
                        {ok, D}
                end
        end, dict:new()).

can_handle_expires(DbName) when is_list(DbName) ->
    can_handle_expires(?l2b(DbName));
can_handle_expires(DbName) ->
    RepDb =  ?l2b(couch_config:get("replicator", "db", "_replicator")),
    UserDb = ?l2b(couch_config:get("couch_httpd_auth", "authentication_db",
                                   "_users")),
    case lists:member(DbName, [RepDb, UserDb]) of
        true ->
            false;
        _ ->
            true
    end.
