%%% -*- erlang -*-
%%%
%%% This file is part of rc_expires released under the MIT license.
%%% See the NOTICE for more information.
%%%

-module(rc_expires_timer).
-behaviour(gen_server).

-export([start_link/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-record(state, {dbname,
                tref}).


start_link(DbName) ->
    gen_server:start_link(?MODULE, [DbName] , []).

init([DbName]) ->
    TRef = erlang:start_timer(1000, self(), clean),
    {ok, #state{dbname=DbName, tref=TRef}}.

handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({timeout, TRef, clean}, #state{dbname=DbName, tref=TRef}=State) ->
    erlang:cancel_timer(TRef),
    rc_expires:clean_expired(DbName),
    NewTRef = erlang:start_timer(10000, self(), clean),
    {noreply, State#state{tref=NewTRef}};

handle_info(_Info, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.
