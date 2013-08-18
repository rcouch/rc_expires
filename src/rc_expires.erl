%%% -*- erlang -*-
%%%
%%% This file is part of rc_expires released under the MIT license.
%%% See the NOTICE for more information.

-module(rc_expires).

-export([clean_expired/1]).
-export([open_doc/2]).
-export([is_expired/2]).

-include("rc_expires.hrl").
-include_lib("couch/include/couch_db.hrl").
-include_lib("couch_mrview/include/couch_mrview.hrl").


%% @doc clean all docs expired in this database.
-spec clean_expired(DbName::binary()) -> ok.
clean_expired(DbName) ->
    NbExpired = do_clean_expired(DbName),
    if NbExpired >= 25 ->
            clean_expired(DbName);
        true -> ok
    end.


%% @doc open a doc if it's not expired, else return  not_found error
-spec open_doc(DbName::binary(), DocId::binary())
    -> {ok, term()} | {error, term()}.
open_doc(DbName, DocId) ->
    {ok, Db} = open_db(DbName),
    try couch_db:open_doc(Db, DocId, []) of
        {ok, Doc} ->
            JsonDoc = couch_doc:to_json_obj(Doc, []),
            io:format("json doc ~p~n", [JsonDoc]),
            case is_expired1(JsonDoc) of
                true ->
                    delete_doc(DbName, DocId),
                    {error, not_found};
                false ->
                    {ok, Doc}
            end;
        {not_found, _} ->
            {error, not_found};
        Else ->
            Else
    after
        couch_db:close(Db)
    end.

%% @doc return true if a doc is expired, error in other cases
-spec is_expired(DbName::binary(), DocId::binary()) -> true | error.
is_expired(DbName, DocId) ->
    {ok, Db} = open_db(DbName),
    try couch_db:open_doc(Db, DocId, []) of
        {ok, Doc} ->
            JsonDoc = couch_doc:to_json_obj(Doc, []),
            case is_expired1(JsonDoc) of
                true ->
                    true;
                _ ->
                    error
            end;
        _ ->
            error
    after
        couch_db:close(Db)
    end.

%% @private
do_clean_expired(DbName) ->
    DefaultTTL = couch_config:get("rcouch", "expiry_secs", 0),
    {ok, Db} = open_db(DbName),

    try couch_mrview:query_view(Db, ?DNAME, <<"expires">>, #mrargs{limit=100},
                                fun view_cb/2, {[], DefaultTTL}) of
        {ok, {ToDelete, _}} ->
            NbProcessed = length(ToDelete),
            delete_expired(ToDelete, DbName),
            NbProcessed;
        _ ->
            0
    after
        couch_db:close(Db)
    end.

%% @private
view_cb({row, Row}, {ToDelete, DefaultTTL}=Acc) ->
    Val = couch_util:get_value(value, Row),
    case is_expired1(Val, DefaultTTL) of
        false ->
            {ok, Acc};
        true ->
            %% add the document to the list of documents to delete
            DocId = couch_util:get_value(id, Row),
            {ok, {[DocId | ToDelete], DefaultTTL}}
    end;
view_cb(_Other, Acc) ->
    {ok, Acc}.


is_expired1(Doc) ->
    DefaultTTL = couch_config:get("rcouch", "expiry_secs", 0),
    is_expired1(Doc, DefaultTTL).

is_expired1({Props}, DefaultTTL) ->
    TTL = couch_util:get_value(<<"ttl">>, Props),
    TS = couch_util:get_value(<<"timestamp">>, Props),

    %% get current time
    {Mega, Sec, _Micro} = os:timestamp(),
    Now = (Mega * 1000000) + Sec,

    %% is this document expired?
    Expires = case {TS, TTL} of
        {undefined, _} ->
            %% never expires, no timestamp set
            0;
        {_, undefined} when DefaultTTL =:= 0 ->
            %% never expires, no TTL
            0;
        {_, undefined} ->
            %% use default TTL
            TS + DefaultTTL;
        _ ->
            TS + TTL
    end,

    case Expires of
        0 ->
            false;
        _ ->
            %% add the document to the list of documents to delete
            if Expires > Now ->
                    false;
                true ->
                    true
            end
    end.

delete_expired([], _DbName) ->
    ok;
delete_expired([DocId | Rest], DbName) ->
    delete_doc(DbName, DocId),
    delete_expired(Rest, DbName).



delete_doc(DbName, DocId) ->
    delete_doc(DbName, DocId, true).

delete_doc(DbName, DocId, ShouldMutate) ->
    {ok, Db} = couch_db:open(DbName, []),
    {ok, Revs} = couch_db:open_doc_revs(Db, DocId, all, []),
    try [Doc#doc{deleted=true} || {ok, #doc{deleted=false}=Doc} <- Revs] of
        [] ->
            not_found;
        Docs when ShouldMutate ->
            try couch_db:update_docs(Db, Docs, []) of
                {ok, _} ->
                    ok
            catch conflict ->
                    % check to see if this was a replication race or if leafs survived
                    delete_doc(DbName, DocId, false)
            end;
        _ ->
            % we have live leafs that we aren't allowed to delete. let's bail
            conflict
    after
        couch_db:close(Db)
    end.


open_db(DbName) when is_list(DbName) ->
    open_db(list_to_binary(DbName));
open_db(DbName) ->
    Options = [{user_ctx, #user_ctx{roles=[<<"_admin">>]}}],
    case couch_db:open_int(DbName, Options) of
        {ok, Db} ->
            ok = ensure_ddoc_exists(Db),
            couch_db:reopen(Db);
        Error ->
            Error
    end.

ensure_ddoc_exists(Db) ->
    case couch_db:open_doc(Db, ?DNAME) of
        {not_found, _Reason} ->
            {ok, DesignDoc} = expires_design_doc(),
            {ok, _Rev} =  couch_db:update_doc(Db, DesignDoc, []);
        {ok, Doc} ->
            {Props} = couch_doc:to_json_obj(Doc, []),
            {Views} = couch_util:get_value(<<"views">>, Props),
            case couch_util:get_value(<<"expires">>, Views) of
                ?EXPIRES_JS_DDOC ->
                    ok;
                _ ->
                    Views1 = lists:keyreplace(<<"expires">>, 1, Views,
                                              {<<"expires">>,
                                               ?EXPIRES_JS_DDOC}),
                    Props1 = lists:keyreplace(<<"views">>, 1, Props, Views1),
                    NewDoc = couch_doc:from_json_obj({Props1}),
                    {ok, _Rev} = couch_db:update_doc(Db, NewDoc, [])
            end
    end,
    ok.

expires_design_doc() ->
    DocProps = [
            {<<"_id">>, ?DNAME},
            {<<"language">>,<<"javascript">>},
            {<<"views">>, {[{<<"expires">>, ?EXPIRES_JS_DDOC}]}}
    ],
    {ok, couch_doc:from_json_obj({DocProps})}.
