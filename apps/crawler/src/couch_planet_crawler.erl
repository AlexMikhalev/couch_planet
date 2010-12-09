%%% This file is part of the couch_planet package and is released under the
%%% Tumbolia Public License. See LICENSE for more details.
%%%
%%% @author Klaus Trainer <klaus.trainer@web.de>

%%% @doc couch_planet_crawler

-module(couch_planet_crawler).
-behaviour(gen_server).

-author('Klaus Trainer <klaus.trainer@web.de>').

-include("couch_planet.hrl").

%% user interface
-export([start_link/1]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_info/2, handle_cast/2,
         terminate/2, code_change/3]).

-define(HTTP_CLIENT_OPTIONS, [{body_format, binary}]).


%% External API

start_link(Url) ->
    gen_server:start_link(?MODULE, Url, []).

init(Url) ->
    spawn_link(fun() -> start(Url) end),
    {ok, 0}.

handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    error_logger:info_msg("~p stopping~n", [?MODULE]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% Internal API

%% @spec start(string()) -> none() | {error, Reason}
start(Url) ->
    DdocUrl = couch_planet:get_app_env(ddoc_url),
    ViewUrl = DdocUrl ++ "/_view/update_status?key=\"" ++ Url ++ "\"",
    Options = [{full_result, false}|?HTTP_CLIENT_OPTIONS],
    case http:request(get, {ViewUrl, []}, ?HTTP_OPTIONS, Options) of
    {error, Reason} ->
        error_logger:error_msg("get: ~p - ~p~n", [ViewUrl, Reason]),
        timer:sleep(?UPDATE_INTERVAL),
        start(Url);
    {ok, {Code, _Body}} when Code =/= 200 ->
        error_logger:error_msg("get: ~p - ~p~n", [ViewUrl, Code]),
        timer:sleep(?UPDATE_INTERVAL),
        start(Url);
    {ok, {200, Body}} ->
        case parse_update_status_view(Body) of
        {error, Reason} ->
            error_logger:error_msg("View parse error - ~p~n", [Reason]),
            timer:sleep(?UPDATE_INTERVAL),
            start(Url);
        UpdateStatus ->
            StatusTable = ets:new(update_status, [private, set]),
            ets:insert(StatusTable, UpdateStatus),
            loop(Url, StatusTable)
        end
    end.

%% @spec loop(string(), tid()) -> none()
loop(Url, StatusTable) ->
    loop(Url, StatusTable, "").

%% @spec loop(string(), tid(), string()) -> none()
loop(Url, StatusTable, DocHash) ->
    case get_doc_if_modified(Url, DocHash) of
    false ->
        NewDocHash = DocHash;
    {error, _Reason} ->
        NewDocHash = DocHash;
    {ok, {NewDocHash, ContentType, Doc}} ->
        case couch_planet_parser:new_entries(Doc, ContentType, Url, StatusTable) of
        {error, Reason} ->
            error_logger:error_msg("Feed parse error - ~p~n", [Reason]);
        [] ->
            ok;
        Entries ->
            do_bulk_docs_request(entries_to_json(Entries), StatusTable)
        end
    end,
    timer:sleep(?UPDATE_INTERVAL),
    loop(Url, StatusTable, NewDocHash).

%% @spec do_bulk_docs_request(binary(), tid()) -> ok
do_bulk_docs_request(Json, StatusTable) ->
    BulkDocsUrl = couch_planet:get_app_env(bulk_docs_url),
    Options = [{full_result, false}|?HTTP_CLIENT_OPTIONS],
    case http:request(post, {BulkDocsUrl, [], "application/json", Json}, ?HTTP_OPTIONS, Options) of
    {error, Reason} ->
        error_logger:error_msg("post: ~p - ~p~n", [BulkDocsUrl, Reason]),
        timer:sleep(?UPDATE_INTERVAL),
        do_bulk_docs_request(Json, StatusTable);
    {ok, {Code, _Body}} when Code =/= 201 ->
        error_logger:error_msg("post: ~p - ~p~n", [BulkDocsUrl, Code]),
        timer:sleep(?UPDATE_INTERVAL),
        do_bulk_docs_request(Json, StatusTable);
    {ok, {201, ResJson}} ->
        case parse_bulk_docs_response(ResJson) of
        {error, Reason} ->
            error_logger:error_msg("post: ~p - ~p~n", [BulkDocsUrl, Reason]);
        IdRevTuples ->
            lists:foreach(
                fun({Id, Rev}) ->
                    true = ets:update_element(StatusTable, Id, {3, Rev})
                end, IdRevTuples),
            ok
        end
    end.

%% @spec parse_bulk_docs_response(binary()) -> [{binary(), binary()}] | {error, Reason}
parse_bulk_docs_response(B) ->
    L= mochijson2:decode(B),
    lists:foldl(
        fun(E, Acc) ->
            {struct, TupleList} = E,
            case lists:keyfind(<<"id">>, 1, TupleList) of
            false ->
                {error, "no 'id' field in bulk update response"};
            {<<"id">>, Id} ->
                case lists:keyfind(<<"rev">>, 1, TupleList) of
                false -> {error, "could not update '" ++ ?b2l(Id)} ++ "'";
                {<<"rev">>, Rev} -> [{Id, Rev}|Acc]
                end
            end
        end, [], L).

%% @spec parse_update_status_view(binary()) -> [{binary(), binary(), binary()}] | {error, Reason}
parse_update_status_view(B) ->
    {struct, L} = mochijson2:decode(B),
    case lists:keyfind(<<"rows">>, 1, L) of
    false ->
        {error, "no rows field in view 'update_status'"};
    {<<"rows">>, Rows} ->
        [{Title, TimeStamp, Rev} || {struct, [_Id, _Key, {<<"value">>, [Title, TimeStamp, Rev]}]} <- Rows]
    end.

%% @spec entries_to_json([#entry{}]) -> binary()
entries_to_json(Entries) ->
   entries_to_json(Entries, <<"{\"all_or_nothing\":false,\"docs\":[">>).
    
entries_to_json([H|T], Acc) ->
    case T of
    [] ->
        Entry = entry_to_json(H),
        <<Acc/binary,Entry/binary,"]}">>;
    _ ->
        Entry = entry_to_json(H),
        entries_to_json(T, <<Acc/binary,Entry/binary,",">>)
    end.

%% @spec entry_to_json(#entry{}) -> binary()
entry_to_json(Entry) ->
    #entry{id=Id, rev=Rev, title=Title, postedTime=PostedTime, type=Type,
           verb=Verb, object=Object, actor=Actor, provider=Provider} = Entry,
    #object{id=OId, summary=OSummary, objectType=OObjectType} = Object,
    #actor{link=ALink, name=AName, objectType=AObjectType} = Actor,
    #provider{id=PId, name=PName, objectType=PObjectType} = Provider,
    B = case Rev of
    <<>> -> <<"{\"_id\":\"",Id/binary,"\",">>;
    _ -> <<"{\"_id\":\"",Id/binary,"\",\"_rev\":\"",Rev/binary,"\",">>
    end,
    <<B/binary,"\"title\":\"",Title/binary,"\",\"postedTime\":\"",PostedTime/binary,"\",\"type\":\"",Type/binary,"\",\"verb\":\"",Verb/binary,"\",\"object\":{\"id\":\"",OId/binary,"\",\"summary\":\"",OSummary/binary,"\",\"objectType\":\"",OObjectType/binary,"\"},\"actor\":{\"link\":\"",ALink/binary,"\",\"name\":\"",AName/binary,"\",\"objectType\":\"",AObjectType/binary,"\"},\"provider\":{\"id\":\"",PId/binary,"\",\"name\":\"",PName/binary,"\",\"objectType\":\"",PObjectType/binary,"\"}}">>.

%% @spec get_doc_if_modified(string(), string()) -> false | {ok, {DocHash, ContentType, Doc}} | {error, Reason}
get_doc_if_modified(Url, DocHash) ->
    case http:request(head, {Url, []}, ?HTTP_OPTIONS, ?HTTP_CLIENT_OPTIONS) of
    {error, Reason} ->
        error_logger:error_msg("head: ~p - ~p~n", [Url, Reason]),
        {error, Reason};
    {ok, {{_, Code, _}, _Headers, _Body}} when Code =/= 200 ->
        error_logger:error_msg("head: ~p - ~p~n", [Url, Code]),
        {error, Code};
    {ok, {{_, 200, _}, Headers, _}} ->
        DocHash1 = get_etag_header(Headers),
        case DocHash1 of
        DocHash ->
            false;
        _ ->
            case http:request(get, {Url, []}, ?HTTP_OPTIONS, ?HTTP_CLIENT_OPTIONS) of
            {error, Reason} ->
                error_logger:error_msg("get: ~p - ~p~n", [Url, Reason]),
                {error, Reason};
            {ok, {{_, Code, _}, _, _}} when Code =/= 200 ->
                error_logger:error_msg("get: ~p - ~p~n", [Url, Code]),
                {error, Code};
            {ok, {{_, 200, _}, Headers1, Body}} ->
                NewDocHash = case DocHash1 of
                undefined ->
                    % so, we need to compute a document hash code ourselves
                    ?i2l(erlang:phash2(Body));
                _ ->
                    DocHash1
                end,
                case NewDocHash of
                DocHash ->
                    false;
                _ ->
                    ContentType0 = get_content_type_from_header(Headers1),
                    ContentType = case ContentType0 of
                    unknown -> get_content_type_from_body(Body);
                    _ -> ContentType0
                    end,
                    {ok, {NewDocHash, ContentType, Body}}
                end
            end
        end
    end.

%% @spec get_etag_header([{string(), string()}]) -> undefined | string()
get_etag_header([]) ->
    undefined;
get_etag_header([{K, V}|T]) ->
    case K of
    "etag" -> V;
    _ -> get_etag_header(T)
    end.

get_content_type_from_header([]) ->
    unknown;
get_content_type_from_header([{K, V}|T]) ->
    case K of
    "content-type" ->
        BreakPos = string:rstr(V, ";") - 1,
        ContentType = case BreakPos > 0 of
        true -> string:sub_string(V, 1, BreakPos);
        _ -> V
        end,
        case ContentType of
        "text/xml" -> rss;
        "application/rss+xml" -> rss;
        "application/atom+xml" -> atom;
        "application/xml" -> atom;
        _ -> unknown
        end;
    _ ->
        get_content_type_from_header(T)
    end.

get_content_type_from_body(Doc) ->
    case re:run(Doc, <<"<feed">>, [{capture, none}]) of
    match ->
        atom;
    nomatch ->
        case re:run(Doc, <<"<channel">>, [{capture, none}]) of
        match -> rss;
        nomatch -> unknown
        end
    end.
