%%% This file is part of the couch_planet package and is released under the
%%% Tumbolia Public License. See LICENSE for more details.
%%%
%%% @author Klaus Trainer <klaus.trainer@web.de>

%%% @doc couch_planet_image_persister

-module(couch_planet_image_persister).

-author('Klaus Trainer <klaus.trainer@web.de>').

-include("couch_planet.hrl").

%% user interface
-export([preprocess_entries/2, attach_images/2]).


%% External API

%% @spec preprocess_entries([#entry{}], tid()) -> [{#entry{}, Urls::[string()]}]
preprocess_entries(Entries, StatusTable) ->
    lists:foldl(
        fun(Entry, Acc) ->
            Id = Entry#entry.id,
            Object0 = Entry#entry.object,
            {NewSummary, Urls} = process_summary(Id, Object0#object.summary),
            case Urls of
            [] ->
                Acc;
            _ ->
                Object1 = Object0#object{summary = NewSummary},
                [{_Id, _TimeStamp, Rev}] = ets:lookup(StatusTable, Id),
                [{Entry#entry{rev = Rev, object = Object1}, Urls}|Acc]
            end
        end, [], Entries).

%% @spec attach_images({EntryId::binary(), Urls::[string()]}, tid()) -> ok
attach_images(IdUrlsTuples, StatusTable) ->
    lists:foreach(
        fun({Id, Urls}) ->
            lists:foreach(
                fun(Url) ->
                    spawn(
                        fun() ->
                            image_attacher(Id, ?b2l(Url), StatusTable)
                        end)
                end, Urls)
        end, IdUrlsTuples),
    ok.


%% Internal API

%% @spec process_summary(binary(), binary()) -> {binary(), Urls::[string()]}
process_summary(EntryId, Summary) ->
    process_summary(EntryId, Summary, {<<>>, []}).

%% @spec process_summary(binary(), binary(), {binary(), [string()]}) -> {binary(), Urls::[string()]}
process_summary(EntryId, Data, {SummaryAcc, Urls}) ->
    case find_next_img(Data) of
    false ->
        {<<SummaryAcc/binary,Data/binary>>, Urls};
    {StartOffs, Length} ->
        <<Prefix:StartOffs/binary,Url:Length/binary,Rest/binary>> = Data,
        DocName = ?l2b(edoc_lib:escape_uri(?b2l(EntryId))),
        UrlHash = ?l2b(?i2l(erlang:phash2(Url))),
        NewUrl = <<"../../../../",DocName/binary,"/",UrlHash/binary>>,
        process_summary(EntryId, Rest, {<<SummaryAcc/binary,Prefix/binary,NewUrl/binary>>, [Url|Urls]})
    end.

find_next_img(Data) ->
    case get(img_regex) of
    undefined ->
        {ok, MP} = re:compile(<<"<img ([^>]* +)??src=('|\\\\\")(https?://.+?)('|\\\\\")( +[^>]*)??(>|/>)">>, [caseless, dotall]),
        put(img_regex, MP);
    MP ->
        ok
    end,
    case re:run(Data, MP, [{capture, [3]}]) of
    nomatch -> false;
    {match, [{StartOffs, Length}]} -> {StartOffs, Length}
    end.

%% @spec image_attacher(binary(), string(), tid()) -> ok
image_attacher(EntryId, Url, StatusTable) ->
    case http:request(get, {Url, []}, ?HTTP_OPTIONS, [{body_format, binary}]) of
    {error, Reason} ->
        error_logger:error_msg("get: ~p - ~p~n", [Url, Reason]),
        timer:sleep(?RETRY_INTERVAL),
        image_attacher(EntryId, Url, StatusTable);
    {ok, {{_, Code, _Body}, _, _}} when Code >= 400 andalso Code < 500 ->
        error_logger:error_msg("get: ~p - ~p~n", [Url, Code]);
    {ok, {{_, Code, _Body}, _, _}} when Code >= 500 ->
        error_logger:error_msg("get: ~p - ~p~n", [Url, Code]),
        timer:sleep(?RETRY_INTERVAL),
        image_attacher(EntryId, Url, StatusTable);
    {ok, {{_, 200, _}, Headers, Body}} ->
        ContentType = case get_header_value("content-type", Headers) of
        undefined -> "application/octet-stream";
        Value -> Value
        end,
        DocName = edoc_lib:escape_uri(?b2l(EntryId)),
        UrlHash = ?i2l(erlang:phash2(?l2b(Url))),
        DocUrl = couch_planet:get_app_env(db_url) ++ "/" ++ DocName ++ "/" ++ UrlHash,
        attach_image(EntryId, DocUrl, ContentType, Body, StatusTable)
    end.

attach_image(EntryId, DocUrl, ContentType, Data, StatusTable) ->
    [{_EntryId, _TimeStamp, Rev0}] = ets:lookup(StatusTable, EntryId),
    case http:request(put, {DocUrl, [{"If-Match", ?b2l(Rev0)}], ContentType, Data}, ?HTTP_OPTIONS, [{body_format, binary}]) of
    {error, Reason} ->
        error_logger:error_msg("put: ~p - ~p~n", [DocUrl, Reason]),
        timer:sleep(?RETRY_INTERVAL),
        attach_image(EntryId, DocUrl, ContentType, Data, StatusTable);
    {ok, {{_, 409, _Body}, _, _}} ->
        case get(random_generator_seeded) of
        undefined ->
            random:seed(now()),
            put(random_generator_seeded, true);
        true ->
            ok
        end,
        timer:sleep(random:uniform(8) * 1000), % just wait for a bit
        attach_image(EntryId, DocUrl, ContentType, Data, StatusTable);
    {ok, {{_, 201, _}, Headers, _Body}} ->
        % just let it crash if there's no ETag response header:
        Rev1 = ?l2b(get_header_value("etag", Headers)),
        % the revision numbers in the ETags are enclosed in double quotes,
        % which we need to remove
        Rev2 = re:replace(Rev1, <<"\"">>, <<>>, [global, {return, binary}]),
        true = ets:update_element(StatusTable, EntryId, {3, Rev2}),
        ok;
    {ok, {{_, Code, _}, _, _}} ->
        error_logger:error_msg("put: ~p - ~p~n", [DocUrl, Code]),
        timer:sleep(?RETRY_INTERVAL),
        attach_image(EntryId, DocUrl, ContentType, Data, StatusTable)
     end.

%% @spec get_header_value(Key::string(), [{string(), string()}]) -> undefined | string()
%% @doc The key must be lower case.
get_header_value(_Key, []) ->
    undefined;
get_header_value(Key, [{Key, Value}|_]) ->
    Value;
get_header_value(Key, [_|T]) ->
    get_header_value(Key, T).
