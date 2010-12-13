%%% This file is part of the couch_planet package and is released under the
%%% Tumbolia Public License. See LICENSE for more details.
%%%
%%% @author Klaus Trainer <klaus.trainer@web.de>

%%% @doc couch_planet_subscriptions_manager

-module(couch_planet_subscriptions_manager).
-behaviour(gen_server).

-author('Klaus Trainer <klaus.trainer@web.de>').

-include("couch_planet.hrl").

%% user interface
-export([start_link/0]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_info/2, handle_cast/2,
         terminate/2, code_change/3]).


%% External API

start_link() ->
    gen_server:start_link(?MODULE, [], []).

init([]) ->
    spawn_link(fun() -> start() end),
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

%% @spec start() -> none()
start() ->
    loop(sets:new(), "").

%% @spec loop(Config::set(), ETag::string()) -> none()
loop(Config0, ETag0) ->
    case read_config(ETag0) of
    {error, _Reason} ->
        ETag1 = ETag0,
        Config1 = Config0;
    {ok, not_modified} ->
        ETag1 = ETag0,
        Config1 = Config0;
    {ok, Urls, ETag1} ->
        Config1 = lists:foldl(
            fun(Url, AccIn) ->
                case sets:is_element(Url, Config0) of
                true ->
                    sets:add_element(Url, AccIn);
                false ->
                    couch_planet_sup:add_child(Url),
                    error_logger:info_msg("Added topic ~p.~n", [Url]),
                    sets:add_element(Url, AccIn)
                end
            end, sets:new(), Urls),
        TopicsToRemove = sets:to_list(sets:subtract(Config0, Config1)),
        lists:foreach(
            fun(Url) ->
                couch_planet_sup:remove_child(Url),
                error_logger:info_msg("Removed topic ~p.~n", [Url])
            end, TopicsToRemove)
    end,
    timer:sleep(?UPDATE_INTERVAL),
    loop(Config1, ETag1).

%% @spec read_config(string()) -> {ok, not_modified} | {ok, Urls::[string()]} | {error, Reason}
%% @doc Read the couch_planet configuration.
read_config(ETag) ->
    read_config(couch_planet:get_app_env(ddoc_url) ++ "/subscriptions.json", ETag).

%% @spec read_config(string(), string()) -> {ok, not_modified} | {ok, Urls::[string()], ETag::string()} | {error, Reason}
%% @doc Read the couch_planet configuration from Url.
read_config(Url, ETag) ->
    case http:request(get, {Url, [{"If-None-Match", ETag}]}, ?HTTP_OPTIONS, [{body_format, binary}]) of
    {error, Reason} ->
        error_logger:error_msg("cannot read configuration~nget: ~p - ~p~n", [Url, Reason]),
        timer:sleep(?UPDATE_INTERVAL),
        read_config(Url, ETag);
    {ok, {{_, 304, _}, _, _}} ->
        {ok, not_modified};
    {ok, {{_, 200, _}, Headers, Body}} ->
        case parse_config(Body) of
        {error, Reason} ->
            error_logger:error_msg("cannot parse configuration from ~p - ~p~n", [Url, Reason]),
            timer:sleep(?UPDATE_INTERVAL),
            read_config(Url, ETag);
        Urls ->
            {ok, Urls, get_etag_header(Headers)}
        end;
    {ok, {{_, Code, _}, _Headers, _Body}} when Code =/= 200 ->
        error_logger:error_msg("cannot read configuration~nget: ~p - ~p~n", [Url, Code]),
        timer:sleep(?UPDATE_INTERVAL),
        read_config(Url, ETag)
    end.


%% Internal API

%% @spec parse_config(binary()) -> Urls::[string()] | {error, Reason}
parse_config(Config) ->
    {struct, L} = mochijson2:decode(Config),
    case lists:keyfind(<<"topics">>, 1, L) of
    false ->
        {error, "no topics"};
    {<<"topics">>, Topics} ->
        case parse_topics(Topics) of
        {error, Reason} -> {error, Reason};
        Urls -> Urls
        end
    end.

%% @spec parse_topics([tuple()]) -> Urls::[string()] | {error, Reason}
parse_topics(Topics) ->
    parse_topics(Topics, []).

parse_topics([], Acc) ->
    Acc;
parse_topics([H|T], Acc) ->
    {struct, L} = H,
    case lists:keyfind(<<"url">>, 1, L) of
    false -> {error, "url field missing in topic"};
    {<<"url">>, Url} -> parse_topics(T, [?b2l(Url)|Acc])
    end.

%% @spec get_etag_header([{string(), string()}]) -> undefined | string()
get_etag_header([]) ->
    undefined;
get_etag_header([{K, V}|T]) ->
    case K of
    "etag" -> V;
    _ -> get_etag_header(T)
    end.
