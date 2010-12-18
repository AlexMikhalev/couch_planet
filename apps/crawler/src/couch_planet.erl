%%% This file is part of the couch_planet package and is released under the
%%% Tumbolia Public License. See LICENSE for more details.
%%%
%%% @author Klaus Trainer <klaus.trainer@web.de>

%%% @doc couch_planet

-module(couch_planet).
-author('Klaus Trainer <klaus.trainer@web.de>').

-include("couch_planet.hrl").

%% user interface
-export([start/1, stop/0]).
%% intermodule exports
-export([get_app_env/1, get_app_env/2]).


%% External API

%% @spec start([string()]) -> ok
%% @doc Start couch_planet.
start([DdocUrl]) ->
    start1(DdocUrl);
start(Args) when length(Args) =:= 2->
    [A1, A2] = Args,
    case A1 of
    "fetch-images" ->
        application:set_env(couch_planet, fetch_images, true),
        start1(A2);
    "fetch-audio" ->
        application:set_env(couch_planet, fetch_audio, true),
        start1(A2);
    _ ->
        bye()
    end;
start(Args) when length(Args) =:= 3->
    [A1, A2, A3] = Args,
    case A1 of
    "fetch-images" when A2 =:= "fetch-audio" ->
        application:set_env(couch_planet, fetch_images, true),
        application:set_env(couch_planet, fetch_audio, true),
        start1(A3);
    "fetch-audio" when A2 =:= "fetch-images" ->
        application:set_env(couch_planet, fetch_audio, true),
        application:set_env(couch_planet, fetch_images, true),
        start1(A3);
    _ ->
        bye()
    end.

%% @spec stop() -> ok
%% @doc Stop the couch_planet application and the calling process.
stop() ->
    stop("couch_planet stop requested").

%% @spec stop(string()) -> ok
%% @doc Stop the couch_planet server.
stop(Reason) ->
    error_logger:info_msg(io_lib:format("~p~n",[Reason])),
    Res = application:stop(couch_planet),
    application:stop(ssl),
    application:stop(inets),
    application:stop(sasl),
    Res.

%% @spec get_app_env(atom()) -> term()
%% @doc The official way to get the values set in couch_planet's configuration
%%      file. Will return `undefined' if that option is unset.
get_app_env(Opt) ->
    get_app_env(Opt, undefined).

%% @spec get_app_env(atom(), term()) -> term()
%% @doc The official way to get the values set in couch_planet's configuration
%%      file. Will return `Default' if that option is unset.
get_app_env(Opt, Default) ->
    case application:get_env(couch_planet, Opt) of
    {ok, Val} ->
        Val;
    _ ->
        case init:get_argument(Opt) of
        {ok, [[Val|_]]} -> Val;
        error -> Default
        end
    end.


%% Internal API

%% @spec start1(string()) -> ok
start1(DdocUrl) ->
    Url = string:strip(DdocUrl, right, $/),
    Offs = string:str(Url, "/_design"),
    case Offs of
    0 ->
        exit("invalid design document URL");
    _ ->
        DbUrl = string:substr(Url, 1, Offs - 1),
        BulkDocsUrl = DbUrl ++ "/_bulk_docs",
        application:set_env(couch_planet, db_url, DbUrl),
        application:set_env(couch_planet, bulk_docs_url, BulkDocsUrl),
        application:set_env(couch_planet, ddoc_url, Url),
        start()
    end.

%% @spec start() -> ok
%% @doc Start the couch_planet server.
start() ->
    couch_planet_deps:ensure(),
    ensure_started(sasl),
    ensure_started(inets),
    ensure_started(ssl),
    application:start(couch_planet).

bye() ->
    exit("invalid design document URL").

ensure_started(App) ->
    case application:start(App) of
    ok -> ok;
    {error, {already_started, App}} -> ok
    end.
