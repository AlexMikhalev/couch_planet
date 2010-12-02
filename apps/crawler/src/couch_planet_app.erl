%%% This file is part of the couch_planet package and is released under the
%%% Tumbolia Public License. See LICENSE for more details.
%%%
%%% @author Klaus Trainer <klaus.trainer@web.de>

%%% @doc Callbacks for the couch_planet application.

-module(couch_planet_app).
-behaviour(application).

-author('Klaus Trainer <klaus.trainer@web.de>').

%% user interface
-export([start/2, stop/1]).


%% @spec start(_Type, _Args) -> ok | {error, Reason}
%% @doc application start callback for couch_planet.
start(_Type, _Args) ->
    register(couch_planet_app, self()),
    couch_planet_deps:ensure(),
    couch_planet_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for couch_planet.
stop(_State) ->
    ok.
