%%% This file is part of the couch_planet package and is released under the
%%% Tumbolia Public License. See LICENSE for more details.
%%%
%%% @author Klaus Trainer <klaus.trainer@web.de>

%%% @doc Library of utility functions for couch_planet.

-module(couch_planet_utils).

-author('Klaus Trainer <klaus.trainer@web.de>').

%% user interface
-export([xml2xml_text2json_text/1]).


%% @spec xml2xml_text2json_text(binary()) -> binary()
xml2xml_text2json_text(Xml) ->
    case get(cdata_regex) of
    undefined ->
        {ok, MP} = re:compile(<<"<!\\[CDATA\\[(.*?)\\]\\]>">>, [dotall]),
        put(cdata_regex, MP);
    MP ->
        ok
    end,
    Xml1 = case re:run(Xml, MP, [{capture, [1], binary}]) of
    nomatch -> Xml;
    {match, [Text]} -> Text
    end,
    XmlText0 = re:replace(Xml1, <<"&gt;">>, <<">">>, [global, {return, binary}]),
    XmlText1 = re:replace(XmlText0, <<"&lt;">>, <<"<">>, [global, {return, binary}]),
    XmlText2 = re:replace(XmlText1, <<"&amp;">>, <<"\\&">>, [global, {return, binary}]),
    JsonText0 = re:replace(XmlText2, <<"\"">>, <<"\\\\\"">>, [global, {return, binary}]),
    case get(json_regex_1) of
    undefined ->
        {ok, MP1} = re:compile(<<"(\\\\\\\\)+\"">>),
        put(json_regex_1, MP1);
    MP1 ->
        ok
    end,
    JsonText1 = re:replace(JsonText0, MP1, <<"\\1\\\\\"">>, [global, {return, binary}]),
    JsonText2 = re:replace(JsonText1, <<"\\\\u">>, <<"\\\\\\\\u">>, [global, {return, binary}]),
    case get(json_regex_2) of
    undefined ->
        {ok, MP2} = re:compile(<<"(\\\\\\\\)+\\\\u">>),
        put(json_regex_2, MP2);
    MP2 ->
        ok
    end,
    re:replace(JsonText2, MP2, <<"\\1\\\\\\\\u">>, [global, {return, binary}]).
