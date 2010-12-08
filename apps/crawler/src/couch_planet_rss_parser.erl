%%% This file is part of the couch_planet package and is released under the
%%% Tumbolia Public License. See LICENSE for more details.
%%%
%%% @author Klaus Trainer <klaus.trainer@web.de>

%%% @doc couch_planet_rss_parser

-module(couch_planet_rss_parser).

-author('Klaus Trainer <klaus.trainer@web.de>').

-include("couch_planet.hrl").

%% user interface
-export([title/1, find_feed_entries/1,
         entry_link/1, entry_time/1, complete_entry/3]).

-import(couch_planet_parser, [xml2xml_text2json_text/1]).


% External API

%% @spec title(binary()) -> binary()
title(Xml) ->
    case get(title_regex) of
    undefined ->
        {ok, MP} = re:compile(<<"<title[^>]*?>(.*?)</title>">>,
            [caseless, dotall]),
        put(title_regex, MP);
    MP ->
        ok
    end,
    case re:run(Xml, MP, [{capture, [1], binary}]) of
    nomatch -> <<>>;
    {match, [Title]} -> xml2xml_text2json_text(Title)
    end.

%% @spec find_feed_entries(binary()) -> [binary()]
find_feed_entries(Xml) ->
    find_feed_entries(Xml, []).

%% @spec entry_link(binary()) -> binary() | false
entry_link(Xml) ->
    case get(entry_link_regex) of
    undefined ->
        {ok, MP} = re:compile(<<"<link[^>]*?>(.*?)</link>">>, [caseless, dotall]),
        put(entry_link_regex, MP);
    MP ->
        ok
    end,
    case re:run(Xml, MP, [{capture, [1], binary}]) of
    nomatch -> false;
    {match, [Link]} -> xml2xml_text2json_text(Link)
    end.

%% @spec entry_time(binary()) -> binary() | false
entry_time(Xml) ->
    case entry_pub_date(Xml) of
    false -> entry_date(Xml);
    Value -> Value
    end.

%% @spec complete_entry(binary(), #entry{}, binary()) -> #entry{}
complete_entry(EntryData, Entry, Link) ->
    Summary = entry_content(EntryData),
    ActorName = entry_author(EntryData),
    #entry{object=Object0, actor=Actor0} = Entry,
    Object = Object0#object{id = Link, summary = Summary},
    Actor = Actor0#actor{name = ActorName},
    Entry#entry{title = title(EntryData), object = Object, actor = Actor}.


%% Internal API

find_feed_entries(Xml, Acc) ->
    case find_next_entry(Xml) of
    false ->
        Acc;
    {StartOffs, Length} ->
        Len = size(<<"</item>">>),
        <<_:StartOffs/binary,Value:Length/binary,_:Len/binary,Rest/binary>> = Xml,
        find_feed_entries(Rest, [Value|Acc])
    end.

find_next_entry(Xml) ->
    case get(entry_regex) of
    undefined ->
        {ok, MP} = re:compile(<<"<item[^>]*?>(.*?)</item>">>,
            [caseless, dotall]),
        put(entry_regex, MP);
    MP ->
        ok
    end,
    case re:run(Xml, MP, [{capture, [1]}]) of
    nomatch -> false;
    {match, [{StartOffs, Length}]} -> {StartOffs, Length}
    end.

entry_pub_date(Xml) ->
    case get(entry_pub_date_regex) of
    undefined ->
        {ok, MP} = re:compile(<<"<pubDate[^>]*?>(.*?)</pubDate>">>,
            [caseless, dotall]),
        put(entry_pub_date_regex, MP);
    MP ->
        ok
    end,
    case re:run(Xml, MP, [{capture, [1], binary}]) of
    nomatch -> false;
    {match, [Value]} -> xml2xml_text2json_text(Value)
    end.

entry_date(Xml) ->
    case get(entry_date_regex) of
    undefined ->
        {ok, MP} = re:compile(<<"<([^>]*?:[^>]*?)date[^>]*?>(.*?)</\\1date>">>,
            [caseless, dotall]),
        put(entry_date_regex, MP);
    MP ->
        ok
    end,
    case re:run(Xml, MP, [{capture, [2], binary}]) of
    nomatch -> false;
    {match, [Value]} -> xml2xml_text2json_text(Value)
    end.

%% @spec entry_content(binary()) -> binary()
entry_content(Xml) ->
    case get(entry_content_regex) of
    undefined ->
        {ok, MP} = re:compile(<<"<description[^>]*?>(.*?)</description>">>,
            [caseless, dotall]),
        put(entry_content_regex, MP);
    MP ->
        ok
    end,
    case re:run(Xml, MP, [{capture, [1], binary}]) of
    nomatch -> <<>>;
    {match, [Content]} -> xml2xml_text2json_text(Content)
    end.

%% @spec entry_author(binary()) -> binary()
entry_author(Xml) ->
    case get(entry_author_regex) of
    undefined ->
        {ok, MP} = re:compile(<<"<author[^>]*?>(.*?)</author>">>,
            [caseless, dotall]),
        put(entry_author_regex, MP);
    MP ->
        ok
    end,
    case re:run(Xml, MP, [{capture, [1], binary}]) of
    nomatch -> <<>>;
    {match, [Value]} -> xml2xml_text2json_text(Value)
    end.
