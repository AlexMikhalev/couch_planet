-define(UPDATE_INTERVAL, 180000).

-define(RETRY_INTERVAL, 60000).

-define(HTTP_OPTIONS, [{timeout, 120000}, {connect_timeout, 20000}]).

-define(a2l(V), atom_to_list(V)).
-define(b2l(V), binary_to_list(V)).
-define(b2t(V), binary_to_term(V)).
-define(i2l(V), integer_to_list(V)).
-define(l2a(V), list_to_atom(V)).
-define(l2b(V), list_to_binary(V)).
-define(l2i(V), list_to_integer(V)).
-define(t2b(V), term_to_binary(V)).


-record(object, {
    id = <<>>,
    summary = <<>>,
    objectType = <<"article">>
}).

-record(actor, {
    link = <<>>,
    name = <<>>,
    objectType = <<"author">>
}).

-record(provider, {
    id = <<>>,
    name = <<>>,
    objectType = <<"service">>
}).

-record(entry, {
    id = <<>>,
    rev = <<>>,
    title = <<>>,
    postedTime = <<>>,
    type = <<"Activity Stream">>,
    verb = <<"post">>,
    object = #object{},
    actor = #actor{},
    provider = #provider{}
}).
