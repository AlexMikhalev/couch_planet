{application, couch_planet,
 [{description, "couch_planet"},
  {vsn, "1.0.0"},
  {modules, [
    couch_planet,
    couch_planet_app,
    couch_planet_crawler,
    couch_planet_deps,
    couch_planet_sup
  ]},
  {registered, [couch_planet_sup]},
  {mod, {couch_planet_app, []}},
  {applications, [inets, ssl, kernel, sasl, stdlib]}
 ]}.
