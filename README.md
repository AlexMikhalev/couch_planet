# couch_planet

Basically, *couch_planet* is a combination of two more or less independent
applications:

1. A feed crawler. Subscribed feeds are regularly polled for updates. When
there are updates, the new (or rather updated) feed entries are parsed,
serialized into JSON (in particular a
[JSON activity stream](http://activitystrea.ms/head/json-activity.html)-like
format), and written to CouchDB.

2. A [CouchApp](http://couchapp.org). It is responsible for client- and
server-side rendering of the JSON documents (a.k.a. feed entries).


## Deploying the CouchApp

Assuming you are in the `apps/couchapp` directory and you have the `couchapp`
command line tool installed, you can push the CouchApp to your CouchDB. For
example:

    couchapp push . http://name:password@hostname:5984/couch_planet

Please see [http://couchapp.org](http://couchapp.org) for more information
on CouchApps.


# Deploying the Feed Crawler

In the `apps/crawler` directory, simply run `make`. Then, if everything was
compiled successfully, you can now start the crawler from the same directory.
For example:

    ./start.sh http://name:password@hostname:5984/couch_planet/_design/couch_planet


## Subscriptions

Provided that you've successfully deployed the CouchApp, as well as the crawler,
and provided that the crawler has access to your CouchDB with the deployed
CouchApp, the crawler will as soon as it is started send updates to the CouchDB,
which then can be rendered by the CouchApp.

The URL of the CouchApp's main page is e.g.

    http://hostname:5984/couch_planet/_design/couch_planet/_list/index/recent-posts?descending=true&limit=10

To edit your subscriptions, you need to login with an admin account. Therefor,
just click "Login" (top right corner) on the main page and enter your
credentials. If your admin succeeds, you'll be automatically forwarded to the
subscriptions page where you can edit your subscriptions. To submit your
changes, just click "Save Subscriptions". When you log out (i.e., click
"Logout"), you will automatically be directed back to the main page.


# Version

Version 1.1.0 – 2010-12-08

See `CHANGES.md` for details.


# Contributors

* Klaus Trainer – original author
* You. Come on!
