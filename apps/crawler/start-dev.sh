#!/bin/sh
cd `dirname $0`
if [ -z "$1" ]; then
	echo "Cannot start: the URL of the couch_planet design document must be specified.\nFor example: http://127.0.0.1:5984/couch_planet/_design/couch_planet";
else
	if [ -z "$2" ]; then
		export SASL_CONFIG="config/elog.config"
	else
		export SASL_CONFIG=$2
	fi
	exec erl -pa $PWD/deps/*/ebin -pa $PWD/ebin -boot start_sasl -config ${SASL_CONFIG} -run couch_planet start $1
fi
