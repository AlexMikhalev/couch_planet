#!/bin/bash

usage ()
{
	echo "usage: $0 [-i|--interactive] [--sasl-config=SASL_CONFIG] [--fetch-images] [--fetch-audio] DESIGN_DOC_URL"
	exit -1
}


cd `dirname $0`

if [ -z $1 ]; then
	echo "cannot start: the URL of the couch_planet design document has to be specified."
	echo -e "e.g.: $0 http://127.0.0.1:5984/couch_planet/_design/couch_planet\n"
	usage
fi

for (( i=1; i<$#; i++ ))
do
	case ${!i} in
	-i)
		INTERACTIVE=1
		;;
	--interactive)
		INTERACTIVE=1
		;;
	--sasl-config=*)
		SASL_CONFIG=`echo ${!i} | sed 's/[-a-zA-Z]*=//'`
		;;
	--fetch-images)
		FETCH_IMAGES="fetch-images"
		;;
	--fetch-audio)
		FETCH_AUDIO="fetch-audio"
		;;
	*)
		echo "Unknown option: ${!i}"
		usage
		;;
	esac
done

DESIGN_DOC=${!i}

if [ -z $INTERACTIVE ]; then
	DETACHED="-detached"
fi

if [ -z $SASL_CONFIG ]; then
	SASL_CONFIG="config/elog.config"
fi

exec erl $DETACHED -pa $PWD/deps/*/ebin -pa $PWD/ebin -boot start_sasl -config ${SASL_CONFIG} -run couch_planet start $FETCH_IMAGES $FETCH_AUDIO $DESIGN_DOC
