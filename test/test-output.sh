#!/bin/sh

for raw in test/resources/*.3FR ; do
    for flags in "" "-U" "-UV" "--privacy" ; do
	name=`basename "$raw" .3FR`
	if ./m3f $flags $raw | diff - "test/resources/$name$flags.txt" ; then
	    echo "m3f $flags ${raw}: success"
	else
	    echo "m3f $flags ${raw}: failed"
	    exit 1
	fi
    done
done
