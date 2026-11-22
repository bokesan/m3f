#!/bin/sh

file=raw$$.3FR

xz -dc test/resources/dark_907x.3FR.xz > $file

error() {
    echo "error: $1"
    rm -f $file $file.bak
    exit 1
}


if ./m3f --focallength=105 --fnumber 6.3 $file ; then
    if ! [ -e $file.bak ]; then
	error "backup file not created"
    fi
    out=`exiftool -fnumber -S $file`
    if [ "$out" != "FNumber: 6.3" ]; then
	error "f-number not correct"
    fi
    out=`exiftool -focallength -S -n $file`
    if [ "$out" != "FocalLength: 105" ]; then
	error "focal length not correct"
    fi
else
    error "m3f write invocation failed"
fi

rm -f $file $file.bak
