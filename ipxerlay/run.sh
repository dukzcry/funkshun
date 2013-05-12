#!/bin/sh

if [ "$1" == "compile" ]; then
	lfec ipxerlay.lfe
else
	lfe -s ipxerlay main -s erlang halt
fi
