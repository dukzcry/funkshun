#!/bin/sh

if [ "$1" == "compile" ]; then
	lfec ipxerlay.lfe
fi

lfe -s ipxerlay main -s erlang halt
