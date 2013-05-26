#!/bin/sh

if [ "$1" == "compile" ]; then
	lfec ipxerlay.lfe
	#erl -pa . -s ipxerlay test_call -s erlang halt
fi

# (inet|inet6 addr port) | (port fd)
erl -pa . -s ipxerlay start_link inet 127.0.0.1 0
