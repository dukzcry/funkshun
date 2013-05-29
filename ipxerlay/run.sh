#!/bin/sh

if [ "$1" == "compile" ]; then
	lfec ipxerlay.lfe
	#erl -pa . -s ipxerlay test_call -s erlang halt
fi

# (port inet|inet6 addr) | (port fd)
erl -pa . -s ipxerlay start_link 0 inet 127.0.0.1
