#!/bin/sh

if [ "$1" == "compile" ]; then
	lfec ipxerlay.lfe
	#erl -pa . -s ipxerlay test_call -s erlang halt
fi

# port = portnum | 0 for random
# (port inet|inet6 addr) | (port fd)
erl -pa . -s ipxerlay start_link 4899 inet 192.168.1.2
