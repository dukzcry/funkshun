#!/bin/sh

if [ "$1" == "compile" ]; then
	lfec ipxerlay.lfe
	#erl -pa . -s ipxerlay test_call -s erlang halt
fi

# to = allowed idle timeout in msecs
# port = portnum | 0 for random
# (to port inet|inet6 addr) | (to port fd)
erl -pa . -s ipxerlay start_link 900000 2130 inet 0.0.0.0
