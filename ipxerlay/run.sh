#!/bin/sh

if [ "$1" == "compile" ]; then
	lfec ipxerlay.lfe
	#erl -pa . -s ipxerlay test_call -s erlang halt
fi

erl -pa . -s ipxerlay start_link "127.0.0.1" "4899"
