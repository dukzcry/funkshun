#!/bin/sh

if [ "$1" == "compile" ]; then
	lfec ipxerlay.lfe
fi

erl -pa . -s ipxerlay start_link "127.0.0.1" "4899" #-s erlang halt
