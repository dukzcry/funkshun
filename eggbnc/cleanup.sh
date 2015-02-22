#!/bin/sh

erl -sname noname2 <<EOF
rpc:call($1,eggbnc,cleanup,[$2]).
EOF
