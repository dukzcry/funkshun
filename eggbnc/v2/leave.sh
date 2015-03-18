#!/bin/sh

erl -sname noname3 <<EOF
rpc:call($1,eggbnc,leave,[$2]).
EOF
