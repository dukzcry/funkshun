#!/bin/sh

erl -sname noname <<EOF
rpc:call($1,eggbnc,dump,[]).
EOF
