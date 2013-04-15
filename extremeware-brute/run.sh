#!/bin/sh

# telnet disable
# telnet enable
# configure log filter "DefaultFilter" add exclude events "All"

[ ! -e ew_telnet.erl ] && \
	curl https://raw.github.com/erlang/otp/maint/lib/common_test/src/unix_telnet.erl \
		| sed -e 's/Password:/password:/' -e 's/unix_telnet/ew_telnet/' > ew_telnet.erl
ct_run -enable smp -config extremeware_brute_SUITE.cfg -suite extremeware_brute_SUITE.erl
