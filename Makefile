.PHONY: build release run clean

build:
\trebar3 compile

release:
\trebar3 release

run:
\t_build/default/rel/real_time_presence_dashboard/bin/real_time_presence_dashboard console

clean:
\trebar3 clean
