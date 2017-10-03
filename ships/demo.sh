#! /bin/sh

DEMO_LIBS="../deps/*/ebin ../ebin"

exec erl -smp enable +stbt db -sname demo -run observer -pa $DEMO_LIBS -s user_default new_sim -s -noshell