#! /bin/sh

DEMO_LIBS="../deps/*/ebin ../ebin"

exec erl -smp enable +stbt db -sname sim -pa $DEMO_LIBS