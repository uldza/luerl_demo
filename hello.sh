#! /bin/sh

DEMO_LIBS="./deps/*/ebin ./ebin"

exec erl -smp enable +stbt db -sname hello -setcookie world -pa $DEMO_LIBS
