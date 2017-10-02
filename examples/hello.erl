#!/usr/bin/env escript
%% -*- erlang -*-
%%! -smp enable -sname hello_world -pa ../_rel/luerl_demo_release/lib/*/ebin

main(_) ->
    io:format("..and now for something completely different!\n",[]),
    luerl:do("print(\"Hello, Robert!\")"),
    % execute a file
    luerl:dofile("./hello.lua"),
    % separately parse, then execute
    State0 = luerl:init(),
    {ok, Chunk, State1} = luerl:load("print(\"Hello, Chunk!\")", State0),
    {_Ret, _NewState} = luerl:do(Chunk, State1),
    done.