#!/usr/bin/env escript
%% -*- erlang -*-
%%! -smp enable -sname hello -pa ../deps/*/ebin ../ebin
main(_) ->
    io:format("..and now for something completely different!\n",[]),
    % do some lua
    luerl:do("print(\"Hello, Robert!\")"),
    % execute a file
    luerl:dofile("./hello.lua"),
    % separately parse, then execute
    State0 = luerl:init(),
    {ok, Chunk, State1} = luerl:load("print(\"Hello, Chunk!\")", State0),
    {_Ret, _NewState} = luerl:do(Chunk, State1),
    % Goodbye and thanks for all the fish!
    done.
