%% File    : hello.erl
%% Purpose : Brief demonstration of Luerl basics.
%% Use     $ erlc hello.erl && erl -pa ./ebin -s hello run -s init stop -noshell
%% Or      $ make hello

-module(hello).
-export([run/0]).

run() ->
    % execute a string
    luerl:do("print(\"Hello, Robert!\")"),
    % execute a file
    luerl:dofile("../world.lua"),
    % separately parse, then execute
    State0 = luerl:init(),
    {ok, Chunk, State1} = luerl:load("print(\"Hello, Chunk!\")", State0),
    {_Ret, _NewState} = luerl:do(Chunk, State1),
    % thanks for all the fish!
    done.
