-module(weberltest).

-export([
         testHandler1/0,
         testHandler2/0,
         test/0
        ]).

testHandler1() ->
    io:format("testHandler1()~n"),
    receive
        {Who, Handle, get, []} ->
            io:format("testHandler1() GET~n"),
            Who ! {self(), Handle, getReply, {ok, "Hello World"}},
            testHandler1();
        _ ->
            io:format("testHandler1 received Any?~n"),
            testHandler1()
    end.

testHandler2() ->
    io:format("testHandler2()~n"),
    receive
        {Who, Handle, get, _Parms} ->
            Who ! {self(), Handle, getReply, {ok, "2 parms"}},
            testHandler2();
        _ ->
            io:format("testHandler2 received Any?~n"),
            testHandler2()
    end.

test() ->
    App = weberl:application([
                              "/([0-9]+)/([0-9]+)", ?MODULE, testHandler2,
                              "/",            ?MODULE, testHandler1
                             ]),
    App ! {self(), run},
    App ! {self(), test}.
