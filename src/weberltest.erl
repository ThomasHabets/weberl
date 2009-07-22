-module(weberltest).

-export([
         testHandler1/0,
         testHandler2/0,
         test/0,
         run/0
        ]).

testHandler1() ->
    io:format("testHandler1()~n"),
    receive
        {Who, Handle, get, []} ->
            io:format("testHandler1() GET~n"),
            Who ! {self(), Handle, getReply, {ok, "Hello World"}},
            ?MODULE:testHandler1();
        _ ->
            io:format("testHandler1 received Any?~n"),
            ?MODULE:testHandler1()
    end.

testHandler2() ->
    io:format("testHandler2()~n"),
    receive
        {Who, Handle, get, _Parms} ->
            Who ! {self(), Handle, getReply, {ok, "2 parms"}},
            ?MODULE:testHandler2();
        _ ->
            io:format("testHandler2 received Any?~n"),
            ?MODULE:testHandler2()
    end.

init() ->
    App = weberl:application([
                              "/(\\d+)/(\\d+)", ?MODULE, testHandler2,
                              "/",              ?MODULE, testHandler1
                             ]),
    App ! {self(), run},
    App.

test() ->
    init() ! {self(), test}.

run() ->
    init().
