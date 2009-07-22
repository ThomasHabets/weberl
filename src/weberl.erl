-module(weberl).

-export([
         application/1,
         applicationLoop/1,
         multiHandler/3,
         multiHandler/4,
         getRegexStrings/2
        ]).

-record(handler, {
          url,
          pid
          }).
          
-record(appData, {
          urls,
          handlers = []
          }).

multiHandler(Mod, Func, Num) ->
    Handlers = lists:map(fun(_) -> spawn(Mod,
                                         Func,
                                         [])
                         end, lists:seq(1, Num)),
    multiHandler(Mod, Func, Num, Handlers).

multiHandler(Mod, Func, Num, Handlers) ->
    receive
        {Who, multiHandler, getHandlerCount} ->
            Who ! {handlerCount, Num},
            multiHandler(Mod, Func, Num, Handlers);
        {Who, multiHandler, setHandlerCount, NewNum} ->
            % FIXME
            multiHandler(Mod, Func, Num, Handlers);
        Any ->
            % FIXME: round robin
            [H|Tail] = Handlers,
            H ! Any,
            multiHandler(Mod, Func, Num, Handlers)
    end.

foo([]) ->
    [];
foo(Urls) ->
    [Url|Tail1] = Urls,
    [Mod|Tail2] = Tail1,
    [Func|Tail3] = Tail2,
    [spawn(weberl, multiHandler, [Mod,Func,3])|foo(Tail3)].

% Run in the context of the app
doRunSpawn([]) ->
    [];
doRunSpawn(Urls) ->
    [Url|Tail1] = Urls,
    [Mod|Tail2] = Tail1,
    [Fun|Tail3] = Tail2,
    Hand = #handler{url = Url,
                    pid = spawn(Mod, Fun,[])},
    [Hand|doRunSpawn(Tail3)].
   
doRun(AppData) ->
    io:format("weberl:app> doRun()~n"),
    #appData{urls=Urls} = AppData,
    #appData{handlers = doRunSpawn(Urls)}.


runGet(AppData, Url) ->
    #appData{handlers = Hands} = AppData,
    runGet2(Hands, AppData, Url).

runGet2([], AppData, Url) ->
    io:format("runGet(~s) 404!~n", [Url]),
    AppData;
runGet2(Hands, AppData, Url) ->
    [Hand|Tail] = Hands,
    io:format("Testing <~s> <~s> ~n", [Url, Hand#handler.url]),
    case re:run(Url, "^" ++ Hand#handler.url ++ "$$" ) of
        {match,Match} ->
            io:format("Match~n"),
            runGet3(Hand, AppData, Url, Match);
        nomatch     ->
            io:format("noMatch~n"),
            runGet2(Tail, AppData, Url)
    end.

getRegexStrings(_Str, []) ->
    [];
getRegexStrings(Str, Capture) ->
    [{S,D}|T] = Capture,
    [string:substr(Str, S+1, D)|getRegexStrings(Str, T)].
runGet3(Hand, AppData, Url, Match) ->
    io:format("runGet3() ~n"),
    P = getRegexStrings(Url, Match),
    io:format("lala 2~n"),
    [H|T] = P,
    io:format("lala 2.5~n"),
    Handle = 123, % FIXME
    io:format("lala 3~n"),
    Hand#handler.pid ! {self(), Handle, get, []},
    io:format("lala 4~n"),
    Handle.

% Run in the context of the app
doTestLoop(AppData) ->
    receive
        {Who, H, getReply, {ok, Msg}} ->
            io:format("App returned> <~s>~n", [Msg]),
            doTestLoop(AppData)
    after 1000 ->
            io:format("doTest() timeout~n"),
            AppData
    end.

doTest(AppData) ->
    io:format("weberl:app> doTest()~n"),
    runGet(AppData, "/"),
    runGet(AppData, "/2009/07"),
    doTestLoop(AppData).

applicationLoop(AppData) ->
    io:format("applicationLoop()~n"),
    receive
        {Who, run} ->
            applicationLoop(doRun(AppData));
        {Who, urls, Urls} ->
            NewAppData = AppData#appData{urls = Urls},
            applicationLoop(NewAppData);
        {Who, test} ->
            doTest(AppData),
            applicationLoop(AppData)
    end.

application(Urls) ->
    App = spawn(?MODULE, applicationLoop, [#appData{}]),
    App ! {self(), urls, Urls},
    App.
