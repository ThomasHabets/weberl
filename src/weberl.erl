-module(weberl).

-export([
         application/1,
         applicationInit/1,
         logInit/0,
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
          handlers = [],
          logger,
          logLevel = 0
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

log(AppData, Fun, Str) ->
    log(AppData, ?MODULE, Fun, Str).
log(AppData, Mod, Fun, Str) ->
    AppData#appData.logger ! {self(), Mod, Fun, Str}.

doRun(AppData) ->
    log(AppData, "doRun", "enter func"),
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
    [H|T] = P,
    Handle = 123, % FIXME
    Hand#handler.pid ! {self(), Handle, get, []},
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


logLoop() ->
    receive
        {Who, Mod, Fun, Str} ->
            io:format("Log ~s:~s()> ~s~n", [Mod,Fun,Str]);
        _ ->
            io:format("Log ??:??> Invalid message sent to logger~n")
    end,
    logLoop().

logInit() ->
    logLoop().

applicationInit(AppData) ->
    LogPid = spawn(?MODULE, logInit, []),
    AppData2 = AppData#appData{logger = LogPid},
    applicationLoop(AppData2).

applicationLoop(AppData) ->
    %log(AppData, "applicationLoop", "Enterfunction"),
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

%
% User functions
%
application(Urls) ->
    App = spawn(?MODULE, applicationInit, [#appData{}]),
    App ! {self(), urls, Urls},
    App ! {self(), debug, 1},
    App.
