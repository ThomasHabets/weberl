-module(weberl).

-export([
         application/1,
         applicationInit/1,
         applicationLoop/1,
         logInit/0,
         logLoop/0,
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

%%
%% From the output of re:run(), which is {match,[{0,3},{5,8},...]}, return
%% a list of the strings themselves.
%%
getRegexStrings(_Str, []) ->
    [];
getRegexStrings(Str, Capture) ->
    [{S,D}|T] = Capture,
    [string:substr(Str, S+1, D)|getRegexStrings(Str, T)].

%%
%% Multiplexer. You send the messages to this process and it will load balance
%% over the real handlers. The handlers reply directly, not via the
%% multiplexer.
%%
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
            ?MODULE:multiHandler(Mod, Func, Num, Handlers);
        {_Who, multiHandler, setHandlerCount, _NewNum} ->
            % FIXME
            ?MODULE:multiHandler(Mod, Func, Num, Handlers);
        Any ->
            % FIXME: round robin
            [H|_Tail] = Handlers,
            H ! Any,
            ?MODULE:multiHandler(Mod, Func, Num, Handlers)
    end.

%
% Spawn all handlers.
% Return a list of #handlers.
%
spawnHandlers([]) ->
    [];
spawnHandlers(Urls) ->
    [Url|Tail1] = Urls,
    [Mod|Tail2] = Tail1,
    [Fun|Tail3] = Tail2,
    Hand = #handler{url = Url,
                    pid = spawn(Mod, Fun,[])},
    [Hand|spawnHandlers(Tail3)].

%
% Log functions
%
logLoop() ->
    receive
        {Who, Mod, Fun, Str} ->
            io:format("Log ~s:~s()> ~s~n", [Mod,Fun,Str]);
        _ ->
            io:format("Log ??:??> Invalid message sent to logger~n")
    end,
    ?MODULE:logLoop().

logInit() ->
    ?MODULE:logLoop().

log(AppData, Fun, Str) ->
    log(AppData, ?MODULE, Fun, Str).
log(AppData, Mod, Fun, Str) ->
    AppData#appData.logger ! {self(), Mod, Fun, Str}.

%
% Start up the app. Triggered by a 'run' signal to the app process.
%
doRun(AppData) ->
    log(AppData, "doRun", "enter func"),
    #appData{urls=Urls} = AppData,
    AppData#appData{handlers = spawnHandlers(Urls)}.

%%
%% Parse and run a HTTP request
%% Input: url
%% Output: send signal to handler (multiHandler frontend for the handler
%%         really)
%%
%% FIXME "runGet" is not a good name for this process
%%
runGet(AppData, Url) ->
    #appData{handlers = Hands} = AppData,
    runGet2(Hands, AppData, Url).

% No handler matched the requested URL
runGet2([], AppData, Url) ->
    io:format("runGet(~s) 404!~n", [Url]),
    AppData;

% Try to find the handler
runGet2(Hands, AppData, Url) ->
    [Hand|Tail] = Hands,
    case re:run(Url, "^" ++ Hand#handler.url ++ "$$" ) of
        {match,Match} ->
            runGet3(Hand, AppData, Url, Match);
        nomatch     ->
            runGet2(Tail, AppData, Url)
    end.

runGet3(Hand, AppData, Url, Match) ->
    log(AppData, "runGet3", "enter function"),
    [_P|T] = getRegexStrings(Url, Match),
    Handle = 123, % FIXME
    Hand#handler.pid ! {self(), Handle, get, T},
    Handle.

%%
%% Wait for any replies from the handlers, and print them.
%%
doTestLoop(AppData) ->
    receive
        {_Who, _H, getReply, {ok, Msg}} ->
            io:format("TEST returned: <~s>~n", [Msg]),
            doTestLoop(AppData)
    after 1000 ->
            io:format("TEST done (timeout)~n"),
            AppData
    end.

%%
%% Fire off a couple of queries
%%
doTest(AppData) ->
    log(AppData, "doTest", "app> doTest()"),
    runGet(AppData, "/"),
    runGet(AppData, "/2009/07"),
    doTestLoop(AppData).

%%
%%
%%
applicationInit(AppData) ->
    LogPid = spawn(?MODULE, logInit, []),
    AppData2 = AppData#appData{logger = LogPid},
    ?MODULE:applicationLoop(AppData2).

%%
%%
%%
applicationLoop(AppData) ->
    log(AppData, "applicationLoop", "Enterfunction"),
    receive
        {Who, urls, Urls} ->
            NewAppData = AppData#appData{urls = Urls},
            ?MODULE:applicationLoop(NewAppData);
        {Who, run} ->
            ?MODULE:applicationLoop(doRun(AppData));
        {Who, test} ->
            doTest(AppData),
            ?MODULE:applicationLoop(AppData)
    end.

%
% User functions
%
application(Urls) ->
    App = spawn(?MODULE, applicationInit, [#appData{}]),
    App ! {self(), urls, Urls},
    App ! {self(), debug, 1},
    App.
