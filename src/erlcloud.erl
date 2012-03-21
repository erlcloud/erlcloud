-module(erlcloud).
-export([start/0]).

-define(APP, erlcloud).

start() ->
    application:load(?APP),
    application:start(public_key),
    application:start(ssl),
    {ok, Apps} = application:get_key(?APP, applications),
    lists:foreach(fun application:start/1, Apps),
    application:start(?APP).
