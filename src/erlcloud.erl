-module(erlcloud).
-export([start/0]).

-define(APP, erlcloud).

start() ->
    application:load(?APP),
    {ok, Apps} = application:get_key(?APP, applications),
    lists:foreach(fun application:start/1, Apps),
    application:start(?APP).
