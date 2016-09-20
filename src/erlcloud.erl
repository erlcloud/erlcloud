-module(erlcloud).
-export([start/0]).


-include("erlcloud_aws.hrl").
-define(APP, erlcloud).

start() ->
    application:load(?APP),
    {ok, Apps} = application:get_key(?APP, applications),
    [application:start(App) || App <- Apps],
    application:start(?APP).

