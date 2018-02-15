-module(erlcloud).
-export([start/0]).


-include("erlcloud.hrl").
-include("erlcloud_aws.hrl").

start() ->
    application:load(?APP),
    {ok, Apps} = application:get_key(?APP, applications),
    [application:start(App) || App <- Apps],
    application:start(?APP).
