-module(erlcloud).
-export([start/0]).

-define(APP, erlcloud).
-define(POOL_NAME, erlcloud_pool).

start() ->
    application:load(?APP),
    {ok, Apps} = application:get_key(?APP, applications),
    [application:start(App) || App <- Apps],
    PoolSize = application:get_env(?APP, pool_size, 50),
    {ok, _} = application:ensure_all_started(hackney_pooler),
    PoolConfig = [{group, erlcloud},
                  {max_count, PoolSize},
                  {init_count, PoolSize}],
    hackney_pooler:new_pool(?POOL_NAME, PoolConfig),
    application:start(?APP).
