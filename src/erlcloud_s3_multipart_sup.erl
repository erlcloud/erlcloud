-module(erlcloud_s3_multipart_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok, {{one_for_all, 5, 10}, [

        #{id => server,
          start => {erlcloud_s3_multipart_server, start_link, []},
          restart => permanent,
          type => worker,
          modules => [erlcloud_s3_multipart_server]
         },

        #{id => worker_sup,
          start => {erlcloud_s3_multipart_worker_sup, start_link, []},
          restart => permanent,
          type => supervisor,
          modules => [erlcloud_s3_multipart_worker_sup]
         }
    ]}}.
