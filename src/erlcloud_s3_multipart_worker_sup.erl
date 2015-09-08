-module(erlcloud_s3_multipart_worker_sup).

-behaviour(supervisor).

-export([start_link/0, start_upload/4]).

-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% Starts empty, supervises multi-part uploads

init([]) -> 
    {ok, {{simple_one_for_one, 0, 1}, [
        #{id => worker,
          start => {erlcloud_s3_multipart_worker, start_link, []},
          restart => temporary,
          type => worker,
          modules => [erlcloud_s3_multipart_worker]
         }
    ]}}.

start_upload(FilePath, UploadId, Bucket, Key) -> 
    StartChildResult = supervisor:start_child(?MODULE, [FilePath, UploadId, Bucket, Key]),

    case StartChildResult of 
        {ok, ChildPid} -> {ok, ChildPid};
        {ok, ChildPid, _Info} -> {ok, ChildPid};
        {error, Error} -> {error, Error}
    end.
