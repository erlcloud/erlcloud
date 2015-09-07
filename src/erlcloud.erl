-module(erlcloud).

-export([start/0, start_s3_multipart_sup/0, put_object_multipart/3]).

-define(APP, erlcloud).

start() ->
    application:load(?APP),
    {ok, Apps} = application:get_key(?APP, applications),
    [application:start(App) || App <- Apps],
    application:start(?APP).

start_s3_multipart_sup() ->
    erlcloud_s3_multipart_sup:start_link().

put_object_multipart(FilePath, Bucket, Key) ->
    erlcloud_s3_multipart_server:put_object(FilePath, Bucket, Key).
