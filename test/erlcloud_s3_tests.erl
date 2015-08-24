%% -*- mode: erlang;erlang-indent-level: 4;indent-tabs-mode: nil -*-
-module(erlcloud_s3_tests).
-include_lib("eunit/include/eunit.hrl").
-include("erlcloud.hrl").
-include("erlcloud_aws.hrl").

%% Unit tests for s3.
%% Currently only test error handling and retries.

%%%===================================================================
%%% Test entry points
%%%===================================================================

operation_test_() ->
    {foreach,
     fun start/0,
     fun stop/1,
     [fun get_bucket_policy_tests/1,
      fun put_object_tests/1,
      fun error_handling_tests/1
     ]}.

start() ->
    meck:new(erlcloud_httpc),
    ok.

stop(_) ->
    meck:unload(erlcloud_httpc).

config() ->
    config(#aws_config{s3_follow_redirect = true}).

config(Config) ->
    Config#aws_config{
      access_key_id = string:copies("A", 20),
      secret_access_key = string:copies("a", 40)}.

httpc_expect(Response) ->
    httpc_expect(get, Response).
    
httpc_expect(Method, Response) ->
    fun(_Url, Method2, _Headers, _Body, _Timeout, _Config) -> 
            Method = Method2,
            Response
    end.
    
get_bucket_policy_tests(_) ->
    Response = {ok, {{200, "OK"}, [], <<"TestBody">>}},
    meck:expect(erlcloud_httpc, request, httpc_expect(Response)),
    Result = erlcloud_s3:get_bucket_policy("BucketName", config()),
    ?_assertEqual({ok, "TestBody"}, Result).

put_object_tests(_) ->
    Response = {ok, {{200, "OK"}, [{"x-amz-version-id", "version_id"}], <<>>}},
    meck:expect(erlcloud_httpc, request, httpc_expect(put, Response)),
    Result = erlcloud_s3:put_object("BucketName", "Key", "Data", config()),
    ?_assertEqual([{version_id, "version_id"}], Result).

error_handling_no_retry() ->
    Response = {ok, {{500, "Internal Server Error"}, [], <<"TestBody">>}},
    meck:expect(erlcloud_httpc, request, httpc_expect(Response)),
    Result = erlcloud_s3:get_bucket_policy("BucketName", config()),
    ?_assertEqual({error,{http_error,500,"Internal Server Error",<<"TestBody">>}}, Result).

error_handling_default_retry() ->
    Response1 = {ok, {{500, "Internal Server Error"}, [], <<"TestBody">>}},
    Response2 = {ok, {{200, "OK"}, [], <<"TestBody">>}},
    meck:sequence(erlcloud_httpc, request, 6, [Response1, Response2]),
    Result = erlcloud_s3:get_bucket_policy(
               "BucketName", 
               config(#aws_config{retry = fun erlcloud_retry:default_retry/1})),
    ?_assertEqual({ok, "TestBody"}, Result).

error_handling_httpc_error() ->
    Response1 = {error, timeout},
    Response2 = {ok, {{200, "OK"}, [], <<"TestBody">>}},
    meck:sequence(erlcloud_httpc, request, 6, [Response1, Response2]),
    Result = erlcloud_s3:get_bucket_policy(
               "BucketName", 
               config(#aws_config{retry = fun erlcloud_retry:default_retry/1})),
    ?_assertEqual({ok, "TestBody"}, Result).

%% Handle redirect by using location from error message.
error_handling_redirect_message() ->
    Response1 = {error, {http_error,307,"Temporary Redirect", 
        <<"<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<Error><Code>TemporaryRedirect</Code>"
          "<Message>Please re-send this request to the specified temporary endpoint. Continue to use the original request endpoint for future requests.</Message>"
          "<Bucket>bucket.name</Bucket>"
          "<Endpoint>bucket.name.s3.eu-central-1.amazonaws.com</Endpoint>"
          "<RequestId>5B157C1FD7B351A9</RequestId>"
          "<HostId>IbIGCfmLGzCxQ14C14VuqbjzLjWZ61M1xF3y9ovUu/j/Qj//BXsrbsAuYQJN//FARyvYtOmj8K0=</HostId></Error>">>, []}},
    Response2 = {ok, {{200, "OK"}, [], <<"TestBody">>}},
    meck:sequence(erlcloud_httpc, request, 6, [Response1, Response2]),
    Result = erlcloud_s3:get_bucket_policy(
               "bucket.name", 
               config(#aws_config{retry = fun erlcloud_retry:default_retry/1})),
    ?_assertEqual({ok, "TestBody"}, Result).

%% Handle redirect by using url from location header.
error_handling_redirect_location() ->
    Response1 = {error, {http_error,301,"Temporary Redirect", <<>>, 
        [{"server","AmazonS3"},
         {"date","Wed, 22 Jul 2015 09:58:03 GMT"},
         {"transfer-encoding","chunked"},
         {"content-type","application/xml"},
         {"location",
          "https://kkuzmin-test-frankfurt.s3.eu-central-1.amazonaws.com/"},
         {"x-amz-id-2",
          "YIgyI9Lb9I/dMpDrRASSD8w5YsNAyhRlF+PDF0jlf9Hq6eVLvSkuj+ftZI2RmU5eXnOKW1Wqh20="},
         {"x-amz-request-id","FAECC30C2CD53BCA"}
    ]}},
    Response2 = {ok, {{200, "OK"}, [], <<"TestBody">>}},
    meck:sequence(erlcloud_httpc, request, 6, [Response1, Response2]),
     Result = erlcloud_s3:get_bucket_policy(
                "bucket.name",
                config(#aws_config{retry = fun erlcloud_retry:default_retry/1})),
     ?_assertEqual({ok, "TestBody"}, Result).

%% Handle redirect by using bucket region from "x-amz-bucket-region" header.
error_handling_redirect_bucket_region() ->
    Response1 = {error, {http_error,301,"Temporary Redirect", <<>>, 
        [{"server","AmazonS3"},
         {"date","Wed, 22 Jul 2015 09:58:03 GMT"},
         {"transfer-encoding","chunked"},
         {"content-type","application/xml"},
         {"x-amz-id-2",
          "YIgyI9Lb9I/dMpDrRASSD8w5YsNAyhRlF+PDF0jlf9Hq6eVLvSkuj+ftZI2RmU5eXnOKW1Wqh20="},
         {"x-amz-request-id","FAECC30C2CD53BCA"},
         {"x-amz-bucket-region","us-west-1"}
    ]}},
    Response2 = {ok, {{200, "OK"}, [], <<"TestBody">>}},
    meck:sequence(erlcloud_httpc, request, 6, [Response1, Response2]),
     Result = erlcloud_s3:get_bucket_policy(
                "bucket.name",
                config(#aws_config{retry = fun erlcloud_retry:default_retry/1})),
     ?_assertEqual({ok, "TestBody"}, Result).

error_handling_tests(_) ->
    [error_handling_no_retry(),
     error_handling_default_retry(),
     error_handling_httpc_error(),
     error_handling_redirect_message(),
     error_handling_redirect_location(),
     error_handling_redirect_bucket_region()
    ].
