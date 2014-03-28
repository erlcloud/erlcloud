-module(erlcloud_cloudtrail).

-include_lib("erlcloud/include/erlcloud.hrl").
-include_lib("erlcloud/include/erlcloud_aws.hrl").

%% EC2 API Functions
-export([
    %% Users
    create_trail/3, create_trail/4, create_trail/5, create_trail/6,
    delete_trail/1, delete_trail/2,
    describe_trails/0, describe_trails/1, describe_trails/2,
    get_trail_status/1, get_trail_status/2,
    start_logging/1, start_logging/2,
    stop_logging/1, stop_logging/2,
    update_trail/6,
    ct_request/3
]).

-import(erlcloud_xml, [get_text/1, get_text/2, get_text/3, get_bool/2, get_list/2, get_integer/2]).

-define(API_VERSION, "2013-11-01").
-define(CLOUD_TRAIL_API_PREFIX, "CloudTrail_20131101.").
-define(SERVICE_NAME, "cloudtrail").

-type headers() :: [{string(), string()}].

%%
%% API
%%
% TODO: Fix to return ok instead of a tuple
-spec(create_trail/3 :: (string(), string(), aws_config()) -> proplist()).
create_trail(Trail, S3BucketName, Config) ->
    Json = [{<<"Name">>, list_to_binary(Trail)}, 
            {<<"S3BucketName">>, list_to_binary(S3BucketName)}
           ],
    ct_request("CreateTrail", Json, Config).

-spec(create_trail/4 :: (string(), string(), string(), aws_config()) -> proplist()).
create_trail(Trail, S3BucketName, SnsTopicName,  Config) ->
    Json = [{<<"Name">>, list_to_binary(Trail)}, 
            {<<"S3BucketName">>, list_to_binary(S3BucketName)},
            {<<"SnsTopicName">>, list_to_binary(SnsTopicName)}
           ],
    ct_request("CreateTrail", Json, Config).

-spec(create_trail/5 :: (string(), string(), string(), boolean(), aws_config()) -> proplist()).
create_trail(Trail, S3BucketName, SnsTopicName, IncludeGlobalServiceEvents, Config) ->
    Json = [{<<"Name">>, list_to_binary(Trail)}, 
            {<<"S3BucketName">>, list_to_binary(S3BucketName)},
            {<<"SnsTopicName">>, list_to_binary(SnsTopicName)},
            {<<"IncludeGlobalServiceEvents">>, list_to_binary(atom_to_list(IncludeGlobalServiceEvents))}
           ],
    ct_request("CreateTrail", Json, Config).

-spec(create_trail/6 :: (string(), string(), string(), string(), boolean(), aws_config()) -> proplist()).
create_trail(Trail, S3BucketName, S3KeyPrefix, SnsTopicName, IncludeGlobalServiceEvents, Config) ->
    Json = [{<<"Name">>, list_to_binary(Trail)}, 
            {<<"S3BucketName">>, list_to_binary(S3BucketName)},
            {<<"S3KeyPrefix">>, list_to_binary(S3KeyPrefix)},
            {<<"SnsTopicName">>, list_to_binary(SnsTopicName)},
            {<<"IncludeGlobalServiceEvents">>, list_to_binary(atom_to_list(IncludeGlobalServiceEvents))}
           ],
    ct_request("CreateTrail", Json, Config).

-spec(delete_trail/1 :: ([string()] ) -> proplist()).
delete_trail(Trail) ->
    delete_trail(Trail, default_config()).

-spec(delete_trail/2 :: ([string()], aws_config()) -> proplist()).
delete_trail(Trail, Config) ->
    Json = [{<<"Name">>, list_to_binary(Trail)}],
    ct_request("DeleteTrail", Json, Config).

-spec(describe_trails/0 :: () -> proplist()).
describe_trails() -> describe_trails([]).

-spec(describe_trails/1 :: ([string()] | aws_config()) -> proplist()).
describe_trails(Config) when is_record(Config, aws_config) ->
    describe_trails([], Config);

%% It appears that CloudTrail API doesn't honor TrailNameList parameter.
%% TODO: Open a ticket with AWS.
describe_trails(Trails) ->
    Json = [{<<"TrailNameList">>, Trails}],
    describe_trails(Json, default_config()).

-spec(describe_trails/2 :: ([string()], aws_config()) -> proplist()).
describe_trails([], Config) ->
    ct_request("DescribeTrails", [], Config);

describe_trails(Trails, Config) ->
    Json = [{<<"TrailNameList">>, jsx:encode(list_to_binary(Trails))}],
    ct_request("DescribeTrails", Json, Config).

-spec(get_trail_status/1 :: ([string()] ) -> proplist()).
get_trail_status(Trail) ->
    get_trail_status(Trail, default_config()).

-spec(get_trail_status/2 :: ([string()], aws_config()) -> proplist()).
get_trail_status(Trail, Config) ->
    Json = [{<<"Name">>, list_to_binary(Trail)}],
    ct_request("GetTrailStatus", Json, Config).

-spec(start_logging/1 :: ([string()] ) -> proplist()).
start_logging(Trail) ->
    start_logging(Trail, default_config()).

-spec(start_logging/2 :: ([string()], aws_config()) -> proplist()).
start_logging(Trail, Config) ->
    Json = [{<<"Name">>, list_to_binary(Trail)}],
    ct_request("StartLogging", Json, Config).

-spec(stop_logging/1 :: ([string()] ) -> proplist()).
stop_logging(Trail) ->
    stop_logging(Trail, default_config()).

-spec(stop_logging/2 :: ([string()], aws_config()) -> proplist()).
stop_logging(Trail, Config) ->
    Json = [{<<"Name">>, list_to_binary(Trail)}],
    ct_request("StopLogging", Json, Config).

% TODO: Provide helper functions. Perhaps also support proplist.
-spec(update_trail/6 :: (string(), string(), string(), string(), boolean(), aws_config()) -> proplist()).
update_trail(Trail, S3BucketName, S3KeyPrefix, SnsTopicName, IncludeGlobalServiceEvents, Config) ->
    Json = [{<<"Name">>, list_to_binary(Trail)}, 
            {<<"S3BucketName">>, list_to_binary(S3BucketName)},
            {<<"S3KeyPrefix">>, list_to_binary(S3KeyPrefix)},
            {<<"SnsTopicName">>, list_to_binary(SnsTopicName)},
            {<<"IncludeGlobalServiceEvents">>, list_to_binary(atom_to_list(IncludeGlobalServiceEvents))}
           ],
    ct_request("UpdateTrail", Json, Config).

%% TODO: Move protocol specific stuff to a separate module.

% Json parameter must be a list of binary key/value tuples.
ct_request(Operation, [], Config = #aws_config{cloudtrail_host = Host, cloudtrail_port = Port}) ->
    request_impl(post, undefined, Host, Port, "/", Operation, [], <<"{}">>, Config);

ct_request(Operation, Body, Config = #aws_config{cloudtrail_host = Host, cloudtrail_port = Port}) ->
    request_impl(post, undefined, Host, Port, "/", Operation, [], jsx:encode(Body), Config)
.
 
request_impl(Method, Protocol, Host, Port, Path, Operation, Params, Body, #aws_config{} = Config) ->
    %% TOD: Make api prefix a part of aws_config
    Api_Operation = lists:flatten(?CLOUD_TRAIL_API_PREFIX, Operation),
    Headers = headers(Config, Api_Operation, Params, Body, ?SERVICE_NAME),
    %%%% io:format("------ Request URL: ~p~nHeaders: ~p~nBody ~p~n", [url(Config), Headers, Body]),
    % ({ok, {{_HTTPVer, OKStatus, _StatusLine}, Headers, Body}})
    case erlcloud_aws:http_headers_body(
            httpc:request(Method, {url(Config), Headers, "application/x-amz-json-1.1", Body},
                            [{timeout, 1000}],
                            [{body_format, binary}])) of
        {ok, {RespHeader, RespBody}} ->
            %% {ok, {{_, 200, _}, _, RespBody}} ->
            %% TODO check crc
            %%%% io:format("-------Response:~nHeader:~p~nBody:~p~n", [RespHeader, RespBody]),
            case Config#aws_config.raw_result of
                true -> {ok, RespBody};
                _ -> {ok, jsx:decode(RespBody)}
            end;
        {error, {http_error, Status, StatusLine, RespBody}} ->
            io:format("-------Call Failed Response:~nStatus:~p~nStatusLine:~p~nBody:~p~n", [Status, StatusLine, RespBody]);
        {error, Reason} ->
            {error, Reason}
    end.

-spec headers(aws_config(), string(), proplist(), binary(), string()) -> headers().
headers(Config, Operation, Params, Body, Service) ->
    Headers = [{"content-type", "application/x-amz-json-1.1"},
               {"host", Config#aws_config.cloudtrail_host},
               {"x-amz-target", Operation}
               ],
    Region =
        case string:tokens(Config#aws_config.cloudtrail_host, ".") of
            [_, Value, _, _] ->
                Value;
            _ ->
                "us-east-1"
        end,
    
    erlcloud_aws:sign_v4(Config, Headers, Body, Region, Service).


default_config() -> erlcloud_aws:default_config().

url(#aws_config{cloudtrail_scheme = Scheme, cloudtrail_host = Host} = Config) ->
    lists:flatten([Scheme, Host, port_spec(Config)]).

port_spec(#aws_config{cloudtrail_port=80}) ->
    "";
port_spec(#aws_config{cloudtrail_port=Port}) ->
    [":", erlang:integer_to_list(Port)].


