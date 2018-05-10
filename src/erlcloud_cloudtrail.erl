-module(erlcloud_cloudtrail).

-include("erlcloud.hrl").
-include("erlcloud_aws.hrl").

%% Library initialization.
-export([configure/2, configure/3, new/2, new/3]).

%% EC2 API Functions
-export([
    %% Users
    create_trail/3, create_trail/4, create_trail/5, create_trail/6,
    delete_trail/1, delete_trail/2,
    describe_trails/0, describe_trails/1, describe_trails/2, describe_trails/3,
    get_trail_status/1, get_trail_status/2,
    start_logging/1, start_logging/2,
    stop_logging/1, stop_logging/2,
    update_trail/4, update_trail/5, update_trail/6,
    ct_request/3
]).

-define(API_VERSION, "2013-11-01").
-define(CLOUD_TRAIL_API_PREFIX, "CloudTrail_20131101.").
-define(SERVICE_NAME, "cloudtrail").

-type headers() :: [{string(), string()}].

-type ct_return() :: {ok, proplist()} | {error, term()}.

-spec new(string(), string()) -> aws_config().
new(AccessKeyID, SecretAccessKey) ->
    #aws_config{access_key_id=AccessKeyID,
                secret_access_key=SecretAccessKey}.

-spec new(string(), string(), string()) -> aws_config().
new(AccessKeyID, SecretAccessKey, Host) ->
    #aws_config{access_key_id=AccessKeyID,
                secret_access_key=SecretAccessKey,
                cloudtrail_host=Host}.

-spec configure(string(), string()) -> ok.
configure(AccessKeyID, SecretAccessKey) ->
    erlcloud_config:configure(AccessKeyID, SecretAccessKey, fun new/2).

-spec configure(string(), string(), string()) -> ok.
configure(AccessKeyID, SecretAccessKey, Host) ->
    erlcloud_config:configure(AccessKeyID, SecretAccessKey, Host, fun new/3).

%%
%% API
%%
-spec create_trail(string(), string(), aws_config()) -> ct_return().
create_trail(Trail, S3BucketName, Config) ->
    Json = [{<<"Name">>, list_to_binary(Trail)}, 
            {<<"S3BucketName">>, list_to_binary(S3BucketName)}
           ],
    ct_request("CreateTrail", Json, Config).

-spec create_trail(string(), string(), string(), aws_config()) -> ct_return().
create_trail(Trail, S3BucketName, SnsTopicName,  Config) ->
    Json = [{<<"Name">>, list_to_binary(Trail)}, 
            {<<"S3BucketName">>, list_to_binary(S3BucketName)},
            {<<"SnsTopicName">>, list_to_binary(SnsTopicName)}
           ],
    ct_request("CreateTrail", Json, Config).

-spec create_trail(string(), string(), string(), boolean(), aws_config()) -> ct_return().
create_trail(Trail, S3BucketName, SnsTopicName, IncludeGlobalServiceEvents, Config) ->
    Json = [{<<"Name">>, list_to_binary(Trail)}, 
            {<<"S3BucketName">>, list_to_binary(S3BucketName)},
            {<<"SnsTopicName">>, list_to_binary(SnsTopicName)},
            {<<"IncludeGlobalServiceEvents">>, list_to_binary(atom_to_list(IncludeGlobalServiceEvents))}
           ],
    ct_request("CreateTrail", Json, Config).

-spec create_trail(string(), string(), string(), string(), boolean(), aws_config()) -> ct_return().
create_trail(Trail, S3BucketName, S3KeyPrefix, SnsTopicName, IncludeGlobalServiceEvents, Config) ->
    Json = [{<<"Name">>, list_to_binary(Trail)}, 
            {<<"S3BucketName">>, list_to_binary(S3BucketName)},
            {<<"S3KeyPrefix">>, list_to_binary(S3KeyPrefix)},
            {<<"SnsTopicName">>, list_to_binary(SnsTopicName)},
            {<<"IncludeGlobalServiceEvents">>, list_to_binary(atom_to_list(IncludeGlobalServiceEvents))}
           ],
    ct_request("CreateTrail", Json, Config).

-spec delete_trail(string() ) -> ct_return().
delete_trail(Trail) ->
    delete_trail(Trail, default_config()).

-spec delete_trail(string(), aws_config()) -> ct_return().
delete_trail(Trail, Config) ->
    Json = [{<<"Name">>, list_to_binary(Trail)}],
    ct_request("DeleteTrail", Json, Config).

-spec describe_trails() -> ct_return().
describe_trails() -> describe_trails([]).

-spec describe_trails([] | aws_config()) -> ct_return().
describe_trails(Config) when is_record(Config, aws_config) ->
    describe_trails([], Config);

describe_trails(Trails) ->
    describe_trails(Trails, default_config()).

-spec describe_trails([string()], aws_config()) -> ct_return().
describe_trails(Trails, Config) ->
    Json = trail_name_list(Trails),
    ct_request("DescribeTrails", Json, Config).

-spec describe_trails([string()], boolean(), aws_config()) -> ct_return().
describe_trails(Trails, IncludeShadowTrails, Config) ->
    TrailNameList = trail_name_list(Trails),
    Json =
        case IncludeShadowTrails of
            true -> TrailNameList;
            false -> [{<<"includeShadowTrails">>, false} | TrailNameList]
        end,
    ct_request("DescribeTrails", Json, Config).

-spec get_trail_status([string()] ) -> ct_return().
get_trail_status(Trail) ->
    get_trail_status(Trail, default_config()).

-spec get_trail_status([string()], aws_config()) -> ct_return().
get_trail_status(Trail, Config) ->
    Json = [{<<"Name">>, list_to_binary(Trail)}],
    ct_request("GetTrailStatus", Json, Config).

-spec start_logging([string()] ) -> ct_return().
start_logging(Trail) ->
    start_logging(Trail, default_config()).

-spec start_logging([string()], aws_config()) -> ct_return().
start_logging(Trail, Config) ->
    Json = [{<<"Name">>, list_to_binary(Trail)}],
    ct_request("StartLogging", Json, Config).

-spec stop_logging([string()] ) -> ct_return().
stop_logging(Trail) ->
    stop_logging(Trail, default_config()).

-spec stop_logging([string()], aws_config()) -> ct_return().
stop_logging(Trail, Config) ->
    Json = [{<<"Name">>, list_to_binary(Trail)}],
    ct_request("StopLogging", Json, Config).

-spec update_trail(string(), string(), string(), aws_config()) -> ct_return().
update_trail(Trail, S3BucketName, SnsTopicName, Config) ->
    Json = [{<<"Name">>, list_to_binary(Trail)}, 
            {<<"S3BucketName">>, list_to_binary(S3BucketName)},
            {<<"SnsTopicName">>, list_to_binary(SnsTopicName)}
           ],
    ct_request("UpdateTrail", Json, Config).

-spec update_trail(string(), string(), string(), boolean(), aws_config()) -> ct_return().
update_trail(Trail, S3BucketName, SnsTopicName, IncludeGlobalServiceEvents, Config) ->
    Json = [{<<"Name">>, list_to_binary(Trail)}, 
            {<<"S3BucketName">>, list_to_binary(S3BucketName)},
            {<<"SnsTopicName">>, list_to_binary(SnsTopicName)},
            {<<"IncludeGlobalServiceEvents">>, list_to_binary(atom_to_list(IncludeGlobalServiceEvents))}
           ],
    ct_request("UpdateTrail", Json, Config).


-spec update_trail(string(), string(), string(), string(), boolean(), aws_config()) -> ct_return().
update_trail(Trail, S3BucketName, S3KeyPrefix, SnsTopicName, IncludeGlobalServiceEvents, Config) ->
    Json = [{<<"Name">>, list_to_binary(Trail)}, 
            {<<"S3BucketName">>, list_to_binary(S3BucketName)},
            {<<"S3KeyPrefix">>, list_to_binary(S3KeyPrefix)},
            {<<"SnsTopicName">>, list_to_binary(SnsTopicName)},
            {<<"IncludeGlobalServiceEvents">>, list_to_binary(atom_to_list(IncludeGlobalServiceEvents))}
           ],
    ct_request("UpdateTrail", Json, Config).

% Json parameter must be a list of binary key/value tuples.
ct_request(Operation, [], Config) ->
    #aws_config{cloudtrail_scheme = Scheme, 
                cloudtrail_host = Host} = Config,
    request_impl(post, Scheme, Host, port_spec(Config), "/", Operation, [], "{}", Config);

ct_request(Operation, Body, Config) ->
    #aws_config{cloudtrail_scheme = Scheme, 
                cloudtrail_host = Host} = Config,
    request_impl(post, Scheme, Host, port_spec(Config), "/", Operation, [], jsx:encode(Body), Config).
 
request_impl(Method, Scheme, Host, Port, Path, Operation, Params, Body, #aws_config{} = Config) ->
    %% TODO: Make api prefix a part of aws_config
    Api_Operation = lists:flatten(?CLOUD_TRAIL_API_PREFIX, Operation),
    Headers = headers(Config, Api_Operation, Params, Body, ?SERVICE_NAME),
    
    case erlcloud_aws:aws_request_form_raw(
        Method, Scheme, Host, Port, Path, Body, 
        [{"content-type", "application/x-amz-json-1.1"} | Headers], 
        Config) of
       {ok, RespBody} ->
            case Config#aws_config.cloudtrail_raw_result of
                true -> {ok, RespBody};
                _ -> {ok, jsx:decode(RespBody)}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

-spec headers(aws_config(), string(), proplist(), string() | binary(), string()) -> headers().
headers(Config, Operation, _Params, Body, Service) ->
    Headers = [
               {"host", Config#aws_config.cloudtrail_host},
               {"x-amz-target", Operation}
               ],
    erlcloud_aws:sign_v4_headers(Config, Headers, Body, erlcloud_aws:aws_region_from_host(Config#aws_config.cloudtrail_host), Service).


default_config() -> erlcloud_aws:default_config().

port_spec(#aws_config{cloudtrail_port=80}) ->
    undefined;
port_spec(#aws_config{cloudtrail_port=Port}) ->
    Port.

trail_name_list([]) ->
    [];
trail_name_list(Trails) ->
    [{<<"trailNameList">>, [list_to_binary(T) || T <- Trails]}].
