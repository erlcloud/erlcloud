-module(erlcloud_sm).
-author("josh").

-include("erlcloud.hrl").
-include("erlcloud_aws.hrl").

%%% Library initialization.
-export([new/2, new/3, new/4]).

%%% API
-export([
    get_secret_value/1, get_secret_value/2, get_secret_value/3
]).

%%%------------------------------------------------------------------------------
%%% Shared types
%%%------------------------------------------------------------------------------

-type version_type() :: id | stage.

%%%------------------------------------------------------------------------------
%%% Library initialization.
%%%------------------------------------------------------------------------------

-spec new(string(), string()) -> aws_config().
new(AccessKeyID, SecretAccessKey) ->
    #aws_config{
        access_key_id = AccessKeyID,
        secret_access_key = SecretAccessKey
    }.


-spec new(string(), string(), string()) -> aws_config().
new(AccessKeyID, SecretAccessKey, Host) ->
    #aws_config{
        access_key_id = AccessKeyID,
        secret_access_key = SecretAccessKey,
        sm_host = Host
    }.


-spec new(string(), string(), string(), non_neg_integer()) -> aws_config().
new(AccessKeyID, SecretAccessKey, Host, Port) ->
    #aws_config{
        access_key_id = AccessKeyID,
        secret_access_key = SecretAccessKey,
        sm_host = Host,
        sm_port = Port
    }.

%%------------------------------------------------------------------------------
%% GetSecretValue
%%------------------------------------------------------------------------------
%% @doc
%% SM API:
%% [https://docs.aws.amazon.com/secretsmanager/latest/apireference/API_GetSecretValue.html]
%% @end
%%------------------------------------------------------------------------------

-spec get_secret_value(SecretId :: binary()) -> proplist().
get_secret_value(SecretId) ->
    get_secret_value(SecretId, undefined, erlcloud_aws:default_config()).

-spec get_secret_value(SecretId :: binary(), Version :: {version_type(), binary()}) -> proplist().
get_secret_value(SecretId, Version) ->
    get_secret_value(SecretId, Version, erlcloud_aws:default_config()).

-spec get_secret_value(SecretId :: binary(), Version :: {version_type(), binary()},
        Config :: aws_config()) -> proplist().
get_secret_value(SecretId, Version, Config) ->
    Json = case Version of
        undefined -> [{<<"SecretId">>, SecretId}];
        {id, Val} -> [{<<"SecretId">>, SecretId}, {<<"VersionId">>, Val}];
        {stage, Val} -> [{<<"SecretId">>, SecretId}, {<<"VersionStage">>, Val}]
    end,
    sm_request(Config, "secretsmanager.GetSecretValue", Json).

%%%------------------------------------------------------------------------------
%%% Internal Functions
%%%------------------------------------------------------------------------------

sm_request(Config, Operation, Body) ->
    case erlcloud_aws:update_config(Config) of
        {ok, Config1} ->
            sm_request_no_update(Config1, Operation, Body);
        {error, Reason} ->
            {error, Reason}
    end.


sm_request_no_update(Config, Operation, Body) ->
    Payload = case Body of
                  [] -> <<"{}">>;
                  _ -> jsx:encode(Body)
              end,
    Headers = headers(Config, Operation, Payload),
    Request = #aws_request{service = kms,
        uri = uri(Config),
        method = post,
        request_headers = Headers,
        request_body = Payload},
    case erlcloud_aws:request_to_return(erlcloud_retry:request(Config, Request, fun sm_result_fun/1)) of
        {ok, {_RespHeaders, <<>>}} -> {ok, []};
        {ok, {_RespHeaders, RespBody}} -> {ok, jsx:decode(RespBody)};
        {error, _} = Error -> Error
    end.


headers(Config, Operation, Body) ->
    Headers = [{"host", Config#aws_config.sm_host},
        {"x-amz-target", Operation},
        {"content-type", "application/x-amz-json-1.1"}],
    Region = erlcloud_aws:aws_region_from_host(Config#aws_config.sm_host),
    erlcloud_aws:sign_v4_headers(Config, Headers, Body, Region, "secretmanager").


uri(#aws_config{kms_scheme = Scheme, kms_host = Host} = Config) ->
    lists:flatten([Scheme, Host, port_spec(Config)]).


port_spec(#aws_config{sm_port = 443}) ->
    "";
port_spec(#aws_config{sm_port = Port}) ->
    [":", erlang:integer_to_list(Port)].


-spec sm_result_fun
        (Request :: aws_request()) -> aws_request().
sm_result_fun(#aws_request{response_type = ok} = Request) ->
    Request;
sm_result_fun(#aws_request{response_type = error, error_type = aws, response_status = Status} = Request)
        when Status >= 500 ->
    Request#aws_request{should_retry = true};
sm_result_fun(#aws_request{response_type = error, error_type = aws} = Request) ->
    Request#aws_request{should_retry = false}.
