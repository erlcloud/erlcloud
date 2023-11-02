-module(erlcloud_sm).
-author("joshua@halloapp.com").

-include("erlcloud.hrl").
-include("erlcloud_aws.hrl").

%%% Library initialization.
-export([new/2, new/3, new/4]).

%%% API
-export([
    create_secret_binary/3, create_secret_binary/4, create_secret_binary/5,
    create_secret_string/3, create_secret_string/4, create_secret_string/5,
    delete_resource_policy/1, delete_resource_policy/2,
    delete_secret/1, delete_secret/2, delete_secret/3,
    describe_secret/1, describe_secret/2,
    get_resource_policy/1, get_resource_policy/2,
    get_secret_value/2, get_secret_value/3,
    put_resource_policy/2, put_resource_policy/3, put_resource_policy/4,
    put_secret_binary/3, put_secret_binary/4, put_secret_binary/5,
    put_secret_string/3, put_secret_string/4, put_secret_string/5
]).

%%%------------------------------------------------------------------------------
%%% Shared types
%%%------------------------------------------------------------------------------

-type sm_response() :: {ok, proplists:proplist()} | {error, term()}.

-type get_secret_value_option() :: {version_id | version_stage, binary()}.
-type get_secret_value_options() :: [get_secret_value_option()].

%% replica region is expected to be a proplist of the following tuples:
%% [{<<"KmsKeyId">>, binary()}, {<<"Region">>, binary()}]
-type replica_region() :: [proplist()].
-type replica_regions() :: [replica_region()].

-type create_secret_option() :: {add_replica_regions, replica_regions()}
                              | {client_request_token, binary()}
                              | {description, binary()}
                              | {force_overwrite_replica_secret, boolean()}
                              | {kms_key_id, binary()}
                              | {secret_binary, binary()} %% Note AWS accepts either SecretBinary or SecretString,
                              | {secret_string, binary()} %% not both at the same time
                              | {tags, proplist()}.
-type create_secret_options() :: [create_secret_option()].

-type delete_secret_option() :: {force_delete_without_recovery, boolean()} %% Note you can't use both this parameter and RecoveryWindowInDays.
                              | {recovery_window_in_days, pos_integer()}.  %% If none of these two options are specified then SM defaults to 30 day recovery window
-type delete_secret_options() :: [delete_secret_option()].

-type put_resource_policy_option() :: {block_public_policy, boolean()}.
-type put_resource_policy_options() :: [put_resource_policy_option()].

-type put_secret_value_option() :: {client_request_token, binary()}
                                 | {secret_binary, binary()}
                                 | {secret_string, binary()}
                                 | {version_stages, [binary()]}.
-type put_secret_value_options() :: [put_secret_value_option()].

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
%% CreateSecret - SecretBinary
%%------------------------------------------------------------------------------
%% @doc
%% Creates a new secret binary. The function internally base64-encodes the binary
%% as it is expected by the AWS SecretManager API, so raw blob is expected
%% to be passed as an attribute.
%%
%% ClientRequestToken is used by AWS for secret versioning purposes.
%% It is recommended to be a UUID type value, and is requred to be between
%% 32 and 64 characters.
%%
%% To store a text secret use CreateSecret - SecretString version of the function
%% instead.
%%
%% SM API:
%% [https://docs.aws.amazon.com/secretsmanager/latest/apireference/API_CreateSecret.html]
%%
%% Example:
%% Name = <<"my-secret-binary">>,
%% ClientRequestToken = <<"7537a353-0de0-4b98-bf55-f8365821ed36">>,
%% %% some binary to store (say, an RSA private key's exponent)
%% {[_E, _Pub], [_E, _N, Priv, _P1, _P2, _E1, _E2, _C]} = crypto:generate_key(rsa, {2048,65537}),
%% erlcloud_sm:create_secret_binary(Name, ClientRequestToken, Priv).
%% @end
%%------------------------------------------------------------------------------

-spec create_secret_binary(Name :: binary(), ClientRequestToken :: binary(),
                           SecretBinary :: binary()) -> sm_response().
create_secret_binary(Name, ClientRequestToken, SecretBinary) ->
    create_secret_binary(Name, ClientRequestToken, SecretBinary, []).

-spec create_secret_binary(Name :: binary(), ClientRequestToken :: binary(),
                           SecretBinary :: binary(),
                           Opts :: create_secret_options()) -> sm_response().
create_secret_binary(Name, ClientRequestToken, SecretBinary, Opts) ->
    create_secret_binary(Name, ClientRequestToken, SecretBinary, Opts, erlcloud_aws:default_config()).

-spec create_secret_binary(Name :: binary(), ClientRequestToken :: binary(),
                           SecretBinary :: binary(), Opts :: create_secret_options(),
                           Config :: aws_config()) -> sm_response().
create_secret_binary(Name, ClientRequestToken, SecretBinary, Opts, Config) ->
    Secret = {secret_binary, base64:encode(SecretBinary)},
    create_secret(Name, ClientRequestToken, [Secret | Opts], Config).

%%------------------------------------------------------------------------------
%% CreateSecret - SecretString
%%------------------------------------------------------------------------------
%% @doc
%% Creates a new secret string. The API expects SecretString is a text data to
%% encrypt and store in the SecretManager. It is recommended a JSON structure
%% of key/value pairs is used for the secret value.
%%
%% ClientRequestToken is used by AWS for secret versioning purposes.
%% It is recommended to be a UUID type value, and is requred to be between
%% 32 and 64 characters.
%%
%% To store a binary (which will be base64 encoded by the library, as it is
%% expected by AWS SecretManager API), use CreateSecret - SecretBinary version
%% of the function.
%%
%% SM API:
%% [https://docs.aws.amazon.com/secretsmanager/latest/apireference/API_CreateSecret.html]
%%
%% Example:
%% Name = <<"my-secret-string">>,
%% ClientRequestToken = <<"7537a353-0de0-4b98-bf55-f8365821ed37">>,
%% %% some user/password json to store
%% Secret = jsx:encode(#{<<"user">> => <<"my-user">>, <<"password">> => <<"superSecretPassword">>}),
%% erlcloud_sm:create_secret_string(Name, ClientRequestToken, Secret).
%%
%% @end
%%------------------------------------------------------------------------------

-spec create_secret_string(Name :: binary(), ClientRequestToken :: binary(),
                           SecretString :: binary()) -> sm_response().
create_secret_string(Name, ClientRequestToken, SecretString) ->
    create_secret_string(Name, ClientRequestToken, SecretString, []).

-spec create_secret_string(Name :: binary(), ClientRequestToken :: binary(),
                           SecretString :: binary(), Opts :: create_secret_options()) -> sm_response().
create_secret_string(Name, ClientRequestToken, SecretString, Opts) ->
    create_secret_string(Name, ClientRequestToken, SecretString, Opts, erlcloud_aws:default_config()).


-spec create_secret_string(Name :: binary(), ClientRequestToken :: binary(),
                           SecretString :: binary(), Opts :: create_secret_options(),
                           Config :: aws_config()) -> sm_response().
create_secret_string(Name, ClientRequestToken, SecretString, Opts, Config) ->
    Secret = {secret_string, SecretString},
    create_secret(Name, ClientRequestToken, [Secret | Opts], Config).

%%------------------------------------------------------------------------------
%% DeleteResourcePolicy
%%------------------------------------------------------------------------------
%% @doc
%% SM API:
%% [https://docs.aws.amazon.com/secretsmanager/latest/apireference/API_DeleteResourcePolicy.html]
%% @end
%%------------------------------------------------------------------------------

-spec delete_resource_policy(SecretId :: binary()) -> sm_response().
delete_resource_policy(SecretId) ->
    delete_resource_policy(SecretId, erlcloud_aws:default_config()).

-spec delete_resource_policy(SecretId :: binary(), Config :: aws_config()) -> sm_response().
delete_resource_policy(SecretId, Config) ->
    Json = [{<<"SecretId">>, SecretId}],
    sm_request(Config, "secretsmanager.DeleteResourcePolicy", Json).

%%------------------------------------------------------------------------------
%% DeleteSecret
%%------------------------------------------------------------------------------
%% @doc
%% SM API:
%% [https://docs.aws.amazon.com/secretsmanager/latest/apireference/API_DeleteSecret.html]
%% @end
%%------------------------------------------------------------------------------

-spec delete_secret(SecretId :: binary()) -> sm_response().
delete_secret(SecretId) ->
    delete_secret(SecretId, []).

-spec delete_secret(SecretId :: binary(), Opts :: delete_secret_options()) -> sm_response().
delete_secret(SecretId, Opts) ->
    delete_secret(SecretId, Opts, erlcloud_aws:default_config()).

-spec delete_secret(SecretId :: binary(), Opts :: delete_secret_options(),
        Config :: aws_config()) -> sm_response().
delete_secret(SecretId, Opts, Config) ->
    Json = lists:map(
        fun
            ({force_delete_without_recovery, Val}) -> {<<"ForceDeleteWithoutRecovery">>, Val};
            ({recovery_window_in_days, Val}) -> {<<"RecoveryWindowInDays">>, Val};
            (Other) -> Other
        end,
        [{<<"SecretId">>, SecretId} | Opts]),
    sm_request(Config, "secretsmanager.DeleteSecret", Json).

%%------------------------------------------------------------------------------
%% DescribeSecret
%%------------------------------------------------------------------------------
%% @doc
%% SM API:
%% [https://docs.aws.amazon.com/secretsmanager/latest/apireference/API_DescibeSecret.html]
%% @end
%%------------------------------------------------------------------------------

-spec describe_secret(SecretId :: binary()) -> sm_response().
describe_secret(SecretId) ->
    describe_secret(SecretId, erlcloud_aws:default_config()).

-spec describe_secret(SecretId :: binary(), Config :: aws_config()) -> sm_response().
describe_secret(SecretId, Config) ->
    Json = [{<<"SecretId">>, SecretId}],
    sm_request(Config, "secretsmanager.DescribeSecret", Json).

%%------------------------------------------------------------------------------
%% GetResourcePolicy
%%------------------------------------------------------------------------------
%% @doc
%% SM API:
%% [https://docs.aws.amazon.com/secretsmanager/latest/apireference/API_GetResourcePolicy.html]
%% @end
%%------------------------------------------------------------------------------

-spec get_resource_policy(SecretId :: binary()) -> sm_response().
get_resource_policy(SecretId) ->
    get_resource_policy(SecretId, erlcloud_aws:default_config()).

-spec get_resource_policy(SecretId :: binary(), Config :: aws_config()) -> sm_response().
get_resource_policy(SecretId, Config) ->
    Json = [{<<"SecretId">>, SecretId}],
    sm_request(Config, "secretsmanager.GetResourcePolicy", Json).

%%------------------------------------------------------------------------------
%% GetSecretValue
%%------------------------------------------------------------------------------
%% @doc
%% SM API:
%% [https://docs.aws.amazon.com/secretsmanager/latest/apireference/API_GetSecretValue.html]
%% @end
%%------------------------------------------------------------------------------

-spec get_secret_value(SecretId :: binary(), Opts :: get_secret_value_options()) -> sm_response().
get_secret_value(SecretId, Opts) ->
    get_secret_value(SecretId, Opts, erlcloud_aws:default_config()).

-spec get_secret_value(SecretId :: binary(), Opts :: get_secret_value_options(),
        Config :: aws_config()) -> sm_response().
get_secret_value(SecretId, Opts, Config) ->
    Json = lists:map(
        fun
            ({version_id, Val}) -> {<<"VersionId">>, Val};
            ({version_stage, Val}) -> {<<"VersionStage">>, Val};
            (Other) -> Other
        end,
        [{<<"SecretId">>, SecretId} | Opts]),
    sm_request(Config, "secretsmanager.GetSecretValue", Json).

%%------------------------------------------------------------------------------
%% PutResourcePolicy
%%------------------------------------------------------------------------------
%% @doc
%% SM API:
%% [https://docs.aws.amazon.com/secretsmanager/latest/apireference/API_PutResourcePolicy.html]
%% @end
%%------------------------------------------------------------------------------

-spec put_resource_policy(SecretId :: binary(), ResourcePolicy :: binary()) -> sm_response().
put_resource_policy(SecretId, ResourcePolicy) ->
    put_resource_policy(SecretId, ResourcePolicy, []).

-spec put_resource_policy(SecretId :: binary(), ResourcePolicy :: binary(),
                          Opts :: put_resource_policy_options()) -> sm_response().
put_resource_policy(SecretId, ResourcePolicy, Opts) ->
    put_resource_policy(SecretId, ResourcePolicy, Opts, erlcloud_aws:default_config()).

-spec put_resource_policy(SecretId :: binary(), ResourcePolicy :: binary(),
                          Opts :: put_resource_policy_options(),
                          Config :: aws_config()) -> sm_response().
put_resource_policy(SecretId, ResourcePolicy, Opts, Config) ->
    Json = lists:map(
        fun
            ({block_public_policy, Val}) -> {<<"BlockPublicPolicy">>, Val};
            (Other) -> Other
        end,
        [{<<"SecretId">>, SecretId}, {<<"ResourcePolicy">>, ResourcePolicy} | Opts]),
    sm_request(Config, "secretsmanager.PutResourcePolicy", Json).

%%------------------------------------------------------------------------------
%% PutSecretValue
%%------------------------------------------------------------------------------
%% @doc
%% SM API:
%% [https://docs.aws.amazon.com/secretsmanager/latest/apireference/API_PutSecretValue.html]
%% @end
%%------------------------------------------------------------------------------
-spec put_secret_string(SecretId :: binary(), ClientRequestToken :: binary(),
                        SecretString :: binary()) -> sm_response().
put_secret_string(SecretId, ClientRequestToken, SecretString) ->
    put_secret_string(SecretId, ClientRequestToken, SecretString, []).

-spec put_secret_string(SecretId :: binary(), ClientRequestToken :: binary(),
                        SecretString :: binary(), Opts :: put_secret_value_options()) -> sm_response().
put_secret_string(SecretId, ClientRequestToken, SecretString, Opts) ->
    put_secret_string(SecretId, ClientRequestToken, SecretString, Opts, erlcloud_aws:default_config()).

-spec put_secret_string(SecretId :: binary(), ClientRequestToken :: binary(),
                        SecretString :: binary(), Opts :: put_secret_value_options(),
                        Config :: aws_config()) -> sm_response().
put_secret_string(SecretId, ClientRequestToken, SecretString, Opts, Config) ->
    Secret = {secret_string, SecretString},
    put_secret(SecretId, ClientRequestToken, [Secret | Opts], Config).

-spec put_secret_binary(SecretId :: binary(), ClientRequestToken :: binary(),
                        SecretBinary :: binary()) -> sm_response().
put_secret_binary(SecretId, ClientRequestToken, SecretBinary) ->
    put_secret_binary(SecretId, ClientRequestToken, SecretBinary, []).

-spec put_secret_binary(SecretId :: binary(), ClientRequestToken :: binary(),
                        SecretBinary :: binary(), Opts :: put_secret_value_options()) -> sm_response().
put_secret_binary(SecretId, ClientRequestToken, SecretBinary, Opts) ->
    put_secret_binary(SecretId, ClientRequestToken, SecretBinary, Opts, erlcloud_aws:default_config()).

-spec put_secret_binary(SecretId :: binary(), ClientRequestToken :: binary(),
                        SecretBinary :: binary(), Opts :: put_secret_value_options(),
                        Config :: aws_config()) -> sm_response().
put_secret_binary(SecretId, ClientRequestToken, SecretBinary, Opts, Config) ->
    Secret = {secret_binary, base64:encode(SecretBinary)},
    put_secret(SecretId, ClientRequestToken, [Secret | Opts], Config).

%%%------------------------------------------------------------------------------
%%% Internal Functions
%%%------------------------------------------------------------------------------

create_secret(SecretName, ClientRequestToken, Opts, Config) ->
    Opts1 = [{client_request_token, ClientRequestToken} | Opts],
    Json = create_secret_payload(SecretName, Opts1),
    sm_request(Config, "secretsmanager.CreateSecret", Json).

put_secret(SecretId, ClientRequestToken, Opts, Config) ->
    Json = lists:map(
        fun
            ({secret_binary, Val}) -> {<<"SecretBinary">>, Val};
            ({secret_string, Val}) -> {<<"SecretString">>, Val};
            ({version_stages, Val}) -> {<<"VersionStages">>, Val};
            (Other) -> Other
        end,
        [{<<"SecretId">>, SecretId}, {<<"ClientRequestToken">>, ClientRequestToken} | Opts]),
    sm_request(Config, "secretsmanager.PutSecretValue", Json).


sm_request(Config, Operation, Body) ->
    case erlcloud_aws:update_config(Config) of
        {ok, Config1} ->
            sm_request_no_update(Config1, Operation, Body);
        {error, Reason} ->
            {error, Reason}
    end.


sm_request_no_update(Config, Operation, Body) ->
    Payload = jsx:encode(Body),
    Headers = headers(Config, Operation, Payload),
    Request = #aws_request{service = sm,
        uri = uri(Config),
        method = post,
        request_headers = Headers,
        request_body = Payload},
    case erlcloud_aws:request_to_return(erlcloud_retry:request(Config, Request, fun sm_result_fun/1)) of
        {ok, {_RespHeaders, <<>>}} -> {ok, []};
        {ok, {_RespHeaders, RespBody}} -> {ok, jsx:decode(RespBody, [{return_maps, false}])};
        {error, _} = Error -> Error
    end.


headers(Config, Operation, Body) ->
    Headers = [{"host", Config#aws_config.sm_host},
        {"x-amz-target", Operation},
        {"content-type", "application/x-amz-json-1.1"}],
    Region = erlcloud_aws:aws_region_from_host(Config#aws_config.sm_host),
    erlcloud_aws:sign_v4_headers(Config, Headers, Body, Region, "secretsmanager").


uri(#aws_config{sm_scheme = Scheme, sm_host = Host} = Config) ->
    lists:flatten([Scheme, Host, port_spec(Config)]).


port_spec(#aws_config{sm_port = 443}) ->
    "";
port_spec(#aws_config{sm_port = Port}) ->
    [":", erlang:integer_to_list(Port)].


-spec sm_result_fun(Request :: aws_request()) -> aws_request().
sm_result_fun(#aws_request{response_type = ok} = Request) ->
    Request;
sm_result_fun(#aws_request{response_type = error,
        error_type = aws, response_status = Status} = Request) when Status >= 500 ->
    Request#aws_request{should_retry = true};
sm_result_fun(#aws_request{response_type = error, error_type = aws} = Request) ->
    Request#aws_request{should_retry = false}.

create_secret_payload(SecretName, Opts) ->
    Json = lists:map(
        fun
            ({add_replica_regions, Val}) -> {<<"AddReplicaRegions">>, Val};
            ({client_request_token, Val}) -> {<<"ClientRequestToken">>, Val};
            ({description, Val}) -> {<<"Description">>, Val};
            ({force_overwrite_replica_secret, Val}) -> {<<"ForceOverwriteReplicaSecret">>, Val};
            ({kms_key_id, Val}) -> {<<"KmsKeyId">>, Val};
            ({secret_binary, Val}) -> {<<"SecretBinary">>, Val};
            ({secret_string, Val}) -> {<<"SecretString">>, Val};
            ({tags, Val}) -> {<<"Tags">>, Val};
            (Other) -> Other
        end,
        [{<<"Name">>, SecretName} | Opts]),
    Json.
