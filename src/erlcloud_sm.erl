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
    get_secret_value/2, get_secret_value/3
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
                              | {secret_binary, binary()}
                              | {secret_string, binary()}
                              | {tags, proplist()}.
-type create_secret_options() :: [create_secret_option()].

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

%%%------------------------------------------------------------------------------
%%% Internal Functions
%%%------------------------------------------------------------------------------

create_secret(SecretName, ClientRequestToken, Opts, Config) ->
    Opts1 = [{client_request_token, ClientRequestToken} | Opts],
    Json = create_secret_payload(SecretName, Opts1),
    sm_request(Config, "secretsmanager.CreateSecret", Json).


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
            ({secret_binary, Val}) -> {<<"SecretBinary">>, Val}; %% note AWS accepts either SecretBinary or SecretString
            ({secret_string, Val}) -> {<<"SecretString">>, Val}; %% not both at the same time
            ({tags, Val}) -> {<<"Tags">>, Val};
            (Other) -> Other
        end,
        [{<<"Name">>, SecretName} | Opts]),
    Json.

