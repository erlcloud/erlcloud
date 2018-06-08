-module(erlcloud_aws).

-export([aws_request/5, aws_request/6, aws_request/7, aws_request/8,
         aws_request_xml/5, aws_request_xml/6, aws_request_xml/7, aws_request_xml/8,
         aws_request2/7,
         aws_request_xml2/5, aws_request_xml2/7,
         aws_request4/8, aws_request4/9,
         aws_request_xml4/6, aws_request_xml4/8,
         aws_region_from_host/1,
         aws_request_form/8,
         aws_request_form_raw/8,
         do_aws_request_form_raw/9,
         param_list/2, default_config/0, auto_config/0, auto_config/1,
         default_config_region/2,
         update_config/1,clear_config/1, clear_expired_configs/0,
         service_config/3, service_host/2,
         configure/1, format_timestamp/1,
         http_headers_body/1,
         http_body/1,
         request_to_return/1,
         sign_v4_headers/5,
         sign_v4/8,
         get_service_status/1,
         is_throttling_error_response/1,
         get_timeout/1,
         profile/0, profile/1, profile/2
]).

-include("erlcloud.hrl").
-include("erlcloud_aws.hrl").
-include_lib("lhttpc/include/lhttpc_types.hrl").

-define(ERLCLOUD_RETRY_TIMEOUT, 10000).
-define(GREGORIAN_EPOCH_OFFSET, 62167219200).
-define(DEFAULT_CONTENT_TYPE, "application/x-www-form-urlencoded; charset=utf-8").

%% 
%% environment variables
-define(AWS_ACCESS,  ["AWS_ACCESS_KEY_ID"]).
-define(AWS_SECRET,  ["AWS_SECRET_ACCESS_KEY"]).
-define(AWS_SESSION, ["AWS_SESSION_TOKEN", "AWS_SECURITY_TOKEN"]).
-define(AWS_REGION,  ["AWS_DEFAULT_REGION", "AWS_REGION"]).

%% types
-type http_client_result() :: result(). % from lhttpc_types.hrl
-type http_client_headers() :: [{string(), string()}].
-type httpc_result_ok() :: {http_client_headers(), binary()}.
-type httpc_result_error() :: {http_error, Status :: pos_integer(), StatusLine :: string(), Body :: binary()}
                            | {socket_error, Reason :: term()}.
-export_type([httpc_result_error/0]).
-type httpc_result() :: {ok, httpc_result_ok()} | {error, httpc_result_error()}.
-export_type([httpc_result/0]).

-record(metadata_credentials, {
         access_key_id :: string(),
         secret_access_key :: string(),
         security_token=undefined :: string(),
         expiration_gregorian_seconds :: integer()
}).

-record(role_credentials, {
         access_key_id :: string(),
         secret_access_key :: string(),
         session_token = undefined :: string(),
         expiration_gregorian_seconds :: integer()
}).

-record(profile_options, {
         session_name :: string(),
         session_secs :: 900..3600,
         external_id :: string()
}).

aws_request_xml(Method, Host, Path, Params, #aws_config{} = Config) ->
    Body = aws_request(Method, Host, Path, Params, Config),
    element(1, xmerl_scan:string(binary_to_list(Body))).
aws_request_xml(Method, Host, Path, Params, AccessKeyID, SecretAccessKey) ->
    Body = aws_request(Method, Host, Path, Params, AccessKeyID, SecretAccessKey),
    element(1, xmerl_scan:string(binary_to_list(Body))).
aws_request_xml(Method, Protocol, Host, Port, Path, Params, #aws_config{} = Config) ->
    Body = aws_request(Method, Protocol, Host, Port, Path, Params, Config),
    element(1, xmerl_scan:string(binary_to_list(Body))).
aws_request_xml(Method, Protocol, Host, Port, Path, Params, AccessKeyID, SecretAccessKey) ->
    Body = aws_request(Method, Protocol, Host, Port, Path, Params, AccessKeyID, SecretAccessKey),
    element(1, xmerl_scan:string(binary_to_list(Body))).

aws_request_xml2(Method, Host, Path, Params, #aws_config{} = Config) ->
    aws_request_xml2(Method, undefined, Host, undefined, Path, Params, Config).
aws_request_xml2(Method, Protocol, Host, Port, Path, Params, #aws_config{} = Config) ->
    case aws_request2(Method, Protocol, Host, Port, Path, Params, Config) of
        {ok, Body} ->
            {ok, element(1, xmerl_scan:string(binary_to_list(Body)))};
        {error, Reason} ->
            {error, Reason}
    end.

aws_request_xml4(Method, Host, Path, Params, Service, #aws_config{} = Config) ->
    aws_request_xml4(Method, undefined, Host, undefined, Path, Params, Service, Config).
aws_request_xml4(Method, Protocol, Host, Port, Path, Params, Service, #aws_config{} = Config) ->
    case aws_request4(Method, Protocol, Host, Port, Path, Params, Service, Config) of
        {ok, Body} ->
            {ok, element(1, xmerl_scan:string(binary_to_list(Body)))};
        {error, Reason} ->
            {error, Reason}
    end.

aws_request(Method, Host, Path, Params, #aws_config{} = Config) ->
    aws_request(Method, undefined, Host, undefined, Path, Params, Config).
aws_request(Method, Host, Path, Params, AccessKeyID, SecretAccessKey) ->
    aws_request(Method, undefined, Host, undefined, Path, Params, AccessKeyID, SecretAccessKey).
aws_request(Method, Protocol, Host, Port, Path, Params, #aws_config{} = Config) ->
    case aws_request2(Method, Protocol, Host, Port, Path, Params, Config) of
        {ok, Body} ->
            Body;
        {error, Reason} ->
            erlang:error({aws_error, Reason})
    end.
aws_request(Method, Protocol, Host, Port, Path, Params, AccessKeyID, SecretAccessKey) ->
    aws_request(Method, Protocol, Host, Port, Path, Params,
                #aws_config{access_key_id = AccessKeyID, secret_access_key = SecretAccessKey}).

%% aws_request2 returns {ok, Body} or {error, Reason} instead of throwing as aws_request does
%% This is the preferred pattern for new APIs
aws_request2(Method, Protocol, Host, Port, Path, Params, Config) ->
    case update_config(Config) of
        {ok, Config1} ->
            aws_request2_no_update(Method, Protocol, Host, Port, Path, Params, Config1);
        {error, Reason} ->
            {error, Reason}
    end.

aws_request2_no_update(Method, Protocol, Host, Port, Path, Params, #aws_config{} = Config) ->
    Timestamp = format_timestamp(erlang:universaltime()),
    QParams = lists:sort(
                [{"Timestamp", Timestamp},
                 {"SignatureVersion", "2"},
                 {"SignatureMethod", "HmacSHA1"},
                 {"AWSAccessKeyId", Config#aws_config.access_key_id}|Params] ++
                    case Config#aws_config.security_token of
                        undefined -> [];
                        Token -> [{"SecurityToken", Token}]
                    end),
    {QueryToSign, Headers} = encode_params(QParams, []),
    RequestToSign = [string:to_upper(atom_to_list(Method)), $\n,
                     string:to_lower(Host), $\n, Path, $\n, QueryToSign],
    Signature = base64:encode(erlcloud_util:sha_mac(Config#aws_config.secret_access_key, RequestToSign)),

    Query = [QueryToSign, "&Signature=", erlcloud_http:url_encode(Signature)],

    aws_request_form(Method, Protocol, Host, Port, Path, Query, Headers, Config).

aws_region_from_host(Host) ->
    case string:tokens(Host, ".") of
        %% the aws endpoint can vary depending on the region
        %% we need to account for that:
        %%  us-west-2: s3.us-west-2.amazonaws.com
        %%  cn-north-1 (AWS China): s3.cn-north-1.amazonaws.com.cn
        %% it's assumed that the first element is the aws service (s3, ec2, etc),
        %% the second is the region identifier, the rest is ignored
        %% the exception (of course) is the dynamodb streams and the marketplace which follows a
        %% different format
        ["streams", "dynamodb", Value | _Rest] ->
            Value;
        [Prefix, "marketplace", Value | _Rest]
            when Prefix =:= "metering"; Prefix =:= "entitlement" ->
                Value;
        [_, Value, _, _ | _Rest] ->
            Value;
        _ ->
            default_config_get(?AWS_REGION, aws_region, "us-east-1")
    end.

aws_request4(Method, Protocol, Host, Port, Path, Params, Service, Config) ->
    aws_request4(Method, Protocol, Host, Port, Path, Params, Service, [], Config).

% If `content-type` is specified in the Headers,
% this is developer's responsibility to encode params properly
aws_request4(Method, Protocol, Host, Port, Path, Params, Service, Headers, Config) ->
    case update_config(Config) of
        {ok, Config1} ->
            aws_request4_no_update(Method, Protocol, Host, Port, Path, Params,
                                   Service, Headers, Config1);
        {error, Reason} ->
            {error, Reason}
    end.

aws_request4_no_update(Method, Protocol, Host, Port, Path, Params, Service,
                       Headers, #aws_config{aws_region = AwsRegion} = Config) ->
    {Query, RequestHeaders} = encode_params(Params, Headers),
    Uri = erlcloud_http:url_encode_loose(Path),
    Region = case AwsRegion of
      undefined -> aws_region_from_host(Host);
      _ -> AwsRegion
    end,
    SignedHeaders = case Method of
        M when M =:= get orelse M =:= head orelse M =:= delete ->
            sign_v4(M, Uri, Config, [{"host", Host}],
                    [], Region, Service, Params);
        _ ->
            sign_v4(Method, Uri, Config,
                    [{"host", Host}], list_to_binary(Query),
                    Region, Service, [])
    end,
    aws_request_form(Method, Protocol, Host, Port, Path, Query,
                     SignedHeaders ++ RequestHeaders, Config).


-spec aws_request_form(Method :: atom(), Protocol :: undefined | string(), Host :: string(),
                        Port :: undefined | integer() | string(), Path :: string(), Form :: [string()],
                        Headers :: list(), Config :: aws_config()) -> {ok, Body :: binary()} | {error, httpc_result_error()}.
aws_request_form(Method, Protocol, Host, Port, Path, Form, Headers, Config) ->
    RequestHeaders = case proplists:is_defined("content-type", Headers) of
      false -> [{"content-type", ?DEFAULT_CONTENT_TYPE} | Headers];
      true -> Headers
    end,
    Scheme = case Protocol of
        undefined -> "https://";
        _ -> [Protocol, "://"]
    end,
    aws_request_form_raw(Method, Scheme, Host, Port, Path, list_to_binary(Form), RequestHeaders, Config).

-spec aws_request_form_raw(Method :: atom(), Scheme :: string() | [string()],
                        Host :: string(), Port :: undefined | integer() | string(),
                        Path :: string(), Form :: iodata(), Headers :: list(),
                        Config :: aws_config()) -> {ok, Body :: binary()} | {error, httpc_result_error()}.
aws_request_form_raw(Method, Scheme, Host, Port, Path, Form, Headers, Config) ->
    do_aws_request_form_raw(Method, Scheme, Host, Port, Path, Form, Headers, Config, false).

do_aws_request_form_raw(Method, Scheme, Host, Port, Path, Form, Headers, Config, ShowRespHeaders) ->
    URL = case Port of
        undefined -> [Scheme, Host, Path];
        _ -> [Scheme, Host, $:, port_to_str(Port), Path]
    end,

    ResultFun =
        fun(#aws_request{response_type = ok} = Request) ->
                Request;
           (#aws_request{response_type = error,
                         error_type = aws,
                         response_status = Status} = Request) when
                %% Retry conflicting operations 409,Conflict and 500s.
                    Status == 409; Status >= 500 ->
                Request#aws_request{should_retry = true};
           (#aws_request{response_type = error,
                         error_type = aws,
                         response_status = Status} = Request) when
                %% Retry for 400, Bad Request is needed due to Amazon
                %% returns it in case of throttling
                    Status == 400; Status == 429 ->
                ShouldRetry = is_throttling_error_response(Request),
                Request#aws_request{should_retry = ShouldRetry};
           (#aws_request{response_type = error} = Request) ->
                Request#aws_request{should_retry = false}
        end,

    %% Note: httpc MUST be used with {timeout, timeout()} option
    %%       Many timeout related failures is observed at prod env
    %%       when library is used in 24/7 manner
    Response =
        case Method of
            M when M =:= get orelse M =:= head orelse M =:= delete ->
                Req = lists:flatten([URL, $?, Form]),
                AwsRequest = #aws_request{uri = Req,
                                          method = M,
                                          request_headers = Headers,
                                          request_body = <<>>},
                erlcloud_retry:request(Config, AwsRequest, ResultFun);
            _ ->
                AwsRequest = #aws_request{uri = lists:flatten(URL),
                                          method = Method,
                                          request_headers = Headers,
                                          request_body = Form},
                erlcloud_retry:request(Config, AwsRequest, ResultFun)
        end,

    case request_to_return(Response) of
        {ok, _} = SuccessRes ->
            show_headers(ShowRespHeaders, SuccessRes);
        {error, {Error, StatusCode, StatusLine, Body, _Headers}} ->
            {error, {Error, StatusCode, StatusLine, Body}};
        {error, Reason} ->
            {error, Reason}
    end.

show_headers(true, {ok, {Headers, Body}}) ->
    {ok, Headers, Body};
show_headers(false, {ok, {_, Body}}) ->
    {ok, Body}.

param_list([], _Key) -> [];
param_list(Values, Key) when is_tuple(Key) ->
    Seq = lists:seq(1, size(Key)),
    lists:flatten(
      [[{lists:append([element(J, Key), ".", integer_to_list(I)]),
         element(J, Value)} || J <- Seq] ||
          {I, Value} <- lists:zip(lists:seq(1, length(Values)), Values)]
     );
param_list([[{_, _}|_]|_] = Values, Key) ->
    lists:flatten(
      [[{lists:flatten([Key, $., integer_to_list(I), $., SubKey]),
         value_to_string(Value)} || {SubKey, Value} <- SValues] ||
          {I, SValues} <- lists:zip(lists:seq(1, length(Values)), Values)]
     );
param_list(Values, Key) ->
    [{lists:flatten([Key, $., integer_to_list(I)]), Value} ||
        {I, Value} <- lists:zip(lists:seq(1, length(Values)), Values)].

value_to_string(Float) when is_float(Float) -> float_to_list(Float);
value_to_string(Integer) when is_integer(Integer) -> integer_to_list(Integer);
value_to_string(Atom) when is_atom(Atom) -> atom_to_list(Atom);
value_to_string(Binary) when is_binary(Binary) -> Binary;
value_to_string(String) when is_list(String) -> String;
value_to_string({{_Yr, _Mo, _Da}, {_Hr, _Min, _Sec}} = Timestamp) -> format_timestamp(Timestamp).

format_timestamp({{Yr, Mo, Da}, {H, M, S}}) ->
    lists:flatten(
      io_lib:format("~4.10.0b-~2.10.0b-~2.10.0bT~2.10.0b:~2.10.0b:~2.10.0bZ",
                    [Yr, Mo, Da, H, M, S])).

encode_params(Params, Headers) ->
  LowerCaseHeaders = lists:map(fun({K, V}) -> {string:to_lower(K), V} end, Headers),
  case proplists:get_value("content-type", LowerCaseHeaders) of
    undefined -> {erlcloud_http:make_query_string(Params),
                  [{"content-type", ?DEFAULT_CONTENT_TYPE} | LowerCaseHeaders]};
    ?DEFAULT_CONTENT_TYPE -> {erlcloud_http:make_query_string(Params), LowerCaseHeaders};
    _ContentType -> {Params, LowerCaseHeaders}
  end.

%%%---------------------------------------------------------------------------
-spec default_config() -> aws_config().
%%%---------------------------------------------------------------------------
%% @doc Generate a default config
%%
%% This function will generate a default configuration, using credentials
%% available in the environment, if available.  If no credentials are
%% available then this will just return a default <code>#aws_config{}</code>
%% record.
%% The credentials are; Id, Key, Token, and Region. For each credential we
%% will first look for the environment variables "AWS_ACCESS_KEY_ID",
%% "AWS_SECRET_ACCESS_KEY", "AWS_SESSION_TOKEN", and "AWS_REGION",
%% respectively. If no value is found, we look at the erlcloud application
%% environment for the keys aws_access_key_id, aws_secret_access_key,
%% aws_security_token, and aws_region, respectively. If still no value is
%% found, it will default to 'undefined'.
%% Id, Key, and Token will be set in #aws_config{}, in the fields
%% access_key_id, secret_access_key, and security_token, respectively.
%% If Region is set, we will attempt to set the appropriate URL for this
%% region for each service.
%% Default values may be tweaked if there is `aws_config' proplist is present
%% in application environment. Keys of this proplist must have same names
%% as `#aws_config{}' fields. All the values defined there would override
%% appropriate fields in `#aws_config{}' record produced byt this function.

%% check the cache
default_config() ->
    case get(aws_config) of
        undefined -> default_config_wrap();
        Config -> Config
    end.

default_config_wrap() ->
    Id = default_config_get(?AWS_ACCESS, aws_access_key_id),
    Key = default_config_get(?AWS_SECRET, aws_secret_access_key),
    Token = default_config_get(?AWS_SESSION, aws_security_token),
    Region = default_config_get(?AWS_REGION, aws_region),
    default_config_override(
        default_config_region(default_config_assert(Id, Key, Token), Region)
    ).

%% try to get config value from OS env, failing that try app env, else
%% return undefined
default_config_get(OsVar, EnvVar) ->
    default_config_get(OsVar, EnvVar, undefined).

default_config_get(OsVar, EnvVar, Default) ->
    case {os_getenv(OsVar), application:get_env(erlcloud, EnvVar, Default)} of
        {OsVal,  _} when OsVal /= false -> OsVal;
        {_, EnvVal} -> EnvVal
    end.

os_getenv([Head | Tail]) ->
    case os:getenv(Head) of
        false -> os_getenv(Tail);
        Value -> Value
    end;
os_getenv([]) ->
    false.


default_config_assert(undefined, _Key, _Token) -> #aws_config{};
default_config_assert(_Id, undefined, _Token) -> #aws_config{};
default_config_assert(Id, Key, Token) ->
    #aws_config{access_key_id = Id,
                secret_access_key = Key,
                security_token = Token}.


default_config_override(AwsConfig) ->
    case application:get_env(erlcloud, aws_config) of
        undefined ->
            AwsConfig;
        {ok, DeltaCfgPL} ->
            map_to_record(
                maps:merge(record_to_map(AwsConfig), maps:from_list(DeltaCfgPL)),
                aws_config
            )
    end.


record_to_map(R) when is_record(R, aws_config) ->
    [aws_config | Vs] = tuple_to_list(R),
    Ks = record_info(fields, aws_config),
    maps:from_list(lists:zip(Ks, Vs)).


map_to_record(M, aws_config) ->
    Vs = lists:map(
        fun(K) ->
            maps:get(K, M)
        end, record_info(fields, aws_config)
    ),
    list_to_tuple([aws_config | Vs]).


%% call service_config/3 with our Region for all services
default_config_region(undefined, _) ->
    undefined;
default_config_region(AwsConfig, undefined) ->
    AwsConfig;
default_config_region(AwsConfig, Region) when is_record(AwsConfig, aws_config) ->
    ConfF = fun(Service, C0) -> service_config(Service, Region, C0) end,
    lists:foldl(ConfF, AwsConfig, default_config_region_services()).

%% horrible hack to get a list of service names, assuming that if a key in
%% #aws_config{} ends with "_host", the prefix is a service (except
%% s3_bucket_after_host).
default_config_region_services() ->
    Fields = record_info(fields, aws_config),
    Ts = [lists:reverse(string:tokens(atom_to_list(F), "_")) || F <- Fields],
    Ss = [list_to_atom(string:join(lists:reverse(R), "_")) || ["host"|R] <- Ts],
    [S || S <- Ss, S =/= s3_bucket_after].

%%%---------------------------------------------------------------------------
-spec auto_config() -> {ok, aws_config()} | undefined.
%%%---------------------------------------------------------------------------
%% @doc Generate config using the best available credentials
%%
%% This function will generate a valid <code>#aws_config{}</code> based on
%% the best available credentials source in the current environment.  The
%% following sources of credentials will be used, in order:
%%
%% <ol>
%%   <li>Environment Variables
%%     <p>An Id, Key and optionally Token, will be sourced from the
%%     environment variables <code>AWS_ACCESS_KEY_ID</code>,
%%     <code>AWS_SECRET_ACCESS_KEY</code> and
%%     <code>AWS_SECURITY_TOKEN</code> respectively.  Both the Id and Key
%%     values must be non-empty for this form of credentials to be considered
%%     valid.</p>
%%   </li>
%%
%%   <li>User Profile
%%     <p>The default user profile credentials will be sourced using the
%%     {@link profile/0} function, if available for the current user.</p>
%%   </li>
%%
%%   <li>ECS Task Role
%%     <p>The credentials available via ECS Task Role will be sourced, if
%%     available.</p>
%%   </li>
%%
%%   <li>Host Metadata
%%     <p>The credentials available via host metadata will be sourced, if
%%     available.</p>
%%   </li>
%% </ol>
%%
%% Alike {@link default_config/0} this function will also check <code>AWS_REGION</code> variable and
%% will try to set the appropriate URL for this region for each service.
%%
%% If none of these credential sources are available, this function will
%% return <code>undefined</code>.
%%
auto_config() ->
    auto_config( [] ).


%%%---------------------------------------------------------------------------
-spec auto_config( ProfileOptions :: [{profile, atom()} | profile_option()]) ->
                         {ok, aws_config()} | undefined.
%%%---------------------------------------------------------------------------
%% @doc Generate config using the best available credentials
%%
%% This function works the same as {@link auto_config/0}, but if credentials
%% are developed from <em>User Profile</em> as the source, the
%% <code>Options</code> parameter provided will be used to control the
%% behavior.  The credential profile name choosen can be controlled by
%% providing <code>{profile, atom()}</code> as part of the options, and if
%% not specified the <code>default</code> profile will be used.
%%
%% @see profile/2
%%
auto_config( ProfileOptions ) ->
    {ok, Cfg } =
        case config_env() of
            {ok, _Config} = Result ->
                Result;
            {error, _} ->
                auto_config_profile( ProfileOptions )
        end,
    % Since default_config_region/2 can return undefined we cannot blindly wrap
    % the result in a {ok, ...} tuple.
    case default_config_region(Cfg, default_config_get(?AWS_REGION, aws_region)) of
        #aws_config{} = Config ->
            {ok, Config};
        Other ->
            Other
    end.

auto_config_profile( ProfileOptions ) ->
    Profile = proplists:get_value( profile, ProfileOptions, default ),
    case profile( Profile, ProfileOptions ) of
        {ok, _Config} = Result -> Result;
        {error, _} -> auto_config_task_metadata()
    end.

auto_config_task_metadata() ->
    case config_metadata(task_credentials) of
        {ok, _Config} = Result -> Result;
        {error, _} -> auto_config_metadata()
    end.

auto_config_metadata() ->
    case config_metadata(instance_metadata) of
        {ok, _Config} = Result -> Result;
        {error, _} -> {ok, undefined}
    end.


config_env() ->
    case {os_getenv(?AWS_ACCESS), os_getenv(?AWS_SECRET),
          os_getenv(?AWS_SESSION)} of
        {KeyId, Secret, T} when is_list(KeyId), is_list(Secret) ->
            Token = if is_list(T) -> T; true -> undefined end,
            Config = #aws_config{access_key_id = KeyId,
                                 secret_access_key = Secret,
                                 security_token = Token},
            {ok, Config};
        _ -> {error, environment_config_unavailable}
    end.

-spec config_metadata(task_credentials | instance_metadata) -> {ok, #metadata_credentials{}} | {error, metadata_not_available | container_credentials_unavailable | httpc_result_error()}.
config_metadata(Source) ->
    Config = #aws_config{},
    case get_metadata_credentials( Source, Config ) of
        {ok, #metadata_credentials{
                access_key_id = Id,
                secret_access_key = Secret,
                security_token = Token,
                expiration_gregorian_seconds = GregorianSecs}} ->
            EpochTimeout = GregorianSecs - ?GREGORIAN_EPOCH_OFFSET,
            {ok, Config#aws_config {
                   access_key_id = Id, secret_access_key = Secret,
                   security_token = Token, expiration = EpochTimeout }};
        {error, _Reason} = Error -> Error
    end.

-spec update_config(aws_config()) -> {ok, aws_config()} | {error, metadata_not_available | container_credentials_unavailable | httpc_result_error()}.
update_config(#aws_config{assume_role =
                          #aws_assume_role{role_arn = RoleArn}} = Config)
    when RoleArn /= undefined ->
    %% The assume role options are defined lets try to assume a role
    {ok, Credentials} = get_role_credentials(Config),
    {ok, Config#aws_config {
        access_key_id = Credentials#role_credentials.access_key_id,
        secret_access_key = Credentials#role_credentials.secret_access_key,
        security_token = Credentials#role_credentials.session_token}};
update_config(#aws_config{access_key_id = KeyId} = Config)
  when is_list(KeyId), KeyId /= [] ->
    %% In order to support caching of the aws_config, we could store the expiration_time
    %% and check it here. If it is about to expire (within 5 minutes is what boto uses)
    %% then we should get the new config.
    {ok, Config};
update_config(#aws_config{} = Config) ->
    %% AccessKey is not set. Try to read from ECS and than metadata.
    case get_metadata_credentials(task_credentials, Config) of
        {error, _} ->
            case get_metadata_credentials(instance_metadata, Config) of
                {error, _} = Error -> Error;
                {ok, Credentials} ->
                    {ok, Config#aws_config {
                           access_key_id = Credentials#metadata_credentials.access_key_id,
                           secret_access_key = Credentials#metadata_credentials.secret_access_key,
                           security_token = Credentials#metadata_credentials.security_token}}
            end;
        {ok, Credentials} ->
            {ok, Config#aws_config {
                   access_key_id = Credentials#metadata_credentials.access_key_id,
                   secret_access_key = Credentials#metadata_credentials.secret_access_key,
                   security_token = Credentials#metadata_credentials.security_token}}
    end.

-spec clear_config(aws_config()) -> ok.
clear_config(#aws_config{assume_role = #aws_assume_role{role_arn = Arn, external_id = ExtId}}) ->
    application:unset_env(erlcloud, {role_credentials, Arn, ExtId}).

-spec clear_expired_configs() -> ok.
clear_expired_configs() ->
    Env = application:get_all_env(erlcloud),
    Now = calendar:datetime_to_gregorian_seconds(calendar:universal_time()),
    [application:unset_env(erlcloud, {role_credentials, Arn, ExtId}) ||
            {{role_credentials, Arn, ExtId},
              #role_credentials{expiration_gregorian_seconds = Ts}} <- Env,
        Ts < Now],
    ok.


%%%---------------------------------------------------------------------------
-spec service_config( Service :: atom() | string() | binary(),
                      Region :: atom() | string() | binary(),
                      Config :: #aws_config{} ) -> #aws_config{}.
%%%---------------------------------------------------------------------------
%% @doc Generate config updated to work with specified AWS service and region
%%
%% This function will generate a new configuration, based on the one
%% provided, and configured to access the specified AWS service in the
%% specified region.  If called for a service without a region based
%% endpoint, the config provided will be returned unaltered.
%%
%% If an invalid service name is provided, then this will throw an error,
%% presuming that this is just a coding error.  This behavior allows the
%% chaining of calls to this interface to allow concise configuraiton of a
%% config for multiple services.
%%
service_config( Service, Region, Config ) when is_atom(Service) ->
    service_config( atom_to_binary(Service, latin1), Region, Config );
service_config( Service, Region, Config ) when is_list(Service) ->
    service_config( list_to_binary(Service), Region, Config );
service_config( Service, Region, Config ) when is_atom(Region) ->
    service_config( Service, atom_to_binary(Region, latin1), Config );
service_config( Service, Region, Config ) when is_list(Region) ->
    service_config( Service, list_to_binary(Region), Config );
service_config( <<"as">>, Region, Config ) ->
    service_config( <<"autoscaling">>, Region, Config );
service_config( <<"autoscaling">> = Service, Region, Config ) ->
    Host = service_host( Service, Region ),
    Config#aws_config{ as_host = Host , autoscaling_host = Host};
service_config( <<"cloudformation">> = Service, Region, Config ) ->
    Host = service_host( Service, Region ),
    Config#aws_config{ cloudformation_host = Host };
service_config( <<"cfn">>, Region, Config ) ->
    service_config( <<"cloudformation">>, Region, Config );
service_config( <<"cloudfront">>, _Region, Config ) -> Config;
service_config( <<"cloudsearch">> = Service, Region, Config ) ->
    Host = service_host( Service, Region ),
    Config#aws_config{ cloudsearch_host = Host };
service_config( <<"cloudtrail">> = Service, Region, Config ) ->
    Host = service_host( Service, Region ),
    Config#aws_config{ cloudtrail_host = Host };
service_config( <<"dynamodb">> = Service, Region, Config ) ->
    Host = service_host( Service, Region ),
    Config#aws_config{ ddb_host = Host };
service_config( <<"directconnect">> = Service, Region, Config ) ->
    Host = service_host( Service, Region ),
    Config#aws_config{ directconnect_host = Host };
service_config( <<"ddb">>, Region, Config ) ->
    service_config( <<"dynamodb">>, Region, Config );
service_config( <<"ddb_streams">>, Region, Config ) ->
    service_config( <<"streams.dynamodb">>, Region, Config );
service_config( <<"streams.dynamodb">> = Service, Region, Config ) ->
    Host = service_host( Service, Region ),
    Config#aws_config{ ddb_streams_host = Host };
service_config( <<"ec2">> = Service, Region, Config ) ->
    Host = service_host( Service, Region ),
    Config#aws_config{ ec2_host = Host };
service_config( <<"ecs">> = Service, Region, Config ) ->
    Host = service_host( Service, Region ),
    Config#aws_config{ ecs_host = Host };
service_config( <<"elasticloadbalancing">> = Service, Region, Config ) ->
    Host = service_host( Service, Region ),
    Config#aws_config{ elb_host = Host };
service_config( <<"elb">>, Region, Config ) ->
    service_config( <<"elasticloadbalancing">>, Region, Config );
service_config( <<"elasticmapreduce">> = Service, Region, Config ) ->
    Host = service_host( Service, Region ),
    Config#aws_config{ emr_host = Host };
service_config( <<"emr">>, Region, Config ) ->
    service_config( <<"elasticmapreduce">>, Region, Config );
service_config( <<"iam">> = Service, <<"cn-north-1">> = Region, Config ) ->
    Host = service_host( Service, Region ),
    Config#aws_config{ iam_host = Host };
service_config( <<"iam">>, _Region, Config ) -> Config;
service_config( <<"inspector">> = Service, Region, Config ) ->
    Host = service_host( Service, Region ),
    Config#aws_config{ inspector_host = Host };
service_config( <<"kinesis">> = Service, Region, Config ) ->
    Host = service_host( Service, Region ),
    Config#aws_config{ kinesis_host = Host };
service_config( <<"kms">> = Service, Region, Config ) ->
    Host = service_host( Service, Region ),
    Config#aws_config{ kms_host = Host };
service_config( <<"lambda">> = Service, Region, Config ) ->
    Host = service_host( Service, Region ),
    Config#aws_config{ lambda_host = Host };
service_config( <<"mon">>, Region, Config ) ->
    service_config( <<"monitoring">>, Region, Config );
service_config( <<"monitoring">> = Service, Region, Config ) ->
    Host = service_host( Service, Region ),
    Config#aws_config{ mon_host = Host };
service_config( <<"mturk">>, Region, Config ) ->
    service_config( <<"mechanicalturk">>, Region, Config );
service_config( <<"mechanicalturk">>, _Region, Config ) -> Config;
service_config( <<"mes">>, Region, Config ) ->
    service_config( <<"entitlement.marketplace">>, Region, Config );
service_config( <<"entitlement.marketplace">> = Service, Region, Config ) ->
    Host = service_host( Service, Region ),
    Config#aws_config{ mes_host = Host };
service_config( <<"mms">>, Region, Config ) ->
    service_config( <<"metering.marketplace">>, Region, Config );
service_config( <<"metering.marketplace">> = Service, Region, Config ) ->
    Host = service_host( Service, Region ),
    Config#aws_config{ mms_host = Host };
service_config( <<"rds">> = Service, Region, Config ) ->
    Host = service_host( Service, Region ),
    Config#aws_config{ rds_host = Host };
service_config( <<"redshift">> = Service, Region, Config ) ->
    Host = service_host( Service, Region ),
    Config#aws_config{ redshift_host = Host };
service_config( <<"route53">>, _Region, Config ) -> Config;
service_config( <<"s3">> = Service, Region, Config ) ->
    Host = service_host( Service, Region ),
    Config#aws_config{ s3_host = Host };
service_config( <<"sdb">> = Service, Region, Config ) ->
    Host = service_host( Service, Region ),
    Config#aws_config{ sdb_host = Host };
service_config( <<"ses">>, Region, Config ) ->
    Host = service_host( <<"email">>, Region ),
    Config#aws_config{ ses_host = Host };
service_config( <<"sns">> = Service, Region, Config ) ->
    Host = service_host( Service, Region ),
    Config#aws_config{ sns_host = Host };
service_config( <<"sqs">> = Service, Region, Config ) ->
    Host = service_host( Service, Region ),
    Config#aws_config{ sqs_host = Host };
service_config( <<"sts">> = Service, Region, Config ) ->
    Host = service_host( Service, Region ),
    Config#aws_config{ sts_host = Host };
service_config( <<"glue">> = Service, Region, Config ) ->
  Host = service_host( Service, Region ),
  Config#aws_config{ glue_host = Host };
service_config( <<"athena">> = Service, Region, Config ) ->
  Host = service_host( Service, Region ),
  Config#aws_config{ athena_host = Host };
service_config( <<"config">> = Service, Region, Config ) ->
    Host = service_host( Service, Region ),
    Config#aws_config{ config_host = Host };
service_config(<<"cloudwatch_logs">>, Region, Config)->
    Host = service_host(<<"logs">>, Region),
    Config#aws_config{cloudwatch_logs_host = Host};
service_config( <<"waf">>, _Region, Config ) -> Config;
service_config( <<"guardduty">> = Service, Region, Config ) ->
    Host = service_host( Service, Region ),
    Config#aws_config{guardduty_host = Host};
service_config( <<"cur">>, Region, Config ) ->
    Host = service_host(<<"cur">>, Region),
    Config#aws_config{cur_host = Host}.

%%%---------------------------------------------------------------------------
-spec service_host( Service :: binary(),
                    Region :: binary() ) -> string().
%%%---------------------------------------------------------------------------
%% @hidden
%% @doc Host name for the specified AWS service and region
%%
%% This function handles the special and general cases of service host
%% names.
%%
service_host( <<"s3">>, <<"us-east-1">> ) -> "s3-external-1.amazonaws.com";
service_host( <<"s3">>, <<"cn-north-1">> ) -> "s3.cn-north-1.amazonaws.com.cn";
service_host( <<"s3">>, <<"us-gov-west-1">> ) ->
    "s3-fips-us-gov-west-1.amazonaws.com";
service_host( <<"s3">>, Region ) ->
    binary_to_list( <<"s3-", Region/binary, ".amazonaws.com">> );
service_host( <<"sdb">>, <<"us-east-1">> ) -> "sdb.amazonaws.com";
service_host( Service, Region ) when is_binary(Service) ->
    binary_to_list( <<Service/binary, $., Region/binary, ".amazonaws.com">> ).

-spec configure(aws_config()) -> {ok, aws_config()}.

configure(#aws_config{} = Config) ->
    put(aws_config, Config),
    {ok, default_config()}.

-spec get_metadata_credentials(instance_metadata | task_credentials, aws_config()) -> {ok, #metadata_credentials{}} | {error, metadata_not_available | container_credentials_unavailable | httpc_result_error()}.
get_metadata_credentials(Source, Config) ->
    %% See if we have cached credentials
    Fun = case Source of
        instance_metadata -> fun get_credentials_from_metadata/1;
        task_credentials -> fun get_credentials_from_task_metadata/1
    end,
    case application:get_env(erlcloud, metadata_credentials) of
        {ok, #metadata_credentials{expiration_gregorian_seconds = Expiration} = Credentials} ->
            Now = calendar:datetime_to_gregorian_seconds(calendar:universal_time()),
            %% Get new credentials if these will expire in less than 5 minutes
            case Expiration - Now < 300 of
                true -> Fun(Config);
                false -> {ok, Credentials}
            end;
        undefined ->
            Fun(Config)
    end.

timestamp_to_gregorian_seconds(undefined) -> undefined;
timestamp_to_gregorian_seconds(Timestamp) ->
    {ok, [Yr, Mo, Da, H, M, S], []} = io_lib:fread("~d-~d-~dT~d:~d:~dZ", binary_to_list(Timestamp)),
    calendar:datetime_to_gregorian_seconds({{Yr, Mo, Da}, {H, M, S}}).

-spec get_credentials_from_metadata(aws_config())
                                   -> {ok, #metadata_credentials{}} | {error, metadata_not_available | httpc_result_error()}.
get_credentials_from_metadata(Config) ->
    %% TODO this function should retry on errors getting credentials
    %% First get the list of roles
    case erlcloud_ec2_meta:get_instance_metadata("iam/security-credentials/", Config) of
        {error, Reason} ->
            {error, Reason};
        {ok, Body} ->
            %% Always use the first role
            [Role | _] = binary:split(Body, <<$\n>>),
            case erlcloud_ec2_meta:get_instance_metadata("iam/security-credentials/" ++ binary_to_list(Role), Config) of
                {error, Reason} ->
                    {error, Reason};
                {ok, Json} ->
                    Creds = jsx:decode(Json),
                    get_credentials_from_metadata_xform( Creds )
            end
    end.

-spec get_credentials_from_task_metadata(aws_config())
                                   -> {ok, #metadata_credentials{}} | {error, metadata_not_available | container_credentials_unavailable | httpc_result_error()}.
get_credentials_from_task_metadata(Config) ->
    %% TODO this function should retry on errors getting credentials
    case erlcloud_ecs_container_credentials:get_container_credentials(Config) of
        {error, Reason} ->
            {error, Reason};
        {ok, Json} ->
            Creds = jsx:decode(Json),
            get_credentials_from_metadata_xform( Creds )
    end.

get_credentials_from_metadata_xform( Creds ) ->
    case {prop_to_list_defined(<<"AccessKeyId">>, Creds),
          prop_to_list_defined(<<"SecretAccessKey">>, Creds),
          prop_to_list_defined(<<"Token">>, Creds),
          timestamp_to_gregorian_seconds(
            proplists:get_value(<<"Expiration">>, Creds))} of
        {Id, Key, Token, GregorianExpire} when is_list(Id), is_list(Key),
                                               is_list(Token),
                                               is_integer(GregorianExpire) ->
            Record = #metadata_credentials{
                        access_key_id = Id, secret_access_key = Key,
                        security_token = Token,
                        expiration_gregorian_seconds = GregorianExpire },
            application:set_env(erlcloud, metadata_credentials, Record),
            {ok, Record};
        _ -> {error, metadata_not_available}
    end.

prop_to_list_defined( Name, Props ) ->
    case proplists:get_value( Name, Props ) of
        undefined -> undefined;
        Value when is_binary(Value) -> binary_to_list(Value)
    end.


-spec get_role_credentials(aws_config()) -> {ok, #role_credentials{}}.
get_role_credentials(#aws_config{assume_role = AssumeRole} = Config) ->
    case application:get_env(erlcloud,
                             {role_credentials,
                              AssumeRole#aws_assume_role.role_arn,
                              AssumeRole#aws_assume_role.external_id}) of
        {ok, #role_credentials{expiration_gregorian_seconds = Expiration} = Credentials} ->
            Now = calendar:datetime_to_gregorian_seconds(calendar:universal_time()),
            %% Get new credentials if these will expire in less than 5 minutes
            case Expiration - Now < 300 of
                true -> get_credentials_from_role(Config);
                false -> {ok, Credentials}
            end;
        undefined ->
            get_credentials_from_role(Config)
    end.

-spec get_credentials_from_role(aws_config()) -> {ok, #role_credentials{}}.
get_credentials_from_role(#aws_config{assume_role = AssumeRole} = Config) ->
    %% We have to reset the assume role to make sure we do not
    %% enter in a infinite loop because erlcloud_sts:assume_role also calls
    %% update_config deep inside when makes the request
    {#aws_config{}=_NewConfig, Creds} =
    erlcloud_sts:assume_role(Config#aws_config{assume_role = #aws_assume_role{}},
                             AssumeRole#aws_assume_role.role_arn,
                             AssumeRole#aws_assume_role.session_name,
                             AssumeRole#aws_assume_role.duration_secs,
                             AssumeRole#aws_assume_role.external_id),
    ExpireAt = calendar:datetime_to_gregorian_seconds(
        proplists:get_value(expiration, Creds)),
    Record = #role_credentials{
        access_key_id =  proplists:get_value(access_key_id, Creds),
        secret_access_key = proplists:get_value(secret_access_key, Creds),
        session_token = proplists:get_value(session_token, Creds),
        expiration_gregorian_seconds = ExpireAt},
    application:set_env(erlcloud,
                        {role_credentials,
                         AssumeRole#aws_assume_role.role_arn,
                         AssumeRole#aws_assume_role.external_id},
                        Record),
    {ok, Record}.

port_to_str(Port) when is_integer(Port) ->
    integer_to_list(Port);
port_to_str(Port) when is_list(Port) ->
    Port.

-spec http_body(http_client_result())
               -> {ok, Body :: binary()} | {error, httpc_result_error()}.
%% Extract the body and do error handling on the return of a httpc:request call.
http_body(Return) ->
    case http_headers_body(Return) of
        {ok, {_, Body}} ->
            {ok, Body};
        {error, Reason} ->
            {error, Reason}
    end.

-spec http_headers_body(http_client_result())
                       -> httpc_result().
%% Extract the headers and body and do error handling on the return of a httpc:request call.
http_headers_body({ok, {{OKStatus, _StatusLine}, Headers, Body}})
  when OKStatus >= 200, OKStatus =< 299 ->
    {ok, {Headers, Body}};
http_headers_body({ok, {{Status, StatusLine}, _Headers, Body}}) ->
    {error, {http_error, Status, StatusLine, Body}};
http_headers_body({error, Reason}) ->
    {error, {socket_error, Reason}}.

get_timeout(#aws_config{timeout = undefined}) ->
    ?ERLCLOUD_RETRY_TIMEOUT;
get_timeout(#aws_config{timeout = Timeout}) ->
    Timeout.

%% Convert an aws_request record to return value as returned by http_headers_body
request_to_return(#aws_request{response_type = ok,
                               response_headers = Headers,
                               response_body = Body}) ->
    {ok, {[ {string:to_lower(H), V} || {H, V} <- Headers ], Body}};
request_to_return(#aws_request{response_type = error,
                               error_type = httpc,
                               httpc_error_reason = Reason}) ->
    {error, {socket_error, Reason}};
request_to_return(#aws_request{response_type = error,
                               error_type = aws,
                               response_status = Status,
                               response_status_line = StatusLine,
                               response_body = Body,
                               response_headers = Headers}) ->
    {error, {http_error, Status, StatusLine, Body, Headers}}.

%% http://docs.aws.amazon.com/general/latest/gr/signature-version-4.html
-spec sign_v4_headers(aws_config(), headers(), string() | binary(), string(), string()) -> headers().
sign_v4_headers(Config, Headers, Payload, Region, Service) ->
    sign_v4(post, "/", Config, Headers, Payload, Region, Service, []).

-spec sign_v4(atom(), list(), aws_config(), headers(), string() | binary(), string(), string(), list()) -> headers().
sign_v4(Method, Uri, Config, Headers, Payload, Region, Service, QueryParams) ->
    Date = iso_8601_basic_time(),
    {PayloadHash, Headers1} =
        sign_v4_content_sha256_header( [{"x-amz-date", Date} | Headers], Payload ),
    Headers2 = case Config#aws_config.security_token of
                   undefined -> Headers1;
                   Token -> [{"x-amz-security-token", Token} | Headers1]
               end,
    {Request, SignedHeaders} = canonical_request(Method, Uri, QueryParams, Headers2, PayloadHash),
    CredentialScope = credential_scope(Date, Region, Service),
    ToSign = to_sign(Date, CredentialScope, Request),
    SigningKey = signing_key(Config, Date, Region, Service),
    Signature = base16(erlcloud_util:sha256_mac( SigningKey, ToSign)),
    Authorization = authorization(Config, CredentialScope, SignedHeaders, Signature),
    [{"Authorization", lists:flatten(Authorization)} | Headers2].

iso_8601_basic_time() ->
    {{Year,Month,Day},{Hour,Min,Sec}} = calendar:now_to_universal_time(os:timestamp()),
    lists:flatten(io_lib:format(
                    "~4.10.0B~2.10.0B~2.10.0BT~2.10.0B~2.10.0B~2.10.0BZ",
                    [Year, Month, Day, Hour, Min, Sec])).

canonical_request(Method, CanonicalURI, QParams, Headers, PayloadHash) ->
    {CanonicalHeaders, SignedHeaders} = canonical_headers(Headers),
    CanonicalQueryString = canonical_query_string(QParams),
    {[string:to_upper(atom_to_list(Method)), $\n,
      CanonicalURI, $\n,
      CanonicalQueryString, $\n,
      CanonicalHeaders, $\n,
      SignedHeaders, $\n,
      PayloadHash],
     SignedHeaders}.

sign_v4_content_sha256_header( Headers, Payload ) ->
    case proplists:get_value( "x-amz-content-sha256", Headers ) of
        undefined ->
            PayloadHash = hash_encode(Payload),
            NewHeaders = [{"x-amz-content-sha256", PayloadHash} | Headers],
            {PayloadHash, NewHeaders};
        PayloadHash -> {PayloadHash, Headers}
    end.

canonical_headers(Headers) ->
    Normalized = [{string:to_lower(Name), trimall(Value)} || {Name, Value} <- Headers],
    Sorted = lists:keysort(1, Normalized),
    Canonical = [[Name, $:, Value, $\n] || {Name, Value} <- Sorted],
    Signed = string:join([Name || {Name, _} <- Sorted], ";"),
    {Canonical, Signed}.

%% @doc calculate canonical query string out of query params and according to v4 documentation
canonical_query_string([]) ->
    "";
canonical_query_string(Params) ->
    Normalized = [{erlcloud_http:url_encode(Name), erlcloud_http:url_encode(erlcloud_http:value_to_string(Value))} || {Name, Value} <- Params],
    Sorted = lists:keysort(1, Normalized),
    string:join([case Value of
                     [] -> [Key, "="];
                     _ -> [Key, "=", Value]
                 end
                 || {Key, Value} <- Sorted, Value =/= none, Value =/= undefined], "&").

trimall(Value) ->
    %% TODO - remove excess internal whitespace in header values
    re:replace(Value, "(^\\s+)|(\\s+$)", "", [global]).

hash_encode(Data) ->
    Hash = erlcloud_util:sha256( Data),
    base16(Hash).

base16(Data) ->
    [binary:bin_to_list(base16:encode(Data))].

credential_scope(Date, Region, Service) ->
    DateOnly = string:left(Date, 8),
    [DateOnly, $/, Region, $/, Service, "/aws4_request"].

to_sign(Date, CredentialScope, Request) ->
    ["AWS4-HMAC-SHA256\n",
     Date, $\n,
     CredentialScope, $\n,
     hash_encode(Request)].

signing_key(Config, Date, Region, Service) ->
    %% TODO cache the signing key so we don't have to recompute for every request
    DateOnly = string:left(Date, 8),
    KDate = erlcloud_util:sha256_mac( "AWS4" ++ Config#aws_config.secret_access_key, DateOnly),
    KRegion = erlcloud_util:sha256_mac( KDate, Region),
    KService = erlcloud_util:sha256_mac( KRegion, Service),
    erlcloud_util:sha256_mac( KService, "aws4_request").

authorization(Config, CredentialScope, SignedHeaders, Signature) ->
    ["AWS4-HMAC-SHA256"
     " Credential=", Config#aws_config.access_key_id, $/, CredentialScope, $,,
     " SignedHeaders=", SignedHeaders, $,,
     " Signature=", Signature].

%% This function fetches http://status.aws.amazon.com/data.json
%% and examine "current" section for on going AWS issues/failures.
%% Example of a return status:
%% [{<<"service_name">>,
%%   <<"Amazon Elastic Compute Cloud (Frankfurt)">>},
%%  {<<"summary">>,<<"[RESOLVED] Internet Connectivity ">>},
%%  {<<"date">>,<<"1436972949">>},
%%  {<<"status">>,1},
%%  {<<"details">>,<<>>},
%%  {<<"description">>,
%%   <<"<div><span class=\"yellowfg\"> 8:22 AM PDT</span>&nbsp;Between 7:55 AM PDT and 8:05 AM PDT we experienced Internet connectivity issues for some instances in the EU-CENTRAL-1 Region. The issue has been resolved and the service is operating normally.</div>">>},
%%  {<<"service">>,<<"ec2-eu-central-1">>}]
%%
%% <<"status">> field values are the following:
%%  0 - service is operating normally;
%%  1 - performance issues;
%%  2 - service disruption.
-spec get_service_status(list(string())) -> ok | list().
get_service_status(ServiceNames) when is_list(ServiceNames) ->
    {ok, Json} = aws_request_form(get, "http", "status.aws.amazon.com", undefined,
        "/data.json", "", [], default_config()),

    case get_filtered_statuses(ServiceNames,
            proplists:get_value(<<"current">>, jsx:decode(Json)))
    of
        [] -> ok;
        ReturnStatuses -> ReturnStatuses
    end.

get_filtered_statuses(ServiceNames, Statuses) ->
    lists:filter(
        fun(S)->
            lists:any(
                fun(InputService)->
                    ServiceNameBin = list_to_binary(InputService),
                    ServiceNameLen = byte_size(ServiceNameBin),
                    case proplists:get_value(<<"service">>, S) of
                        <<ServiceNameBin:ServiceNameLen/binary, _/binary>> -> true;
                        _ -> false
                    end
                end,
            ServiceNames)
        end,
    Statuses).

-spec is_throttling_error_response(aws_request()) -> true | false.
is_throttling_error_response(#aws_request{response_status = 429}) ->
    true;
is_throttling_error_response(RequestResponse) ->
    #aws_request{
         response_type = error,
         error_type = aws,
         response_body = RespBody} = RequestResponse,

    case binary:match(RespBody, <<"Throttling">>) of
        nomatch ->
            false;
        _ ->
            true
    end.


%%%---------------------------------------------------------------------------
-spec profile() -> {ok, aws_config()} | {error, string()}.
%%%---------------------------------------------------------------------------
%% @doc Retrieve a config based on default credentials
%%
%% This function will retrieve the credentials for the <em>default</em>
%% profile, in the same way as the AWS CLI tools, and construct a {@link aws_config(). config record} to be used accessing services.
%%
%% @see profile/2
%%
profile() ->
    profile( default, [] ).


%%%---------------------------------------------------------------------------
-spec profile( Name :: atom() ) -> {ok, aws_config()} | {error, string()}.
%%%---------------------------------------------------------------------------
%% @doc Retrieve a config based on named credentials profile
%%
%% This function will retrieve the credentials for the named profile, in the
%% same way as the AWS CLI tools, and construct a {@link aws_config(). config record} to be used accessing services.
%%
%% @see profile/2
%%
profile( Name ) ->
    profile( Name, [] ).


-type profile_option() :: {role_session_name, string()}
                          | {role_session_secs, 900..3600}.

%%%---------------------------------------------------------------------------
-spec profile( Name :: atom(), Options :: [profile_option()] ) ->
                     {ok, aws_config()} | {error, string()}.
%%%---------------------------------------------------------------------------
%% @doc Retrieve a config based on named credentials profile
%%
%% This function will read the <code>$HOME/.aws/credentials</code> file used
%% by the AWS CLI tools and construct a {@link aws_config(). config record}
%% to be used accessing services for the named profile.  This supports both
%% direct credentials that appear in the profile:
%%
%% <code><pre>
%%  [default]
%%  aws_access_key_id = XXXXXXXXXXXXXXXXXXX2
%%  aws_secret_access_key = yyyyyyyyyyyyyyyyyyyyyyyyyy+yyyy/yyyyyyyy2
%% </pre></code>
%%
%% as well as indirection using <em>source_profile</em>:
%%
%% <code><pre>
%%  [foo]
%%  source_profile = default
%% </pre></code>
%%
%% and finally, will supports the <em>role_arn</em> specification, and will
%% assume the role indicated using the credentials current when interpreting
%% the profile in which they it is declared:
%%
%% <code><pre>
%%  [foo]
%%  role_arn=arn:aws:iam::892406118791:role/centralized-users
%%  source_profile = default
%% </pre></code>
%%
%% When using the the <em>role_arn</em> specification, you may supply the
%% following two options to control the way in which the assume_role request
%% is made via AWS STS service:
%%
%% <ul>
%%  <li><code>'role_session_name'</code>
%%    <p>The name that should be used
%%    for the <code>RoleSessionName</code> parameter.  If this option is not
%%    specified, then it will default to "erlcloud"</p>
%%  </li>
%%  <li><code>'role_duration_secs'</code>
%%    <p>The number of seconds that
%%    should be used for the <code>DurationSeconds</code> parameter.  If
%%    this option is not specified, then it will default to 900 seconds.</p>
%%  </li>
%%  <li><code>'external_id'</code>
%%    <p>The identifier that is used in the <code>ExternalId</code>
%%    parameter.  If this option is not specified, then it will default to
%%    'undefined', which will work for normal in-account roles, but will
%%    need to be specified for roles in external accounts.</p>
%%  </li>
%% </ul>
%%
profile( Name, Options ) ->
    try
        OptionsRec = profile_options( Options ),
        Profiles = profiles_read(),
        profiles_resolve( Name, Profiles, OptionsRec )
    catch
        throw:Error -> Error
    end.

profile_options( Options ) ->
    SessionName = proplists:get_value( role_session_name, Options, "erlcloud" ),
    SessionDuration = proplists:get_value( role_duration_secs, Options, 900 ),
    ExternalId = proplists:get_value( external_id, Options ),
    #profile_options{ session_name = SessionName, external_id = ExternalId,
                      session_secs = SessionDuration }.

profiles_read() ->
    HOME = profiles_home(),
    Path = HOME ++ "/.aws/credentials",
    case file:read_file( Path ) of
        {ok, Contents} ->
            profiles_parse( Contents );
        Error ->
            error_msg( "could not read credentials file ~s, because ~p",
                   [Path, Error] )
    end.

profiles_home() ->
    case os:getenv("HOME") of
        false ->
            error_msg( "HOME environment variable is not set" );
        HOME -> HOME
    end.

profiles_parse( Content ) ->
    case eini:parse( Content ) of
        {ok, Profiles} -> Profiles;
        Error ->
            error_msg( "failed to parse credentials, because: ~p", [Error] )
    end.

profiles_resolve( Name, Profiles, Options ) ->
    profiles_resolve( Name, Profiles, undefined, undefined, Options ).

profiles_resolve( BinName, Profiles, Role, ExternalId, Options )
  when is_binary(BinName) ->
    try binary_to_existing_atom( BinName, latin1 ) of
        Name -> profiles_resolve( Name, Profiles, Role, ExternalId, Options )
    catch
        error:badarg ->
            error_msg( "invalid source_profile reference to ~s", [BinName] )
    end;
profiles_resolve( Name, Profiles, BinRole, ExternalId, Options )
  when is_binary(BinRole) ->
    Role = binary_to_list( BinRole ),
    profiles_resolve( Name, Profiles, Role, ExternalId, Options );
profiles_resolve( Name, Profiles, Role, BinExternalId, Options )
  when is_binary(BinExternalId) ->
    ExternalId = binary_to_list( BinExternalId ),
    profiles_resolve( Name, Profiles, Role, ExternalId, Options );
profiles_resolve( Name, Profiles, Role, ExternalId, Options ) ->
    case proplists:get_value( Name, Profiles ) of
        undefined ->
            error_msg( "profile ~s does not exist", [Name] );
        Keys ->
            profiles_recurse( Keys, Profiles, Role, ExternalId, Options )
    end.

profiles_recurse( Keys, Profiles, Role, ExternalId, Options ) ->
    case profiles_credentials( Keys ) of
        {ok, Credential} ->
            profiles_assume( Credential, Role, ExternalId, Options );
        {cont, ProfileName, NextRole, NextExternalId} ->
            {ok, Config} = profiles_resolve( ProfileName, Profiles,
                                             NextRole, NextExternalId, Options ),
            profiles_assume( Config, Role, ExternalId, Options )
    end.

profiles_credentials( Keys ) ->
    Names = [aws_access_key_id, aws_secret_access_key, aws_security_token,
             source_profile],
    case [proplists:get_value( K, Keys ) || K <- Names] of
        [Id, Secret, undefined, undefined] when Id =/= undefined, Secret =/= undefined ->
            {ok, {binary_to_list(Id), binary_to_list(Secret)}};
        [Id, Secret, Token, undefined] when Id =/= undefined, Secret =/= undefined, Token =/= undefined ->
            {ok, {binary_to_list(Id), binary_to_list(Secret), binary_to_list(Token)}};
        [undefined, undefined, undefined, SourceProfile] ->
            profiles_credentials( Keys, SourceProfile )
    end.

profiles_credentials( Keys, SourceProfile ) ->
    Names = [role_arn, external_id],
    [RoleArn, ExternalId] = [proplists:get_value( K, Keys ) || K <- Names],
    {cont, SourceProfile, RoleArn, ExternalId}.

profiles_assume( Credential, undefined, __ExternalId, _Options ) ->
    RCfg = default_config_region(#aws_config{}, default_config_get(?AWS_REGION, aws_region)),
    Config = config_credential(Credential, RCfg),
    {ok, Config};
profiles_assume( Credential, Role, ExternalId,
                 #profile_options{ session_name = Name,
                                   external_id = DefaultExternalId,
                                   session_secs = Duration } ) ->
    ExtId = if ExternalId =/= undefined -> ExternalId;
               ExternalId =:= undefined -> DefaultExternalId
            end,
    RCfg = default_config_region(#aws_config{}, default_config_get(?AWS_REGION, aws_region)),
    Config = config_credential(Credential, RCfg),
    {AssumedConfig, _Creds} =
        erlcloud_sts:assume_role( Config, Role, Name, Duration, ExtId ),
    {ok, AssumedConfig}.


config_credential({Id, Secret}, Config) ->
    Config#aws_config{ access_key_id = Id, secret_access_key = Secret };
config_credential({Id, Secret, Token}, Config) ->
    Config#aws_config{ access_key_id = Id, secret_access_key = Secret, security_token = Token };
config_credential(#aws_config{} = Config, _) -> Config.




error_msg( Message ) ->
    Error = iolist_to_binary( Message ),
    throw( {error, Error} ).

error_msg( Format, Values ) ->
    Error = iolist_to_binary( io_lib:format( Format, Values ) ),
    throw( {error, Error} ).
