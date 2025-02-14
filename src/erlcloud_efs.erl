-module(erlcloud_efs).

-include("erlcloud.hrl").
-include("erlcloud_aws.hrl").

%% API
-export([
    describe_file_systems/0,
    describe_file_systems/1,
    describe_file_systems/2,
    describe_file_systems_all/0,
    describe_file_systems_all/1,
    describe_file_systems_all/2
]).

-type params() :: [param_name() | {param_name(), param_value()}].
-type param_name() :: binary() | string() | atom() | integer().
-type param_value() :: binary() | string() | atom() | integer().

-type file_system() :: proplist().
-type file_systems() :: [file_system()].
-type token() :: binary().

-define(EFS_API_VERSION, "2015-02-01").

%% -----------------------------------------------------------------------------
%% Exported functions
%% -----------------------------------------------------------------------------

-spec describe_file_systems() -> Result when
    Result :: {ok, file_systems()} | {ok, file_systems(), token()} | {error, term()}.
describe_file_systems() ->
    AwsConfig = erlcloud_aws:default_config(),
    describe_file_systems(AwsConfig).

-spec describe_file_systems(Arg) -> Result when
    Arg :: aws_config() | params(),
    Result :: {ok, file_systems()} | {ok, file_systems(), token()} | {error, term()}.
describe_file_systems(AwsConfig) when is_record(AwsConfig, aws_config) ->
    describe_file_systems(AwsConfig, _Params = []);
describe_file_systems(Params) ->
    AwsConfig = erlcloud_aws:default_config(),
    describe_file_systems(AwsConfig, Params).

-spec describe_file_systems(AwsConfig, Params) -> Result when
    AwsConfig :: aws_config(),
    Params :: params(),
    Result :: {ok, file_systems()} | {ok, file_systems(), token()} | {error, term()}.
describe_file_systems(AwsConfig, Params) ->
    Path = [?EFS_API_VERSION, "file-systems"],
    case request(AwsConfig, _Method = get, Path, Params) of
        {ok, Response} ->
            FileSystems = proplists:get_value(<<"FileSystems">>, Response),
            case proplists:get_value(<<"NextMarker">>, Response, null) of
                null ->
                    {ok, FileSystems};
                Marker ->
                    {ok, FileSystems, Marker}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

-spec describe_file_systems_all() -> Result when
    Result :: {ok, file_systems()} | {error, term()}.
describe_file_systems_all() ->
    AwsConfig = erlcloud_aws:default_config(),
    describe_file_systems_all(AwsConfig).

-spec describe_file_systems_all(Arg) -> Result when
    Arg :: aws_config() | params(),
    Result :: {ok, file_systems()} | {error, term()}.
describe_file_systems_all(AwsConfig) when is_record(AwsConfig, aws_config) ->
    describe_file_systems_all(AwsConfig, _Params = []);
describe_file_systems_all(Params) when is_list(Params) ->
    AwsConfig = erlcloud_aws:default_config(),
    describe_file_systems_all(AwsConfig, Params).

-spec describe_file_systems_all(AwsConfig, Params) -> Result when
    AwsConfig :: aws_config(),
    Params :: params(),
    Result :: {ok, file_systems()} | {error, term()}.
describe_file_systems_all(AwsConfig, Params) ->
    case describe_file_systems(AwsConfig, Params) of
        {ok, FileSystems} ->
            {ok, FileSystems};
        {ok, FileSystems, Token} ->
            describe_file_systems_next(AwsConfig, Params, Token, FileSystems);
        {error, Reason} ->
            {error, Reason}
    end.

%% -----------------------------------------------------------------------------
%% Local functions
%% -----------------------------------------------------------------------------

describe_file_systems_next(AwsConfig, Params, Marker0, FileSystems0) ->
    case describe_file_systems(AwsConfig, [{<<"Marker">>, Marker0} | Params]) of
        {ok, FileSystems} ->
            FileSystems1 = lists:append(FileSystems0, FileSystems),
            {ok, FileSystems1};
        {ok, FileSystems, Marker1} ->
            FileSystems1 = lists:append(FileSystems0, FileSystems),
            describe_file_systems_next(AwsConfig, Params, Marker1, FileSystems1);
        {error, Reason} ->
            {error, Reason}
    end.

request(AwsConfig, Method, Path, Params) ->
    request(AwsConfig, Method, Path, Params, _RequestBody = <<>>).

request(AwsConfig0, Method, Path, Params, RequestBody) ->
    case erlcloud_aws:update_config(AwsConfig0) of
        {ok, AwsConfig1} ->
            AwsRequest0 = init_request(AwsConfig1, Method, Path, Params, RequestBody),
            AwsRequest1 = erlcloud_retry:request(AwsConfig1, AwsRequest0, fun should_retry/1),
            case AwsRequest1#aws_request.response_type of
                ok ->
                    decode_response(AwsRequest1);
                error ->
                    decode_error(AwsRequest1)
            end;
        {error, Reason} ->
            {error, Reason}
    end.

init_request(AwsConfig, Method, Path, Params, Payload) ->
    Scheme = AwsConfig#aws_config.efs_scheme,
    Host = AwsConfig#aws_config.efs_host,
    Port = AwsConfig#aws_config.efs_port,
    Service = "elasticfilesystem",
    NormPath = norm_path(Path),
    NormParams = norm_params(Params),
    Region = erlcloud_aws:aws_region_from_host(Host),
    Headers = [{"host", Host}, {"content-type", "application/json"}],
    SignedHeaders = erlcloud_aws:sign_v4(
        Method, NormPath, AwsConfig, Headers, Payload, Region, Service, Params
    ),
    #aws_request{
        service = efs,
        method = Method,
        uri = Scheme ++ Host ++ ":" ++ integer_to_list(Port) ++ NormPath ++ NormParams,
        request_headers = SignedHeaders,
        request_body = Payload
    }.

norm_path(Path) ->
    binary_to_list(iolist_to_binary(["/" | lists:join("/", Path)])).

norm_params([] = _Params) ->
    "";
norm_params(Params) ->
    "?" ++ erlcloud_aws:canonical_query_string(Params).

should_retry(Request) when
    Request#aws_request.response_type == ok
->
    Request;
should_retry(Request) when
    Request#aws_request.response_type == error,
    Request#aws_request.response_status == 429
->
    Request#aws_request{should_retry = true};
should_retry(Request) when
    Request#aws_request.response_type == error,
    Request#aws_request.response_status >= 500
->
    Request#aws_request{should_retry = true};
should_retry(Request) ->
    Request#aws_request{should_retry = false}.

decode_response(AwsRequest) ->
    case AwsRequest#aws_request.response_body of
        <<>> ->
            ok;
        ResponseBody ->
            Json = jsx:decode(ResponseBody, [{return_maps, false}]),
            {ok, Json}
    end.

decode_error(AwsRequest) when AwsRequest#aws_request.error_type == aws ->
    Type = extract_error_type(AwsRequest),
    Message = extract_error_message(AwsRequest),
    {error, {Type, Message}};
decode_error(AwsRequest) ->
    erlcloud_aws:request_to_return(AwsRequest).

extract_error_type(AwsRequest) ->
    ResponseBody = AwsRequest#aws_request.response_body,
    ResponseObject = jsx:decode(ResponseBody, [{return_maps, false}]),
    case proplists:get_value(<<"ErrorCode">>, ResponseObject) of
        undefined ->
            Headers = AwsRequest#aws_request.response_headers,
            ErrorType = proplists:get_value("x-amzn-errortype", Headers),
            iolist_to_binary(ErrorType);
        Code ->
            Code
    end.

extract_error_message(AwsRequest) ->
    ResponseBody = AwsRequest#aws_request.response_body,
    Object = jsx:decode(ResponseBody, [{return_maps, false}]),
    proplists:get_value(<<"Message">>, Object, <<>>).
