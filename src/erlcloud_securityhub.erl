-module(erlcloud_securityhub).

-include("erlcloud.hrl").
-include("erlcloud_aws.hrl").

%% API
-export([
    describe_hub/1,
    describe_hub/2
]).

-type securityhub() :: proplist().

-type param_name() :: binary() | string() | atom().
-type param_value() :: binary() | string() | atom() | integer().
-type params() :: [param_name() | {param_name(), param_value()}].

-spec describe_hub(AwsConfig) -> Result
    when AwsConfig :: aws_config(),
    Result :: {ok, securityhub()} | {error, not_found} | {error, term()}.
describe_hub(AwsConfig)
    when is_record(AwsConfig, aws_config) ->
    describe_hub(AwsConfig, _Params = []);
describe_hub(Params) ->
    AwsConfig = erlcloud_aws:default_config(),
    describe_hub(AwsConfig, Params).

-spec describe_hub(AwsConfig, Params) -> Result
    when AwsConfig :: aws_config(),
    Params :: params(),
    Result :: {ok, securityhub()}| {error, not_found} | {error, term()}.
describe_hub(AwsConfig, Params) ->
    Path = ["accounts"],
    case request(AwsConfig, _Method = get, Path, Params) of
        {ok, Response} ->
            {ok, Response};
        {error, {<<"ResourceNotFoundException">>, _Message}} ->
            {error, not_found};
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
    Host = AwsConfig#aws_config.securityhub_host,
    Service = "securityhub",
    NormPath = norm_path(Path),
    NormParams = norm_params(Params),
    Region = erlcloud_aws:aws_region_from_host(Host),
    Headers = [{"host", Host}, {"content-type", "application/json"}],
    SignedHeaders = erlcloud_aws:sign_v4(Method, NormPath, AwsConfig, Headers, Payload, Region, Service, Params),
    #aws_request{
        service = securityhub,
        method = Method,
        uri = "https://" ++ Host ++ NormPath ++ NormParams,
        request_headers = SignedHeaders,
        request_body = Payload
    }.

norm_path(Path) ->
    binary_to_list(iolist_to_binary(["/" | lists:join("/", Path)])).

norm_params([] = _Params) ->
    "";
norm_params(Params) ->
    "?" ++ erlcloud_aws:canonical_query_string(Params).

should_retry(Request)
    when Request#aws_request.response_type == ok ->
    Request;
should_retry(Request)
    when Request#aws_request.response_type == error,
    Request#aws_request.response_status == 429 ->
    Request#aws_request{should_retry = true};
should_retry(Request)
    when Request#aws_request.response_type == error,
    Request#aws_request.response_status >= 500 ->
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

decode_error(AwsRequest) ->
    case AwsRequest#aws_request.error_type of
        aws ->
            Type = extract_error_type(AwsRequest),
            Message = extract_error_message(AwsRequest),
            {error, {Type, Message}};
        _ ->
            erlcloud_aws:request_to_return(AwsRequest)
    end.

extract_error_type(AwsRequest) ->
    Headers = AwsRequest#aws_request.response_headers,
    Value = proplists:get_value("x-amzn-errortype", Headers),
    iolist_to_binary(Value).

extract_error_message(AwsRequest) ->
    ResponseBody = AwsRequest#aws_request.response_body,
    Object = jsx:decode(ResponseBody, [{return_maps, false}]),
    proplists:get_value(<<"message">>, Object, <<>>).