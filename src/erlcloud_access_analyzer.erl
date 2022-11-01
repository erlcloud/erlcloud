-module(erlcloud_access_analyzer).

-include("erlcloud.hrl").
-include("erlcloud_aws.hrl").

%% API
-export([
    list_analyzers/1,
    list_analyzers/2,
    list_analyzers_all/1,
    list_analyzers_all/2,
    get_analyzer/2,
    create_analyzer/2,
    delete_analyzer/2
]).

-type param_name() :: binary() | string() | atom().
-type param_value() :: binary() | string() | atom() | integer(). 
-type params() :: [param_name() | {param_name(), param_value()}].

-type analyzer() :: proplist().
-type analyzers() :: [analyzer()].
-type token() :: binary().

-type create_analyzer_spec() :: proplist().

%% -----------------------------------------------------------------------------
%% Exported functions
%% -----------------------------------------------------------------------------

-spec list_analyzers(AwsConfig) -> Result
when AwsConfig :: aws_config(),
     Result :: {ok, analyzers()} | {ok, analyzers(), token()} | {error, term()}.
list_analyzers(AwsConfig)
  when is_record(AwsConfig, aws_config) ->
    list_analyzers(AwsConfig, _Params = []);
list_analyzers(Params) ->
    AwsConfig = erlcloud_aws:default_config(),
    list_analyzers(AwsConfig, Params).

-spec list_analyzers(AwsConfig, Params) -> Result
when AwsConfig :: aws_config(),
     Params :: params(),
     Result :: {ok, analyzers()} | {ok, analyzers(), token()} | {error, term()}.
list_analyzers(AwsConfig, Params) ->
    Path = ["analyzer"],
    case request(AwsConfig, _Method = get, Path, Params) of
        {ok, Response} ->
            Analyzers = proplists:get_value(<<"analyzers">>, Response),
            case proplists:get_value(<<"nextToken">>, Response) of
                undefined ->
                    {ok, Analyzers};
                Token ->
                    {ok, Analyzers, Token}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

-spec list_analyzers_all(AwsConfig) -> Result
when AwsConfig :: aws_config(),
     Result :: {ok, analyzers()} | {ok, analyzers(), token()} | {error, term()}.
list_analyzers_all(AwsConfig)
  when is_record(AwsConfig, aws_config) ->
    list_analyzers_all(AwsConfig, _Params = []);
list_analyzers_all(Params) ->
    AwsConfig = erlcloud_aws:default_config(),
    list_analyzers(AwsConfig, Params).

-spec list_analyzers_all(AwsConfig, Params) -> Result
when AwsConfig :: aws_config(),
     Params :: params(),
     Result :: {ok, analyzers()} | {ok, analyzers(), token()} | {error, term()}.
list_analyzers_all(AwsConfig, Params) ->
    case list_analyzers(AwsConfig, Params) of
        {ok, Analyzers} ->
            {ok, Analyzers};
        {ok, Analyzers, Token} ->
            list_analyzers_next(AwsConfig, Params, Token, Analyzers);
        {error, Reason} ->
            {error, Reason}
    end.

-spec get_analyzer(AwsConfig, AnalyzerName) -> Result
when AwsConfig :: aws_config(),
     AnalyzerName :: binary() | string(),
     Result :: {ok, analyzer()} | {error, not_found} | {error, term()}.
get_analyzer(AwsConfig, AnalyzerName) ->
    Path = ["analyzer", AnalyzerName],
    case request(AwsConfig, _Method = get, Path) of
        {ok, Response} ->
            Analyzer = proplists:get_value(<<"analyzer">>, Response),
            {ok, Analyzer};
        {error, {<<"ResourceNotFoundException">>, _Message}} ->
            {error, not_found};
        {error, Reason} ->
            {error, Reason}
    end.

-spec create_analyzer(AwsConfig, Spec) -> Result
when AwsConfig :: aws_config(),
     Spec :: create_analyzer_spec(),
     Result :: {ok, Arn :: binary()} | {error, term()}.
create_analyzer(AwsConfig, Spec) ->
    Path = ["analyzer"],
    RequestBody = jsx:encode(Spec),
    case request(AwsConfig, _Method = put, Path, _Params = [], RequestBody) of
        {ok, Response} ->
            Arn = proplists:get_value(<<"arn">>, Response),
            {ok, Arn};
        {error, Reason} ->
            {error, Reason}
    end.

-spec delete_analyzer(AwsConfig, AnalyzerName) -> Result
when AwsConfig :: aws_config(),
     AnalyzerName :: binary() | string(),
     Result :: ok | {error, not_found} | {error, term()}.
delete_analyzer(AwsConfig, AnalyzerName) ->
    delete_analyser(AwsConfig, AnalyzerName, _Params = []).

-spec delete_analyser(AwsConfig, AnalyzerName, Params) -> Result
when AwsConfig :: aws_config(),
     AnalyzerName :: binary() | string(),
     Params :: params(),
     Result :: ok | {error, not_found} | {error, term()}.
delete_analyser(AwsConfig, AnalyzerName, Params) ->
    Path = ["analyzer", AnalyzerName],
    case request(AwsConfig, _Method = delete, Path, Params) of
        ok ->
            ok;
        {error, {<<"ResourceNotFoundException">>, _Message}} ->
            {error, not_found};
        {error, Reason} ->
            {error, Reason}
    end.

%% -----------------------------------------------------------------------------
%% Local functions
%% -----------------------------------------------------------------------------

list_analyzers_next(AwsConfig, Params, Token0, Analyzers0) ->
    case list_analyzers(AwsConfig, [{<<"nextToken">>, Token0} | Params]) of
        {ok, Analyzers} ->
            Analyzers1 = lists:append(Analyzers0, Analyzers),
            {ok, Analyzers1};
        {ok, Analyzers, Token1} ->
            Analyzers1 = lists:append(Analyzers0, Analyzers),
            list_analyzers_next(AwsConfig, Params, Token1, Analyzers1);
        {error, Reason} ->
            {error, Reason}
    end.

request(AwsConfig, Method, Path) ->
    request(AwsConfig, Method, Path, _Params = []).

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
    Host = AwsConfig#aws_config.access_analyzer_host,
    Service = "access-analyzer",
    NormPath = norm_path(Path),
    NormParams = norm_params(Params),
    Region = erlcloud_aws:aws_region_from_host(Host),
    Headers = [{"host", Host}, {"content-type", "application/json"}],
    SignedHeaders = erlcloud_aws:sign_v4(Method, NormPath, AwsConfig, Headers, Payload, Region, Service, Params),
    #aws_request{
        service = access_analyzer,
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
