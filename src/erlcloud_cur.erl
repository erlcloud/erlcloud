-module(erlcloud_cur).


%% AWS Cost and Usage Report API implementation
%% https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/api-reference.html

-include("erlcloud_aws.hrl").

%% Initialization API
-export([new/2, new/3, new/4, new/5]).

%% API
-export([
    describe_report_definitions/1,
    describe_report_definitions/2
]).


%%------------------------------------------------------------------------------
%% Initialization API
%%------------------------------------------------------------------------------

-spec new(string(), string()) -> aws_config().
new(AccessKeyID, SecretAccessKey) ->
    #aws_config{access_key_id     = AccessKeyID,
                secret_access_key = SecretAccessKey,
                retry             = fun erlcloud_retry:default_retry/1}.

-spec new(string(), string(), string()) -> aws_config().
new(AccessKeyID, SecretAccessKey, Host) ->
    #aws_config{access_key_id     = AccessKeyID,
                secret_access_key = SecretAccessKey,
                cur_host          = Host,
                retry             = fun erlcloud_retry:default_retry/1}.

-spec new(string(), string(), string(), non_neg_integer()) -> aws_config().
new(AccessKeyID, SecretAccessKey, Host, Port) ->
    #aws_config{access_key_id     = AccessKeyID,
                secret_access_key = SecretAccessKey,
                cur_host          = Host,
                cur_port          = Port,
                retry             = fun erlcloud_retry:default_retry/1}.

-spec new(string(), string(), string(), non_neg_integer(), string()) ->
    aws_config().
new(AccessKeyID, SecretAccessKey, Host, Port, Scheme) ->
    #aws_config{access_key_id     = AccessKeyID,
                secret_access_key = SecretAccessKey,
                cur_host          = Host,
                cur_port          = Port,
                cur_scheme        = Scheme,
                retry             = fun erlcloud_retry:default_retry/1}.


%%------------------------------------------------------------------------------
%% API
%%------------------------------------------------------------------------------

%% @doc
%% Cost & Usage Report API:
%% https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/describe-report-definitions.html
%%
%% `
%% {ok, R} = erlcloud_cur:describe_report_definitions(Config).
%% `
%%
-spec describe_report_definitions(aws_config()) -> {ok, map()} | {error, any()}.
describe_report_definitions(Config) ->
    describe_report_definitions(#{}, Config).


%% `
%% Opts = #{<<"MaxResults">> => 5},
%% {ok, R} = erlcloud_cur:describe_report_definitions(Opts, Config).
%% `

-spec describe_report_definitions(map(), aws_config()) -> {ok, map()} | {error, any()}.
describe_report_definitions(Options, Config) ->
    request(Config, <<"DescribeReportDefinitions">>, Options).

%%------------------------------------------------------------------------------
%% Internal Functions
%%------------------------------------------------------------------------------

request(Config0, Operation, Input) ->
    case erlcloud_aws:update_config(Config0) of
        {ok, Config} ->
            Body       = jsx:encode(Input),
            Headers    = get_headers(Config, Operation, Body),
            AwsRequest = #aws_request{service         = cur,
                                      uri             = get_url(Config),
                                      method          = post,
                                      request_headers = Headers,
                                      request_body    = Body},
            request(Config, AwsRequest);
        {error, Reason} ->
            {error, Reason}
    end.

request(Config, Request) ->
    Result = erlcloud_retry:request(Config, Request, fun handle_result/1),
    case erlcloud_aws:request_to_return(Result) of
        {ok, {_, <<>>}}     -> {ok, #{}};
        {ok, {_, RespBody}} ->
            {ok, jsx:decode(RespBody, [return_maps])};
        {error, _} = Error  -> Error
    end.

handle_result(#aws_request{response_type = ok} = Request) ->
    Request;
handle_result(#aws_request{response_type    = error,
                            error_type      = aws,
                            response_status = Status} = Request)
  when Status >= 500 ->
    Request#aws_request{should_retry = true};
handle_result(#aws_request{response_type = error,
                           error_type    = aws} = Request) ->
    Request#aws_request{should_retry = false}.

get_headers(#aws_config{cur_host = Host} = Config, Operation, Body) ->
    Headers = [{"host",         Host},
               {"x-amz-target", make_amz_target(Operation)},
               {"content-type", "application/x-amz-json-1.1"}],
    Region = erlcloud_aws:aws_region_from_host(Host),
    erlcloud_aws:sign_v4_headers(Config, Headers, Body, Region, "cur").

get_url(#aws_config{cur_scheme = Scheme,
                    cur_host   = Host,
                    cur_port   = Port}) ->
    Scheme ++ Host ++ ":" ++ integer_to_list(Port).

make_amz_target(Operation) ->
    "AWSOrigamiServiceGatewayService." ++ binary_to_list(Operation).