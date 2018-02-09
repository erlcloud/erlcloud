-module(erlcloud_cur_tests).

-include_lib("eunit/include/eunit.hrl").
-include("erlcloud_aws.hrl").

%% API
-export([]).

-define(EHTTPC, erlcloud_httpc).

-define(REPORT_DEFINITION_INPUT,
    #{<<"AdditionalArtifacts">> => [],
      <<"AdditionalSchemaElements">> => [],
      <<"Compression">> => <<"GZIP">>,
      <<"Format">> => <<"textORcsv">>,
      <<"ReportName">> => <<"test-report-name">>,
      <<"S3Bucket">> => <<"bucket-test">>,
      <<"S3Prefix">> => <<>>,
      <<"S3Region">> => <<"us-east-1">>,
      <<"TimeUnit">> => <<"DAILY">>}
).

-define(GET_REPORT_DEFINITIONS, #{<<"ReportDefinitions">> => [?REPORT_DEFINITION_INPUT]}).

setup() ->
    meck:new(?EHTTPC, [passthrough]),
    meck:expect(?EHTTPC, request, 6, fun do_erlcloud_httpc_request/6),
    [?EHTTPC].

erlcloud_cur_test_() ->
    {
        foreach,
        fun setup/0,
        fun meck:unload/1,
        [
            fun test_describe_report_definitions/0,
            fun test_describe_report_definitions_pagination/0
        ]
    }.


%%------------------------------------------------------------------------------
%% Tests
%%------------------------------------------------------------------------------

test_describe_report_definitions() ->
    Request  = #{},
    Expected = {ok, ?GET_REPORT_DEFINITIONS},
    TestFun  =
      fun(Config) ->
          erlcloud_cur:describe_report_definitions(Config)
      end,
    do_test(Request, Expected, TestFun).


test_describe_report_definitions_pagination() ->
    Options = #{<<"MaxResults">> => 5},
    Expected = {ok, ?GET_REPORT_DEFINITIONS},
    TestFun  =
      fun(Config) ->
          erlcloud_cur:describe_report_definitions(Options, Config)
      end,
    Request = Options,
    do_test(Request, Expected, TestFun).

%%------------------------------------------------------------------------------
%% Internal Functions
%%------------------------------------------------------------------------------

do_test(Request, ExpectedResult, TestedFun) ->
    Config = erlcloud_cur:new("test-access-key", "test-secret-key"),
    ?assertEqual(ExpectedResult, TestedFun(Config)),
    Encoded = jsx:encode(Request),
    ?assertMatch([{_, {?EHTTPC, request, [_, post, _, Encoded, _, _]}, _}],
                 meck:history(?EHTTPC)).

do_erlcloud_httpc_request(_, post, Headers, _Body, _, _) ->
    Target = proplists:get_value("x-amz-target", Headers),
    ["AWSOrigamiServiceGatewayService", Operation] = string:tokens(Target, "."),
    RespBody =
        case Operation of
            "DescribeReportDefinitions" -> ?GET_REPORT_DEFINITIONS
        end,
    {ok, {{200, "OK"}, [], jsx:encode(RespBody)}}.
