-module(erlcloud_access_analyzer_tests).

-include_lib("eunit/include/eunit.hrl").
-include("erlcloud_aws.hrl").

-define(TEST_AWS_CONFIG,
    #aws_config{
        access_key_id = "TEST_ACCESS_KEY_ID",
        secret_access_key = "TEST_ACCESS_KEY",
        security_token = "TEST_SECURITY_TOKEN"
    }
).

api_test_() ->
    {
        foreach,
        fun() -> meck:new(erlcloud_httpc) end,
        fun(_) -> meck:unload() end,
        [
            fun list_analyzers_tests/1,
            fun get_analyzer_tests/1,
            fun create_analyzer_tests/1,
            fun delete_analyzer_tests/1
        ]
    }.

list_analyzers_tests(_) ->
    [
        {
            "ListAnalyzers",
            fun() ->
                AwsConfig = ?TEST_AWS_CONFIG,
                meck:expect(
                    erlcloud_httpc, request,
                    fun(A1, A2, A3, A4, A5, A6) ->
                        Url = "https://access-analyzer.us-east-1.amazonaws.com/analyzer?maxResults=10",
                        Method = get,
                        RequestContent = <<>>,
                        case [A1, A2, A3, A4, A5, A6] of
                            [Url, Method, _Headers, RequestContent, _Timeout, AwsConfig] ->
                                ResponseContent = <<
                                    "{"
                                        "\"analyzers\":["
                                            "{"
                                                "\"arn\":\"arn:aws:access-analyzer:us-east-1:123456789012:analyzer/test-1\","
                                                "\"name\":\"test-1\","
                                                "\"type\":\"ACCOUNT\","
                                                "\"createdAt\":\"2022-01-01T01:02:03Z\","
                                                "\"status\":\"ACTIVE\""
                                            "},"
                                            "{"
                                                "\"arn\":\"arn:aws:access-analyzer:us-east-1:123456789012:analyzer/test-2\","
                                                "\"name\":\"test-2\","
                                                "\"type\":\"ACCOUNT\","
                                                "\"createdAt\":\"2022-02-02T01:02:03Z\","
                                                "\"status\":\"ACTIVE\""
                                            "}"
                                        "]"
                                    "}"
                                >>,
                                {ok, {{200, "OK"}, [], ResponseContent}}
                        end
                    end
                ),
                Params = [{"maxResults", 10}],
                Result = erlcloud_access_analyzer:list_analyzers(AwsConfig, Params),
                Analyzers = [
                    [
                        {<<"arn">>, <<"arn:aws:access-analyzer:us-east-1:123456789012:analyzer/test-1">>},
                        {<<"name">>, <<"test-1">>},
                        {<<"type">>, <<"ACCOUNT">>},
                        {<<"createdAt">>, <<"2022-01-01T01:02:03Z">>},
                        {<<"status">>, <<"ACTIVE">>}
                    ],
                    [
                        {<<"arn">>, <<"arn:aws:access-analyzer:us-east-1:123456789012:analyzer/test-2">>},
                        {<<"name">>, <<"test-2">>},
                        {<<"type">>, <<"ACCOUNT">>},
                        {<<"createdAt">>, <<"2022-02-02T01:02:03Z">>},
                        {<<"status">>, <<"ACTIVE">>}
                    ]
                ],
                ?assertEqual({ok, Analyzers}, Result)
            end
        },
        {
            "ListAnalyzers [all]",
            fun() ->
                AwsConfig = ?TEST_AWS_CONFIG,
                meck:expect(
                    erlcloud_httpc, request,
                    fun(A1, A2, A3, A4, A5, A6) ->
                        Url1 = "https://access-analyzer.us-east-1.amazonaws.com/analyzer?maxResults=1&type=ACCOUNT",
                        Url2 = "https://access-analyzer.us-east-1.amazonaws.com/analyzer?maxResults=1&nextToken=TEST_NEXT_TOKEN&type=ACCOUNT",
                        Method = get,
                        RequestContent = <<>>,
                        case [A1, A2, A3, A4, A5, A6] of
                            [Url1, Method, _Headers, RequestContent, _Timeout, AwsConfig] ->
                                ResponseContent = <<
                                    "{"
                                        "\"analyzers\":["
                                            "{"
                                                "\"arn\":\"arn:aws:access-analyzer:us-east-1:123456789012:analyzer/test-1\","
                                                "\"name\":\"test-1\","
                                                "\"type\":\"ACCOUNT\","
                                                "\"createdAt\":\"2022-01-01T01:02:03Z\","
                                                "\"status\":\"ACTIVE\""
                                            "}"
                                        "],"
                                        "\"nextToken\":\"TEST_NEXT_TOKEN\""
                                    "}"
                                >>,
                                {ok, {{200, "OK"}, [], ResponseContent}};
                            [Url2, Method, _Headers, RequestContent, _Timeout, AwsConfig] ->
                                ResponseContent = <<
                                    "{"
                                        "\"analyzers\":["
                                            "{"
                                                "\"arn\":\"arn:aws:access-analyzer:us-east-1:123456789012:analyzer/test-2\","
                                                "\"name\":\"test-2\","
                                                "\"type\":\"ACCOUNT\","
                                                "\"createdAt\":\"2022-02-02T01:02:03Z\","
                                                "\"status\":\"ACTIVE\""
                                            "}"
                                        "]"
                                    "}"
                                >>,
                                {ok, {{200, "OK"}, [], ResponseContent}}
                        end
                    end
                ),
                Params = [{"type", "ACCOUNT"}, {"maxResults", 1}],
                Result = erlcloud_access_analyzer:list_analyzers_all(AwsConfig, Params),
                Analyzers = [
                    [
                        {<<"arn">>, <<"arn:aws:access-analyzer:us-east-1:123456789012:analyzer/test-1">>},
                        {<<"name">>, <<"test-1">>},
                        {<<"type">>, <<"ACCOUNT">>},
                        {<<"createdAt">>, <<"2022-01-01T01:02:03Z">>},
                        {<<"status">>, <<"ACTIVE">>}
                    ],
                    [
                        {<<"arn">>, <<"arn:aws:access-analyzer:us-east-1:123456789012:analyzer/test-2">>},
                        {<<"name">>, <<"test-2">>},
                        {<<"type">>, <<"ACCOUNT">>},
                        {<<"createdAt">>, <<"2022-02-02T01:02:03Z">>},
                        {<<"status">>, <<"ACTIVE">>}
                    ]
                ],
                ?assertEqual({ok, Analyzers}, Result)
            end
        }
    ].

get_analyzer_tests(_) ->
    [
        {
            "GetAnalyzer",
            fun() ->
                AwsConfig = ?TEST_AWS_CONFIG,
                meck:expect(
                    erlcloud_httpc, request,
                    fun(A1, A2, A3, A4, A5, A6) ->
                        Url = "https://access-analyzer.us-east-1.amazonaws.com/analyzer/test-1",
                        Method = get,
                        RequestContent = <<>>,
                        case [A1, A2, A3, A4, A5, A6] of
                            [Url, Method, _Headers, RequestContent, _Timeout, AwsConfig] ->
                                ResponseContent = <<
                                    "{"
                                        "\"analyzer\":{"
                                            "\"arn\":\"arn:aws:access-analyzer:us-east-1:123456789012:analyzer/test-1\","
                                            "\"name\":\"test-1\","
                                            "\"type\":\"ACCOUNT\","
                                            "\"createdAt\":\"2022-01-01T01:02:03Z\","
                                            "\"status\":\"ACTIVE\""
                                        "}"
                                    "}"
                                >>,
                                {ok, {{200, "OK"}, [], ResponseContent}}
                        end
                    end
                ),
                Result = erlcloud_access_analyzer:get_analyzer(AwsConfig, _AnalyzerName = "test-1"),
                Analyzer = [
                    {<<"arn">>, <<"arn:aws:access-analyzer:us-east-1:123456789012:analyzer/test-1">>},
                    {<<"name">>, <<"test-1">>},
                    {<<"type">>, <<"ACCOUNT">>},
                    {<<"createdAt">>, <<"2022-01-01T01:02:03Z">>},
                    {<<"status">>, <<"ACTIVE">>}
                ],
                ?assertEqual({ok, Analyzer}, Result)
            end
        },
        {
            "GetAnalyzer -> not_found",
            fun() ->
                AwsConfig = ?TEST_AWS_CONFIG,
                meck:expect(
                    erlcloud_httpc, request,
                    fun(A1, A2, A3, A4, A5, A6) ->
                        Url = "https://access-analyzer.us-east-1.amazonaws.com/analyzer/test-2",
                        Method = get,
                        RequestContent = <<>>,
                        case [A1, A2, A3, A4, A5, A6] of
                            [Url, Method, _Headers, RequestContent, _Timeout, AwsConfig] ->
                                ResponseHeaders = [{"x-amzn-errortype", "ResourceNotFoundException"}],
                                ResponseContent = <<"{\"message\": \"Analyzer not found\"}">>,
                                {ok, {{404, "NotFound"}, ResponseHeaders, ResponseContent}}
                        end
                    end
                ),
                Result = erlcloud_access_analyzer:get_analyzer(AwsConfig, _AnalyzerName = "test-2"),
                ?assertEqual({error, not_found}, Result)
            end
        }
    ].

create_analyzer_tests(_) ->
    [
        {
            "CreateAnalyzer",
            fun() ->
                AwsConfig = ?TEST_AWS_CONFIG,
                meck:expect(
                    erlcloud_httpc, request,
                    fun(A1, A2, A3, A4, A5, A6) ->
                        Url = "https://access-analyzer.us-east-1.amazonaws.com/analyzer",
                        Method = put,
                        RequestContent = <<
                            "{"
                                "\"analyzerName\":\"test-1\","
                                "\"type\":\"ACCOUNT\""
                            "}"
                        >>,
                        case [A1, A2, A3, A4, A5, A6] of
                            [Url, Method, _Headers, RequestContent, _Timeout, AwsConfig] ->
                                ResponseContent = <<"{\"arn\":\"arn:aws:access-analyzer:us-east-1:123456789012:analyzer/test-1\"}">>,
                                {ok, {{200, "OK"}, [], ResponseContent}}
                        end
                    end
                ),
                AnalyzerSpec = [
                    {<<"analyzerName">>, <<"test-1">>},
                    {<<"type">>, <<"ACCOUNT">>}
                ],
                Result = erlcloud_access_analyzer:create_analyzer(AwsConfig, AnalyzerSpec),
                Arn = <<"arn:aws:access-analyzer:us-east-1:123456789012:analyzer/test-1">>,
                ?assertEqual({ok, Arn}, Result)
            end
        }
    ].

delete_analyzer_tests(_) ->
    [
        {
            "DeleteAnalyzer",
            fun() ->
                AwsConfig = ?TEST_AWS_CONFIG,
                meck:expect(
                    erlcloud_httpc, request,
                    fun(A1, A2, A3, A4, A5, A6) ->
                        Url = "https://access-analyzer.us-east-1.amazonaws.com/analyzer/test-1",
                        Method = delete,
                        RequestContent = <<>>,
                        case [A1, A2, A3, A4, A5, A6] of
                            [Url, Method, _Headers, RequestContent, _Timeout, AwsConfig] ->
                                ResponseContent = <<>>,
                                {ok, {{200, "OK"}, [], ResponseContent}}
                        end
                    end
                ),
                Result = erlcloud_access_analyzer:delete_analyzer(AwsConfig, _AnalyzerName = "test-1"),
                ?assertEqual(ok, Result)
            end
        },
        {
            "DeleteAnalyzer -> not_found",
            fun() ->
                AwsConfig = ?TEST_AWS_CONFIG,
                meck:expect(
                    erlcloud_httpc, request,
                    fun(A1, A2, A3, A4, A5, A6) ->
                        Url = "https://access-analyzer.us-east-1.amazonaws.com/analyzer/test-1",
                        Method = delete,
                        RequestContent = <<>>,
                        case [A1, A2, A3, A4, A5, A6] of
                            [Url, Method, _Headers, RequestContent, _Timeout, AwsConfig] ->
                                ResponseHeaders = [{"x-amzn-errortype", "ResourceNotFoundException"}],
                                ResponseContent = <<"{\"message\": \"Analyzer not found\"}">>,
                                {ok, {{404, "NotFound"}, ResponseHeaders, ResponseContent}}
                        end
                    end
                ),
                Result = erlcloud_access_analyzer:delete_analyzer(AwsConfig, _AnalyzerName = "test-1"),
                ?assertEqual({error, not_found}, Result)
            end
        }
    ].
