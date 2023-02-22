-module(erlcloud_securityhub_test).

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
        [fun describe_hub_tests/1]
    }.

describe_hub_tests(_) ->
    [
        {
            "SecurityHub",
            fun() ->
                AwsConfig = ?TEST_AWS_CONFIG,
                meck:expect(
                    erlcloud_httpc, request,
                    fun(A1, A2, A3, A4, A5, A6) ->
                        Url = "https://securityhub.us-east-1.amazonaws.com/accounts",
                        Method = get,
                        RequestContent = <<>>,
                        case [A1, A2, A3, A4, A5, A6] of
                            [Url, Method, _Headers, RequestContent, _Timeout, AwsConfig] ->
                                ResponseContent = <<
                                    "{"
                                        "\"HubArn\":\"arn:aws:securityhub:us-east-1:123456789012:hub/default\","
                                        "\"AutoEnableControls\":\"true\","
                                        "\"SubscribedAt\":\"2023-02-15T15:46:42.158Z\""
                                    "}"
                                >>,
                                {ok, {{200, "OK"}, [], ResponseContent}}
                        end
                    end
                ),

                Result = erlcloud_securityhub:describe_hub(AwsConfig, []),
                DescribeHub = [
                        {<<"HubArn">>, <<"arn:aws:securityhub:us-east-1:123456789012:hub/default">>},
                        {<<"AutoEnableControls">>, <<"true">>},
                        {<<"SubscribedAt">>, <<"2023-02-15T15:46:42.158Z">>}
                ],

                ?assertEqual({ok, DescribeHub}, Result)
            end
        },
        {
            "SecurityHub -> not_found",
            fun() ->
                AwsConfig = ?TEST_AWS_CONFIG,
                meck:expect(
                    erlcloud_httpc, request,
                    fun(A1, A2, A3, A4, A5, A6) ->
                        Url = "https://securityhub.us-east-1.amazonaws.com/accounts?HubArn=test",
                        Method = get,
                        RequestContent = <<>>,
                        case [A1, A2, A3, A4, A5, A6] of
                            [Url, Method, _Headers, RequestContent, _Timeout, AwsConfig] ->
                                ResponseHeaders = [{"x-amzn-errortype", "ResourceNotFoundException"}],
                                ResponseContent = <<"{\"message\": \"not found\"}">>,
                                {ok, {{404, "NotFound"}, ResponseHeaders, ResponseContent}}
                        end
                    end
                ),
                Params = [{<<"HubArn">>, <<"test">>}],
                Result = erlcloud_securityhub:describe_hub(AwsConfig, Params),
                ?assertEqual({error, not_found}, Result)
            end
        }
    ].

