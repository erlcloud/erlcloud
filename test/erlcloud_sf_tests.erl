-module(erlcloud_sf_tests).

-include_lib("eunit/include/eunit.hrl").
-include("erlcloud_aws.hrl").

-define(VALID_JSON, #{<<"valid">> => <<"json">>}).
-define(SOME_BINARY, <<"someName">>).
-define(OTHER_BINARY, <<"otherName">>).
-define(SOME_NUMBER, 42).
-define(SOME_BOOLEAN, true).

setup() ->
    meck:new(erlcloud_httpc, [passthrough]),
    meck:expect(erlcloud_httpc, request, 6, fun meck_sf_request/6),
    [erlcloud_httpc].

meck_sf_request(_, post, Headers, _, _, _) ->
    Target = proplists:get_value("x-amz-target", Headers),
    ["AWSStepFunctions", Operation] = string:tokens(Target, "."),
    RespBody = meck_aws_body(Operation),
    {ok, {{200, "OK"}, [], RespBody}}.

erlcloud_sf_test_() ->
    {
        foreach,
        fun setup/0,
        fun meck:unload/1,
        [
            fun test_create_activity/0,
            fun test_create_sm/0,
            fun test_delete_activity/0,
            fun test_delete_sm/0,
            fun test_describe_activity/0,
            fun test_describe_execution/0,
            fun test_describe_sm/0,
            fun test_describe_sm_for_execution/0,
            fun test_get_activity_task/0,
            fun test_get_activity_task_opt/0,
            fun test_get_execution_history/0,
            fun test_get_execution_history_opt/0,
            fun test_list_activities/0,
            fun test_list_activities_opt/0,
            fun test_list_executions/0,
            fun test_list_executions_opt/0,
            fun test_list_sms/0,
            fun test_list_sms_opt/0,
            fun test_send_task_failure/0,
            fun test_send_task_failure_opt/0,
            fun test_send_task_heartbeat/0,
            fun test_send_task_success/0,
            fun test_start_execution/0,
            fun test_start_execution_opt/0,
            fun test_stop_execution/0,
            fun test_stop_execution_opt/0,
            fun test_update_sm/0
        ]
    }.

meck_aws_body("CreateActivity") -> jsx:encode(?VALID_JSON);
meck_aws_body("CreateStateMachine") -> jsx:encode(?VALID_JSON);
meck_aws_body("DeleteActivity") -> <<>>;
meck_aws_body("DeleteStateMachine") -> <<>>;
meck_aws_body("DescribeActivity") -> jsx:encode(?VALID_JSON);
meck_aws_body("DescribeExecution") -> jsx:encode(?VALID_JSON);
meck_aws_body("DescribeStateMachine") -> jsx:encode(?VALID_JSON);
meck_aws_body("DescribeStateMachineForExecution") -> jsx:encode(?VALID_JSON);
meck_aws_body("GetActivityTask") -> jsx:encode(?VALID_JSON);
meck_aws_body("GetExecutionHistory") -> jsx:encode(?VALID_JSON);
meck_aws_body("ListActivities") -> jsx:encode(?VALID_JSON);
meck_aws_body("ListExecutions") -> jsx:encode(?VALID_JSON);
meck_aws_body("ListStateMachines") -> jsx:encode(?VALID_JSON);
meck_aws_body("SendTaskFailure") -> <<>>;
meck_aws_body("SendTaskHearbeat") -> <<>>;
meck_aws_body("SendTaskSuccess") -> <<>>;
meck_aws_body("StartExecution") -> jsx:encode(?VALID_JSON);
meck_aws_body("StopExecution") -> jsx:encode(?VALID_JSON);
meck_aws_body("UpdateStateMachine") -> jsx:encode(?VALID_JSON).

test_create_activity() ->
    erlcloud_sf:configure("test-access-key", "test-secret-key"),
    TestFun = fun() -> erlcloud_sf:create_activity(?SOME_BINARY) end,
    ReqJson = jsx:encode(#{<<"name">> => ?SOME_BINARY}),
    check_map_response(TestFun), check_output_req(ReqJson).

test_create_sm() ->
    erlcloud_sf:configure("test-access-key", "test-secret-key"),
    TestFun = fun() -> erlcloud_sf:create_state_machine(?VALID_JSON, ?SOME_BINARY, ?OTHER_BINARY) end,
    ReqJson = jsx:encode(
        #{<<"definition">>  => jsx:encode(?VALID_JSON),
          <<"name">>        => ?SOME_BINARY,
          <<"roleArn">>     => ?OTHER_BINARY}),
    check_map_response(TestFun), check_output_req(ReqJson).

 test_delete_activity() ->
     erlcloud_sf:configure("test-access-key", "test-secret-key"),
     TestFun = fun() -> erlcloud_sf:delete_activity(?SOME_BINARY) end,
     ReqJson = jsx:encode(#{<<"activityArn">> => ?SOME_BINARY}),
     check_null_response(TestFun), check_output_req(ReqJson).

 test_delete_sm() ->
     erlcloud_sf:configure("test-access-key", "test-secret-key"),
     TestFun = fun() -> erlcloud_sf:delete_state_machine(?SOME_BINARY) end,
     ReqJson = jsx:encode(#{<<"stateMachineArn">>  => ?SOME_BINARY}),
     check_null_response(TestFun), check_output_req(ReqJson).

 test_describe_activity() ->
     erlcloud_sf:configure("test-access-key", "test-secret-key"),
     TestFun = fun() -> erlcloud_sf:describe_activity(?SOME_BINARY) end,
     ReqJson = jsx:encode(#{<<"activityArn">>  => ?SOME_BINARY}),
     check_map_response(TestFun), check_output_req(ReqJson).

 test_describe_execution() ->
     erlcloud_sf:configure("test-access-key", "test-secret-key"),
     TestFun = fun() -> erlcloud_sf:describe_execution(?SOME_BINARY) end,
     ReqJson = jsx:encode(#{<<"executionArn">>  => ?SOME_BINARY}),
     check_map_response(TestFun), check_output_req(ReqJson).

 test_describe_sm() ->
     erlcloud_sf:configure("test-access-key", "test-secret-key"),
     TestFun = fun() -> erlcloud_sf:describe_state_machine(?SOME_BINARY) end,
     ReqJson = jsx:encode(#{<<"stateMachineArn">>  => ?SOME_BINARY}),
     check_map_response(TestFun), check_output_req(ReqJson).

 test_describe_sm_for_execution() ->
     erlcloud_sf:configure("test-access-key", "test-secret-key"),
     TestFun = fun() -> erlcloud_sf:describe_state_machine_for_execution(?SOME_BINARY) end,
     ReqJson = jsx:encode(#{<<"executionArn">>  => ?SOME_BINARY}),
     check_map_response(TestFun), check_output_req(ReqJson).

test_get_activity_task() ->
    erlcloud_sf:configure("test-access-key", "test-secret-key"),
    TestFun = fun() -> erlcloud_sf:get_activity_task(?SOME_BINARY) end,
    ReqJson = jsx:encode(#{<<"activityArn">>  => ?SOME_BINARY}),
    check_map_response(TestFun), check_output_req(ReqJson).

test_get_activity_task_opt() ->
    erlcloud_sf:configure("test-access-key", "test-secret-key"),
    ValidOptions = #{<<"workerName">> => ?OTHER_BINARY},
    Options = maps:merge(?VALID_JSON, ValidOptions),
    Cfg = erlcloud_aws:default_config(),
    TestFun = fun() -> erlcloud_sf:get_activity_task(?SOME_BINARY, Options, Cfg) end,
    ReqJson = jsx:encode(maps:merge(#{<<"activityArn">>   => ?SOME_BINARY},
                                    ValidOptions)),
    check_map_response(TestFun), check_output_req(ReqJson).

test_get_execution_history() ->
    erlcloud_sf:configure("test-access-key", "test-secret-key"),
    TestFun = fun() -> erlcloud_sf:get_execution_history(?SOME_BINARY) end,
    ReqJson = jsx:encode(#{<<"executionArn">>  => ?SOME_BINARY}),
    check_map_response(TestFun), check_output_req(ReqJson).

test_get_execution_history_opt() ->
    erlcloud_sf:configure("test-access-key", "test-secret-key"),
    ValidOptions = #{<<"maxResults">>   => ?SOME_NUMBER,
                     <<"nextToken">>    => ?OTHER_BINARY,
                     <<"reverseOrder">> => ?SOME_BOOLEAN},
    Options = maps:merge(?VALID_JSON, ValidOptions),
    Cfg = erlcloud_aws:default_config(),
    TestFun = fun() -> erlcloud_sf:get_execution_history(?SOME_BINARY, Options, Cfg) end,
    ReqJson = jsx:encode(maps:merge(#{<<"executionArn">>   => ?SOME_BINARY},
                                    ValidOptions)),
    check_map_response(TestFun), check_output_req(ReqJson).

test_list_activities() ->
    erlcloud_sf:configure("test-access-key", "test-secret-key"),
    TestFun = fun() -> erlcloud_sf:list_activities() end,
    ReqJson = jsx:encode(#{}),
    check_map_response(TestFun), check_output_req(ReqJson).

test_list_activities_opt() ->
    erlcloud_sf:configure("test-access-key", "test-secret-key"),
    ValidOptions = #{<<"maxResults">>   => ?SOME_NUMBER,
                     <<"nextToken">>    => ?OTHER_BINARY},
    Options = maps:merge(?VALID_JSON, ValidOptions),
    Cfg = erlcloud_aws:default_config(),
    TestFun = fun() -> erlcloud_sf:list_activities(Options, Cfg) end,
    ReqJson = jsx:encode(ValidOptions),
    check_map_response(TestFun), check_output_req(ReqJson).

test_list_executions() ->
    erlcloud_sf:configure("test-access-key", "test-secret-key"),
    TestFun = fun() -> erlcloud_sf:list_executions(?SOME_BINARY) end,
    ReqJson = jsx:encode(#{<<"stateMachineArn">> => ?SOME_BINARY}),
    check_map_response(TestFun), check_output_req(ReqJson).

test_list_executions_opt() ->
    erlcloud_sf:configure("test-access-key", "test-secret-key"),
    ValidOptions = #{<<"maxResults">>   => ?SOME_NUMBER,
                     <<"nextToken">>    => ?OTHER_BINARY,
                     <<"statusFilter">> => ?OTHER_BINARY},
    Options = maps:merge(?VALID_JSON, ValidOptions),
    Cfg = erlcloud_aws:default_config(),
    TestFun = fun() -> erlcloud_sf:list_executions(?SOME_BINARY, Options, Cfg) end,
    ReqJson = jsx:encode(maps:merge(#{<<"stateMachineArn">> => ?SOME_BINARY},
                                    ValidOptions)),
    check_map_response(TestFun), check_output_req(ReqJson).

test_list_sms() ->
    erlcloud_sf:configure("test-access-key", "test-secret-key"),
    TestFun = fun() -> erlcloud_sf:list_state_machines() end,
    ReqJson = jsx:encode(#{}),
    check_map_response(TestFun), check_output_req(ReqJson).

test_list_sms_opt() ->
    erlcloud_sf:configure("test-access-key", "test-secret-key"),
    ValidOptions = #{<<"maxResults">>   => ?SOME_NUMBER,
                     <<"nextToken">>    => ?OTHER_BINARY},
    Options = maps:merge(?VALID_JSON, ValidOptions),
    Cfg = erlcloud_aws:default_config(),
    TestFun = fun() -> erlcloud_sf:list_state_machines(Options, Cfg) end,
    ReqJson = jsx:encode(ValidOptions),
    check_map_response(TestFun), check_output_req(ReqJson).

test_send_task_failure() ->
    erlcloud_sf:configure("test-access-key", "test-secret-key"),
    TestFun = fun() -> erlcloud_sf:send_task_failure(?SOME_BINARY) end,
    ReqJson = jsx:encode(#{<<"taskToken">> => ?SOME_BINARY}),
    check_null_response(TestFun), check_output_req(ReqJson).

test_send_task_failure_opt() ->
    erlcloud_sf:configure("test-access-key", "test-secret-key"),
    ValidOptions = #{<<"error">>   => ?SOME_BINARY,
                     <<"cause">>   => ?OTHER_BINARY},
    Options = maps:merge(?VALID_JSON, ValidOptions),
    Cfg = erlcloud_aws:default_config(),
    TestFun = fun() -> erlcloud_sf:send_task_failure(?SOME_BINARY, Options, Cfg) end,
    ReqJson = jsx:encode(maps:merge(#{<<"taskToken">> => ?SOME_BINARY},
                                    ValidOptions)),
    check_null_response(TestFun), check_output_req(ReqJson).

test_send_task_heartbeat() ->
    erlcloud_sf:configure("test-access-key", "test-secret-key"),
    TestFun = fun() -> erlcloud_sf:send_task_heartbeat(?SOME_BINARY) end,
    ReqJson = jsx:encode(#{<<"taskToken">> => ?SOME_BINARY}),
    check_null_response(TestFun), check_output_req(ReqJson).

test_send_task_success() ->
    erlcloud_sf:configure("test-access-key", "test-secret-key"),
    TestFun = fun() -> erlcloud_sf:send_task_success(?VALID_JSON, ?OTHER_BINARY) end,
    ReqJson = jsx:encode(#{<<"output">>     => jsx:encode(?VALID_JSON),
                           <<"taskToken">>  => ?OTHER_BINARY}),
    check_null_response(TestFun), check_output_req(ReqJson).

test_start_execution() ->
    erlcloud_sf:configure("test-access-key", "test-secret-key"),
    TestFun = fun() -> erlcloud_sf:start_execution(?SOME_BINARY) end,
    ReqJson = jsx:encode(#{<<"stateMachineArn">>    => ?SOME_BINARY,
                           <<"input">>              => #{}}),
    check_map_response(TestFun), check_output_req(ReqJson).

test_start_execution_opt() ->
    erlcloud_sf:configure("test-access-key", "test-secret-key"),
    ValidOptions = #{<<"name">>     => ?SOME_BINARY,
                     <<"input">>    => ?OTHER_BINARY},
    Options = maps:merge(?VALID_JSON, ValidOptions),
    Cfg = erlcloud_aws:default_config(),
    TestFun = fun() -> erlcloud_sf:start_execution(?SOME_BINARY, Options, Cfg) end,
    ReqJson = jsx:encode(maps:merge(#{<<"stateMachineArn">> => ?SOME_BINARY},
                                    ValidOptions)),
    check_map_response(TestFun), check_output_req(ReqJson).

test_stop_execution() ->
    erlcloud_sf:configure("test-access-key", "test-secret-key"),
    TestFun = fun() -> erlcloud_sf:stop_execution(?SOME_BINARY) end,
    ReqJson = jsx:encode(#{<<"executionArn">> => ?SOME_BINARY}),
    check_map_response(TestFun), check_output_req(ReqJson).

test_stop_execution_opt() ->
    erlcloud_sf:configure("test-access-key", "test-secret-key"),
    ValidOptions = #{<<"cause">>     => ?SOME_BINARY,
                     <<"error">>    => ?OTHER_BINARY},
    Options = maps:merge(?VALID_JSON, ValidOptions),
    Cfg = erlcloud_aws:default_config(),
    TestFun = fun() -> erlcloud_sf:stop_execution(?SOME_BINARY, Options, Cfg) end,
    ReqJson = jsx:encode(maps:merge(#{<<"executionArn">> => ?SOME_BINARY},
                                    ValidOptions)),
    check_map_response(TestFun), check_output_req(ReqJson).

test_update_sm() ->
    erlcloud_sf:configure("test-access-key", "test-secret-key"),
    TestFun = fun() -> erlcloud_sf:create_state_machine(?VALID_JSON, ?SOME_BINARY, ?OTHER_BINARY) end,
    ReqJson = jsx:encode(
        #{<<"definition">>  => jsx:encode(?VALID_JSON),
          <<"name">>        => ?SOME_BINARY,
          <<"roleArn">>     => ?OTHER_BINARY}),
    check_map_response(TestFun), check_output_req(ReqJson).


%*******************************************************************************
% HELPERS
%*******************************************************************************
check_map_response(TestFun) ->
    TestResult = TestFun(),
    ?assertMatch({ok, M} when is_map(M), TestResult),
    ?assertEqual({ok, ?VALID_JSON}, TestResult).

check_null_response(TestFun) ->
    ?assertEqual(ok, TestFun()).

check_output_req(ExpectedReq) ->
    ?assertMatch([{_, {erlcloud_httpc, request, [_, post, _, ExpectedReq, _, _]}, _}],
                 meck:history(erlcloud_httpc)).
