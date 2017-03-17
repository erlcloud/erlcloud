-module(erlcloud_emr_tests).
-include_lib("eunit/include/eunit.hrl").

%% Unit tests for emr.
%% These tests work by using meck to mock erlcloud_httpc. There are two classes
%% of test: input and output.
%%
%% Input tests verify that different function args produce the desired query
%% parameters.
%%
%% An input test list provides a list of funs and the parameters that are
%% expected to result.
%%
%% Output tests verify that the http response produces the correct return from
%% the fun.  An output test lists provides a list of response bodies and the
%% expected return.


%% The _emr_test macro provides line number annotation to a test,
%% similar to _test, but doesn't wrap in a fun
-define(_emr_test(T), {?LINE, T}).
%% The _f macro is a terse way to wrap code in a fun.
%% Similar to _test but doesn't annotate with a line number
-define(_f(F), fun() -> F end).

-define(ACCESS_KEY_ID, string:copies("A", 20)).
-define(SECRET_ACCESS_KEY, string:copies("a", 40)).

%% These values are from the documentation for the RunJobFlow API
%% http://docs.aws.amazon.com/ElasticMapReduce/latest/API/API_RunJobFlow.html

-define(RUN_JOB_FLOW_INPUT, [{<<"Name">>,<<"Development Job Flow">>},
 {<<"Instances">>,
  [{<<"KeepJobFlowAliveWhenNoSteps">>,<<"false">>},
   {<<"TerminationProtected">>,<<"false">>},
   {<<"InstanceGroups">>,
    [[{<<"Name">>,<<"Master Instance Group">>},
      {<<"InstanceRole">>,<<"MASTER">>},
      {<<"InstanceCount">>,1},
      {<<"InstanceType">>,<<"m1.small">>},
      {<<"Market">>,<<"ON_DEMAND">>}]]}]},
 {<<"Steps">>,
  [[{<<"Name">>,<<"Example Streaming Step">>},
    {<<"ActionOnFailure">>,<<"CANCEL_AND_WAIT">>},
    {<<"HadoopJarStep">>,
     [{<<"Jar">>,<<"/home/hadoop/contrib/streaming/hadoop-streaming.jar">>},
      {<<"Args">>,
       [<<"-input">>,<<"s3://elasticmapreduce/samples/wordcount/input">>,
        <<"-output">>,<<"s3://examples-bucket/example-output">>,<<"-mapper">>,
        <<"s3://elasticmapreduce/samples/wordcount/wordSplitter.py">>,
        <<"-reducer">>,<<"aggregate">>]}]}]]},
 {<<"BootstrapActions">>,[]},
 {<<"VisibleToAllUsers">>,<<"false">>},
 {<<"NewSupportedProduct">>,[]},
 {<<"AmiVersion">>,<<"3.8.0">>}]).

-define(RUN_JOB_FLOW_OUTPUT, [{<<"JobFlowId">>,<<"j-ZKIY4CKQRX72">>}]).

%% Example values from
%% http://docs.aws.amazon.com/ElasticMapReduce/latest/API/API_AddJobFlowSteps.html

-define(JOB_FLOW_ID, <<"j-3TS0OIYO4NFN">>).
-define(JOB_FLOW_STEPS, [[{<<"Name">>,<<"Example Jar Step">>},
  {<<"ActionOnFailure">>,<<"CANCEL_AND_WAIT">>},
  {<<"HadoopJarStep">>,
   [{<<"Jar">>,
     <<"s3n:\\/\\/elasticmapreduce\\/samples\\/cloudburst\\/cloudburst.jar">>},
    {<<"Args">>,
     [<<"s3n:\\/\\/elasticmapreduce\\/samples\\/cloudburst\\/input\\/s_suis.br">>,
      <<"s3n:\\/\\/elasticmapreduce\\/samples\\/cloudburst\\/input\\/100k.br">>,
      <<"s3n:\\/\\/examples-bucket\\/cloudburst\\/output">>,<<"36">>,<<"3">>,
      <<"0">>,<<"1">>,<<"240">>,<<"48">>,<<"24">>,<<"24">>,<<"128">>,
      <<"16">>]}]}]]).

-define(ADD_JOB_FLOW_INPUT, [{<<"JobFlowId">>,<<"j-3TS0OIYO4NFN">>},
 {<<"Steps">>,
  [[{<<"Name">>,<<"Example Jar Step">>},
    {<<"ActionOnFailure">>,<<"CANCEL_AND_WAIT">>},
    {<<"HadoopJarStep">>,
     [{<<"Jar">>,
       <<"s3n:\\/\\/elasticmapreduce\\/samples\\/cloudburst\\/cloudburst.jar">>},
      {<<"Args">>,
       [<<"s3n:\\/\\/elasticmapreduce\\/samples\\/cloudburst\\/input\\/s_suis.br">>,
        <<"s3n:\\/\\/elasticmapreduce\\/samples\\/cloudburst\\/input\\/100k.br">>,
        <<"s3n:\\/\\/examples-bucket\\/cloudburst\\/output">>,<<"36">>,
        <<"3">>,<<"0">>,<<"1">>,<<"240">>,<<"48">>,<<"24">>,<<"24">>,
        <<"128">>,<<"16">>]}]}]]}]).

%% AddJobFlowSteps doesn't have an example output format.

-define(ADD_JOB_FLOW_OUTPUT, [{<<"StepIds">>,[?JOB_FLOW_ID]}]).

%% Values from
%% http://docs.aws.amazon.com/ElasticMapReduce/latest/API/API_TerminateJobFlows.html

-define(TERMINATE_JOB_FLOW_INPUT, [{<<"JobFlowIds">>,[<<"j-3TS0OIYO4NFN">>]}]).

%% TerminateJobFlows doesn't have an example output format.

%% DescribeSteps doesn't have any example input/output

%% Values from
%% http://docs.aws.amazon.com/ElasticMapReduce/latest/API/API_SetTerminationProtection.html

-define(SET_TERMINATION_PROTECTION_INPUT, [
    {<<"JobFlowIds">>, [<<"j-3TS0OIYO4NFN">>]},
    {<<"TerminationProtected">>, true}
]).

%%==============================================================================
%% Test generator functions
%%==============================================================================

erlcloud_emr_test_() ->
    {foreach, fun start/0, fun stop/1, [
        fun emr_input_tests/1,
        fun emr_output_tests/1
    ]}.

%%==============================================================================
%% Setup functions
%%==============================================================================

start() ->
    meck:new(erlcloud_httpc).

stop(_) ->
    meck:unload(erlcloud_httpc).

%%==============================================================================
%% Test functions
%%==============================================================================


emr_input_tests(_) ->
    input_tests(jsx:encode(?RUN_JOB_FLOW_OUTPUT), [
        ?_emr_test(
            {"Run job flow input",
             ?_f(erlcloud_emr:run_job_flow(?RUN_JOB_FLOW_INPUT)),
             ?RUN_JOB_FLOW_INPUT}
        ),
        ?_emr_test(
            {"Add job flows input",
             ?_f(erlcloud_emr:add_job_flow_steps(?JOB_FLOW_ID, ?JOB_FLOW_STEPS)),
             ?ADD_JOB_FLOW_INPUT}
        ),
        ?_emr_test(
            {"Terminate job flows input",
             ?_f(erlcloud_emr:terminate_job_flows([?JOB_FLOW_ID])),
             ?TERMINATE_JOB_FLOW_INPUT}
        ),
        ?_emr_test(
            {"Set termination protection - input test",
             ?_f(erlcloud_emr:set_termination_protection([?JOB_FLOW_ID], true)),
             ?SET_TERMINATION_PROTECTION_INPUT}
        )
    ]).


emr_output_tests(_) ->
    output_tests(?_f(erlcloud_emr:run_job_flow(?RUN_JOB_FLOW_INPUT)), [
        ?_emr_test(
            {"Job flow output test",
             jsx:encode(?RUN_JOB_FLOW_OUTPUT),
             {ok, ?RUN_JOB_FLOW_OUTPUT}}
        )
    ]),
    output_tests(?_f(erlcloud_emr:add_job_flow_steps(?JOB_FLOW_ID, ?JOB_FLOW_STEPS)), [
        ?_emr_test(
           {"Add job flow output test",
            jsx:encode(?ADD_JOB_FLOW_OUTPUT),
            {ok, ?ADD_JOB_FLOW_OUTPUT}}
        )
    ]).

%%==============================================================================
%% Internal functions
%%==============================================================================


input_tests(ResponseBody, Tests) ->
    [input_test(ResponseBody, Test) || Test <- Tests].


input_test(ResponseBody, {Line, {Description, Fun, ExpectedParams}}) ->
    {Description, {Line,
        fun() ->
            meck:expect(
                erlcloud_httpc,
                request,
                fun(_Url, post, _Headers, RequestBody, _Timeout, _Config) ->
                    ActualParams = jsx:decode(RequestBody),
                    ?assertEqual(sort_json(ExpectedParams), sort_json(ActualParams)),
                    {ok, {{200, "OK"}, [], ResponseBody}}
                end
            ),
            erlcloud_emr:configure(?ACCESS_KEY_ID, ?SECRET_ACCESS_KEY),
            Fun()
        end
    }}.


output_tests(Fun, Tests) ->
    [output_test(Fun, Test) || Test <- Tests].


output_test(Fun, {Line, {Description, ResponseBody, Expected}}) ->
    {Description, {Line,
        fun() ->
            meck:expect(
                erlcloud_httpc,
                request,
                fun(_Url, post, _Headers, _Body, _Timeout, _Config) ->
                    {ok, {{200, "OK"}, [], ResponseBody}}
                end
            ),
            erlcloud_emr:configure(?ACCESS_KEY_ID, ?SECRET_ACCESS_KEY),
            ?assertEqual(Expected, _Actual = Fun())
        end
    }}.


sort_json([{_, _} | _] = Json) ->
    Sorted = [{Key, sort_json(Value)} || {Key, Value} <- Json],
    lists:keysort(1, Sorted);
sort_json([_ | _] = Json) ->
    [sort_json(Item) || Item <- Json];
sort_json(Value) ->
    Value.
