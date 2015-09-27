%% -*- mode: erlang;erlang-indent-level: 4;indent-tabs-mode: nil -*-
-module(erlcloud_inspector_tests).
-include_lib("eunit/include/eunit.hrl").
-include("erlcloud.hrl").

%% Unit tests for inspector.
%% These tests work by using meck to mock erlcloud_httpc. There are two classes of test: input and output.
%%
%% Input tests verify that different function args produce the desired JSON request.
%% An input test list provides a list of funs and the JSON that is expected to result.
%%
%% Output tests verify that the http response produces the correct return from the fun.
%% An output test lists provides a list of response bodies and the expected return.

%% The _inspector_test macro provides line number annotation to a test, similar to _test, but doesn't wrap in a fun
-define(_inspector_test(T), {?LINE, T}).
%% The _f macro is a terse way to wrap code in a fun. Similar to _test but doesn't annotate with a line number
-define(_f(F), fun() -> F end).

-export([validate_body/2]).

%%%===================================================================
%%% Common Test Values
%%%===================================================================
-define(ACCOUNT_ID, <<"1234567890">>).
-define(ASSESSMENT_ARN, <<"arn:assessment">>).
-define(ASSESSMENT_NAME, <<"Test Assessment">>).
-define(APPLICATION_ARN, <<"arn:application">>).
-define(APPLICATION_NAME, <<"Test Application">>).
-define(FINDING_ARN, <<"arn:finding">>).
-define(INSTANCE_ID, <<"i-123456">>).
-define(RULES_PACKAGE_ARN, <<"arn:rules_package">>).
-define(RESOURCE_GROUP_ARN, <<"arn:resource_group">>).
-define(RESOURCE_GROUP_TAGS, [[{<<"key">>, <<"key1">>}, {<<"values">>, [<<"foobar">>]}]]).
-define(ROLE_ARN, <<"arn:role">>).
-define(RUN_ARN, <<"arn:run">>).
-define(RUN_NAME, <<"Test Run">>).

-define(FINDING_DESCRIPTION,
        [{<<"key">>,
          [{<<"facility">>, ?RULES_PACKAGE_ARN},
           {<<"id">>, <<"Use Secure Protocols-description">>}]},
         {<<"parameters">>,
          [[{<<"name">>, <<"INSECURE_PROTOCOL_LIST">>},
            {<<"value">>, <<"smtp (port 25, Simple Mail Transfer)">>}],
           [{<<"name">>, <<"INSTANCE_ID">>},
            {<<"value">>, ?INSTANCE_ID}]]}]).


%%%===================================================================
%%% Test entry points
%%%===================================================================
operation_test_() ->
    {foreach,
     fun start/0,
     fun stop/1,
     [fun add_attributes_to_findings_tests/1,
      fun attach_assessment_and_rules_package_tests/1,
      fun create_application_tests/1,
      fun create_assessment_tests/1,
      fun create_resource_group_tests/1,
      fun delete_application_tests/1,
      fun delete_assessment_tests/1,
      fun delete_run_tests/1,
      fun describe_application_tests/1,
      fun describe_assessment_tests/1,
      fun describe_cross_account_access_role_tests/1,
      fun describe_finding_tests/1,
      fun describe_resource_group_tests/1,
      fun describe_rules_package_tests/1,
      fun describe_run_tests/1,
      fun detach_assessment_and_rules_package_tests/1,
      fun get_assessment_telemetry_tests/1,
      fun list_applications_tests/1,
      fun list_assessment_agents_tests/1,
      fun list_assessments_tests/1,
      fun list_attached_assessments_tests/1,
      fun list_attached_rules_packages_tests/1,
      fun list_findings_tests/1,
      fun list_rules_packages_tests/1,
      fun list_runs_tests/1,
      fun list_tags_for_resource_tests/1,
      fun localize_text_tests/1,
      fun preview_agents_for_resource_group_tests/1,
      fun register_cross_account_access_role_tests/1,
      fun remove_attributes_from_findings_tests/1,
      fun run_assessment_tests/1,
      fun set_tags_for_resource_tests/1,
      fun start_data_collection_tests/1,
      fun stop_data_collection_tests/1,
      fun update_application_tests/1,
      fun update_assessment_tests/1
     ]
    }.


start() ->
    meck:new(erlcloud_httpc),
    ok.


stop(_) ->
    meck:unload(erlcloud_httpc).


%%%===================================================================
%%% Actual test specifiers
%%%===================================================================

add_attributes_to_findings_tests(_) ->
    Action = "AddAttributesToFindings",
    Attributes = [{key, foo}, {value, bar}],
    Function = ?_f(erlcloud_inspector:add_attributes_to_findings(Attributes, [?FINDING_ARN])),
    PostData = jsx:encode([{<<"attributes">>, Attributes},
                           {<<"findingArns">>, [?FINDING_ARN]}]),
    Response = [{<<"message">>,<<"Success">>}],
    all_tests(Action, Function, PostData, Response).


attach_assessment_and_rules_package_tests(_) ->
    Action = "AttachAssessmentAndRulesPackage",
    Function = ?_f(erlcloud_inspector:attach_assessment_and_rules_package(?ASSESSMENT_ARN, ?RULES_PACKAGE_ARN)),
    PostData = jsx:encode([{<<"assessmentArn">>, ?ASSESSMENT_ARN},
                           {<<"rulesPackageArn">>, ?RULES_PACKAGE_ARN}]),
    Response = [{<<"message">>,
                 <<"Successfully attached ", ?ASSESSMENT_ARN/binary, " to ", ?RULES_PACKAGE_ARN/binary>>}],
    all_tests(Action, Function, PostData, Response).


create_application_tests(_) ->
    Action = "CreateApplication",
    Function = ?_f(erlcloud_inspector:create_application(?APPLICATION_NAME, ?RESOURCE_GROUP_ARN)),
    PostData = jsx:encode([{<<"applicationName">>, ?APPLICATION_NAME},
                           {<<"resourceGroupArn">>, ?RESOURCE_GROUP_ARN}]),
    Response = [{<<"applicationArn">>, ?APPLICATION_ARN}],
    all_tests(Action, Function, PostData, Response).


create_assessment_tests(_) ->
    Action = "CreateAssessment",
    DurationInSeconds = 3600,
    Function = ?_f(erlcloud_inspector:create_assessment(?APPLICATION_ARN, ?ASSESSMENT_NAME, DurationInSeconds)),
    PostData = jsx:encode([{<<"applicationArn">>, ?APPLICATION_ARN},
                           {<<"assessmentName">>, ?ASSESSMENT_NAME},
                           {<<"durationInSeconds">>, DurationInSeconds}]),
    Response = [{<<"assessmentArn">>, ?ASSESSMENT_ARN}],
    all_tests(Action, Function, PostData, Response).


create_resource_group_tests(_) ->
    Action = "CreateResourceGroup",
    Function = ?_f(erlcloud_inspector:create_resource_group(?RESOURCE_GROUP_TAGS)),
    PostData = jsx:encode([{<<"resourceGroupTags">>, jsx:encode(?RESOURCE_GROUP_TAGS)}]),
    Response = [{<<"resourceGroupArn">>, ?RESOURCE_GROUP_ARN}],
    all_tests(Action, Function, PostData, Response).


delete_application_tests(_) ->
    Action = "DeleteApplication",
    Function = ?_f(erlcloud_inspector:delete_application(?APPLICATION_ARN)),
    PostData = jsx:encode([{<<"applicationArn">>, ?APPLICATION_ARN}]),
    Response = [{<<"message">>, <<"Successfully deleted application ", ?APPLICATION_ARN/binary>>}],
    all_tests(Action, Function, PostData, Response).


delete_assessment_tests(_) ->
    Action = "DeleteAssessment",
    Function = ?_f(erlcloud_inspector:delete_assessment(?ASSESSMENT_ARN)),
    PostData = jsx:encode([{<<"assessmentArn">>, ?ASSESSMENT_ARN}]),
    Response = [{<<"message">>, <<"Successfully deleted assessment ", ?ASSESSMENT_ARN/binary>>}],
    all_tests(Action, Function, PostData, Response).


delete_run_tests(_) ->
    Action = "DeleteRun",
    Function = ?_f(erlcloud_inspector:delete_run(?RUN_ARN)),
    PostData = jsx:encode([{<<"runArn">>, ?RUN_ARN}]),
    Response = [{<<"message">>, <<"Successfully deleted run ", ?RUN_ARN/binary>>}],
    all_tests(Action, Function, PostData, Response).


describe_application_tests(_) ->
    Action = "DescribeApplication",
    Function = ?_f(erlcloud_inspector:describe_application(?APPLICATION_ARN)),
    PostData = jsx:encode([{<<"applicationArn">>, ?APPLICATION_ARN}]),
    Response = [{<<"application">>,
                 [{<<"applicationArn">>, ?APPLICATION_ARN},
                  {<<"applicationName">>, ?APPLICATION_NAME},
                  {<<"resourceGroupArn">>, ?RESOURCE_GROUP_ARN}]}],
    all_tests(Action, Function, PostData, Response).


describe_assessment_tests(_) ->
    Action = "DescribeAssessment",
    Function = ?_f(erlcloud_inspector:describe_assessment(?ASSESSMENT_ARN)),
    PostData = jsx:encode([{<<"assessmentArn">>, ?ASSESSMENT_ARN}]),
    Response = [{<<"assessment">>,
                 [{<<"applicationArn">>, ?APPLICATION_ARN},
                  {<<"assessmentArn">>, ?ASSESSMENT_ARN},
                  {<<"assessmentName">>, ?ASSESSMENT_NAME},
                  {<<"assessmentState">>, <<"COMPLETED">>},
                  {<<"dataCollected">>, true},
                  {<<"durationInSeconds">>, 3600},
                  {<<"endTime">>, 1452523814},
                  {<<"startTime">>, 1452520154},
                  {<<"userAttributesForFindings">>,[]}]}],
    all_tests(Action, Function, PostData, Response).


describe_cross_account_access_role_tests(_) ->
    Action = "DescribeAssessment",
    Function = ?_f(erlcloud_inspector:describe_cross_account_access_role()),
    PostData = jsx:encode([{}]),
    Response = [{<<"roleArn">>, ?ROLE_ARN},
                {<<"valid">>, true}],
    all_tests(Action, Function, PostData, Response).


describe_finding_tests(_) ->
    Action = "DescribeFinding",
    Function = ?_f(erlcloud_inspector:describe_finding(?FINDING_ARN)),
    PostData = jsx:encode([{<<"findingArn">>, ?FINDING_ARN}]),
    Response = [{<<"finding">>,
                 [{<<"agentId">>, ?INSTANCE_ID},
                  {<<"attributes">>,
                   [[{<<"key">>, <<"INSECURE_PROTOCOL_LIST">>},
                     {<<"value">>, <<"smtp (port 25, Simple Mail Transfer)">>}],
                    [{<<"key">>, <<"INSTANCE_ID">>},
                     {<<"value">>, ?INSTANCE_ID}]]},
                  {<<"description">>,
                   [{<<"key">>,
                     [{<<"facility">>, ?RULES_PACKAGE_ARN},
                      {<<"id">>, <<"Use Secure Protocols-description">>}]},
                    {<<"parameters">>,
                     [[{<<"name">>, <<"INSECURE_PROTOCOL_LIST">>},
                       {<<"value">>, <<"smtp (port 25, Simple Mail Transfer)">>}],
                      [{<<"name">>, <<"INSTANCE_ID">>},
                       {<<"value">>, ?INSTANCE_ID}]]}]},
                  {<<"finding">>,
                   [{<<"key">>,
                     [{<<"facility">>, ?RULES_PACKAGE_ARN},
                      {<<"id">>, <<"Use Secure Protocols-finding">>}]},
                    {<<"parameters">>,
                     [[{<<"name">>, <<"INSECURE_PROTOCOL_LIST">>},
                       {<<"value">>, <<"smtp (port 25, Simple Mail Transfer)">>}],
                      [{<<"name">>, <<"INSTANCE_ID">>},
                       {<<"value">>, ?INSTANCE_ID}]]}]},
                  {<<"findingArn">>, ?FINDING_ARN},
                  {<<"recommendation">>,
                   [{<<"key">>,
                     [{<<"facility">>, ?RULES_PACKAGE_ARN},
                      {<<"id">>, <<"Use Secure Protocols-recommendation">>}]},
                    {<<"parameters">>,
                     [[{<<"name">>, <<"INSECURE_PROTOCOL_LIST">>},
                       {<<"value">>, <<"smtp (port 25, Simple Mail Transfer)">>}],
                      [{<<"name">>, <<"INSTANCE_ID">>},
                       {<<"value">>, ?INSTANCE_ID}]]}]},
                  {<<"ruleName">>, <<"Use Secure Protocols">>},
                  {<<"rulesPackageArn">>, ?RULES_PACKAGE_ARN},
                  {<<"runArn">>, ?RUN_ARN},
                  {<<"severity">>, <<"Informational">>},
                  {<<"userAttributes">>,[]}]}],
    all_tests(Action, Function, PostData, Response).


describe_resource_group_tests(_) ->
    Action = "DescribeResourceGroup",
    Function = ?_f(erlcloud_inspector:describe_resource_group(?RESOURCE_GROUP_ARN)),
    PostData = jsx:encode([{<<"resourceGroupArn">>, ?RESOURCE_GROUP_ARN}]),
    Response = [{<<"resourceGroup">>,
                 [{<<"resourceGroupArn">>, ?RESOURCE_GROUP_ARN},
                  {<<"resourceGroupTags">>, jsx:encode(?RESOURCE_GROUP_TAGS)}]}],
    all_tests(Action, Function, PostData, Response).


describe_rules_package_tests(_) ->
    Action = "DescribeRulesPackage",
    Function = ?_f(erlcloud_inspector:describe_rules_package(?RULES_PACKAGE_ARN)),
    PostData = jsx:encode([{<<"rulesPackageArn">>, ?RULES_PACKAGE_ARN}]),
    Response = [{<<"rulesPackage">>,
                 [{<<"description">>,
                   [{<<"key">>,
                     [{<<"facility">>, ?RULES_PACKAGE_ARN},
                      {<<"id">>, <<"rule package description id">>}]},
                    {<<"parameters">>, []}]},
                  {<<"provider">>, <<"Amazon Web Services, Inc.">>},
                  {<<"rulesPackageArn">>, ?RULES_PACKAGE_ARN},
                  {<<"rulesPackageName">>, <<"Authentication Best Practices">>},
                  {<<"version">>, <<"1.0">>}]}],
    all_tests(Action, Function, PostData, Response).


describe_run_tests(_) ->
    Action = "DescribeRun",
    Function = ?_f(erlcloud_inspector:describe_run(?RUN_ARN)),
    PostData = jsx:encode([{<<"runArn">>, ?RUN_ARN}]),
    Response = [{<<"run">>,
                 [{<<"assessmentArn">>, ?ASSESSMENT_ARN},
                  {<<"completionTime">>, 1452523816.858},
                  {<<"creationTime">>, 1452523814.357},
                  {<<"rulesPackages">>, [?RULES_PACKAGE_ARN]},
                  {<<"runArn">>, ?RUN_ARN},
                  {<<"runName">>, ?RUN_NAME},
                  {<<"runState">>, <<"Completed">>}]}],
    all_tests(Action, Function, PostData, Response).


detach_assessment_and_rules_package_tests(_) ->
    Action = "DetachAssessmentAndRulesPackage",
    Function = ?_f(erlcloud_inspector:detach_assessment_and_rules_package(?ASSESSMENT_ARN, ?RULES_PACKAGE_ARN)),
    PostData = jsx:encode([{<<"assessmentArn">>, ?ASSESSMENT_ARN},
                           {<<"rulesPackageArn">>, ?RULES_PACKAGE_ARN}]),
    Response = [{<<"message">>,
                 <<"Successfully dettached ", ?ASSESSMENT_ARN/binary, " to ", ?RULES_PACKAGE_ARN/binary>>}],
    all_tests(Action, Function, PostData, Response).


get_assessment_telemetry_tests(_) ->
    Action = "GetAssessmentTelemetry",
    Function = ?_f(erlcloud_inspector:get_assessment_telemetry(?ASSESSMENT_ARN)),
    PostData = jsx:encode([{<<"assessmentArn">>, ?ASSESSMENT_ARN}]),
    Response = [{<<"telemetry">>,[]}],
    all_tests(Action, Function, PostData, Response).


list_applications_tests(_) ->
    Action = "ListApplications",
    Function = ?_f(erlcloud_inspector:list_applications()),
    PostData = jsx:encode([{}]),
    Response = [{<<"applicationArnList">>, [?APPLICATION_ARN]}],
    all_tests(Action, Function, PostData, Response).


list_assessment_agents_tests(_) ->
    Action = "ListAssessmentAgents",
    Function = ?_f(erlcloud_inspector:list_assessment_agents(?ASSESSMENT_ARN)),
    PostData = jsx:encode([{<<"assessmentArn">>, ?ASSESSMENT_ARN}]),
    Response = [{<<"agentList">>,
                 [[{<<"accountId">>, ?ACCOUNT_ID},
                   {<<"agentHealth">>, <<"HEALTHY">>},
                   {<<"agentHealthCode">>, <<"HEALTHY">>},
                   {<<"agentId">>, ?INSTANCE_ID},
                   {<<"assessmentArn">>, ?ASSESSMENT_ARN},
                   {<<"telemetry">>, []}]]}],
    all_tests(Action, Function, PostData, Response).


list_assessments_tests(_) ->
    Action = "ListAssessments",
    Function = ?_f(erlcloud_inspector:list_assessments()),
    PostData = jsx:encode([{}]),
    Response = [{<<"assessmentArnList">>, [?ASSESSMENT_ARN]}],
    all_tests(Action, Function, PostData, Response).


list_attached_assessments_tests(_) ->
    Action = "ListAttachedAssessments",
    Function = ?_f(erlcloud_inspector:list_attached_assessments(?RULES_PACKAGE_ARN)),
    PostData = jsx:encode([{<<"rulesPackageArn">>, ?RULES_PACKAGE_ARN}]),
    Response = [{<<"assessmentArnList">>, [?ASSESSMENT_ARN]}],
    all_tests(Action, Function, PostData, Response).


list_attached_rules_packages_tests(_) ->
    Action = "ListAttachedRulesPackages",
    Function = ?_f(erlcloud_inspector:list_attached_rules_packages(?ASSESSMENT_ARN)),
    PostData = jsx:encode([{<<"assessmentArn">>, ?ASSESSMENT_ARN}]),
    Response = [{<<"rulesPackageArnList">>, [?RULES_PACKAGE_ARN]}],
    all_tests(Action, Function, PostData, Response).


list_findings_tests(_) ->
    Action = "ListFindings",
    Function = ?_f(erlcloud_inspector:list_findings()),
    PostData = jsx:encode([{}]),
    Response = [{<<"findingArnList">>, [?FINDING_ARN]}],
    all_tests(Action, Function, PostData, Response).


list_rules_packages_tests(_) ->
    Action = "ListRulesPackages",
    Function = ?_f(erlcloud_inspector:list_rules_packages()),
    PostData = jsx:encode([{}]),
    Response = [{<<"rulesPackageArnList">>, [?RULES_PACKAGE_ARN]}],
    all_tests(Action, Function, PostData, Response).


list_runs_tests(_) ->
    Action = "ListRuns",
    Function = ?_f(erlcloud_inspector:list_runs()),
    PostData = jsx:encode([{}]),
    Response = [{<<"runArnList">>, [?RUN_ARN]}],
    all_tests(Action, Function, PostData, Response).


list_tags_for_resource_tests(_) ->
    Action = "ListTagsForResource",
    Function = ?_f(erlcloud_inspector:list_tags_for_resource(?ASSESSMENT_ARN)),
    PostData = jsx:encode([{<<"resourceArn">>, ?ASSESSMENT_ARN}]),
    Response = [{<<"tagList">>, []}],
    all_tests(Action, Function, PostData, Response).


localize_text_tests(_) ->
    Action = "LocalizeText",
    Function = ?_f(erlcloud_inspector:localize_text(?FINDING_DESCRIPTION)),
    PostData = jsx:encode([{<<"locale">>, <<"en_US">>},
                           {<<"localizedTexts">>, [?FINDING_DESCRIPTION]}]),
    Response = [{<<"results">>,
                 [<<"This rule helps determine whether your EC2 instances allow support for insecure and unencrypted ports/services such as FTP, Telnet, HTTP, IMAP, POP version 3, SMTP, SNMP versions 1 and 2, rsh, and rlogin. ">>]}],
    all_tests(Action, Function, PostData, Response).


preview_agents_for_resource_group_tests(_) ->
    Action = "PreviewAgentsForResourceGroup",
    Function = ?_f(erlcloud_inspector:preview_agents_for_resource_group(?RESOURCE_GROUP_ARN)),
    PostData = jsx:encode([{<<"resourceGroupArn">>, ?RESOURCE_GROUP_ARN}]),
    Response = [{<<"agentPreviewList">>,[]}],
    all_tests(Action, Function, PostData, Response).


register_cross_account_access_role_tests(_) ->
    Action = "RegisterCrossAccountAccessRole",
    Function = ?_f(erlcloud_inspector:register_cross_account_access_role(?ROLE_ARN)),
    PostData = jsx:encode([{<<"roleArn">>, ?ROLE_ARN}]),
    Response = [{<<"message">>, <<"Cross-account access role registered successfully">>}],
    all_tests(Action, Function, PostData, Response).


remove_attributes_from_findings_tests(_) ->
    Action = "AddAttributesToFindings",
    AttributeKeys = [<<"foo">>],
    Function = ?_f(erlcloud_inspector:remove_attributes_from_findings(AttributeKeys, [?FINDING_ARN])),
    PostData = jsx:encode([{<<"attributeKeys">>, AttributeKeys},
                           {<<"findingArns">>, [?FINDING_ARN]}]),
    Response = [{<<"message">>, <<"Success">>}],
    all_tests(Action, Function, PostData, Response).


run_assessment_tests(_) ->
    Action = "RunAssessment",
    Function = ?_f(erlcloud_inspector:run_assessment(?ASSESSMENT_ARN, ?RUN_NAME)),
    PostData = jsx:encode([{<<"assessmentArn">>, ?ASSESSMENT_ARN},
                           {<<"runName">>, ?RUN_NAME}]),
    Response = [{<<"runArn">>, ?RUN_ARN}],
    all_tests(Action, Function, PostData, Response).


set_tags_for_resource_tests(_) ->
    Action = "SetTagsForResource",
    Tags = [[{<<"Key">>, <<"foo">>}, {<<"Value">>, <<"bar">>}]],
    Function = ?_f(erlcloud_inspector:set_tags_for_resource(?ASSESSMENT_ARN, Tags)),
    PostData = jsx:encode([{<<"resourceArn">>, ?ASSESSMENT_ARN},
                           {<<"tags">>, Tags}]),
    Response = [{<<"message">>, <<"Success">>}],
    all_tests(Action, Function, PostData, Response).


start_data_collection_tests(_) ->
    Action = "StartDataCollection",
    Function = ?_f(erlcloud_inspector:start_data_collection(?ASSESSMENT_ARN)),
    PostData = jsx:encode([{<<"assessmentArn">>, ?ASSESSMENT_ARN}]),
    Response = [{<<"message">>, <<"Success">>}],
    all_tests(Action, Function, PostData, Response).


stop_data_collection_tests(_) ->
    Action = "StopDataCollection",
    Function = ?_f(erlcloud_inspector:stop_data_collection(?ASSESSMENT_ARN)),
    PostData = jsx:encode([{<<"assessmentArn">>, ?ASSESSMENT_ARN}]),
    Response = [{<<"message">>, <<"Success">>}],
    all_tests(Action, Function, PostData, Response).


update_application_tests(_) ->
    Action = "UpdateApplication",
    Function = ?_f(erlcloud_inspector:update_application(?APPLICATION_ARN, ?APPLICATION_NAME, ?RESOURCE_GROUP_ARN)),
    PostData = jsx:encode([{<<"applicationArn">>, ?APPLICATION_ARN},
                           {<<"applicationName">>, ?APPLICATION_NAME},
                           {<<"resourceGroupArn">>, ?RESOURCE_GROUP_ARN}]),
    Response = [{<<"message">>,
      <<"Successfully updated application ", ?APPLICATION_ARN/binary>>}],
    all_tests(Action, Function, PostData, Response).


update_assessment_tests(_) ->
    Action = "UpdateAssessment",
    DurationInSeconds = 3600,
    Function = ?_f(erlcloud_inspector:update_assessment(?ASSESSMENT_ARN, ?ASSESSMENT_NAME, DurationInSeconds)),
    PostData = jsx:encode([{<<"assessmentArn">>, ?ASSESSMENT_ARN},
                           {<<"assessmentName">>, ?ASSESSMENT_NAME},
                           {<<"durationInSeconds">>, DurationInSeconds}]),
    Response = [{<<"message">>,
      <<"Successfully updated assessment ", ?ASSESSMENT_ARN/binary>>}],
    all_tests(Action, Function, PostData, Response).


%%%===================================================================
%%% Input test helpers
%%%===================================================================

-type expected_body() :: string().

sort_json([{_, _} | _] = Json) ->
    %% Value is an object
    SortedChildren = [{K, sort_json(V)} || {K,V} <- Json],
    lists:keysort(1, SortedChildren);
sort_json([_|_] = Json) ->
    %% Value is an array
    [sort_json(I) || I <- Json];
sort_json(V) ->
    V.

%% verifies that the parameters in the body match the expected parameters
-spec validate_body(binary(), expected_body()) -> ok.
validate_body(Body, Expected) ->
    Want = sort_json(jsx:decode(Expected)),
    Actual = sort_json(jsx:decode(Body)),
    case Want =:= Actual of
        true -> ok;
        false ->
            ?debugFmt("~nEXPECTED~n~p~nACTUAL~n~p~n", [Want, Actual])
    end,
    ?assertEqual(Want, Actual).


%% returns the mock of the erlcloud_httpc function input tests expect to be called.
%% Validates the request body and responds with the provided response.
-spec input_expect(string(), expected_body()) -> fun().
input_expect(Response, Expected) ->
    fun(_Url, post, _Headers, Body, _Timeout, _Config) ->
            validate_body(Body, Expected),
            {ok, {{200, "OK"}, [], Response}}
    end.


%% input_test converts an input_test specifier into an eunit test generator
-type input_test_spec() :: {pos_integer(), {fun(), expected_body()} | {string(), fun(), expected_body()}}.
-spec input_test(string(), input_test_spec()) -> tuple().
input_test(Response, {Line, {Description, Fun, Expected}})
  when is_list(Description) ->
    {Description,
     {Line,
      fun() ->
              meck:expect(erlcloud_httpc, request, input_expect(Response, Expected)),
              erlcloud_inspector:configure(string:copies("A", 20), string:copies("a", 40)),
              Fun()
      end}}.


%% input_tests converts a list of input_test specifiers into an eunit test generator
-spec input_tests(string(), [input_test_spec()]) -> [tuple()].
input_tests(Response, Tests) ->
    [input_test(Response, Test) || Test <- Tests].

%%%===================================================================
%%% Output test helpers
%%%===================================================================

%% returns the mock of the erlcloud_httpc function output tests expect to be called.
-spec output_expect(string()) -> fun().
output_expect(Response) ->
    fun(_Url, post, _Headers, _Body, _Timeout, _Config) ->
            {ok, {{200, "OK"}, [], Response}}
    end.

%% output_test converts an output_test specifier into an eunit test generator
-type output_test_spec() :: {pos_integer(), {string(), term()} | {string(), string(), term()}}.
-spec output_test(fun(), output_test_spec()) -> tuple().
output_test(Fun, {Line, {Description, Response, Result}}) ->
    {Description,
     {Line,
      fun() ->
              meck:expect(erlcloud_httpc, request, output_expect(Response)),
              erlcloud_inspector:configure(string:copies("A", 20), string:copies("a", 40)),
              Actual = Fun(),
              case Result =:= Actual of
                  true -> ok;
                  false ->
                      ?debugFmt("~nEXPECTED~n~p~nACTUAL~n~p~n", [Result, Actual])
              end,
              ?assertEqual(Result, Actual)
      end}}.


%% output_tests converts a list of output_test specifiers into an eunit test generator
-spec output_tests(fun(), [output_test_spec()]) -> [term()].
output_tests(Fun, Tests) ->
    [output_test(Fun, Test) || Test <- Tests].

all_tests(Action, Function, PostData, Response) ->
    InputTests = [?_inspector_test(
                  {Action ++ " input test",
                   Function,
                   PostData
                  }
                  )],
    OutputTests = [?_inspector_test(
                   {Action ++ " example response",
                    jsx:encode(Response),
                    {ok, Response}}
                   )],
    
    input_tests(<<>>, InputTests) ++
        output_tests(Function, OutputTests).