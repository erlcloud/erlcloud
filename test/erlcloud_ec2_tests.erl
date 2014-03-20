%% -*- mode: erlang;erlang-indent-level: 4;indent-tabs-mode: nil -*-
-module(erlcloud_ec2_tests).
-include_lib("eunit/include/eunit.hrl").
-include("erlcloud.hrl").
-include("erlcloud_ec2.hrl").

%% Unit tests for ec2.
%% These tests work by using meck to mock httpc. There are two classes of test: input and output.
%%
%% Input tests verify that different function args produce the desired query parameters.
%% An input test list provides a list of funs and the parameters that are expected to result.
%%
%% Output tests verify that the http response produces the correct return from the fun.
%% An output test lists provides a list of response bodies and the expected return.

%% The _ec2_test macro provides line number annotation to a test, similar to _test, but doesn't wrap in a fun
-define(_ec2_test(T), {?LINE, T}).
%% The _f macro is a terse way to wrap code in a fun. Similar to _test but doesn't annotate with a line number
-define(_f(F), fun() -> F end).
                            
%%%===================================================================
%%% Test entry points
%%%===================================================================

describe_tags_test_() ->
    {foreach,
     fun start/0,
     fun stop/1,
     [fun describe_tags_input_tests/1,
      fun describe_tags_output_tests/1]}.

start() ->
    meck:new(httpc, [unstick]),
    ok.

stop(_) ->
    meck:unload(httpc).

%%%===================================================================
%%% Input test helpers
%%%===================================================================

%% common_params returns the list of parameters that are not validated by these tests.
%% They should be checked by lower level unit tests.
-spec common_params() -> [string()].
common_params() ->
    ["AWSAccessKeyId",
     "SignatureMethod",
     "SignatureVersion",
     "Timestamp",
     "Version",
     "Signature"].

%% validate_param checks that the query parameter is either a common param or expected
%% by the test case. If expected, returns expected with the param deleted to be used in
%% subsequent calls.
-type expected_param() :: {string(), string()}.
-spec validate_param(string(), [expected_param()]) -> [expected_param()].
validate_param(Param, Expected) ->
    case string:tokens(Param, "=") of
        [Key, Value] -> 
            ok;
        [Key] ->
            Value = "",
            ok
    end,
    case lists:member(Key, common_params()) of
        true ->
            Expected;
        false ->
            Expected1 = lists:delete({Key, Value}, Expected),
            %?debugFmt("EXPECTED ~p~nEXPECTED1 ~p", [Expected, Expected1]),
            case length(Expected) - 1 =:= length(Expected1) of
                true -> ok;
                false -> 
                    ?debugFmt("Parameter not expected: ~p", [{Key, Value}])
            end,
            ?assertEqual(length(Expected) - 1, length(Expected1)),
            Expected1
    end.

%% verifies that the parameters in the body match the expected parameters
-spec validate_params(binary(), [expected_param()]) -> ok.
validate_params(Body, Expected) ->
    ParamList = string:tokens(binary_to_list(Body), "&"),
    Remain = lists:foldl(fun validate_param/2, Expected, ParamList),
    ?assertEqual([], Remain).

%% returns the mock of the httpc function input tests expect to be called.
%% Validates the query body and responds with the provided response.
-spec input_expect(string(), [expected_param()]) -> fun().
input_expect(Response, Expected) ->
    fun(post, {_Url, [] = _Headers, _ContentType, Body}, _, []) -> 
            validate_params(Body, Expected),
            {ok, {{0, 200, 0}, 0, Response}} 
    end.

%% input_test converts an input_test specifier into an eunit test generator
-type input_test_spec() :: {pos_integer(), {fun(), [expected_param()]} | {string(), fun(), [expected_param()]}}.
-spec input_test(string(), input_test_spec()) -> tuple().
input_test(Response, {Line, {Description, Fun, Params}}) when
      is_list(Description) ->
    {Description, 
     {Line,
      fun() ->
              meck:expect(httpc, request, input_expect(Response, Params)),
              %% Configure to make sure there is a key. Would like to do this in start, but
              %% that isn't called in the same process
              erlcloud_ec2:configure(string:copies("A", 20), string:copies("a", 40)),
              Fun()
      end}}.
%% input_test(Response, {Line, {Fun, Params}}) ->
%%     input_test(Response, {Line, {"", Fun, Params}}).

%% input_tests converts a list of input_test specifiers into an eunit test generator
-spec input_tests(string(), [input_test_spec()]) -> [tuple()].
input_tests(Response, Tests) ->
    [input_test(Response, Test) || Test <- Tests].

%%%===================================================================
%%% Output test helpers
%%%===================================================================

%% returns the mock of the httpc function output tests expect to be called.
-spec output_expect(string()) -> fun().
output_expect(Response) ->
    fun(post, {_Url, [] = _Headers, _ContentType, _Body}, _, []) -> 
            {ok, {{0, 200, 0}, 0, Response}} 
    end.

%% output_test converts an output_test specifier into an eunit test generator
-type output_test_spec() :: {pos_integer(), {string(), term()} | {string(), string(), term()}}.
-spec output_test(fun(), output_test_spec()) -> tuple().
output_test(Fun, {Line, {Description, Response, Result}}) ->
    {Description,
     {Line,
      fun() ->
              meck:expect(httpc, request, output_expect(Response)),
              erlcloud_ec2:configure(string:copies("A", 20), string:copies("a", 40)),
              Actual = Fun(),
              ?assertEqual(Result, Actual)
      end}}.
%% output_test(Fun, {Line, {Response, Result}}) ->
%%     output_test(Fun, {Line, {"", Response, Result}}).
      
%% output_tests converts a list of output_test specifiers into an eunit test generator
-spec output_tests(fun(), [output_test_spec()]) -> [term()].       
output_tests(Fun, Tests) ->
    [output_test(Fun, Test) || Test <- Tests].


%%%===================================================================
%%% Actual test specifiers
%%%===================================================================


%% DescribeTags test based on the API examples:
%% http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeTags.html
describe_tags_input_tests(_) ->
    Tests =
        [?_ec2_test(
            {"This example describes all the tags in your account.",
             ?_f(erlcloud_ec2:describe_tags()),
             [{"Action", "DescribeTags"}]}),
         ?_ec2_test(
            {"This example describes only the tags for the AMI with ID ami-1a2b3c4d.",
             ?_f(erlcloud_ec2:describe_tags([{resource_id, ["ami-1a2b3c4d"]}])),
             [{"Action", "DescribeTags"},
              {"Filter.1.Name", "resource-id"},
              {"Filter.1.Value.1", "ami-1a2b3c4d"}]}),
         ?_ec2_test(
            {"This example describes the tags for all your instances.",
             ?_f(erlcloud_ec2:describe_tags([{resource_type, ["instance"]}])),
             [{"Action", "DescribeTags"},
              {"Filter.1.Name", "resource-type"},
              {"Filter.1.Value.1", "instance"}]}),
         ?_ec2_test(
            {"This example describes the tags for all your instances tagged with the key webserver.",
             ?_f(erlcloud_ec2:describe_tags([{key, ["webserver"]}])),
             [{"Action", "DescribeTags"},
              {"Filter.1.Name", "key"},
              {"Filter.1.Value.1", "webserver"}]}),
         ?_ec2_test(
            {"This example describes the tags for all your instances tagged with either stack=Test or stack=Production.",
             ?_f(erlcloud_ec2:describe_tags([{resource_type, ["instance"]}, {key, ["stack"]}, {value, ["Test", "Production"]}])),
             [{"Action", "DescribeTags"},
              {"Filter.1.Name", "resource-type"},
              {"Filter.1.Value.1", "instance"},
              {"Filter.2.Name", "key"},
              {"Filter.2.Value.1", "stack"},
              {"Filter.3.Name", "value"},
              {"Filter.3.Value.1", "Test"},
              {"Filter.3.Value.2", "Production"}]}),
         ?_ec2_test(
            {"This example describes the tags for all your instances tagged with Purpose=[empty string].",
             ?_f(erlcloud_ec2:describe_tags([{resource_type, ["instance"]}, {key, ["Purpose"]}, {value, [""]}])),
             [{"Action", "DescribeTags"},
              {"Filter.1.Name", "resource-type"},
              {"Filter.1.Value.1", "instance"},
              {"Filter.2.Name", "key"},
              {"Filter.2.Value.1", "Purpose"},
              {"Filter.3.Name", "value"},
              {"Filter.3.Value.1", ""}]})],

    Response = "
<DescribeTagsResponse xmlns=\"http://ec2.amazonaws.com/doc/2012-12-01/\">
   <requestId>7a62c49f-347e-4fc4-9331-6e8eEXAMPLE</requestId>
   <tagSet/>
</DescribeTagsResponse>",
    input_tests(Response, Tests).

describe_tags_output_tests(_) ->
    Tests = 
        [?_ec2_test(
            {"This example describes all the tags in your account.", "
<DescribeTagsResponse xmlns=\"http://ec2.amazonaws.com/doc/2012-12-01/\">
   <requestId>7a62c49f-347e-4fc4-9331-6e8eEXAMPLE</requestId>
   <tagSet>
      <item>
         <resourceId>ami-1a2b3c4d</resourceId>
         <resourceType>image</resourceType>
         <key>webserver</key>
         <value/>
      </item>
       <item>
         <resourceId>ami-1a2b3c4d</resourceId>
         <resourceType>image</resourceType>
         <key>stack</key>
         <value>Production</value>
      </item>
      <item>
         <resourceId>i-5f4e3d2a</resourceId>
         <resourceType>instance</resourceType>
         <key>webserver</key>
         <value/>
      </item>
       <item>
         <resourceId>i-5f4e3d2a</resourceId>
         <resourceType>instance</resourceType>
         <key>stack</key>
         <value>Production</value>
      </item>
      <item>
         <resourceId>i-12345678</resourceId>
         <resourceType>instance</resourceType>
         <key>database_server</key>
         <value/>
      </item>
       <item> 
         <resourceId>i-12345678</resourceId>
         <resourceType>instance</resourceType>
         <key>stack</key>
         <value>Test</value>
      </item>
    </tagSet>
</DescribeTagsResponse>",
             {ok, [#ec2_tag{resource_id="ami-1a2b3c4d", resource_type="image", key="webserver", value=""},
                   #ec2_tag{resource_id="ami-1a2b3c4d", resource_type="image", key="stack", value="Production"},
                   #ec2_tag{resource_id="i-5f4e3d2a", resource_type="instance", key="webserver", value=""},
                   #ec2_tag{resource_id="i-5f4e3d2a", resource_type="instance", key="stack", value="Production"},
                   #ec2_tag{resource_id="i-12345678", resource_type="instance", key="database_server", value=""},
                   #ec2_tag{resource_id="i-12345678", resource_type="instance", key="stack", value="Test"}]}}),
         ?_ec2_test(
            {"This example describes only the tags for the AMI with ID ami-1a2b3c4d.", "
<DescribeTagsResponse xmlns=\"http://ec2.amazonaws.com/doc/2012-12-01/\">
   <requestId>7a62c49f-347e-4fc4-9331-6e8eEXAMPLE</requestId>
   <tagSet>
      <item>
         <resourceId>ami-1a2b3c4d</resourceId>
         <resourceType>image</resourceType>
         <key>webserver</key>
         <value/>
      </item>
      <item>
         <resourceId>ami-1a2b3c4d</resourceId>
         <resourceType>image</resourceType>
         <key>stack</key>
         <value>Production</value>
      </item>
    </tagSet>
</DescribeTagsResponse>",
             {ok, [#ec2_tag{resource_id="ami-1a2b3c4d", resource_type="image", key="webserver", value=""},
                   #ec2_tag{resource_id="ami-1a2b3c4d", resource_type="image", key="stack", value="Production"}]}})],
    
    %% Remaining AWS API examples return subsets of the same data
    output_tests(?_f(erlcloud_ec2:describe_tags()), Tests).
