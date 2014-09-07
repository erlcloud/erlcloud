%% -*- mode: erlang;erlang-indent-level: 4;indent-tabs-mode: nil -*-
-module(erlcloud_iam_tests).
-include_lib("eunit/include/eunit.hrl").
-include("erlcloud.hrl").
-include_lib("../include/erlcloud_aws.hrl").

%% Unit tests for iam.
%% These tests work by using meck to mock erlcloud_httpc. There are two classes of test: input and output.
%%
%% Input tests verify that different function args produce the desired JSON request.
%% An input test list provides a list of funs and the JSON that is expected to result.
%%
%% Output tests verify that the http response produces the correct return from the fun.
%% An output test lists provides a list of response bodies and the expected return.

%% The _ddb_test macro provides line number annotation to a test, similar to _test, but doesn't wrap in a fun
-define(_iam_test(T), {?LINE, T}).
%% The _f macro is a terse way to wrap code in a fun. Similar to _test but doesn't annotate with a line number
-define(_f(F), fun() -> F end).

%%%===================================================================
%%% Test entry points
%%%===================================================================

iam_api_test_() ->
    {foreach,
     fun start/0,
     fun stop/1,
     [fun list_users_input_tests/1,
      fun list_users_output_tests/1,
      fun list_groups_input_tests/1,
      fun list_groups_output_tests/1,
      fun list_roles_input_tests/1,
      fun list_roles_output_tests/1
    ]}.

start() ->
    meck:new(erlcloud_httpc),
    ok.

stop(_) ->
    meck:unload(erlcloud_httpc).

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
    io:format("Remain: ~p", [Remain]),
    ?assertEqual([], Remain).

%% returns the mock of the erlcloud_httpc function input tests expect to be called.
%% Validates the query body and responds with the provided response.
-spec input_expect(string(), [expected_param()]) -> fun().
input_expect(Response, Expected) ->
    fun(_Url, post, _Headers, Body, _Timeout, _Config) -> 
            validate_params(Body, Expected),
            {ok, {{200, "OK"}, [], list_to_binary(Response)}} 
    end.

%% input_test converts an input_test specifier into an eunit test generator
-type input_test_spec() :: {pos_integer(), {fun(), [expected_param()]} | {string(), fun(), [expected_param()]}}.
-spec input_test(string(), input_test_spec()) -> tuple().
input_test(Response, {Line, {Description, Fun, Params}}) when
      is_list(Description) ->
    {Description, 
     {Line,
      fun() ->
              meck:expect(erlcloud_httpc, request, input_expect(Response, Params)),
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

%% returns the mock of the erlcloud_httpc function output tests expect to be called.
-spec output_expect(string()) -> fun().
output_expect(Response) ->
    fun(_Url, post, _Headers, _Body, _Timeout, _Config) -> 
            {ok, {{200, "OK"}, [], list_to_binary(Response)}} 
    end.

%% output_test converts an output_test specifier into an eunit test generator
-type output_test_spec() :: {pos_integer(), {string(), term()} | {string(), string(), term()}}.
-spec output_test(fun(), output_test_spec()) -> tuple().
output_test(Fun, {Line, {Description, Response, Result}}) ->
    {Description,
     {Line,
      fun() ->
              meck:expect(erlcloud_httpc, request, output_expect(Response)),
              erlcloud_ec2:configure(string:copies("A", 20), string:copies("a", 40)),
              Actual = Fun(),
              io:format("Actual: ~p~n", [Actual]),
              io:format("Result: ~p~n", [Result]),
              ?assertEqual(Result, Actual)
      end}}.
      
%% output_tests converts a list of output_test specifiers into an eunit test generator
-spec output_tests(fun(), [output_test_spec()]) -> [term()].       
output_tests(Fun, Tests) ->
    [output_test(Fun, Test) || Test <- Tests].


%% ListUsers test based on the API examples:
%% http://docs.aws.amazon.com/IAM/latest/APIReference/API_ListUsers.html
list_users_input_tests(_) ->
    Tests = 
        [?_iam_test(
            {"Test returning all users in an account.",
             ?_f(erlcloud_iam:list_users("test")),
             [
              {"Action", "ListUsers"},
              {"PathPrefix", "test"}
              ]})
        ],

   Response = "
<ListUsersResponse>
   <ResponseMetadata>
      <RequestId>7a62c49f-347e-4fc4-9331-6e8eEXAMPLE</RequestId>
   </ResponseMetadata>
</ListUsersResponse>",
    input_tests(Response, Tests).
 
list_users_output_tests(_) ->
    Tests = [?_iam_test(
                {"This lists all users in your account",
                 "<ListUsersResponse>
                       <ListUsersResult>
                          <Users>
                             <member>
                                <Path>/division_abc/</Path>
                                <UserName>Andrew</UserName>
                                <UserId>AID2MAB8DPLSRHEXAMPLE</UserId>
                                <Arn>arn:aws:iam::123456789012:user/division_abc/Andrew</Arn>
                                <CreateDate>2012-05-08T23:34:01Z</CreateDate>
                             </member>
                             <member>
                                <Path>/division_abc/</Path>
                                <UserName>Jackie</UserName>
                                <UserId>AIDIODR4TAW7CSEXAMPLE</UserId>
                                <Arn>arn:aws:iam::123456789012:user/division_abc/Jackie</Arn>
                                <CreateDate>2012-05-08T23:34:01Z</CreateDate>
                             </member>
                          </Users>
                          <IsTruncated>false</IsTruncated>
                       </ListUsersResult>
                       <ResponseMetadata>
                          <RequestId>7a62c49f-347e-4fc4-9331-6e8eEXAMPLE</RequestId>
                       </ResponseMetadata>
                    </ListUsersResponse>",
                    {ok, [
                            [{path, "/division_abc/"},
                             {user_name, "Andrew"},
                             {user_id, "AID2MAB8DPLSRHEXAMPLE"},
                             {arn, "arn:aws:iam::123456789012:user/division_abc/Andrew"},
                             {create_date, {{2012,5,8},{23,34,1}}}],
                             [{path, "/division_abc/"},
                             {user_name, "Jackie"},
                             {user_id, "AIDIODR4TAW7CSEXAMPLE"},
                             {arn, "arn:aws:iam::123456789012:user/division_abc/Jackie"},
                             {create_date, {{2012,5,8},{23,34,1}}}]
                         ]}})
                ],
    output_tests(?_f(erlcloud_iam:list_users("test")), Tests). 


%% ListGroups test based on the API examples:
%% http://docs.aws.amazon.com/IAM/latest/APIReference/API_ListGroups.html
list_groups_input_tests(_) ->
    Tests = 
        [?_iam_test(
            {"Test returning all groups in an account.",
             ?_f(erlcloud_iam:list_groups("test")),
             [
              {"Action", "ListGroups"},
              {"PathPrefix", "test"}
              ]})
        ],

   Response = "
<ListGroupsResponse>
   <ResponseMetadata>
      <RequestId>7a62c49f-347e-4fc4-9331-6e8eEXAMPLE</RequestId>
   </ResponseMetadata>
</ListGroupsResponse>",
    input_tests(Response, Tests).
 
list_groups_output_tests(_) ->
    Tests = [?_iam_test(
                {"This lists all groups in your account",
                 "<ListGroupsResponse>
                       <ListGroupsResult>
                          <Groups>
                             <member>
                                <Path>/division_abc/</Path>
                                <GroupName>Admins</GroupName>
                                <GroupId>AGPACKCEVSQ6C2EXAMPLE</GroupId>
                                <Arn>arn:aws:iam::123456789012:group/Admins</Arn>
                                <CreateDate>2012-05-08T23:34:01Z</CreateDate>
                             </member>
                             <member>
                                <Path>/division_abc/subdivision_xyz/</Path>
                                <GroupName>Test</GroupName>
                                <GroupId>AGP2MAB8DPLSRHEXAMPLE</GroupId>
                                <Arn>arn:aws:iam::123456789012:group/division_abc/subdivision_xyz/Test</Arn>
                                <CreateDate>2012-05-08T23:34:01Z</CreateDate>
                             </member>
                             <member>
                                <Path>/division_abc/subdivision_xyz/product_1234/</Path>
                                <GroupName>Managers</GroupName>
                                <GroupId>AGPIODR4TAW7CSEXAMPLE</GroupId>
                                <Arn>arn:aws:iam::123456789012:group/division_abc/subdivision_xyz/product_1234/Managers</Arn>
                                <CreateDate>2012-05-08T23:34:01Z</CreateDate>
                             </member>
                          </Groups>
                          <IsTruncated>false</IsTruncated>
                       </ListGroupsResult>
                       <ResponseMetadata>
                          <RequestId>7a62c49f-347e-4fc4-9331-6e8eEXAMPLE</RequestId>
                       </ResponseMetadata>
                    </ListGroupsResponse>",
                    {ok, [
                            [{path, "/division_abc/"},
                             {group_name, "Admins"},
                             {group_id, "AGPACKCEVSQ6C2EXAMPLE"},
                             {arn, "arn:aws:iam::123456789012:group/Admins"},
                             {create_date, {{2012,5,8},{23,34,1}}}],
                             [{path, "/division_abc/subdivision_xyz/"},
                             {group_name, "Test"},
                             {group_id, "AGP2MAB8DPLSRHEXAMPLE"},
                             {arn, "arn:aws:iam::123456789012:group/division_abc/subdivision_xyz/Test"},
                             {create_date, {{2012,5,8},{23,34,1}}}],
                             [{path, "/division_abc/subdivision_xyz/product_1234/"},
                             {group_name, "Managers"},
                             {group_id, "AGPIODR4TAW7CSEXAMPLE"},
                             {arn, "arn:aws:iam::123456789012:group/division_abc/subdivision_xyz/product_1234/Managers"},
                             {create_date, {{2012,5,8},{23,34,1}}}]
                         ]}})
                ],
    output_tests(?_f(erlcloud_iam:list_groups("test")), Tests). 

%% ListRoles test based on the API examples:
%% http://docs.aws.amazon.com/IAM/latest/APIReference/API_ListRoles.html
list_roles_input_tests(_) ->
    Tests = 
        [?_iam_test(
            {"Test returning all roles in an account.",
             ?_f(erlcloud_iam:list_roles("test")),
             [
              {"Action", "ListRoles"},
              {"PathPrefix", "test"}
              ]})
        ],

   Response = "
        <ListRolesResponse>
           <ResponseMetadata>
              <RequestId>7a62c49f-347e-4fc4-9331-6e8eEXAMPLE</RequestId>
           </ResponseMetadata>
        </ListRolesResponse>",
    input_tests(Response, Tests).
 
list_roles_output_tests(_) ->
    Tests = [?_iam_test(
                {"This lists all roles in your account",
                 "<ListRolesResponse xmlns=\"https://iam.amazonaws.com/doc/2010-05-08/\">
                  <ListRolesResult>
                    <IsTruncated>false</IsTruncated>
                    <Roles>
                      <member>
                        <Path>/application_abc/component_xyz/</Path>
                        <Arn>arn:aws:iam::123456789012:role/application_abc/component_xyz/S3Access</Arn>
                        <RoleName>S3Access</RoleName>
                        <AssumeRolePolicyDocument>{\"Version\":\"2012-10-17\",\"Statement\":[{\"Effect\":\"Allow\",\"Principal\":{\"Service\":[\"ec2.amazonaws.com\"]},\"Action\":[\"sts:AssumeRole\"]}]}</AssumeRolePolicyDocument>
                        <CreateDate>2012-05-09T15:45:35Z</CreateDate>
                        <RoleId>AROACVSVTSZYEXAMPLEYK</RoleId>
                      </member>
                      <member>
                        <Path>/application_abc/component_xyz/</Path>
                        <Arn>arn:aws:iam::123456789012:role/application_abc/component_xyz/SDBAccess</Arn>
                        <RoleName>SDBAccess</RoleName>
                        <AssumeRolePolicyDocument>{\"Version\":\"2012-10-17\",\"Statement\":[{\"Effect\":\"Allow\",\"Principal\":{\"Service\":[\"ec2.amazonaws.com\"]},\"Action\":[\"sts:AssumeRole\"]}]}</AssumeRolePolicyDocument>
                        <CreateDate>2012-05-09T15:45:45Z</CreateDate>
                        <RoleId>AROAC2ICXG32EXAMPLEWK</RoleId>
                      </member>
                    </Roles>
                  </ListRolesResult>
                  <ResponseMetadata>
                    <RequestId>20f7279f-99ee-11e1-a4c3-27EXAMPLE804</RequestId>
                  </ResponseMetadata>
                </ListRolesResponse>",
                    {ok, [
                            [{path, "/application_abc/component_xyz/"},
                             {role_name, "S3Access"},
                             {role_id, "AROACVSVTSZYEXAMPLEYK"},
                             {assume_role_policy_doc, "{\"Version\":\"2012-10-17\",\"Statement\":[{\"Effect\":\"Allow\",\"Principal\":{\"Service\":[\"ec2.amazonaws.com\"]},\"Action\":[\"sts:AssumeRole\"]}]}"},
                             {create_date, {{2012,5,9},{15,45,35}}},
                             {arn, "arn:aws:iam::123456789012:role/application_abc/component_xyz/S3Access"}],
                             [{path, "/application_abc/component_xyz/"},
                             {role_name, "SDBAccess"},
                             {role_id, "AROAC2ICXG32EXAMPLEWK"},
                             {assume_role_policy_doc, "{\"Version\":\"2012-10-17\",\"Statement\":[{\"Effect\":\"Allow\",\"Principal\":{\"Service\":[\"ec2.amazonaws.com\"]},\"Action\":[\"sts:AssumeRole\"]}]}"},
                             {create_date, {{2012,5,9},{15,45,45}}},
                             {arn, "arn:aws:iam::123456789012:role/application_abc/component_xyz/SDBAccess"}]
                         ]}})
                ],
    output_tests(?_f(erlcloud_iam:list_roles("test")), Tests). 


