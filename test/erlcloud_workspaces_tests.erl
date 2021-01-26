%% -*- mode: erlang;erlang-indent-level: 4;indent-tabs-mode: nil -*-
-module(erlcloud_workspaces_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("include/erlcloud_workspaces.hrl").

%% Unit tests for erlcloud_workspaces.
%% These tests work by using meck to mock erlcloud_httpc. There are two classes of test: input and output.
%%
%% Input tests verify that different function args produce the desired JSON request.
%% An input test list provides a list of funs and the JSON that is expected to result.
%%
%% Output tests verify that the http response produces the correct return from the fun.
%% An output test lists provides a list of response bodies and the expected return.

%% The _workspaces_test macro provides line number annotation to a test, similar to _test, but doesn't wrap in a fun
-define(_workspaces_test(T), {?LINE, T}).
%% The _f macro is a terse way to wrap code in a fun. Similar to _test but doesn't annotate with a line number
-define(_f(F), fun() -> F end).

-export([validate_body/2]).

%%%===================================================================
%%% Test entry points
%%%===================================================================
operation_test_() ->
    {foreach,
     fun start/0,
     fun stop/1,
     [
      fun describe_tags_input_tests/1,
      fun describe_tags_output_tests/1,
      fun describe_workspaces_input_tests/1,
      fun describe_workspaces_output_tests/1,
      fun describe_workspace_directories_input_tests/1,
      fun describe_workspace_directories_output_tests/1
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

%% DescribeTags test based on the API examples:
%% https://docs.aws.amazon.com/workspaces/latest/api/API_DescribeTags.html
describe_tags_input_tests(_) ->
    Tests = 
        [?_workspaces_test(
            {"DescribeTags example request",
             ?_f(erlcloud_workspaces:describe_tags([{resource_id, "ws-c8wvb67p1"}])), "
{
    \"ResourceId\": \"ws-c8wvb67p1\"
}"
            })
        ],

    Response = "
{    
    \"TagList\": [ 
      { 
         \"Key\": \"testkey\",
         \"Value\": \"testvalue\"
      }
   ]
}",
    input_tests(Response, Tests).


describe_tags_output_tests(_) ->
    Tests =
        [?_workspaces_test(
            {"DescribeTags example response", "
{    
    \"TagList\": [ 
      { 
         \"Key\": \"testkey\",
         \"Value\": \"testvalue\"
      }
   ]
}",
            {ok,[#workspaces_tag{
                    key = <<"testkey">>,
                    value = <<"testvalue">>
                }]}
        })],
    output_tests(?_f(erlcloud_workspaces:describe_tags([{out, record}])), Tests).

%% DescribeWorkspaces test based on the API examples:
%% https://docs.aws.amazon.com/workspaces/latest/api/API_DescribeWorkspaces.html
describe_workspaces_input_tests(_) ->
    Tests = 
        [?_workspaces_test(
            {"DescribeWorkspaces example request",
             ?_f(erlcloud_workspaces:describe_workspaces([{bundle_id, "wsb-clj85qzj2"},
                                                   {directory_id, "d-93671bd1a2"},
                                                   {limit, 5},
                                                   {next_token, "next-page"},
                                                   {user_name, <<"root">>},
                                                   {workspace_ids, [<<"ws-c8wvb67p1">>, <<"ws-c8wvb67p2">>]}])), "
{
    \"BundleId\": \"wsb-clj85qzj2\",
    \"DirectoryId\": \"d-93671bd1a2\",
    \"Limit\": 5,
    \"NextToken\": \"next-page\",
    \"UserName\": \"root\",
    \"WorkspaceIds\": [\"ws-c8wvb67p1\", \"ws-c8wvb67p2\"]
    
}"
            })
        ],

    Response = "
{    
    \"Workspaces\": [{
        \"BundleId\": \"wsb-clj85qzj2\",
        \"ComputerName\": \"A-3AK2EEJBJC1MA\",
        \"DirectoryId\": \"d-93671bd1a2\",
        \"IpAddress\": \"172.16.0.121\",
        \"ModificationStates\": [],
        \"State\": \"STOPPED\",
        \"SubnetId\": \"subnet-08b608fe3a2ecc504\",
        \"UserName\": \"root1\",
        \"UserRealm\": \"corp.amazonworkspaces.com\",
        \"WorkspaceId\": \"ws-c8wvb67pa\",
        \"WorkspaceProperties\": {
            \"ComputeTypeName\": \"STANDARD\",
            \"RecycleMode\": \"DISABLED\",
            \"RootVolumeSizeGib\": 80,
            \"RunningMode\": \"AUTO_STOP\",
            \"RunningModeAutoStopTimeoutInMinutes\": 60,
            \"UserVolumeSizeGib\":50
        }
    }]
}",
    input_tests(Response, Tests).


describe_workspaces_output_tests(_) ->
    Tests =
        [?_workspaces_test(
            {"DescribeWorkspaces example response", "
{    
    \"Workspaces\": [{
        \"BundleId\": \"wsb-clj85qzj2\",
        \"ComputerName\": \"A-3AK2EEJBJC1MA\",
        \"DirectoryId\": \"d-93671bd1a2\",
        \"IpAddress\": \"172.16.0.121\",
        \"ModificationStates\": [],
        \"State\": \"STOPPED\",
        \"SubnetId\": \"subnet-08b608fe3a2ecc504\",
        \"UserName\": \"root\",
        \"UserRealm\": \"corp.amazonworkspaces.com\",
        \"WorkspaceId\": \"ws-c8wvb67pa\",
        \"WorkspaceProperties\": {
            \"ComputeTypeName\": \"STANDARD\",
            \"RecycleMode\": \"DISABLED\",
            \"RootVolumeSizeGib\": 80,
            \"RunningMode\": \"AUTO_STOP\",
            \"RunningModeAutoStopTimeoutInMinutes\": 60,
            \"UserVolumeSizeGib\":50
        }
    }]
}",
            {ok,#describe_workspaces{next_token = undefined,
                 workspaces = [
                   #workspace{
                      bundle_id = <<"wsb-clj85qzj2">>,
                      computer_name = <<"A-3AK2EEJBJC1MA">>,
                      directory_id = <<"d-93671bd1a2">>,error_code = undefined,
                      error_message = undefined,ip_address = <<"172.16.0.121">>,
                      modification_states = [],
                      root_volume_encryption_enabled = undefined,
                      state = <<"STOPPED">>,
                      subnet_id = <<"subnet-08b608fe3a2ecc504">>,
                      user_name = <<"root">>,
                      user_volume_encryption_enabled = undefined,
                      volume_encryption_key = undefined,
                      workspace_id = <<"ws-c8wvb67pa">>,
                      workspace_properties = #workspace_properties{
                                               computer_type_name = <<"STANDARD">>,
                                               root_volume_size_gib = 80,running_mode = <<"AUTO_STOP">>,
                                               running_mode_auto_stop_timeout_in_minutes = 60,
                                               user_volume_size_gib = 50}}]}}
        })],
    output_tests(?_f(erlcloud_workspaces:describe_workspaces([{out, record}])), Tests).

%% DescribeWorkspaceDirectories test based on the API examples:
%% https://docs.aws.amazon.com/workspaces/latest/api/API_DescribeWorkspaceDirectories.html
describe_workspace_directories_input_tests(_) ->
    Tests = 
        [?_workspaces_test(
            {"DescribeWorkspaceDirectories example request",
             ?_f(erlcloud_workspaces:describe_workspace_directories([
                       {directory_ids, ["d-93671bd1a1", "d-93671bd1a2"]},
                       {limit, 5},
                       {next_token, "next-page"}])), "
{
    \"DirectoryIds\": [\"d-93671bd1a1\", \"d-93671bd1a2\"],
    \"Limit\": 5,
    \"NextToken\": \"next-page\"
    
}"
            })
        ],

    Response = "
{    
    \"Directories\": [
        {
            \"DirectoryId\": \"d-93671bd1a1\",
            \"Alias\": \"d-93671bd1a1\",
            \"DirectoryName\": \"corp.amazonworkspaces.com\",
            \"RegistrationCode\": \"wsdub+SATMNS\",
            \"SubnetIds\": [
                \"subnet-02d62b601c121d0cd\",
                \"subnet-08b608fe3a2ecc505\"
            ],
            \"DnsIpAddresses\": [
                \"172.16.1.36\",
                \"172.16.0.49\"
            ],
            \"CustomerUserName\": \"Administrator\",
            \"IamRoleId\": \"arn:aws:iam::352283894008:role/workspaces_DefaultRole\",
            \"DirectoryType\": \"SIMPLE_AD\",
            \"WorkspaceSecurityGroupId\": \"sg-064d3dbf40db978bd\",
            \"State\": \"REGISTERED\",
            \"WorkspaceCreationProperties\": {
                \"EnableWorkDocs\": true,
                \"EnableInternetAccess\": true,
                \"UserEnabledAsLocalAdministrator\": true,
                \"EnableMaintenanceMode\": true
            },
            \"WorkspaceAccessProperties\": {
                \"DeviceTypeWindows\": \"ALLOW\",
                \"DeviceTypeOsx\": \"ALLOW\",
                \"DeviceTypeWeb\": \"DENY\",
                \"DeviceTypeIos\": \"ALLOW\",
                \"DeviceTypeAndroid\": \"ALLOW\",
                \"DeviceTypeChromeOs\": \"ALLOW\",
                \"DeviceTypeZeroClient\": \"ALLOW\"
            },
            \"Tenancy\": \"SHARED\",
            \"SelfservicePermissions\": {
                \"RestartWorkspace\": \"ENABLED\",
                \"IncreaseVolumeSize\": \"ENABLED\",
                \"ChangeComputeType\": \"ENABLED\",
                \"SwitchRunningMode\": \"ENABLED\",
                \"RebuildWorkspace\": \"ENABLED\"
            }
        }
    ]
}",
    input_tests(Response, Tests).


describe_workspace_directories_output_tests(_) ->
    Tests =
        [?_workspaces_test(
            {"DescribeWorkspaceDirectories example response", "
{    
    \"Directories\": [
        {
            \"DirectoryId\": \"d-93671bd1a1\",
            \"Alias\": \"d-93671bd1a1\",
            \"DirectoryName\": \"corp.amazonworkspaces.com\",
            \"RegistrationCode\": \"wsdub+SATMNS\",
            \"SubnetIds\": [
                \"subnet-02d62b601c121d0cd\",
                \"subnet-08b608fe3a2ecc505\"
            ],
            \"DnsIpAddresses\": [
                \"172.16.1.36\",
                \"172.16.0.49\"
            ],
            \"CustomerUserName\": \"Administrator\",
            \"IamRoleId\": \"arn:aws:iam::352283111111:role/workspaces_DefaultRole\",
            \"DirectoryType\": \"SIMPLE_AD\",
            \"WorkspaceSecurityGroupId\": \"sg-064d3dbf40db978bd\",
            \"State\": \"REGISTERED\",
            \"WorkspaceCreationProperties\": {
                \"EnableWorkDocs\": true,
                \"EnableInternetAccess\": true,
                \"UserEnabledAsLocalAdministrator\": true,
                \"EnableMaintenanceMode\": true
            },
            \"WorkspaceAccessProperties\": {
                \"DeviceTypeWindows\": \"ALLOW\",
                \"DeviceTypeOsx\": \"ALLOW\",
                \"DeviceTypeWeb\": \"DENY\",
                \"DeviceTypeIos\": \"ALLOW\",
                \"DeviceTypeAndroid\": \"ALLOW\",
                \"DeviceTypeChromeOs\": \"ALLOW\",
                \"DeviceTypeZeroClient\": \"ALLOW\"
            },
            \"Tenancy\": \"SHARED\",
            \"SelfservicePermissions\": {
                \"RestartWorkspace\": \"ENABLED\",
                \"IncreaseVolumeSize\": \"ENABLED\",
                \"ChangeComputeType\": \"ENABLED\",
                \"SwitchRunningMode\": \"ENABLED\",
                \"RebuildWorkspace\": \"ENABLED\"
            }
        }
    ]
}",
            {ok,#describe_workspace_directories{
        next_token = undefined,
        workspace_directories =
            [#workspace_directory{
                 alias = <<"d-93671bd1a1">>,
                 customer_user_name = <<"Administrator">>,
                 directory_id = <<"d-93671bd1a1">>,
                 directory_name = <<"corp.amazonworkspaces.com">>,
                 directory_type = <<"SIMPLE_AD">>,
                 dns_ip_address = [<<"172.16.1.36">>,<<"172.16.0.49">>],
                 iam_role_id =
                     <<"arn:aws:iam::352283111111:role/workspaces_DefaultRole">>,
                 ip_group_ids = undefined,
                 registration_code = <<"wsdub+SATMNS">>,
                 selfservice_permissions =
                     #workspaces_selfservice_permissions{
                         change_compute_type = <<"ENABLED">>,
                         increase_volume_size = <<"ENABLED">>,
                         rebuild_workspace = <<"ENABLED">>,
                         restart_workspace = <<"ENABLED">>,
                         switch_running_mode = <<"ENABLED">>},
                 state = <<"REGISTERED">>,
                 subnet_ids =
                     [<<"subnet-02d62b601c121d0cd">>,
                      <<"subnet-08b608fe3a2ecc505">>],
                 tenancy = <<"SHARED">>,
                 workspace_access_properties =
                     #workspace_access_properties{
                         device_type_android = <<"ALLOW">>,
                         device_type_chrome_os = <<"ALLOW">>,
                         device_type_ios = <<"ALLOW">>,device_type_osx = <<"ALLOW">>,
                         device_type_web = <<"DENY">>,
                         device_type_windows = <<"ALLOW">>,
                         device_type_zero_client = <<"ALLOW">>},
                 workspace_creation_properties =
                     #workspace_creation_properties{
                         custom_security_group_id = undefined,default_ou = undefined,
                         enable_internet_access = true,
                         enable_maintenance_mode = true,enable_work_docs = true,
                         user_enabled_as_local_administrator = true},
                 workspace_security_group_id = <<"sg-064d3dbf40db978bd">>}]}}
        })],
    output_tests(?_f(erlcloud_workspaces:describe_workspace_directories([{out, record}])), Tests).


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
    Want = sort_json(jsx:decode(list_to_binary(Expected), [{return_maps, false}])),
    Actual = sort_json(jsx:decode(Body, [{return_maps, false}])),
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
            {ok, {{200, "OK"}, [], list_to_binary(Response)}}
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
              erlcloud_workspaces:configure(string:copies("A", 20), string:copies("a", 40)),
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
-spec output_expect(binary()) -> fun().
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
              meck:expect(erlcloud_httpc, request, output_expect(list_to_binary(Response))),
              erlcloud_workspaces:configure(string:copies("A", 20), string:copies("a", 40)),
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
