%% -*- mode: erlang;erlang-indent-level: 4;indent-tabs-mode: nil -*-
-module(erlcloud_iam_tests).
-include_lib("eunit/include/eunit.hrl").
-include("erlcloud.hrl").
-include_lib("../include/erlcloud_aws.hrl").
-compile([export_all]).

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
     [fun get_account_authorization_details_input_tests/1,
      fun get_account_authorization_details_output_tests/1,
      fun get_account_summary_input_tests/1,
      fun get_account_summary_output_tests/1,
      fun get_account_password_policy_input_tests/1,
      fun get_account_password_policy_output_tests/1,
      fun get_user_input_tests/1,
      fun get_user_output_tests/1,
      fun get_group_policy_input_tests/1,
      fun get_group_policy_output_tests/1,
      fun get_login_profile_input_tests/1,
      fun get_login_profile_output_tests/1,
      fun get_role_policy_input_tests/1,
      fun get_role_policy_output_tests/1,
      fun get_user_policy_input_tests/1,
      fun get_user_policy_output_tests/1,
      fun generate_credential_report_input_tests/1,
      fun generate_credential_report_output_tests/1,
      fun list_access_keys_input_tests/1,
      fun list_access_keys_output_tests/1,
      fun list_users_input_tests/1,
      fun list_users_output_tests/1,
      fun list_groups_input_tests/1,
      fun list_groups_output_tests/1,
      fun list_roles_input_tests/1,
      fun list_roles_output_tests/1,
      fun list_groups_for_user_input_tests/1,
      fun list_groups_for_user_output_tests/1,
      fun list_user_policies_input_tests/1,
      fun list_user_policies_output_tests/1,
      fun list_group_policies_input_tests/1,
      fun list_group_policies_output_tests/1,
      fun list_role_policies_input_tests/1,
      fun list_role_policies_output_tests/1,
      fun list_instance_profiles_input_tests/1,
      fun list_instance_profiles_output_tests/1,
      fun get_instance_profile_input_tests/1,
      fun get_instance_profile_output_tests/1,
      fun get_credential_report_input_tests/1,
      fun get_credential_report_output_tests/1,
      fun list_attached_user_policies_input_tests/1,
      fun list_attached_user_policies_output_tests/1,
      fun list_attached_group_policies_input_tests/1,
      fun list_attached_group_policies_output_tests/1,
      fun list_attached_role_policies_input_tests/1,
      fun list_attached_role_policies_output_tests/1,
      fun get_policy_input_tests/1,
      fun get_policy_output_tests/1,
      fun get_policy_version_input_tests/1,
      fun get_policy_version_output_tests/1
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

get_account_summary_input_tests(_) ->
    Tests = 
        [?_iam_test(
            {"Test returning account summary.",
             ?_f(erlcloud_iam:get_account_summary()),
             [
              {"Action", "GetAccountSummary"}
              ]})
        ],

   Response = "
<GetAccountSummaryResponse>
   <ResponseMetadata>
      <RequestId>7a62c49f-347e-4fc4-9331-6e8eEXAMPLE</RequestId>
   </ResponseMetadata>
</GetAccountSummaryResponse>",
    input_tests(Response, Tests).

get_account_summary_output_tests(_) ->
    Tests = [?_iam_test(
                {"This returns the account summary",
                 "<GetAccountSummaryResponse>
                    <GetAccountSummaryResult>
                      <SummaryMap>
                        <entry>
                          <key>Groups</key>
                          <value>31</value>
                        </entry>
                        <entry>
                          <key>GroupsQuota</key>
                          <value>50</value>
                        </entry>
                        <entry>
                          <key>UsersQuota</key>
                          <value>150</value>
                        </entry>
                        <entry>
                          <key>Users</key>
                          <value>35</value>
                        </entry>
                        <entry>
                          <key>GroupPolicySizeQuota</key>
                          <value>10240</value>
                        </entry>
                        <entry>
                          <key>AccessKeysPerUserQuota</key>
                          <value>2</value>
                        </entry>
                        <entry>
                          <key>GroupsPerUserQuota</key>
                          <value>10</value>
                        </entry>
                        <entry>
                          <key>UserPolicySizeQuota</key>
                          <value>10240</value>
                        </entry>
                        <entry>
                          <key>SigningCertificatesPerUserQuota</key>
                          <value>2</value>
                        </entry>
                        <entry>
                          <key>ServerCertificates</key>
                          <value>0</value>
                        </entry>
                        <entry>
                          <key>ServerCertificatesQuota</key>
                          <value>10</value>
                        </entry>
                        <entry>
                          <key>AccountMFAEnabled</key>
                          <value>0</value>
                        </entry>
                        <entry>
                          <key>MFADevicesInUse</key>
                          <value>10</value>
                        </entry>
                        <entry>
                          <key>MFADevices</key>
                          <value>20</value>
                        </entry>
                      </SummaryMap>
                    </GetAccountSummaryResult>
                    <ResponseMetadata>
                      <RequestId>f1e38443-f1ad-11df-b1ef-a9265EXAMPLE</RequestId>
                    </ResponseMetadata>
                    </GetAccountSummaryResponse>",
                 {ok,[[{mfa_devices,20},
                       {mfa_devices_in_use,10},
                       {account_mfa_enabled,false},
                       {server_certificates_quota,10},
                       {server_certificates,0},
                       {signing_certificates_per_user_quota,2},
                       {user_policy_size_quota,10240},
                       {groups_per_user_quota,10},
                       {access_keys_per_user_quota,2},
                       {group_policy_size_quota,10240},
                       {users,35},
                       {users_quota,150},
                       {groups_quota,50},
                       {groups,31}]
                     ]}}),
              ?_iam_test(
                {"This returns the account summary",
                 "<GetAccountSummaryResponse>
                    <GetAccountSummaryResult>
                      <SummaryMap>
                        <entry>
                          <key>Groups</key>
                          <value>31</value>
                        </entry>
                        <entry>
                          <key>GroupsQuota</key>
                          <value>50</value>
                        </entry>
                        <entry>
                          <key>UsersQuota</key>
                          <value>150</value>
                        </entry>
                        <entry>
                          <key>Users</key>
                          <value>35</value>
                        </entry>
                        <entry>
                          <key>GroupPolicySizeQuota</key>
                          <value>10240</value>
                        </entry>
                        <entry>
                          <key>AccessKeysPerUserQuota</key>
                          <value>2</value>
                        </entry>
                        <entry>
                          <key>GroupsPerUserQuota</key>
                          <value>10</value>
                        </entry>
                        <entry>
                          <key>UserPolicySizeQuota</key>
                          <value>10240</value>
                        </entry>
                        <entry>
                          <key>SigningCertificatesPerUserQuota</key>
                          <value>2</value>
                        </entry>
                        <entry>
                          <key>ServerCertificates</key>
                          <value>0</value>
                        </entry>
                        <entry>
                          <key>ServerCertificatesQuota</key>
                          <value>10</value>
                        </entry>
                        <entry>
                          <key>AccountMFAEnabled</key>
                          <value>1</value>
                        </entry>
                        <entry>
                          <key>MFADevicesInUse</key>
                          <value>10</value>
                        </entry>
                        <entry>
                          <key>MFADevices</key>
                          <value>20</value>
                        </entry>
                      </SummaryMap>
                    </GetAccountSummaryResult>
                    <ResponseMetadata>
                      <RequestId>f1e38443-f1ad-11df-b1ef-a9265EXAMPLE</RequestId>
                    </ResponseMetadata>
                    </GetAccountSummaryResponse>",
                 {ok,[[{mfa_devices,20},
                       {mfa_devices_in_use,10},
                       {account_mfa_enabled,true},
                       {server_certificates_quota,10},
                       {server_certificates,0},
                       {signing_certificates_per_user_quota,2},
                       {user_policy_size_quota,10240},
                       {groups_per_user_quota,10},
                       {access_keys_per_user_quota,2},
                       {group_policy_size_quota,10240},
                       {users,35},
                       {users_quota,150},
                       {groups_quota,50},
                       {groups,31}]
                     ]}})
            ],
    output_tests(?_f(erlcloud_iam:get_account_summary()), Tests).

-define(GET_ACCOUNT_PASSWORD_POLICY_RESP,
        "<GetAccountPasswordPolicyResponse xmlns=\"https://iam.amazonaws.com/doc/2010-05-08/\">
            <GetAccountPasswordPolicyResult>
              <PasswordPolicy>
                <AllowUsersToChangePassword>true</AllowUsersToChangePassword>
                <RequireUppercaseCharacters>true</RequireUppercaseCharacters>
                <RequireSymbols>true</RequireSymbols>
                <ExpirePasswords>false</ExpirePasswords>
                <PasswordReusePrevention>12</PasswordReusePrevention>
                <RequireLowercaseCharacters>true</RequireLowercaseCharacters>
                <MaxPasswordAge>90</MaxPasswordAge>
                <HardExpiry>false</HardExpiry>
                <RequireNumbers>true</RequireNumbers>
                <MinimumPasswordLength>12</MinimumPasswordLength>
              </PasswordPolicy>
            </GetAccountPasswordPolicyResult>
            <ResponseMetadata>
              <RequestId>7a62c49f-347e-4fc4-9331-6e8eEXAMPLE</RequestId>
            </ResponseMetadata>
            </GetAccountPasswordPolicyResponse>").

get_account_password_policy_input_tests(_) ->
    Tests = 
        [?_iam_test(
            {"Test returning account password policy.",
             ?_f(erlcloud_iam:get_account_password_policy()),
             [
              {"Action", "GetAccountPasswordPolicy"}
              ]})
        ],

    input_tests(?GET_ACCOUNT_PASSWORD_POLICY_RESP, Tests).

get_account_password_policy_output_tests(_) ->
    Tests = [?_iam_test(
             {"This returns the account password policy",
              ?GET_ACCOUNT_PASSWORD_POLICY_RESP,
              {ok,[[{allow_users_to_change_password,true},
                    {expire_passwords,false},
                    {hard_expiry,false},
                    {max_password_age,90},
                    {minimum_password_length,12},
                    {password_reuse_prevention,12},
                    {require_lowercase_characters,true},
                    {require_numbers,true},
                    {require_symbols,true},
                    {require_uppercase_characters,true}]]}
             })
            ],
    output_tests(?_f(erlcloud_iam:get_account_password_policy()), Tests).

-define(GET_USER_RESP,
        "<GetUserResponse>
           <GetUserResult>
             <User>
               <UserId>AIDACKCEVSQ6C2EXAMPLE</UserId>
               <Path>/division_abc/subdivision_xyz/</Path>
               <UserName>Bob</UserName>
               <Arn>arn:aws:iam::123456789012:user/division_abc/subdivision_xyz/Bob</Arn>
               <CreateDate>2013-10-02T17:01:44Z</CreateDate>
               <PasswordLastUsed>2014-10-10T14:37:51Z</PasswordLastUsed>
             </User>
           </GetUserResult>
           <ResponseMetadata>
             <RequestId>7a62c49f-347e-4fc4-9331-6e8eEXAMPLE</RequestId>
           </ResponseMetadata>
         </GetUserResponse>").

get_user_input_tests(_) ->
    Tests = 
        [?_iam_test(
            {"Test returning User.",
             ?_f(erlcloud_iam:get_user()),
             [
              {"Action", "GetUser"}
              ]})
        ],

    input_tests(?GET_USER_RESP, Tests).

get_user_output_tests(_) ->
    Tests = [?_iam_test(
                {"This returns the User",
                 ?GET_USER_RESP,
                 {ok,[{path,"/division_abc/subdivision_xyz/"},
                      {user_name,"Bob"},
                      {user_id,"AIDACKCEVSQ6C2EXAMPLE"},
                      {arn,"arn:aws:iam::123456789012:user/division_abc/subdivision_xyz/Bob"},
                      {create_date,{{2013,10,2},{17,1,44}}},
                      {password_last_used,{{2014,10,10},{14,37,51}}}]}
                })
            ],
    output_tests(?_f(erlcloud_iam:get_user()), Tests).

-define(GET_GROUP_POLICY_RESP,
        "<GetGroupPolicyResponse>
           <GetGroupPolicyResult>
             <GroupName>Admins</GroupName>
             <PolicyName>AdminRoot</PolicyName>
             <PolicyDocument>
               {\"Version\":\"2012-10-17\",\"Statement\":[{\"Effect\":\"Allow\",\"Action\":\"*\",\"Resource\":\"*\"}]}
             </PolicyDocument>
           </GetGroupPolicyResult>
           <ResponseMetadata>
             <RequestId>7a62c49f-347e-4fc4-9331-6e8eEXAMPLE</RequestId>
           </ResponseMetadata>
         </GetGroupPolicyResponse>").

get_group_policy_input_tests(_) ->
    Tests = 
        [?_iam_test(
            {"Test returning group policy.",
             ?_f(erlcloud_iam:get_group_policy("Admins", "AdminRoot")),
             [
              {"Action", "GetGroupPolicy"},
              {"GroupName", "Admins"},
              {"PolicyName", "AdminRoot"}
              ]})
        ],

    input_tests(?GET_GROUP_POLICY_RESP, Tests).

get_group_policy_output_tests(_) ->
    Tests = [?_iam_test(
             {"This returns the group policy",
              ?GET_GROUP_POLICY_RESP,
              {ok,[[{policy_name,"AdminRoot"},
                    {group_name,"Admins"},
                    {policy_document,"\n               {\"Version\":\"2012-10-17\",\"Statement\":[{\"Effect\":\"Allow\",\"Action\":\"*\",\"Resource\":\"*\"}]}\n             "}]]}
             })
            ],
    output_tests(?_f(erlcloud_iam:get_group_policy("Admins", "AdminRoot")), Tests).

-define(GET_LOGIN_PROFILE_RESP,
        "<GetLoginProfileResponse>
           <GetLoginProfileResult>
             <LoginProfile>
               <UserName>Bob</UserName>
               <CreateDate>2011-09-19T23:00:56Z</CreateDate>
             </LoginProfile>
           </GetLoginProfileResult>
           <ResponseMetadata>
             <RequestId>7a62c49f-347e-4fc4-9331-6e8eEXAMPLE</RequestId>
          </ResponseMetadata>
         </GetLoginProfileResponse>").

get_login_profile_input_tests(_) ->
    Tests = 
        [?_iam_test(
            {"Test returning login profile.",
             ?_f(erlcloud_iam:get_login_profile("Bob")),
             [
              {"Action", "GetLoginProfile"},
              {"UserName", "Bob"}
              ]})
        ],

    input_tests(?GET_LOGIN_PROFILE_RESP, Tests).

get_login_profile_output_tests(_) ->
    Tests = [?_iam_test(
             {"This returns the login profile",
              ?GET_LOGIN_PROFILE_RESP,
              {ok,[[{user_name,"Bob"},
                    {create_date,{{2011,9,19},{23,0,56}}}]]}
             })
            ],
    output_tests(?_f(erlcloud_iam:get_login_profile("Bob")), Tests).

-define(GET_ROLE_POLICY_RESP,
        "<GetRolePolicyResponse xmlns=\"https://iam.amazonaws.com/doc/2010-05-08/\">
          <GetRolePolicyResult>
            <PolicyName>S3AccessPolicy</PolicyName>
            <RoleName>S3Access</RoleName>
            <PolicyDocument>{\"Version\":\"2012-10-17\",\"Statement\":[{\"Effect\":\"Allow\",\"Action\":[\"s3:*\"],\"Resource\":[\"*\"]}]}</PolicyDocument>
          </GetRolePolicyResult>
          <ResponseMetadata>
            <RequestId>7e7cd8bc-99ef-11e1-a4c3-27EXAMPLE804</RequestId>
          </ResponseMetadata>
         </GetRolePolicyResponse>").

get_role_policy_input_tests(_) ->
    Tests = 
        [?_iam_test(
            {"Test returning role policy.",
             ?_f(erlcloud_iam:get_role_policy("S3Access", "S3AccessPolicy")),
             [
              {"Action", "GetRolePolicy"},
              {"PolicyName", "S3AccessPolicy"},
              {"RoleName", "S3Access"}
              ]})
        ],

    input_tests(?GET_ROLE_POLICY_RESP, Tests).

get_role_policy_output_tests(_) ->
    Tests = [?_iam_test(
             {"This returns the role policy",
              ?GET_ROLE_POLICY_RESP,
              {ok,[[{policy_name,"S3AccessPolicy"},
                    {role_name,"S3Access"},
                    {policy_document,"{\"Version\":\"2012-10-17\",\"Statement\":[{\"Effect\":\"Allow\",\"Action\":[\"s3:*\"],\"Resource\":[\"*\"]}]}"}]]}
             })
            ],
    output_tests(?_f(erlcloud_iam:get_role_policy("S3Access", "S3AccessPolicy")), Tests).

-define(GET_USER_POLICY_RESP,
        "<GetUserPolicyResponse>
           <GetUserPolicyResult>
             <UserName>Bob</UserName>
             <PolicyName>AllAccessPolicy</PolicyName>
             <PolicyDocument>{\"Version\":\"2012-10-17\",\"Statement\":[{\"Effect\":\"Allow\",\"Action\":\"*\",\"Resource\":\"*\"}]}</PolicyDocument>
           </GetUserPolicyResult>
           <ResponseMetadata>
             <RequestId>7a62c49f-347e-4fc4-9331-6e8eEXAMPLE</RequestId>
           </ResponseMetadata>
         </GetUserPolicyResponse>").

get_user_policy_input_tests(_) ->
    Tests = 
        [?_iam_test(
            {"Test returning user policy.",
             ?_f(erlcloud_iam:get_user_policy("Bob", "AllAccessPolicy")),
             [
              {"Action", "GetUserPolicy"},
              {"UserName", "Bob"},
              {"PolicyName", "AllAccessPolicy"}
              ]})
        ],

    input_tests(?GET_USER_POLICY_RESP, Tests).

get_user_policy_output_tests(_) ->
    Tests = [?_iam_test(
             {"This returns the user policy",
              ?GET_USER_POLICY_RESP,
              {ok,[[{policy_name,"AllAccessPolicy"},
                    {user_name,"Bob"},
                    {policy_document,"{\"Version\":\"2012-10-17\",\"Statement\":[{\"Effect\":\"Allow\",\"Action\":\"*\",\"Resource\":\"*\"}]}"}]]}
             })
            ],
    output_tests(?_f(erlcloud_iam:get_user_policy("Bob", "AllAccessPolicy")), Tests).
    
list_access_keys_input_tests(_) ->
    Tests = 
        [?_iam_test(
            {"Test returning all users in an account.",
             ?_f(erlcloud_iam:list_access_keys("test")),
             [
              {"Action", "ListAccessKeys"},
              {"UserName", "test"}
              ]})
        ],

   Response = "
<ListAccessKeysResponse>
   <ResponseMetadata>
      <RequestId>7a62c49f-347e-4fc4-9331-6e8eEXAMPLE</RequestId>
   </ResponseMetadata>
</ListAccessKeysResponse>",
    input_tests(Response, Tests).

list_access_keys_output_tests(_) ->
    Tests = [?_iam_test(
                {"This lists all access keys for a user",
                 "<ListAccessKeysResponse>
                     <ListAccessKeysResult>
                        <UserName>Bob</UserName>
                        <AccessKeyMetadata>
                           <member>
                              <UserName>Bob</UserName>
                              <AccessKeyId>AKIAIOSFODNN7EXAMPLE</AccessKeyId>
                              <CreateDate>2012-05-08T23:34:01Z</CreateDate>
                              <Status>Active</Status>
                           </member>
                           <member>
                              <UserName>Bob</UserName>
                              <AccessKeyId>AKIAI44QH8DHBEXAMPLE</AccessKeyId>
                              <CreateDate>2012-05-08T23:34:01Z</CreateDate>
                              <Status>Inactive</Status>
                           </member>
                        </AccessKeyMetadata>
                        <IsTruncated>false</IsTruncated>
                     </ListAccessKeysResult>
                     <ResponseMetadata>
                        <RequestId>7a62c49f-347e-4fc4-9331-6e8eEXAMPLE</RequestId>
                     </ResponseMetadata>
                    </ListAccessKeysResponse>",
                 {ok, [
                       [{user_name, "Bob"},
                        {access_key_id, "AKIAIOSFODNN7EXAMPLE"},
                        {create_date, {{2012,5,8},{23,34,1}}},
                        {status, "Active"}],
                       [{user_name, "Bob"},
                        {access_key_id, "AKIAI44QH8DHBEXAMPLE"},
                        {create_date, {{2012,5,8},{23,34,1}}},
                        {status, "Inactive"}]
                      ]}})
            ],
    output_tests(?_f(erlcloud_iam:list_access_keys("test")), Tests). 

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
                              <UserId>AID2MAB8DPLSRHEXAMPLE</UserId>
                              <Path>/division_abc/subdivision_xyz/engineering/</Path>
                              <UserName>Andrew</UserName>
                              <Arn>arn:aws:iam::123456789012:user/division_abc/subdivision_xyz/engineering/Andrew</Arn>
                              <CreateDate>2012-09-05T19:38:48Z</CreateDate>
                              <PasswordLastUsed>2014-09-08T21:47:36Z</PasswordLastUsed>
                           </member>
                           <member>
                              <UserId>AIDIODR4TAW7CSEXAMPLE</UserId>
                              <Path>/division_abc/subdivision_xyz/engineering/</Path>
                              <UserName>Jackie</UserName>
                              <Arn>arn:aws:iam::123456789012:user/division_abc/subdivision_xyz/engineering/Jackie</Arn>
                              <CreateDate>2014-04-09T15:43:45Z</CreateDate>
                              <PasswordLastUsed>2014-09-24T16:18:07Z</PasswordLastUsed>
                           </member>
                        </Users>
                        <IsTruncated>false</IsTruncated>
                     </ListUsersResult>
                     <ResponseMetadata>
                        <RequestId>7a62c49f-347e-4fc4-9331-6e8eEXAMPLE</RequestId>
                     </ResponseMetadata>
                    </ListUsersResponse>",
                 {ok,[[{path,"/division_abc/subdivision_xyz/engineering/"},
                       {user_name,"Andrew"},
                       {user_id,"AID2MAB8DPLSRHEXAMPLE"},
                       {arn,"arn:aws:iam::123456789012:user/division_abc/subdivision_xyz/engineering/Andrew"},
                       {create_date,{{2012,9,5},{19,38,48}}},
                       {password_last_used,{{2014,9,8},{21,47,36}}}],
                      [{path,"/division_abc/subdivision_xyz/engineering/"},
                       {user_name,"Jackie"},
                       {user_id,"AIDIODR4TAW7CSEXAMPLE"},
                       {arn,"arn:aws:iam::123456789012:user/division_abc/subdivision_xyz/engineering/Jackie"},
                       {create_date,{{2014,4,9},{15,43,45}}},
                       {password_last_used,{{2014,9,24},{16,18,7}}}]]}
                })
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
                 {ok,[[{arn,"arn:aws:iam::123456789012:group/Admins"},
                       {create_date,{{2012,5,8},{23,34,1}}},
                       {group_id,"AGPACKCEVSQ6C2EXAMPLE"},
                       {group_name,"Admins"},
                       {path,"/division_abc/"}],
                      [{arn,"arn:aws:iam::123456789012:group/division_abc/subdivision_xyz/Test"},
                       {create_date,{{2012,5,8},{23,34,1}}},
                       {group_id,"AGP2MAB8DPLSRHEXAMPLE"},
                       {group_name,"Test"},
                       {path,"/division_abc/subdivision_xyz/"}],
                      [{arn,"arn:aws:iam::123456789012:group/division_abc/subdivision_xyz/product_1234/Managers"},
                       {create_date,{{2012,5,8},{23,34,1}}},
                       {group_id,"AGPIODR4TAW7CSEXAMPLE"},
                       {group_name,"Managers"},
                       {path,"/division_abc/subdivision_xyz/product_1234/"}]
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

-define(LIST_GROUPS_FOR_USER_RESP,
        "<ListGroupsForUserResponse>
           <ListGroupsForUserResult>
             <Groups>
               <member>
                 <Path>/</Path>
                 <GroupName>Admins</GroupName>
                 <GroupId>AGPACKCEVSQ6C2EXAMPLE</GroupId>
                 <Arn>arn:aws:iam::123456789012:group/Admins</Arn>
               </member>
             </Groups>
           <IsTruncated>false</IsTruncated>
         </ListGroupsForUserResult>
         <ResponseMetadata>
           <RequestId>7a62c49f-347e-4fc4-9331-6e8eEXAMPLE</RequestId>
         </ResponseMetadata>
       </ListGroupsForUserResponse>").

list_groups_for_user_input_tests(_) ->
    Tests = 
        [?_iam_test(
            {"Test returning groups for a user.",
             ?_f(erlcloud_iam:list_groups_for_user("Bob")),
             [
              {"Action", "ListGroupsForUser"},
              {"UserName", "Bob"}
              ]})
        ],

    input_tests(?LIST_GROUPS_FOR_USER_RESP, Tests).

list_groups_for_user_output_tests(_) ->
    Tests = [?_iam_test(
             {"This returns the groups for a user",
              ?LIST_GROUPS_FOR_USER_RESP,
              {ok,[[{arn,"arn:aws:iam::123456789012:group/Admins"},
                    {create_date,undefined},
                    {group_id,"AGPACKCEVSQ6C2EXAMPLE"},
                    {group_name,"Admins"},
                    {path,"/"}]]}
             })
            ],
    output_tests(?_f(erlcloud_iam:list_groups_for_user("Bob")), Tests).

-define(LIST_USER_POLICIES_RESP,
        "<ListUserPoliciesResponse>
           <ListUserPoliciesResult>
             <PolicyNames>
               <member>AllAccessPolicy</member>
               <member>KeyPolicy</member>
             </PolicyNames>
             <IsTruncated>false</IsTruncated>
           </ListUserPoliciesResult>
           <ResponseMetadata>
             <RequestId>7a62c49f-347e-4fc4-9331-6e8eEXAMPLE</RequestId>
           </ResponseMetadata>
         </ListUserPoliciesResponse>").

list_user_policies_input_tests(_) ->
    Tests = 
        [?_iam_test(
            {"Test returning policies for a user.",
             ?_f(erlcloud_iam:list_user_policies("Bob")),
             [
              {"Action", "ListUserPolicies"},
              {"UserName", "Bob"}
              ]})
        ],

    input_tests(?LIST_USER_POLICIES_RESP, Tests).

list_user_policies_output_tests(_) ->
    Tests = [?_iam_test(
             {"This returns the policies for a user",
              ?LIST_USER_POLICIES_RESP,
              {ok,[[{policy_name,"AllAccessPolicy"}],
                   [{policy_name,"KeyPolicy"}]]}
             })
            ],
    output_tests(?_f(erlcloud_iam:list_user_policies("Bob")), Tests).

-define(LIST_GROUP_POLICIES_RESP,
        "<ListGroupPoliciesResponse>
           <ListGroupPoliciesResult>
             <PolicyNames>
               <member>AdminRoot</member>
               <member>KeyPolicy</member>
             </PolicyNames>
             <IsTruncated>false</IsTruncated>
           </ListGroupPoliciesResult>
         <ResponseMetadata>
           <RequestId>7a62c49f-347e-4fc4-9331-6e8eEXAMPLE</RequestId>
         </ResponseMetadata>
       </ListGroupPoliciesResponse>").

list_group_policies_input_tests(_) ->
    Tests = 
        [?_iam_test(
            {"Test returning policies for a group.",
             ?_f(erlcloud_iam:list_group_policies("Admins")),
             [
              {"Action", "ListGroupPolicies"},
              {"GroupName", "Admins"}
              ]})
        ],

    input_tests(?LIST_GROUP_POLICIES_RESP, Tests).

list_group_policies_output_tests(_) ->
    Tests = [?_iam_test(
             {"This returns the policies for a group",
              ?LIST_GROUP_POLICIES_RESP,
              {ok,[[{policy_name,"AdminRoot"}],
                   [{policy_name,"KeyPolicy"}]]}
             })
            ],
    output_tests(?_f(erlcloud_iam:list_group_policies("Admins")), Tests).

-define(LIST_ROLE_POLICIES_RESP,
        "<ListRolePoliciesResponse xmlns=\"https://iam.amazonaws.com/doc/2010-05-08/\">
           <ListRolePoliciesResult>
             <PolicyNames>
               <member>CloudwatchPutMetricPolicy</member>
               <member>S3AccessPolicy</member>
             </PolicyNames>
             <IsTruncated>false</IsTruncated>
           </ListRolePoliciesResult>
           <ResponseMetadata>
             <RequestId>8c7e1816-99f0-11e1-a4c3-27EXAMPLE804</RequestId>
           </ResponseMetadata>
         </ListRolePoliciesResponse>").

list_role_policies_input_tests(_) ->
    Tests = 
        [?_iam_test(
            {"Test returning policies for a role.",
             ?_f(erlcloud_iam:list_role_policies("S3Access")),
             [
              {"Action", "ListRolePolicies"},
              {"RoleName", "S3Access"}
              ]})
        ],

    input_tests(?LIST_ROLE_POLICIES_RESP, Tests).

list_role_policies_output_tests(_) ->
    Tests = [?_iam_test(
             {"This returns the policies for a role",
              ?LIST_ROLE_POLICIES_RESP,
              {ok,[[{policy_name,"CloudwatchPutMetricPolicy"}],
                   [{policy_name,"S3AccessPolicy"}]]}
             })
            ],
    output_tests(?_f(erlcloud_iam:list_role_policies("S3Access")), Tests).

-define(LIST_INSTANCE_PROFILES_RESP,
        "<ListInstanceProfilesResponse xmlns=\"https://iam.amazonaws.com/doc/2010-05-08/\">
           <ListInstanceProfilesResult>
             <IsTruncated>false</IsTruncated>
             <InstanceProfiles>
               <member>
                 <InstanceProfileId>AIPACIFN4OZXG7EXAMPLE</InstanceProfileId>
                 <Roles/>
                 <InstanceProfileName>Database</InstanceProfileName>
                 <Path>/application_abc/component_xyz/</Path>
                 <Arn>arn:aws:iam::123456789012:instance-profile/application_abc/component_xyz/Database</Arn>
                 <CreateDate>2012-05-09T16:27:03Z</CreateDate>
               </member>
               <member>
                 <InstanceProfileId>AIPACZLSXM2EYYEXAMPLE</InstanceProfileId>
                 <Roles/>
                 <InstanceProfileName>Webserver</InstanceProfileName>
                 <Path>/application_abc/component_xyz/</Path>
                 <Arn>arn:aws:iam::123456789012:instance-profile/application_abc/component_xyz/Webserver</Arn>
                 <CreateDate>2012-05-09T16:27:11Z</CreateDate>
               </member>
             </InstanceProfiles>
           </ListInstanceProfilesResult>
           <ResponseMetadata>
             <RequestId>fd74fa8d-99f3-11e1-a4c3-27EXAMPLE804</RequestId>
           </ResponseMetadata>
         </ListInstanceProfilesResponse>").

list_instance_profiles_input_tests(_) ->
    Tests = 
        [?_iam_test(
            {"Test returning instance profiles.",
             ?_f(erlcloud_iam:list_instance_profiles()),
             [
              {"Action", "ListInstanceProfiles"},
              {"PathPrefix", "%2F"}
              ]})
        ],

    input_tests(?LIST_INSTANCE_PROFILES_RESP, Tests).

list_instance_profiles_output_tests(_) ->
    Tests = [?_iam_test(
             {"This returns the instance profiles",
              ?LIST_INSTANCE_PROFILES_RESP,
              {ok,[[{instance_profile_id,"AIPACIFN4OZXG7EXAMPLE"},
                    {roles,[]},
                    {instance_profile_name,"Database"},
                    {path,"/application_abc/component_xyz/"},
                    {arn,"arn:aws:iam::123456789012:instance-profile/application_abc/component_xyz/Database"},
                    {create_date,{{2012,5,9},{16,27,3}}}],
                   [{instance_profile_id,"AIPACZLSXM2EYYEXAMPLE"},
                    {roles,[]},
                    {instance_profile_name,"Webserver"},
                    {path,"/application_abc/component_xyz/"},
                    {arn,"arn:aws:iam::123456789012:instance-profile/application_abc/component_xyz/Webserver"},
                    {create_date,{{2012,5,9},{16,27,11}}}]]}
             })
            ],
    output_tests(?_f(erlcloud_iam:list_instance_profiles()), Tests).

-define(GET_INSTANCE_PROFILE_RESP,
        "<GetInstanceProfileResponse xmlns=\"https://iam.amazonaws.com/doc/2010-05-08/\">
        <GetInstanceProfileResult>
          <InstanceProfile>
            <InstanceProfileId>AIPAD5ARO2C5EXAMPLE3G</InstanceProfileId>
            <Roles>
              <member>
                <Path>/application_abc/component_xyz/</Path>
                <Arn>arn:aws:iam::123456789012:role/application_abc/component_xyz/S3Access</Arn>
                <RoleName>S3Access</RoleName>
                <AssumeRolePolicyDocument>{\"Version\":\"2012-10-17\",\"Statement\":[{\"Effect\":\"Allow\",\"Principal\":{\"Service\":[\"ec2.amazonaws.com\"]},\"Action\":[\"sts:AssumeRole\"]}]}</AssumeRolePolicyDocument>
                <CreateDate>2012-05-09T15:45:35Z</CreateDate>
                <RoleId>AROACVYKSVTSZFEXAMPLE</RoleId>
              </member>
            </Roles>
            <InstanceProfileName>Webserver</InstanceProfileName>
            <Path>/application_abc/component_xyz/</Path>
            <Arn>arn:aws:iam::123456789012:instance-profile/application_abc/component_xyz/Webserver</Arn>
            <CreateDate>2012-05-09T16:11:10Z</CreateDate>
          </InstanceProfile>
        </GetInstanceProfileResult>
        <ResponseMetadata>
          <RequestId>37289fda-99f2-11e1-a4c3-27EXAMPLE804</RequestId>
        </ResponseMetadata>
        </GetInstanceProfileResponse>").

get_instance_profile_input_tests(_) ->
    Tests = 
        [?_iam_test(
            {"Test returning instance profile.",
             ?_f(erlcloud_iam:get_instance_profile("Webserver")),
             [
              {"Action", "GetInstanceProfile"},
              {"InstanceProfileName", "Webserver"}
              ]})
        ],

    input_tests(?GET_INSTANCE_PROFILE_RESP, Tests).

get_instance_profile_output_tests(_) ->
    Tests = [?_iam_test(
             {"This returns the instance profile",
              ?GET_INSTANCE_PROFILE_RESP,
              {ok,[[{instance_profile_id,"AIPAD5ARO2C5EXAMPLE3G"},
                    {roles,[[{path,"/application_abc/component_xyz/"},
                             {role_name,"S3Access"},
                             {role_id,"AROACVYKSVTSZFEXAMPLE"},
                             {assume_role_policy_doc,"{\"Version\":\"2012-10-17\",\"Statement\":[{\"Effect\":\"Allow\",\"Principal\":{\"Service\":[\"ec2.amazonaws.com\"]},\"Action\":[\"sts:AssumeRole\"]}]}"},
                             {create_date,{{2012,5,9},{15,45,35}}},
                             {arn,"arn:aws:iam::123456789012:role/application_abc/component_xyz/S3Access"}
                             ]]},
                    {instance_profile_name,"Webserver"},
                    {path,"/application_abc/component_xyz/"},
                    {arn,"arn:aws:iam::123456789012:instance-profile/application_abc/component_xyz/Webserver"},
                    {create_date,{{2012,5,9},{16,11,10}}}]
                  ]}
             })
            ],
    output_tests(?_f(erlcloud_iam:get_instance_profile("Webserver")), Tests).

-define(GET_ACCOUNT_AUTHORIZATION_DETAILS_RESP,
        "<GetAccountAuthorizationDetailsResponse xmlns=\"https://iam.amazonaws.com/doc/2010-05-08/\">
          <GetAccountAuthorizationDetailsResult>
            <IsTruncated>false</IsTruncated>
            <UserDetailList>
              <member>
                <GroupList>
                  <member>Admins</member>
                </GroupList>
                <UserId>AIDACKCEVSQ6C2EXAMPLE</UserId>
                <Path>/</Path>
                <UserName>Alice</UserName>
                <Arn>arn:aws:iam::123456789012:user/Alice</Arn>
                <CreateDate>2013-10-14T18:32:24Z</CreateDate>
              </member>
              <member>
                <GroupList>
                  <member>Admins</member>
                </GroupList>
                <UserPolicyList>
                  <member>
                    <PolicyName>DenyBillingPolicy</PolicyName>
                    <PolicyDocument>{\"Version\":\"2012-10-17\",\"Statement\":{\"Effect\":\"Deny\",\"Action\":\"aws-portal:*\",\"Resource\":\"*\"}}</PolicyDocument>
                  </member>
                </UserPolicyList>
                <UserId>AIDACKCEVSQ6C3EXAMPLE</UserId>
                <Path>/</Path>
                <UserName>Bob</UserName>
                <Arn>arn:aws:iam::123456789012:user/Bob</Arn>
                <CreateDate>2013-10-14T18:32:25Z</CreateDate>
              </member>
              <member>
                <GroupList>
                  <member>Dev</member>
                </GroupList>
                <UserId>AIDACKCEVSQ6C4EXAMPLE</UserId>
                <Path>/</Path>
                <UserName>Charlie</UserName>
                <Arn>arn:aws:iam::123456789012:user/Charlie</Arn>
                <CreateDate>2013-10-14T18:33:56Z</CreateDate>
              </member>
              <member>
                <GroupList>
                  <member>Dev</member>
                </GroupList>
                <UserId>AIDACKCEVSQ6C5EXAMPLE</UserId>
                <Path>/</Path>
                <UserName>Danielle</UserName>
                <Arn>arn:aws:iam::123456789012:user/Danielle</Arn>
                <CreateDate>2013-10-14T18:33:56Z</CreateDate>
              </member>
              <member>
                <GroupList>
                  <member>Finance</member>
                  <member>Admins</member>
                </GroupList>
                <UserId>AIDACKCEVSQ6C6EXAMPLE</UserId>
                <Path>/</Path>
                <UserName>Elaine</UserName>
                <Arn>arn:aws:iam::123456789012:user/Elaine</Arn>
                <CreateDate>2013-10-14T18:57:48Z</CreateDate>
              </member>
            </UserDetailList>
            <GroupDetailList>
              <member>
                <GroupId>AIDACKCEVSQ6C7EXAMPLE</GroupId>
                <GroupName>Admins</GroupName>
                <Path>/</Path>
                <Arn>arn:aws:iam::123456789012:group/Admins</Arn>
                <CreateDate>2013-10-14T18:32:24Z</CreateDate>
                <GroupPolicyList>
                  <member>
                    <PolicyName>AdministratorAccess-201409151020</PolicyName>
                    <PolicyDocument>{\"Version\":\"2012-10-17\",\"Statement\":{\"Effect\":\"Allow\",\"Action\":\"*\",\"Resource\":\"*\"}}</PolicyDocument>
                  </member>
                </GroupPolicyList>
              </member>
              <member>
                <GroupId>AIDACKCEVSQ6C8EXAMPLE</GroupId>
                <GroupName>Dev</GroupName>
                <Path>/</Path>
                <Arn>arn:aws:iam::123456789012:group/Dev</Arn>
                <CreateDate>2013-10-14T18:33:55Z</CreateDate>
                <GroupPolicyList>
                  <member>
                    <PolicyName>PowerUserAccess-201310141133</PolicyName>
                    <PolicyDocument>{\"Version\":\"2012-10-17\",\"Statement\":{\"Effect\":\"Allow\",\"NotAction\":\"iam:*\",\"Resource\":\"*\"}}</PolicyDocument>
                  </member>
                </GroupPolicyList>
              </member>
              <member>
                <GroupId>AIDACKCEVSQ6C9EXAMPLE</GroupId>
                <GroupName>Finance</GroupName>
                <Path>/</Path>
                <Arn>arn:aws:iam::123456789012:group/Finance</Arn>
                <CreateDate>2013-10-14T18:57:48Z</CreateDate>
                <GroupPolicyList>
                  <member>
                    <PolicyName>policygen-201310141157</PolicyName>
                    <PolicyDocument>{\"Version\":\"2012-10-17\",\"Statement\":[{\"Action\":[\"aws-portal:*\"],\"Sid\":\"Stmt1381777017000\",\"Resource\":[\"*\"],\"Effect\":\"Allow\"}]}</PolicyDocument>
                  </member>
                </GroupPolicyList>
              </member>
            </GroupDetailList>
            <RoleDetailList>
              <member>
                <RolePolicyList>
                  <member>
                    <PolicyName>S3andDDBaccess-EC2role-201407301009</PolicyName>
                    <PolicyDocument>{\"Version\":\"2012-10-17\",\"Statement\":{\"Effect\":\"Allow\",\"Action\":[ \"s3:*\",\"dynamodb:*\"],\"Resource\":\"*\"}}</PolicyDocument>
                  </member>
                </RolePolicyList>
                <InstanceProfileList>
                  <member>
                    <InstanceProfileName>EC2role</InstanceProfileName>
                    <Roles>
                      <member>
                        <Path>/</Path>
                        <Arn>arn:aws:iam::123456789012:role/EC2role</Arn>
                        <RoleName>EC2role</RoleName>
                        <AssumeRolePolicyDocument>{\"Version\":\"2012-10-17\",\"Statement\":[{\"Sid\":\"\",\"Effect\":\"Allow\",\"Principal\":{\"Service\":\"ec2.amazonaws.com\"},\"Action\":\"sts:AssumeRole\"}]}</AssumeRolePolicyDocument>
                        <CreateDate>2014-07-30T17:09:20Z</CreateDate>
                        <RoleId>AROAFP4BKI7Y7TEXAMPLE</RoleId>
                      </member>
                    </Roles>
                    <Path>/</Path>
                    <Arn>arn:aws:iam::123456789012:instance-profile/EC2role</Arn>
                    <InstanceProfileId>AIPAFFYRBHWXW2EXAMPLE</InstanceProfileId>
                    <CreateDate>2014-07-30T17:09:20Z</CreateDate>
                  </member>
                </InstanceProfileList>
                <Path>/</Path>
                <Arn>arn:aws:iam::123456789012:role/EC2role</Arn>
                <RoleName>EC2role</RoleName>
                <AssumeRolePolicyDocument>{\"Version\":\"2012-10-17\",\"Statement\":[{\"Sid\":\"\",\"Effect\":\"Allow\",\"Principal\":{\"Service\":\"ec2.amazonaws.com\"},\"Action\":\"sts:AssumeRole\"}]}</AssumeRolePolicyDocument>
                <CreateDate>2014-07-30T17:09:20Z</CreateDate>
                <RoleId>AROAFP4BKI7Y7TEXAMPLE</RoleId>
              </member>
            </RoleDetailList>
          </GetAccountAuthorizationDetailsResult>
          <ResponseMetadata>
            <RequestId>92e79ae7-7399-11e4-8c85-4b53eEXAMPLE</RequestId>
          </ResponseMetadata>
        </GetAccountAuthorizationDetailsResponse>").

get_account_authorization_details_input_tests(_) ->
    Tests = 
        [?_iam_test(
            {"Test returning the authorization details.",
             ?_f(erlcloud_iam:get_account_authorization_details()),
             [
              {"Action", "GetAccountAuthorizationDetails"}
              ]})
        ],

    input_tests(?GET_ACCOUNT_AUTHORIZATION_DETAILS_RESP, Tests).

get_account_authorization_details_output_tests(_) ->
    Tests = [?_iam_test(
             {"This returns the authorization details",
              ?GET_ACCOUNT_AUTHORIZATION_DETAILS_RESP,
              {ok, [{roles,
                     [[{arn,"arn:aws:iam::123456789012:role/EC2role"},
                       {assume_role_policy_document,
                        "{\"Version\":\"2012-10-17\",\"Statement\":[{\"Sid\":\"\",\"Effect\":\"Allow\",\"Principal\":{\"Service\":\"ec2.amazonaws.com\"},\"Action\":\"sts:AssumeRole\"}]}"},
                       {create_date,{{2014,7,30},{17,9,20}}},
                       {instance_profiles,
                        [[{instance_profile_id,"AIPAFFYRBHWXW2EXAMPLE"},
                          {roles,
                           [[{path,"/"},
                             {role_name,"EC2role"},
                             {role_id,"AROAFP4BKI7Y7TEXAMPLE"},
                             {assume_role_policy_doc,
                              "{\"Version\":\"2012-10-17\",\"Statement\":[{\"Sid\":\"\",\"Effect\":\"Allow\",\"Principal\":{\"Service\":\"ec2.amazonaws.com\"},\"Action\":\"sts:AssumeRole\"}]}"},
                             {create_date,{{2014,7,30},{17,9,20}}},
                             {arn,"arn:aws:iam::123456789012:role/EC2role"}]]},
                          {instance_profile_name,"EC2role"},
                          {path,"/"},
                          {arn,"arn:aws:iam::123456789012:instance-profile/EC2role"},
                          {create_date,{{2014,7,30},{17,9,20}}}]]},
                       {path,"/"},
                       {role_id,"AROAFP4BKI7Y7TEXAMPLE"},
                       {role_name,"EC2role"},
                       {role_policy_list,
                        [[{policy_document,
                           "{\"Version\":\"2012-10-17\",\"Statement\":{\"Effect\":\"Allow\",\"Action\":[ \"s3:*\",\"dynamodb:*\"],\"Resource\":\"*\"}}"},
                          {policy_name,"S3andDDBaccess-EC2role-201407301009"}]]}]]},
                    {groups,
                     [[{arn,"arn:aws:iam::123456789012:group/Admins"},
                       {create_date,{{2013,10,14},{18,32,24}}},
                       {group_id,"AIDACKCEVSQ6C7EXAMPLE"},
                       {group_name,"Admins"},
                       {group_policy_list,
                        [[{policy_document,
                           "{\"Version\":\"2012-10-17\",\"Statement\":{\"Effect\":\"Allow\",\"Action\":\"*\",\"Resource\":\"*\"}}"},
                          {policy_name,"AdministratorAccess-201409151020"}]]},
                       {path,"/"}],
                      [{arn,"arn:aws:iam::123456789012:group/Dev"},
                       {create_date,{{2013,10,14},{18,33,55}}},
                       {group_id,"AIDACKCEVSQ6C8EXAMPLE"},
                       {group_name,"Dev"},
                       {group_policy_list,
                        [[{policy_document,
                           "{\"Version\":\"2012-10-17\",\"Statement\":{\"Effect\":\"Allow\",\"NotAction\":\"iam:*\",\"Resource\":\"*\"}}"},
                          {policy_name,"PowerUserAccess-201310141133"}]]},
                       {path,"/"}],
                      [{arn,"arn:aws:iam::123456789012:group/Finance"},
                       {create_date,{{2013,10,14},{18,57,48}}},
                       {group_id,"AIDACKCEVSQ6C9EXAMPLE"},
                       {group_name,"Finance"},
                       {group_policy_list,
                        [[{policy_document,
                           "{\"Version\":\"2012-10-17\",\"Statement\":[{\"Action\":[\"aws-portal:*\"],\"Sid\":\"Stmt1381777017000\",\"Resource\":[\"*\"],\"Effect\":\"Allow\"}]}"},
                          {policy_name,"policygen-201310141157"}]]},
                       {path,"/"}]]},
                    {users,
                     [[{arn,"arn:aws:iam::123456789012:user/Alice"},
                       {create_date,{{2013,10,14},{18,32,24}}},
                       {group_list,[[{group_name,"Admins"}]]},
                       {path,"/"},
                       {user_id,"AIDACKCEVSQ6C2EXAMPLE"},
                       {user_name,"Alice"},
                       {user_policy_list,[]}],
                      [{arn,"arn:aws:iam::123456789012:user/Bob"},
                       {create_date,{{2013,10,14},{18,32,25}}},
                       {group_list,[[{group_name,"Admins"}]]},
                       {path,"/"},
                       {user_id,"AIDACKCEVSQ6C3EXAMPLE"},
                       {user_name,"Bob"},
                       {user_policy_list,
                        [[{policy_document,
                           "{\"Version\":\"2012-10-17\",\"Statement\":{\"Effect\":\"Deny\",\"Action\":\"aws-portal:*\",\"Resource\":\"*\"}}"},
                          {policy_name,"DenyBillingPolicy"}]]}],
                      [{arn,"arn:aws:iam::123456789012:user/Charlie"},
                       {create_date,{{2013,10,14},{18,33,56}}},
                       {group_list,[[{group_name,"Dev"}]]},
                       {path,"/"},
                       {user_id,"AIDACKCEVSQ6C4EXAMPLE"},
                       {user_name,"Charlie"},
                       {user_policy_list,[]}],
                      [{arn,"arn:aws:iam::123456789012:user/Danielle"},
                       {create_date,{{2013,10,14},{18,33,56}}},
                       {group_list,[[{group_name,"Dev"}]]},
                       {path,"/"},
                       {user_id,"AIDACKCEVSQ6C5EXAMPLE"},
                       {user_name,"Danielle"},
                       {user_policy_list,[]}],
                      [{arn,"arn:aws:iam::123456789012:user/Elaine"},
                       {create_date,{{2013,10,14},{18,57,48}}},
                       {group_list,[[{group_name,"Finance"}],[{group_name,"Admins"}]]},
                       {path,"/"},
                       {user_id,"AIDACKCEVSQ6C6EXAMPLE"},
                       {user_name,"Elaine"},
                       {user_policy_list,[]}]]}]}
             })
            ],
    output_tests(?_f(erlcloud_iam:get_account_authorization_details()), Tests).

-define(GENERATE_CREDENTIAL_REPORT_RESP,
        "<GenerateCredentialReportResponse xmlns=\"https://iam.amazonaws.com/doc/2010-05-08/\">
           <GenerateCredentialReportResult>
             <Description>No report exists. Starting a new report generation task</Description>
             <State>STARTED</State>
           </GenerateCredentialReportResult>
           <ResponseMetadata>
             <RequestId>29f47818-99f5-11e1-a4c3-27EXAMPLE804</RequestId>
          </ResponseMetadata>
        </GenerateCredentialReportResponse>").

generate_credential_report_input_tests(_) ->
    Tests = 
        [?_iam_test(
         {"Test generating credential report.",
          ?_f(erlcloud_iam:generate_credential_report()),
          [
           {"Action", "GenerateCredentialReport"}
          ]})
        ],
    
    input_tests(?GENERATE_CREDENTIAL_REPORT_RESP, Tests).

generate_credential_report_output_tests(_) ->
    Tests = [?_iam_test(
             {"This returns the credential report",
              ?GENERATE_CREDENTIAL_REPORT_RESP,
              {ok,[[{description,"No report exists. Starting a new report generation task"},
                    {state,"STARTED"}]]}
             })
            ],
    output_tests(?_f(erlcloud_iam:generate_credential_report()), Tests).

-define(GET_CREDENTIAL_REPORT_RESP,
        "<GetCredentialReportResponse xmlns=\"https://iam.amazonaws.com/doc/2010-05-08/\">
           <GetCredentialReportResult>
             <Content>BASE-64 ENCODED FILE CONTENTS</Content>
             <ReportFormat>text/csv</ReportFormat>
             <GeneratedTime>2014-08-28T21:42:50Z</GeneratedTime>
           </GetCredentialReportResult>
           <ResponseMetadata>
             <RequestId>29f47818-99f5-11e1-a4c3-27EXAMPLE804</RequestId>
           </ResponseMetadata>
         </GetCredentialReportResponse>").

get_credential_report_input_tests(_) ->
    Tests = 
        [?_iam_test(
         {"Test get credential report.",
          ?_f(erlcloud_iam:get_credential_report()),
          [
           {"Action", "GetCredentialReport"}
          ]})
        ],
    
    input_tests(?GET_CREDENTIAL_REPORT_RESP, Tests).

get_credential_report_output_tests(_) ->
    Tests = [?_iam_test(
             {"This returns the credential report",
              ?GET_CREDENTIAL_REPORT_RESP,
              {ok,[[{content,"BASE-64 ENCODED FILE CONTENTS"},
                    {report_format,"text/csv"},
                    {generated_time,{{2014,8,28},{21,42,50}}}]]}
             })
            ],
    output_tests(?_f(erlcloud_iam:get_credential_report()), Tests).

-define(LIST_ATTACHED_USER_POLICIES_RESP,
        "<ListAttachedUserPoliciesResponse xmlns=\"https://iam.amazonaws.com/doc/2010-05-08/\">
          <ListAttachedUserPoliciesResult>
            <AttachedPolicies>
              <member>
                <PolicyName>AdministratorAccess</PolicyName>
                <PolicyArn>arn:aws:iam::aws:policy/AdministratorAccess</PolicyArn>
              </member>
            </AttachedPolicies>
            <IsTruncated>false</IsTruncated>
          </ListAttachedUserPoliciesResult>
          <ResponseMetadata>
            <RequestId>75980e78-3ea6-11e4-9d0d-6f969EXAMPLE</RequestId>
          </ResponseMetadata>
        </ListAttachedUserPoliciesResponse>").

list_attached_user_policies_input_tests(_) ->
    Tests = 
        [?_iam_test(
            {"Test returning the list of user attached policies.",
             ?_f(erlcloud_iam:list_attached_user_policies("Alice", "/")),
             [
              {"Action", "ListAttachedUserPolicies"},
              {"UserName", "Alice"},
              {"PathPrefix", http_uri:encode("/")}
              ]})
        ],

    input_tests(?LIST_ATTACHED_USER_POLICIES_RESP, Tests).

list_attached_user_policies_output_tests(_) ->
    Tests = 
        [?_iam_test(
            {"This returns the list of user attached policies.",
            ?LIST_ATTACHED_USER_POLICIES_RESP,
            {ok, [[{arn, "arn:aws:iam::aws:policy/AdministratorAccess"},
                   {policy_name, "AdministratorAccess"}]]}
            })
        ],
    output_tests(?_f(erlcloud_iam:list_attached_user_policies("Alice", "/")), Tests).

-define(LIST_ATTACHED_GROUP_POLICIES_RESP,
        "<ListAttachedGroupPoliciesResponse xmlns=\"https://iam.amazonaws.com/doc/2010-05-08/\">
          <ListAttachedGroupPoliciesResult>
            <AttachedPolicies>
              <member>
                <PolicyName>ReadOnlyAccess</PolicyName>
                <PolicyArn>arn:aws:iam::aws:policy/ReadOnlyAccess</PolicyArn>
              </member>
            </AttachedPolicies>
            <IsTruncated>false</IsTruncated>
          </ListAttachedGroupPoliciesResult>
          <ResponseMetadata>
            <RequestId>710f2d3f-3df1-11e4-9d0d-6f969EXAMPLE</RequestId>
          </ResponseMetadata>
        </ListAttachedGroupPoliciesResponse>").

list_attached_group_policies_input_tests(_) ->
    Tests = 
        [?_iam_test(
            {"Test returning the list of group attached policies.",
             ?_f(erlcloud_iam:list_attached_group_policies("ReadOnlyUsers", "/")),
             [
              {"Action", "ListAttachedGroupPolicies"},
              {"GroupName", "ReadOnlyUsers"},
              {"PathPrefix", http_uri:encode("/")}
              ]})
        ],

    input_tests(?LIST_ATTACHED_GROUP_POLICIES_RESP, Tests).

list_attached_group_policies_output_tests(_) ->
    Tests = 
        [?_iam_test(
            {"This returns the list of group attached policies.",
            ?LIST_ATTACHED_GROUP_POLICIES_RESP,
            {ok, [[{arn, "arn:aws:iam::aws:policy/ReadOnlyAccess"},
                   {policy_name, "ReadOnlyAccess"}]]}
            })
        ],
    output_tests(?_f(erlcloud_iam:list_attached_group_policies("ReadOnlyUsers", "/")), Tests).

-define(LIST_ATTACHED_ROLE_POLICIES_RESP,
        "<ListAttachedRolePoliciesResponse xmlns=\"https://iam.amazonaws.com/doc/2010-05-08/\">
          <ListAttachedRolePoliciesResult>
            <AttachedPolicies>
              <member>
                <PolicyName>ReadOnlyAccess</PolicyName>
                <PolicyArn>arn:aws:iam::aws:policy/ReadOnlyAccess</PolicyArn>
              </member>
            </AttachedPolicies>
            <IsTruncated>false</IsTruncated>
          </ListAttachedRolePoliciesResult>
          <ResponseMetadata>
            <RequestId>9a3b490d-3ea5-11e4-9d0d-6f969EXAMPLE</RequestId>
          </ResponseMetadata>
        </ListAttachedRolePoliciesResponse>").

list_attached_role_policies_input_tests(_) ->
    Tests = 
        [?_iam_test(
            {"Test returning the list of role attached policies.",
             ?_f(erlcloud_iam:list_attached_role_policies("ReadOnlyRole", "/")),
             [
              {"Action", "ListAttachedRolePolicies"},
              {"RoleName", "ReadOnlyRole"},
              {"PathPrefix", http_uri:encode("/")}
              ]})
        ],

    input_tests(?LIST_ATTACHED_ROLE_POLICIES_RESP, Tests).

list_attached_role_policies_output_tests(_) ->
    Tests = 
        [?_iam_test(
            {"This returns the list of role attached policies.",
            ?LIST_ATTACHED_ROLE_POLICIES_RESP,
            {ok, [[{arn, "arn:aws:iam::aws:policy/ReadOnlyAccess"},
                   {policy_name, "ReadOnlyAccess"}]]}
            })
        ],
    output_tests(?_f(erlcloud_iam:list_attached_role_policies("ReadOnlyRole", "/")), Tests).

-define(GET_POLICY_RESP, 
        "<GetPolicyResponse xmlns=\"https://iam.amazonaws.com/doc/2010-05-08/\">
          <GetPolicyResult>
            <Policy>
              <PolicyName>S3-read-only-example-bucket</PolicyName>
              <DefaultVersionId>v1</DefaultVersionId>
              <PolicyId>AGPACKCEVSQ6C2EXAMPLE</PolicyId>
              <Path>/</Path>
              <Arn>arn:aws:iam::123456789012:policy/S3-read-only-example-bucket</Arn>
              <AttachmentCount>9</AttachmentCount>
              <CreateDate>2014-09-15T17:36:14Z</CreateDate>
              <UpdateDate>2014-09-15T20:31:47Z</UpdateDate>
            </Policy>
          </GetPolicyResult>
          <ResponseMetadata>
            <RequestId>684f0917-3d22-11e4-a4a0-cffb9EXAMPLE</RequestId>
          </ResponseMetadata>
        </GetPolicyResponse>").
get_policy_input_tests(_) ->
    Tests =
        [?_iam_test(
            {"Test returning a policy.",
             ?_f(erlcloud_iam:get_policy("arn:aws:iam::123456789012:policy/S3-read-only-example-bucket")),
             [
              {"Action", "GetPolicy"},
              {"PolicyArn", http_uri:encode("arn:aws:iam::123456789012:policy/S3-read-only-example-bucket")}
              ]})
        ],

    input_tests(?GET_POLICY_RESP, Tests).

get_policy_output_tests(_) ->
    Tests = 
        [?_iam_test(
            {"This returns a policy.",
            ?GET_POLICY_RESP,
            {ok, [[{update_date, {{2014,9,15},{20,31,47}}},
                   {create_date, {{2014,9,15},{17,36,14}}},
                   {attachment_count, 9},
                   {arn, "arn:aws:iam::123456789012:policy/S3-read-only-example-bucket"},
                   {path, "/"},
                   {policy_id, "AGPACKCEVSQ6C2EXAMPLE"},
                   {default_version_id, "v1"},
                   {policy_name, "S3-read-only-example-bucket"}
                 ]]}
            })
        ],
    output_tests(?_f(erlcloud_iam:get_policy("arn:aws:iam::123456789012:policy/S3-read-only-example-bucket")), Tests).

-define(GET_POLICY_VERSION_RESP,
        "<GetPolicyVersionResponse xmlns=\"https://iam.amazonaws.com/doc/2010-05-08/\">
          <GetPolicyVersionResult>
            <PolicyVersion>
              <Document>
              {\"Version\":\"2012-10-17\",\"Statement\":[{\"Effect\":\"Allow\",\"Action\":[\"s3:Get*\",\"s3:List*\"],
              \"Resource\":[\"arn:aws:s3:::EXAMPLE-BUCKET\",\"arn:aws:s3:::EXAMPLE-BUCKET/*\"]}]}
              </Document>
              <IsDefaultVersion>true</IsDefaultVersion>
              <VersionId>v1</VersionId>
              <CreateDate>2014-09-15T20:31:47Z</CreateDate>
            </PolicyVersion>
          </GetPolicyVersionResult>
          <ResponseMetadata>
            <RequestId>d472f28e-3d23-11e4-a4a0-cffb9EXAMPLE</RequestId>
          </ResponseMetadata>
        </GetPolicyVersionResponse>").
get_policy_version_input_tests(_) ->
    Tests =
        [?_iam_test(
            {"Test returning a policy version.",
             ?_f(erlcloud_iam:get_policy_version("arn:aws:iam::123456789012:policy/S3-read-only-example-bucket", "v1")),
             [
              {"Action", "GetPolicyVersion"},
              {"PolicyArn", http_uri:encode("arn:aws:iam::123456789012:policy/S3-read-only-example-bucket")},
              {"VersionId", "v1"}
              ]})
        ],

    input_tests(?GET_POLICY_VERSION_RESP, Tests).

get_policy_version_output_tests(_) ->
    Tests = 
        [?_iam_test(
            {"This returns a policy version.",
            ?GET_POLICY_VERSION_RESP,
            {ok, [[{create_date, {{2014,9,15},{20,31,47}}},
                   {version_id, "v1"},
                   {is_default_version, true},
                   {policy_document, 
                        "\n              {\"Version\":\"2012-10-17\",\"Statement\":[{\"Effect\":\"Allow\",\"Action\":[\"s3:Get*\",\"s3:List*\"],\n              \"Resource\":[\"arn:aws:s3:::EXAMPLE-BUCKET\",\"arn:aws:s3:::EXAMPLE-BUCKET/*\"]}]}\n              "}
                 ]]}
            })
        ],
    output_tests(?_f(erlcloud_iam:get_policy_version("arn:aws:iam::123456789012:policy/S3-read-only-example-bucket", "v1")), Tests).


