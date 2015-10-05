%% Amazon Identity and Access Management Service(IAM)
% TODO: Add pagination support
-module(erlcloud_iam).

-include_lib("erlcloud/include/erlcloud.hrl").
-include_lib("erlcloud/include/erlcloud_aws.hrl").
-include_lib("xmerl/include/xmerl.hrl").

%% Library initialization.
-export([configure/2, configure/3, new/2, new/3]).

%% IAM API Functions
-export([
    %% Users
    get_user/0, get_user/1, get_user/2, 
    list_access_keys/0, list_access_keys/1, list_access_keys/2,
    list_users/0, list_users/1, list_users/2,
    list_groups_for_user/1, list_groups_for_user/2,
    list_user_policies/1, list_user_policies/2,
    list_attached_user_policies/1, list_attached_user_policies/2, list_attached_user_policies/3,
    get_user_policy/2, get_user_policy/3,
    get_login_profile/1, get_login_profile/2,
    list_groups/0, list_groups/1, list_groups/2,
    list_group_policies/1, list_group_policies/2,
    get_group_policy/2, get_group_policy/3,
    list_attached_group_policies/1, list_attached_group_policies/2, list_attached_group_policies/3,
    list_roles/0, list_roles/1, list_roles/2,
    list_role_policies/1, list_role_policies/2,
    list_attached_role_policies/1, list_attached_role_policies/2, list_attached_role_policies/3,
    get_role_policy/2, get_role_policy/3,
    get_policy/1, get_policy/2,
    get_policy_version/2, get_policy_version/3,
    list_instance_profiles/0, list_instance_profiles/1, list_instance_profiles/2,
    get_instance_profile/1, get_instance_profile/2,
    get_account_authorization_details/0, get_account_authorization_details/1,
    get_account_summary/0, get_account_summary/1,
    get_account_password_policy/0, get_account_password_policy/1,
    generate_credential_report/0, generate_credential_report/1,
    get_credential_report/0, get_credential_report/1
]).

-export([get_uri/2]).

-import(erlcloud_xml, [get_text/1, get_text/2, get_text/3, get_bool/2, get_list/2, get_integer/2]).

-define(API_VERSION, "2010-05-08").

-spec(new/2 :: (string(), string()) -> aws_config()).
new(AccessKeyID, SecretAccessKey) ->
    #aws_config{access_key_id=AccessKeyID,
                secret_access_key=SecretAccessKey}.

-spec(new/3 :: (string(), string(), string()) -> aws_config()).
new(AccessKeyID, SecretAccessKey, Host) ->
    #aws_config{access_key_id=AccessKeyID,
                secret_access_key=SecretAccessKey,
                iam_host=Host}.

-spec(configure/2 :: (string(), string()) -> ok).
configure(AccessKeyID, SecretAccessKey) ->
    put(aws_config, new(AccessKeyID, SecretAccessKey)),
    ok.

-spec(configure/3 :: (string(), string(), string()) -> ok).
configure(AccessKeyID, SecretAccessKey, Host) ->
    put(aws_config, new(AccessKeyID, SecretAccessKey, Host)),
    ok.


%
% Users API
%
-spec(get_user/0 :: () -> proplist()).
get_user() -> get_user([]).
-spec(get_user/1 :: (string() | aws_config()) -> proplist()).
get_user(#aws_config{} = Config) ->
    get_user("", Config);
get_user(UserName) ->
    get_user(UserName, default_config()).

-spec(get_user/2 :: (string(), aws_config()) -> proplist()).
get_user("", Config) ->
    get_user_impl([], Config);
get_user(UserName, Config) ->
    get_user_impl([{"UserName", UserName}], Config).
    
get_user_impl(UserNameParam, #aws_config{} = Config) ->
    ItemPath = "/GetUserResponse/GetUserResult/User",
    case iam_query(Config, "GetUser", UserNameParam, ItemPath, data_type("User")) of
        {ok, [User]} -> {ok, User};
        {error, _} = Error -> Error
    end.

-spec(list_access_keys/0 :: () -> proplist()).
list_access_keys() ->
    list_access_keys([]).

-spec(list_access_keys/1 :: (string() | aws_config()) -> proplist()).
list_access_keys(#aws_config{} = Config) ->
    list_access_keys([], Config);
list_access_keys(UserName) ->
    list_access_keys(UserName, default_config()).

-spec(list_access_keys/2 :: (string(), aws_config()) -> proplist()).
list_access_keys(UserName, #aws_config{} = Config) when is_list(UserName) ->
    Params = case UserName of
                 [] -> [];
                 UserName -> [{"UserName", UserName}]
             end,
    ItemPath = "/ListAccessKeysResponse/ListAccessKeysResult/AccessKeyMetadata/member",
    iam_query(Config, "ListAccessKeys", Params, ItemPath, data_type("AccessKeyMetadata")).

% TODO: Make sure to handle pagination of results
-spec(list_users/0 :: () -> proplist()).
list_users() -> list_users("/").
-spec(list_users/1 :: (string() | aws_config()) -> proplist()).
list_users(#aws_config{} = Config) ->
    list_users("/", Config);
list_users(PathPrefix) ->
    list_users(PathPrefix, default_config()).

-spec(list_users/2 :: (string(), aws_config()) -> proplist()).
list_users(PathPrefix, #aws_config{} = Config)
  when is_list(PathPrefix) ->
    ItemPath = "/ListUsersResponse/ListUsersResult/Users/member",
    iam_query(Config, "ListUsers", [{"PathPrefix", PathPrefix}], ItemPath, data_type("User")).

-spec(list_groups_for_user/1 :: (string()) -> proplist()).
list_groups_for_user(UserName) ->
    list_groups_for_user(UserName, default_config()).

-spec(list_groups_for_user/2 :: (string(), aws_config()) -> proplist()).
list_groups_for_user(UserName, #aws_config{} = Config) ->
    ItemPath = "/ListGroupsForUserResponse/ListGroupsForUserResult/Groups/member",
    iam_query(Config, "ListGroupsForUser", [{"UserName", UserName}], ItemPath, data_type("Group")).

-spec(list_user_policies/1 :: (string()) -> proplist()).
list_user_policies(UserName) ->
    list_user_policies(UserName, default_config()).

-spec(list_user_policies/2 :: (string(), aws_config()) -> proplist()).
list_user_policies(UserName, #aws_config{} = Config) ->
    ItemPath = "/ListUserPoliciesResponse/ListUserPoliciesResult/PolicyNames/member",
    DataType = [{policy_name, "String"}],
    iam_query(Config, "ListUserPolicies", [{"UserName", UserName}], ItemPath, DataType).

-spec(get_user_policy/2 :: (string(), string()) -> proplist()).
get_user_policy(UserName, PolicyName) ->
    get_user_policy(UserName, PolicyName, default_config()).

-spec(get_user_policy/3 :: (string(), string(), aws_config()) -> proplist()).
get_user_policy(UserName, PolicyName, #aws_config{} = Config) ->
    ItemPath = "/GetUserPolicyResponse/GetUserPolicyResult",
    Params = [{"UserName", UserName}, {"PolicyName", PolicyName}],
    iam_query(Config, "GetUserPolicy", Params, ItemPath, data_type("UserPolicyList")).

-spec(list_attached_user_policies/1 :: (string()) -> proplist()).
list_attached_user_policies(UserName) ->
    list_attached_user_policies(UserName, "/", default_config()).

-spec(list_attached_user_policies/2 :: (string(), string() | aws_config()) -> proplist()).
list_attached_user_policies(UserName, #aws_config{} = Config) ->
    list_attached_user_policies(UserName, "/", Config);
list_attached_user_policies(UserName, PathPrefix)
  when is_list(UserName), is_list(PathPrefix) ->
    list_attached_user_policies(UserName, PathPrefix, default_config()).

-spec(list_attached_user_policies/3 :: (string(), string(), aws_config()) -> proplist()).
list_attached_user_policies(UserName, [], Config) ->
    list_attached_user_policies(UserName, "/", Config);
list_attached_user_policies(UserName, PathPrefix, #aws_config{} = Config)
  when is_list(UserName), is_list(PathPrefix) ->
    ItemPath = "/ListAttachedUserPoliciesResponse/ListAttachedUserPoliciesResult/AttachedPolicies/member",
    Params = [{"UserName", UserName}, {"PathPrefix", PathPrefix}],
    iam_query(Config, "ListAttachedUserPolicies", Params, ItemPath, data_type("AttachedPolicy")).

-spec(get_login_profile/1 :: (string()) -> proplist()).
get_login_profile(UserName) ->
    get_login_profile(UserName, default_config()).

-spec(get_login_profile/2 :: (string(), aws_config()) -> proplist()).
get_login_profile(UserName, #aws_config{} = Config) ->
    ItemPath = "/GetLoginProfileResponse/GetLoginProfileResult/LoginProfile",
    iam_query(Config, "GetLoginProfile", [{"UserName", UserName}], ItemPath, data_type("LoginProfile")).

%
% Groups API
%
-spec(list_groups/0 :: () -> proplist()).
list_groups() -> list_groups([]).
-spec(list_groups/1 :: (string() | aws_config()) -> proplist()).
list_groups(#aws_config{} = Config) ->
    list_groups("/", Config);
list_groups(PathPrefix) ->
    list_groups(PathPrefix, default_config()).

-spec(list_groups/2 :: (string(), aws_config()) -> proplist()).
list_groups(PathPrefix, Config)
  when is_list(PathPrefix) ->
    ItemPath = "/ListGroupsResponse/ListGroupsResult/Groups/member",
    iam_query(Config, "ListGroups", [{"PathPrefix", PathPrefix}], ItemPath, data_type("Group")).

-spec(list_group_policies/1 :: (string()) -> proplist()).
list_group_policies(GroupName) ->
    list_group_policies(GroupName, default_config()).

-spec(list_group_policies/2 :: (string(), aws_config()) -> proplist()).
list_group_policies(GroupName, #aws_config{} = Config) ->
    ItemPath = "/ListGroupPoliciesResponse/ListGroupPoliciesResult/PolicyNames/member",
    DataTypeDef = [{policy_name, "String"}],
    iam_query(Config, "ListGroupPolicies", [{"GroupName", GroupName}], ItemPath, DataTypeDef).

-spec(get_group_policy/2 :: (string(), string()) -> proplist()).
get_group_policy(GroupName, PolicyName) ->
    get_group_policy(GroupName, PolicyName, default_config()).

-spec(get_group_policy/3 :: (string(), string(), aws_config()) -> proplist()).
get_group_policy(GroupName, PolicyName, #aws_config{} = Config) ->
    ItemPath = "/GetGroupPolicyResponse/GetGroupPolicyResult",
    Params = [{"GroupName", GroupName}, {"PolicyName", PolicyName}],
    iam_query(Config, "GetGroupPolicy", Params, ItemPath, data_type("GroupPolicyList")).

-spec(list_attached_group_policies/1 :: (string()) -> proplist()).
list_attached_group_policies(GroupName) ->
    list_attached_group_policies(GroupName, "/", default_config()).

-spec(list_attached_group_policies/2 :: (string(), string() | aws_config()) -> proplist()).
list_attached_group_policies(GroupName, #aws_config{} = Config) ->
    list_attached_group_policies(GroupName, "/", Config);
list_attached_group_policies(GroupName, PathPrefix)
  when is_list(GroupName), is_list(PathPrefix) ->
    list_attached_group_policies(GroupName, PathPrefix, default_config()).

-spec(list_attached_group_policies/3 :: (string(), string(), aws_config()) -> proplist()).
list_attached_group_policies(GroupName, PathPrefix, #aws_config{} = Config)
  when is_list(GroupName), is_list(PathPrefix) ->
    ItemPath = "/ListAttachedGroupPoliciesResponse/ListAttachedGroupPoliciesResult/AttachedPolicies/member",
    Params = [{"GroupName", GroupName}, {"PathPrefix", PathPrefix}],
    iam_query(Config, "ListAttachedGroupPolicies", Params, ItemPath, data_type("AttachedPolicy")).

%
% Roles API
%
-spec(list_roles/0 :: () -> proplist()).
list_roles() -> list_roles([]).
-spec(list_roles/1 :: (string() | aws_config()) -> proplist()).
list_roles(#aws_config{} = Config) ->
    list_roles("/", Config);
list_roles(PathPrefix) ->
    list_roles(PathPrefix, default_config()).

-spec(list_roles/2 :: (string(), aws_config()) -> proplist()).
list_roles(PathPrefix, #aws_config{} = Config)
  when is_list(PathPrefix) ->
    ItemPath = "/ListRolesResponse/ListRolesResult/Roles/member",
    iam_query(Config, "ListRoles", [{"PathPrefix", PathPrefix}], ItemPath, data_type("Role")).

-spec(list_role_policies/1 :: (string()) -> proplist()).
list_role_policies(RoleName) ->
    list_role_policies(RoleName, default_config()).

-spec(list_role_policies/2 :: (string(), aws_config()) -> proplist()).
list_role_policies(RoleName, #aws_config{} = Config) ->
    ItemPath = "/ListRolePoliciesResponse/ListRolePoliciesResult/PolicyNames/member",
    DataTypeDef = [{policy_name, "String"}],
    iam_query(Config, "ListRolePolicies", [{"RoleName", RoleName}], ItemPath, DataTypeDef).

-spec(get_role_policy/2 :: (string(), string()) -> proplist()).
get_role_policy(RoleName, PolicyName) ->
    get_role_policy(RoleName, PolicyName, default_config()).

-spec(get_role_policy/3 :: (string(), string(), aws_config()) -> proplist()).
get_role_policy(RoleName, PolicyName, #aws_config{} = Config) ->
    ItemPath = "/GetRolePolicyResponse/GetRolePolicyResult",
    Params = [{"RoleName", RoleName}, {"PolicyName", PolicyName}],
    iam_query(Config, "GetRolePolicy", Params, ItemPath, data_type("RolePolicyList")).

-spec(list_attached_role_policies/1 :: (string()) -> proplist()).
list_attached_role_policies(RoleName) ->
    list_attached_role_policies(RoleName, "/", default_config()).

-spec(list_attached_role_policies/2 :: (string(), string() | aws_config()) -> proplist()).
list_attached_role_policies(RoleName, #aws_config{} = Config) ->
    list_attached_role_policies(RoleName, "/", Config);
list_attached_role_policies(RoleName, PathPrefix)
  when is_list(RoleName), is_list(PathPrefix) ->
    list_attached_role_policies(RoleName, PathPrefix, default_config()).

-spec(list_attached_role_policies/3 :: (string(), string(), aws_config()) -> proplist()).
list_attached_role_policies(RoleName, PathPrefix, #aws_config{} = Config)
  when is_list(RoleName), is_list(PathPrefix) ->
    ItemPath = "/ListAttachedRolePoliciesResponse/ListAttachedRolePoliciesResult/AttachedPolicies/member",
    Params = [{"RoleName", RoleName}, {"PathPrefix", PathPrefix}],
    iam_query(Config, "ListAttachedRolePolicies", Params, ItemPath, data_type("AttachedPolicy")).

%
% Policies API
%
-spec(get_policy/1 :: (string()) -> proplist()).
get_policy(PolicyArn) -> get_policy(PolicyArn, default_config()).
-spec(get_policy/2 :: (string(), aws_config()) -> proplist()).
get_policy(PolicyArn, #aws_config{} = Config)
  when is_list(PolicyArn) ->
    ItemPath = "/GetPolicyResponse/GetPolicyResult/Policy",
    iam_query(Config, "GetPolicy", [{"PolicyArn", PolicyArn}], ItemPath, data_type("Policy")).

-spec(get_policy_version/2 :: (string(), string()) -> proplist()).
get_policy_version(PolicyArn, VersionId) ->
    get_policy_version(PolicyArn, VersionId, default_config()).
-spec(get_policy_version/3 :: (string(), string(), aws_config()) -> proplist()).
get_policy_version(PolicyArn, VersionId, #aws_config{} = Config)
  when is_list(PolicyArn), is_list(VersionId) ->
    ItemPath = "/GetPolicyVersionResponse/GetPolicyVersionResult/PolicyVersion",
    iam_query(Config, "GetPolicyVersion", [{"PolicyArn", PolicyArn}, {"VersionId", VersionId}], ItemPath, data_type("PolicyVersion")).

%
% InstanceProfile
%
-spec(list_instance_profiles/0 :: () -> proplist()).
list_instance_profiles() ->
    list_instance_profiles(default_config()).

-spec(list_instance_profiles/1 :: (string() | aws_config()) -> proplist()).
list_instance_profiles(#aws_config{} = Config) ->
    list_instance_profiles("/", Config);

list_instance_profiles(PathPrefix) ->
    list_instance_profiles(PathPrefix, default_config()).

-spec(list_instance_profiles/2 :: (string(), aws_config()) -> proplist()).
list_instance_profiles(PathPrefix, #aws_config{} = Config) ->
    ItemPath = "/ListInstanceProfilesResponse/ListInstanceProfilesResult/InstanceProfiles/member",
    Params = [{"PathPrefix", PathPrefix}],
    iam_query(Config, "ListInstanceProfiles", Params, ItemPath, data_type("InstanceProfile")).

-spec(get_instance_profile/1 :: (string()) -> proplist()).
get_instance_profile(ProfileName) ->
    get_instance_profile(ProfileName, default_config()).

-spec(get_instance_profile/2 :: (string(), aws_config()) -> proplist()).
get_instance_profile(ProfileName, #aws_config{} = Config) ->
    ItemPath = "/GetInstanceProfileResponse/GetInstanceProfileResult/InstanceProfile",
    Params = [{"InstanceProfileName", ProfileName}],
    iam_query(Config, "GetInstanceProfile", Params, ItemPath, data_type("InstanceProfile")).

%
% Account APIs
%
-spec(get_account_authorization_details/0 :: () -> proplist()).
get_account_authorization_details() ->
    get_account_authorization_details(default_config()).
  
-spec(get_account_authorization_details/1 :: (aws_config()) -> proplist()).
get_account_authorization_details(#aws_config{} = Config) ->
    ItemPath = "/GetAccountAuthorizationDetailsResponse/GetAccountAuthorizationDetailsResult",
    DataTypeDef = data_type("AccountAuthorizationDetails"),
    case iam_query(Config, "GetAccountAuthorizationDetails", [], ItemPath, DataTypeDef) of
        {ok, [Summary]} ->
            {ok, Summary};
        {error, _} = Error -> Error
    end.

-spec(get_account_summary/0 :: () -> proplist()).
get_account_summary() ->
    get_account_summary(default_config()).

-spec(get_account_summary/1 :: (aws_config()) -> proplist()).
get_account_summary(#aws_config{} = Config) ->
    case iam_query(Config, "GetAccountSummary", []) of
        {ok, Doc} ->
            Items = xmerl_xpath:string("/GetAccountSummaryResponse/GetAccountSummaryResult/SummaryMap", Doc),
            {ok, [extract_account_summary(Item) || Item <- Items]};
        {error, _} = Error ->
            Error
    end.

-spec(get_account_password_policy/0 :: () -> proplist()).
get_account_password_policy() ->
    get_account_password_policy(default_config()).

-spec(get_account_password_policy/1 :: (aws_config()) -> proplist()).
get_account_password_policy(#aws_config{} = Config) ->
    ItemPath = "/GetAccountPasswordPolicyResponse/GetAccountPasswordPolicyResult/PasswordPolicy",
    DataTypeDef = data_type("PasswordPolicy"),
    iam_query(Config, "GetAccountPasswordPolicy", [], ItemPath, DataTypeDef).


-spec(generate_credential_report/0 :: () -> proplist()).
generate_credential_report() ->
    generate_credential_report(default_config()).

-spec(generate_credential_report/1 :: (aws_config()) -> proplist()).
generate_credential_report(Config) ->
    ItemPath = "/GenerateCredentialReportResponse/GenerateCredentialReportResult",
    DataTypeDef = [{"State", state, "String"},
                   {"Description", description, "String"}],
    iam_query(Config, "GenerateCredentialReport", [], ItemPath, DataTypeDef).

-spec(get_credential_report/0 :: () -> proplist()).
get_credential_report() ->
    get_credential_report(default_config()).

-spec(get_credential_report/1 :: (aws_config()) -> proplist()).
get_credential_report(Config) ->
    ItemPath = "/GetCredentialReportResponse/GetCredentialReportResult",
    DataTypeDef = [{"GeneratedTime", generated_time, "DateTime"},
                   {"ReportFormat", report_format, "String"},
                   {"Content", content, "String"}],
    iam_query(Config, "GetCredentialReport", [], ItemPath, DataTypeDef).

%
% Utils
%
iam_query(Config, Action, Params) ->
    iam_query(Config, Action, Params, ?API_VERSION).

iam_query(Config, Action, Params, ApiVersion) ->
    QParams = [{"Action", Action}, {"Version", ApiVersion}|Params],
    erlcloud_aws:aws_request_xml4(post, Config#aws_config.iam_host,
                                  "/", QParams, "iam", Config).

iam_query(Config, Action, Params, ItemPath, DataTypeDef) ->
    case iam_query(Config, Action, Params) of
        {ok, Doc} ->
            Items = xmerl_xpath:string(ItemPath, Doc),
            {ok, [extract_values(DataTypeDef, Item) || Item <- Items]};
        {error, _} = Error ->
            Error
    end.

extract_values(DataTypeDef, Item) ->
    extract_values(DataTypeDef, Item, []).

extract_values([{Key, AKey, [{_,_,_}|_] = DataTypeDef}|T], Item, Acc) ->
    Items = xmerl_xs:select(Key, Item),
    Items1 = [extract_values(DataTypeDef, I) || I <- Items],
    extract_values(T, Item, [{AKey, Items1}|Acc]);
extract_values([{Key, AKey, [{_,_}|_] = DataTypeDef}|T], Item, Acc) ->
    Items = xmerl_xs:select(Key, Item),
    Items1 = [extract_values(DataTypeDef, I) || I <- Items],
    extract_values(T, Item, [{AKey, Items1}|Acc]);
extract_values([{AKey, Type}|T], Item, Acc) ->
    {Module, Fun} = data_fun(Type),
    extract_values(T, Item, [{AKey, apply(Module, Fun, [Item])}|Acc]);
extract_values([{Key, AKey, Type}|T], Item, Acc) ->
    {Module, Fun} = data_fun(Type),
    extract_values(T, Item, [{AKey, apply(Module, Fun, [Key, Item])}|Acc]);
extract_values([], _Item, Acc) ->
    Acc.

default_config() -> erlcloud_aws:default_config().

extract_account_summary(Item) ->
    Entries = xmerl_xs:select("/SummaryMap/entry", Item),
    Extract = [{"AccessKeysPerUserQuota", access_keys_per_user_quota, get_integer},
               {"AccountMFAEnabled", account_mfa_enabled, get_bool},
               {"AssumeRolePolicySizeQuota", assume_role_policy_size_quota, get_integer},
               {"GroupPolicySizeQuota", group_policy_size_quota, get_integer},
               {"Groups", groups, get_integer},
               {"GroupsPerUserQuota", groups_per_user_quota, get_integer},
               {"GroupsQuota", groups_quota, get_integer},
               {"InstanceProfiles", instance_profiles, get_integer},
               {"InstanceProfilesQuota", instance_profiles_quota, get_integer},
               {"MFADevices", mfa_devices, get_integer},
               {"MFADevicesInUse", mfa_devices_in_use, get_integer},
               {"RolePolicySizeQuota", role_policy_size_quota, get_integer},
               {"Roles", roles, get_integer},
               {"RolesQuota", roles_quota, get_integer},
               {"ServerCertificates", server_certificates, get_integer},
               {"ServerCertificatesQuota", server_certificates_quota, get_integer},
               {"SigningCertificatesPerUserQuota", signing_certificates_per_user_quota, get_integer},
               {"UserPolicySizeQuota", user_policy_size_quota, get_integer},
               {"Users", users, get_integer},
               {"UsersQuota", users_quota, get_integer}],
    lists:foldl(
      fun(E, As) ->
              Key = get_text("key", E),
              case lists:keyfind(Key, 1, Extract) of
                  false ->
                      As;
                  {Key, PKey, ValueFun} ->
                      [{PKey, apply(erlcloud_xml, ValueFun, ["value", E])}|As]
              end
      end, [], Entries).

data_type("AccountAuthorizationDetails") ->
    [{"UserDetailList/member", users, data_type("UserDetail")},
     {"GroupDetailList/member", groups, data_type("GroupDetail")},
     {"RoleDetailList/member", roles, data_type("RoleDetail")}];
data_type("AccessKeyMetadata") ->
    [{"Status", status, "String"},
     {"CreateDate", create_date, "DateTime"},
     {"AccessKeyId", access_key_id, "String"},
     {"UserName", user_name, "String"}];
data_type("InstanceProfile") ->
    [{"CreateDate", create_date, "DateTime"},
     {"Arn", arn, "String"},
     {"Path", path, "String"},
     {"InstanceProfileName", instance_profile_name, "String"},
     {"Roles/member", roles, data_type("Role")},
     {"InstanceProfileId", instance_profile_id, "String"}];
data_type("Group") ->
    [{"Path", path, "String"},
     {"GroupName", group_name, "String"},
     {"GroupId", group_id, "String"},
     {"CreateDate", create_date, "DateTime"},
     {"Arn", arn, "String"}];
data_type("GroupDetail") ->
    [{"Path", path, "String"},
     {"GroupPolicyList/member", group_policy_list, data_type("PolicyDetail")},
     {"GroupName", group_name, "String"},
     {"GroupId", group_id, "String"},
     {"CreateDate", create_date, "DateTime"},
     {"Arn", arn, "String"}];
data_type("GroupPolicyList") ->
    [{"PolicyDocument", policy_document, "Uri"},
     {"GroupName", group_name, "String"},
     {"PolicyName", policy_name, "String"}];
data_type("LoginProfile") ->
    [{"CreateDate", create_date, "DateTime"},
     {"UserName", user_name, "String"}];
data_type("PasswordPolicy") ->
    [{"RequireUppercaseCharacters", require_uppercase_characters, "Boolean"},
     {"RequireSymbols", require_symbols, "Boolean"},
     {"RequireNumbers", require_numbers, "Boolean"},
     {"RequireLowercaseCharacters", require_lowercase_characters, "Boolean"},
     {"PasswordReusePrevention", password_reuse_prevention, "Integer"},
     {"MinimumPasswordLength", minimum_password_length, "Integer"},
     {"MaxPasswordAge", max_password_age, "Integer"},
     {"HardExpiry", hard_expiry, "Boolean"},
     {"ExpirePasswords", expire_passwords, "Boolean"},
     {"AllowUsersToChangePassword", allow_users_to_change_password, "Boolean"}];
data_type("PolicyDetail") ->
    [{"PolicyName", policy_name, "String"},
     {"PolicyDocument", policy_document, "Uri"}];
data_type("Role") ->
    [{"Arn", arn, "String"},
     {"CreateDate", create_date, "DateTime"},
     {"AssumeRolePolicyDocument", assume_role_policy_doc, "Uri"},
     {"RoleId", role_id, "String"},
     {"RoleName", role_name, "String"},
     {"Path", path, "String"}];
data_type("RoleDetail") ->
    [{"RolePolicyList/member", role_policy_list, data_type("PolicyDetail")},
     {"RoleName", role_name, "String"},
     {"RoleId", role_id, "String"},
     {"Path", path, "String"},
     {"InstanceProfileList/member", instance_profiles, data_type("InstanceProfile")},
     {"CreateDate", create_date, "DateTime"},
     {"AssumeRolePolicyDocument", assume_role_policy_document, "Uri"},
     {"Arn", arn, "String"}];
data_type("RolePolicyList") ->
    [{"PolicyDocument", policy_document, "Uri"},
     {"RoleName", role_name, "String"},
     {"PolicyName", policy_name, "String"}];
data_type("Policy") ->
    [{"PolicyName", policy_name, "String"},
     {"DefaultVersionId", default_version_id, "String"},
     {"PolicyId", policy_id, "String"},
     {"Path", path, "String"},
     {"Arn", arn, "String"},
     {"AttachmentCount", attachment_count, "Integer"},
     {"CreateDate", create_date, "DateTime"},
     {"UpdateDate", update_date, "DateTime"}];
data_type("PolicyVersion") ->
    [{"Document", policy_document, "Uri"},
     {"IsDefaultVersion", is_default_version, "Boolean"},
     {"VersionId", version_id, "String"},
     {"CreateDate", create_date, "DateTime"}];
data_type("AttachedPolicy") ->
    [{"PolicyName", policy_name, "String"},
     {"PolicyArn", arn, "String"}];
data_type("User") ->
    [{"PasswordLastUsed", password_last_used, "DateTime"},
     {"CreateDate", create_date, "DateTime"},
     {"Arn", arn, "String"},
     {"UserId", user_id, "String"},
     {"UserName", user_name, "String"},
     {"Path", path, "String"}];
data_type("UserDetail") ->
    [{"UserPolicyList/member", user_policy_list, data_type("PolicyDetail")},
     {"UserName", user_name, "String"},
     {"UserId", user_id, "String"},
     {"Path", path, "String"},
     {"GroupList/member", group_list, [{group_name, "String"}]},
     {"CreateDate", create_date, "DateTime"},
     {"Arn", arn, "String"}];
data_type("UserPolicyList") ->
    [{"PolicyDocument", policy_document, "Uri"},
     {"UserName", user_name, "String"},
     {"PolicyName", policy_name, "String"}].

data_fun("String") -> {erlcloud_xml, get_text};
data_fun("DateTime") -> {erlcloud_xml, get_time};
data_fun("Integer") -> {erlcloud_xml, get_integer};
data_fun("Boolean") -> {erlcloud_xml, get_bool};
data_fun("Uri") -> {?MODULE, get_uri}.

get_uri(Key, Item) ->
    http_uri:decode(erlcloud_xml:get_text(Key, Item)).
