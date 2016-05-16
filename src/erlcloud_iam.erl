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
    list_users/0, list_users/1, list_users/2,
    list_groups_for_user/1, list_groups_for_user/2,
    list_user_policies/1, list_user_policies/2,
    get_user_policy/2, get_user_policy/3,
    get_login_profile/1, get_login_profile/2,
    list_groups/0, list_groups/1, list_groups/2,
    list_group_policies/1, list_group_policies/2,
    get_group_policy/2, get_group_policy/3,
    list_roles/0, list_roles/1, list_roles/2,
    list_role_policies/1, list_role_policies/2,
    get_role_policy/2, get_role_policy/3,
    list_instance_profiles/1, list_instance_profiles/2,
    get_account_password_policy/0, get_account_password_policy/1
]).

-import(erlcloud_xml, [get_text/1, get_text/2, get_bool/2]).

-define(API_VERSION, "2010-05-08").

-spec new(string(), string()) -> aws_config().
new(AccessKeyID, SecretAccessKey) ->
    #aws_config{access_key_id=AccessKeyID,
                secret_access_key=SecretAccessKey}.

-spec new(string(), string(), string()) -> aws_config().
new(AccessKeyID, SecretAccessKey, Host) ->
    #aws_config{access_key_id=AccessKeyID,
                secret_access_key=SecretAccessKey,
                ec2_host=Host}.

-spec configure(string(), string()) -> ok.
configure(AccessKeyID, SecretAccessKey) ->
    put(aws_config, new(AccessKeyID, SecretAccessKey)),
    ok.

-spec configure(string(), string(), string()) -> ok.
configure(AccessKeyID, SecretAccessKey, Host) ->
    put(aws_config, new(AccessKeyID, SecretAccessKey, Host)),
    ok.


%
% Users API
%
-spec get_user() -> proplist().
get_user() -> get_user([]).
-spec get_user(string() | aws_config()) -> proplist().
get_user(Config)
  when is_record(Config, aws_config) ->
    get_user("", Config);
get_user(UserName) ->
    get_user(UserName, default_config()).

-spec get_user(string(), aws_config()) -> proplist().
get_user("", Config) ->
    get_user_impl([], Config);
get_user(UserName, Config) ->
    get_user_impl([{"UserName", UserName}], Config).
    
get_user_impl(UserNameParam, Config)
  when is_record(Config, aws_config) ->
    case iam_query(Config, "GetUser", UserNameParam) of
        {ok, Doc} ->
            Items = xmerl_xpath:string("/GetUserResponse/GetUserResult/User", Doc),
            [User] = [extract_user_item(Item) || Item <- Items],
            {ok, User};
        {error, _} = Error ->
            Error
    end.

% TODO: Make sure to handle pagination of results
-spec list_users() -> proplist().
list_users() -> list_users([]).
-spec list_users([string()] | aws_config()) -> proplist().
list_users(Config)
  when is_record(Config, aws_config) ->
    list_users("/", Config);
list_users(PathPrefix) ->
    list_users(PathPrefix, default_config()).

-spec list_users([string()], aws_config()) -> proplist().
list_users(PathPrefix, Config)
  when is_list(PathPrefix) ->
    case iam_query(Config, "ListUsers", [{"PathPrefix", PathPrefix}]) of
        {ok, Doc} ->
            Items = xmerl_xpath:string("/ListUsersResponse/ListUsersResult/Users/member", Doc),
            {ok, [extract_user_item(Item) || Item <- Items]};
        {error, _} = Error ->
            Error
    end.

-spec list_groups_for_user(string()) -> proplist().
list_groups_for_user(UserName) ->
    list_groups_for_user(UserName, default_config()).

-spec list_groups_for_user(string(), aws_config()) -> proplist().
list_groups_for_user(UserName, Config)
  when is_record(Config, aws_config) ->
    case iam_query(Config, "ListGroupsForUser", [{"UserName", UserName}]) of
        {ok, Doc} ->
            Items = xmerl_xpath:string("/ListGroupsForUserResponse/ListGroupsForUserResult/Groups/member", Doc),
            {ok, [extract_group_item(Item) || Item <- Items]};
        {error, _} = Error -> Error
    end.

-spec list_user_policies(string()) -> proplist().
list_user_policies(UserName) ->
    list_user_policies(UserName, default_config()).

-spec list_user_policies(string(), aws_config()) -> proplist().
list_user_policies(UserName, Config)
  when is_record(Config, aws_config) ->
    case iam_query(Config, "ListUserPolicies", [{"UserName", UserName}]) of
        {ok, Doc} ->
            Items = xmerl_xpath:string("/ListUserPoliciesResponse/ListUserPoliciesResult/PolicyNames/member", Doc),
            {ok, [[{policy_name, get_text(Item)}] || Item <- Items]};
        {error, _} = Error -> Error
    end.

-spec get_user_policy(string(), string()) -> proplist().
get_user_policy(UserName, PolicyName) ->
    get_user_policy(UserName, PolicyName, default_config()).

-spec get_user_policy(string(), string(), aws_config()) -> proplist().
get_user_policy(UserName, PolicyName, Config) 
  when is_record(Config, aws_config) ->
     case iam_query(Config, "GetUserPolicy", [{"UserName", UserName}, {"PolicyName", PolicyName}]) of
        {ok, Doc} ->
            Items = xmerl_xpath:string("/GetUserPolicyResponse/GetUserPolicyResult", Doc),
            {ok, [[
                   {policy_name, get_text("PolicyName", Item)},
                   {user_name, get_text("UserName", Item)},
                   {policy_document, get_text("PolicyDocument", Item)}] || Item <- Items]};
        {error, _} = Error ->
            Error
    end.


-spec get_login_profile(string()) -> proplist().
get_login_profile(UserName) ->
    get_login_profile(UserName, default_config()).

-spec get_login_profile(string(), aws_config()) -> proplist().
get_login_profile(UserName, Config)
  when is_record(Config, aws_config) ->
    case iam_query(Config, "GetLoginProfile", [{"UserName", UserName}]) of
        {ok, Doc} ->
            Items = xmerl_xpath:string("/GetLoginProfileResponse/GetLoginProfileResult/LoginProfile", Doc),
            {ok, [[{user_name, get_text("UserName", Item)},
                    {create_date, erlcloud_xml:get_time("CreateDate", Item)}] || Item <- Items]};
        {error, _} = Error -> Error
    end.

%
% Groups API
%
-spec list_groups() -> proplist().
list_groups() -> list_groups([]).
-spec list_groups([string()] | aws_config()) -> proplist().
list_groups(Config)
  when is_record(Config, aws_config) ->
    list_groups("/", Config);
list_groups(PathPrefix) ->
    list_groups(PathPrefix, default_config()).

-spec list_groups([string()], aws_config()) -> proplist().
list_groups(PathPrefix, Config)
  when is_list(PathPrefix) ->
    case iam_query(Config, "ListGroups", [{"PathPrefix", PathPrefix}]) of
        {ok, Doc} ->
            Items = xmerl_xpath:string("/ListGroupsResponse/ListGroupsResult/Groups/member", Doc),
            {ok, [extract_group_item(Item) || Item <- Items]};
        {error, _} = Error ->
            Error
    end.

-spec list_group_policies(string()) -> proplist().
list_group_policies(GroupName) ->
    list_group_policies(GroupName, default_config()).

-spec list_group_policies(string(), aws_config()) -> proplist().
list_group_policies(GroupName, Config)
  when is_record(Config, aws_config) ->
    case iam_query(Config, "ListGroupPolicies", [{"GroupName", GroupName}]) of
        {ok, Doc} ->
            Items = xmerl_xpath:string("/ListGroupPoliciesResponse/ListGroupPoliciesResult/PolicyNames/member", Doc),
            {ok, [[{policy_name, get_text(Item)}] || Item <- Items]};
        {error, _} = Error -> Error
    end.

-spec get_group_policy(string(), string()) -> proplist().
get_group_policy(GroupName, PolicyName) ->
    get_group_policy(GroupName, PolicyName, default_config()).

-spec get_group_policy(string(), string(), aws_config()) -> proplist().
get_group_policy(GroupName, PolicyName, Config) 
  when is_record(Config, aws_config) ->
     case iam_query(Config, "GetGroupPolicy", [{"GroupName", GroupName}, {"PolicyName", PolicyName}]) of
        {ok, Doc} ->
            Items = xmerl_xpath:string("/GetGroupPolicyResponse/GetGroupPolicyResult", Doc),
            {ok, [[
                   {policy_name, get_text("PolicyName", Item)},
                   {group_name, get_text("GroupName", Item)},
                   {policy_document, get_text("PolicyDocument", Item)}] || Item <- Items]};
        {error, _} = Error ->
            Error
    end.



%
% Roles API
%
-spec list_roles() -> proplist().
list_roles() -> list_roles([]).
-spec list_roles([string()] | aws_config()) -> proplist().
list_roles(Config)
  when is_record(Config, aws_config) ->
    list_roles("/", Config);
list_roles(PathPrefix) ->
    list_roles(PathPrefix, default_config()).

-spec list_roles([string()], aws_config()) -> proplist().
list_roles(PathPrefix, Config)
  when is_list(PathPrefix) ->
    case iam_query(Config, "ListRoles", [{"PathPrefix", PathPrefix}]) of
        {ok, Doc} ->
            Items = xmerl_xpath:string("/ListRolesResponse/ListRolesResult/Roles/member", Doc),
            {ok, [extract_role_item(Item) || Item <- Items]};
        {error, _} = Error ->
            Error
    end.

-spec list_role_policies(string()) -> proplist().
list_role_policies(RoleName) ->
    list_role_policies(RoleName, default_config()).

-spec list_role_policies([string()], aws_config()) -> proplist().
list_role_policies(RoleName, Config)
  when is_record(Config, aws_config) ->
    case iam_query(Config, "ListRolePolicies", [{"RoleName", RoleName}]) of
        {ok, Doc} ->
            Items = xmerl_xpath:string("/ListRolePoliciesResponse/ListRolePoliciesResult/PolicyNames/member", Doc),
            {ok, [[{policy_name, get_text(Item)}] || Item <- Items]};
        {error, _} = Error ->
            Error
    end.

-spec get_role_policy(string(), string()) -> proplist().
get_role_policy(RoleName, PolicyName) ->
    get_role_policy(RoleName, PolicyName, default_config()).

-spec get_role_policy(string(), string(), aws_config()) -> proplist().
get_role_policy(RoleName, PolicyName, Config) 
  when is_record(Config, aws_config) ->
     case iam_query(Config, "GetRolePolicy", [{"RoleName", RoleName}, {"PolicyName", PolicyName}]) of
        {ok, Doc} ->
            Items = xmerl_xpath:string("/GetRolePolicyResponse/GetRolePolicyResult", Doc),
            {ok, [[
                   {policy_name, get_text("PolicyName", Item)},
                   {role_name, get_text("RoleName", Item)},
                   {policy_document, get_text("PolicyDocument", Item)}] || Item <- Items]};
        {error, _} = Error ->
            Error
    end.


%
% InstanceProfile
%
-spec list_instance_profiles(string() | aws_config()) -> proplist().
list_instance_profiles(Config) 
  when is_record(Config, aws_config) ->
    list_instance_profiles("/", Config);

list_instance_profiles(PathPrefix) ->
    list_instance_profiles(PathPrefix, default_config()).

-spec list_instance_profiles(string(), aws_config()) -> proplist().
list_instance_profiles(PathPrefix, Config)
  when is_record(Config, aws_config) ->
    case iam_query(Config, "ListInstanceProfiles", [{"PathPrefix", PathPrefix}]) of
        {ok, Doc} ->
            Items = xmerl_xpath:string("/ListInstanceProfilesResponse/ListInstanceProfilesResult/InstanceProfiles/member", Doc),
            {ok, [[
                    {instance_profile_id, get_text("InstanceProfileId", Item)},
                    {roles, [extract_role_item(X)|| X <- xmerl_xpath:string("Roles/member", Item)]},
                    {instance_profile_name, get_text("InstanceProfileName", Item)},
                    {path, get_text("Path", Item)},
                    {arn, get_text("Arn", Item)},
                    {create_date, erlcloud_xml:get_time("CreateDate", Item)}] || Item <- Items]};
        {error, _} = Error ->
            Error
    end.


%
% Account APIs
%
-spec get_account_password_policy() -> proplist().
get_account_password_policy() ->
    get_account_password_policy(default_config()).

-spec get_account_password_policy(aws_config()) -> proplist().
get_account_password_policy(Config)
  when is_record(Config, aws_config) ->
    case iam_query(Config, "GetRolePolicy", []) of
        {ok, Doc} ->
            Items = xmerl_xpath:string("/GetAccountPasswordPolicyResponse/GetAccountPasswordPolicyResult/PasswordPolicy", Doc),
            {ok, [[
                   {min_pwd_length, get_text("MinimumPasswordLength", Item)},
                   {require_upper_case, get_bool("RequireUppercaseCharacters", Item)},
                   {require_lower_case, get_bool("RequireLowercaseCharacters", Item)},
                   {require_numbers, get_bool("RequireNumbers", Item)},
                   {require_symbols, get_bool("RequireSymbols", Item)},
                   {allow_pwd_change, get_bool("AllowUsersToChangePassword", Item)}] || Item <- Items]};

        {error, _} = Error ->
            Error
    end.

%
% Utils
%
iam_query(Config, Action, Params) ->
    iam_query(Config, Action, Params, ?API_VERSION).

iam_query(Config, Action, Params, ApiVersion) ->
    QParams = [{"Action", Action}, {"Version", ApiVersion}|Params],
    erlcloud_aws:aws_request_xml2(post, Config#aws_config.iam_host,
                                  "/", QParams, Config).

default_config() -> erlcloud_aws:default_config().

extract_user_item(Item) ->
    [{path, get_text("Path", Item)},
     {user_name, get_text("UserName", Item)},
     {user_id, get_text("UserId", Item)},
     {arn, get_text("Arn", Item)},
     {create_date, erlcloud_xml:get_time("CreateDate", Item)}
    ].

extract_group_item(Item) ->
    [{path, get_text("Path", Item)},
     {group_name, get_text("GroupName", Item)},
     {group_id, get_text("GroupId", Item)},
     {arn, get_text("Arn", Item)},
     {create_date, erlcloud_xml:get_time("CreateDate", Item)}
    ].

extract_role_item(Item) ->
    [{path, get_text("Path", Item)},
     {role_name, get_text("RoleName", Item)},
     {role_id, get_text("RoleId", Item)},
     {assume_role_policy_doc, http_uri:decode(get_text("AssumeRolePolicyDocument", Item))},
     {create_date, erlcloud_xml:get_time("CreateDate", Item)},
     {arn, get_text("Arn", Item)}
    ].

