-module(erlcloud_cognito_user_pools).

-include("erlcloud_aws.hrl").

-export([configure/2, configure/3, new/2, new/3]).

-export([
    list_users/1,
    list_users/2,
    list_users/5,
    list_users/6,
    list_all_users/1,
    list_all_users/2,
    list_all_users/3,

    admin_list_groups_for_user/2,
    admin_list_groups_for_user/3,
    admin_list_groups_for_user/4,
    admin_list_groups_for_user/5,

    admin_get_user/2,
    admin_get_user/3,

    admin_create_user/2,
    admin_create_user/3,
    admin_create_user/4,
    admin_delete_user/2,
    admin_delete_user/3,

    admin_add_user_to_group/3,
    admin_add_user_to_group/4,

    admin_remove_user_from_group/3,
    admin_remove_user_from_group/4,

    create_group/2,
    create_group/3,
    create_group/5,
    create_group/6,

    delete_group/2,
    delete_group/3,

    admin_reset_user_password/2,
    admin_reset_user_password/3,
    admin_reset_user_password/4,

    admin_update_user_attributes/3,
    admin_update_user_attributes/4,
    admin_update_user_attributes/5,

    change_password/3,
    change_password/4,

    list_user_pools/0,
    list_user_pools/1,
    list_user_pools/2,
    list_all_user_pools/0,
    list_all_user_pools/1,

    admin_set_user_password/3,
    admin_set_user_password/4,
    admin_set_user_password/5,

    describe_user_pool/1,
    describe_user_pool/2,

    get_user_pool_mfa_config/1,
    get_user_pool_mfa_config/2,

    list_identity_providers/1,
    list_identity_providers/3,
    list_identity_providers/4,
    list_all_identity_providers/1,
    list_all_identity_providers/2,

    describe_identity_provider/2,
    describe_identity_provider/3,

    describe_user_pool_client/2,
    describe_user_pool_client/3,

    list_user_pool_clients/1,
    list_user_pool_clients/3,
    list_user_pool_clients/4,
    list_all_user_pool_clients/1,
    list_all_user_pool_clients/2,

    admin_list_devices/2,
    admin_list_devices/3,
    admin_list_devices/5,
    admin_list_all_devices/2,
    admin_list_all_devices/3,

    admin_forget_device/3,
    admin_forget_device/4,

    admin_confirm_signup/2,
    admin_confirm_signup/3,
    admin_confirm_signup/4,

    admin_initiate_auth/4,
    admin_initiate_auth/5,
    admin_initiate_auth/8,

    respond_to_auth_challenge/4,
    respond_to_auth_challenge/5,
    respond_to_auth_challenge/8,

    create_identity_provider/4,
    create_identity_provider/5,
    create_identity_provider/6,
    create_identity_provider/7,

    delete_identity_provider/2,
    delete_identity_provider/3,

    update_identity_provider/2,
    update_identity_provider/3,
    update_identity_provider/4,
    update_identity_provider/5,
    update_identity_provider/6,

    request/2,
    request/3
]).

-define(MAX_RESULTS, 60).
-define(API_VERSION, "2016-04-18").

-spec new(string(), string()) -> aws_config().
new(AccessKeyID, SecretAccessKey) ->
    #aws_config{access_key_id     = AccessKeyID,
                secret_access_key = SecretAccessKey,
                retry             = fun erlcloud_retry:default_retry/1}.

-spec new(string(), string(), string()) -> aws_config().
new(AccessKeyID, SecretAccessKey, Host) ->
    #aws_config{access_key_id           = AccessKeyID,
                secret_access_key       = SecretAccessKey,
                cognito_user_pools_host = Host,
                retry                   = fun erlcloud_retry:default_retry/1}.

-spec configure(string(), string()) -> ok.
configure(AccessKeyID, SecretAccessKey) ->
    put(aws_config, new(AccessKeyID, SecretAccessKey)),
    ok.

-spec configure(string(), string(), string()) -> ok.
configure(AccessKeyID, SecretAccessKey, Host) ->
    put(aws_config, new(AccessKeyID, SecretAccessKey, Host)),
    ok.

-spec list_users(binary()) -> {ok, map()} | {error, any()}.
list_users(UserPoolId) ->
    list_users(UserPoolId, undefined, undefined, undefined, undefined).

-spec list_users(binary(), aws_config()) -> {ok, map()} | {error, any()}.
list_users(UserPoolId, Config) ->
    Body = #{
        <<"UserPoolId">> => unicode:characters_to_binary(UserPoolId)
    },
    request(Config, "ListUsers", Body).

-spec list_users(binary(),
                 [binary()] | undefined,
                 binary() | undefined,
                 number() | undefined,
                 binary() | undefined) -> {ok, map()} | {error, any()}.
list_users(UserPoolId, AttributesToGet, Filter, Limit, PaginationToken) ->
    Config = erlcloud_aws:default_config(),
    list_users(UserPoolId, AttributesToGet, Filter, Limit, PaginationToken, Config).

list_users(UserPoolId, AttributesToGet, Filter, Limit, PaginationToken, Config) ->
    BaseBody = #{
        <<"UserPoolId">>      => UserPoolId,
        <<"AttributesToGet">> => AttributesToGet,
        <<"Filter">>          => Filter,
        <<"Limit">>           => Limit,
        <<"PaginationToken">> => PaginationToken
    },
    Body = erlcloud_util:filter_undef(BaseBody),
    request(Config, "ListUsers", Body).

-spec list_all_users(binary()) -> {ok, map()} | {error, any()}.
list_all_users(UserPoolId) ->
    list_all_users(UserPoolId, undefined).

-spec list_all_users(binary(), binary() | undefined | aws_config()) ->
    {ok, map()} | {error, any()}.
list_all_users(UserPoolId, Config) when is_record(Config, aws_config) ->
    list_all_users(UserPoolId, undefined, Config);
list_all_users(UserPoolId, Filter) ->
    Config = erlcloud_aws:default_config(),
    list_all_users(UserPoolId, Filter, Config).

-spec list_all_users(binary(), binary() | undefined, aws_config()) ->
    {ok, map()} | {error, any()}.
list_all_users(UserPoolId, Filter, Config) ->
    Fun  = fun list_users/6,
    Args = [UserPoolId, undefined, Filter],
    list_all(Fun, Args, Config, <<"Users">>, <<"PaginationToken">>).

-spec admin_list_groups_for_user(binary(), binary()) ->
    {ok, map()} | {error, any()}.
admin_list_groups_for_user(UserName, UserPoolId) ->
    Config = erlcloud_aws:default_config(),
    admin_list_groups_for_user(UserName, UserPoolId, Config).

-spec admin_list_groups_for_user(binary(), binary(), aws_config()) ->
    {ok, map()} | {error, any()}.
admin_list_groups_for_user(UserName, UserPoolId, Config) ->
    Body = #{
        <<"Username">>   => UserName,
        <<"UserPoolId">> => UserPoolId
    },
    request(Config, "AdminListGroupsForUser", Body).

-spec admin_list_groups_for_user(binary(), binary(), number(),
                                 binary() | undefined) ->
    {ok, map()} | {error, any()}.
admin_list_groups_for_user(UserName, UserPoolId, Limit, NextToken) ->
    Config = erlcloud_aws:default_config(),
    admin_list_groups_for_user(UserName, UserPoolId, Limit, NextToken, Config).

-spec admin_list_groups_for_user(binary(), binary(), number(),
                                 binary() | undefined, aws_config()) ->
    {ok, map()} | {error, any()}.
admin_list_groups_for_user(UserName, UserPoolId, Limit, NextToken, Config) ->
    Body = #{
        <<"Username">>   => UserName,
        <<"UserPoolId">> => UserPoolId,
        <<"Limit">>      => Limit,
        <<"NextToken">>  => NextToken
    },
    request(Config, "AdminListGroupsForUser", Body).

-spec admin_get_user(binary(), binary()) -> {ok, map()} | {error, any()}.
admin_get_user(UserName, UserPoolId) ->
    Config = erlcloud_aws:default_config(),
    admin_get_user(UserName, UserPoolId, Config).

-spec admin_get_user(binary(), binary(), aws_config()) ->
    {ok, map()} | {error, any()}.
admin_get_user(UserName, UserPoolId, Config) ->
    Body = #{
        <<"Username">>   => UserName,
        <<"UserPoolId">> => UserPoolId
    },
    request(Config, "AdminGetUser", Body).

-spec admin_create_user(binary(), binary()) ->
    {ok, map()} | {error, any()}.
admin_create_user(UserName, UserPoolId) ->
    admin_create_user(UserName, UserPoolId, #{}).

-spec admin_create_user(binary(), binary(), maps:maps()) ->
    {ok, map()} | {error, any()}.
admin_create_user(UserName, UserPoolId, OptionalArgs) ->
    Config = erlcloud_aws:default_config(),
    admin_create_user(UserName, UserPoolId, OptionalArgs, Config).

-spec admin_create_user(binary(), binary(), maps:maps(), aws_config()) ->
    {ok, map()} | {error, any()}.
admin_create_user(UserName, UserPoolId, OptionalArgs, Config) ->
    Body = OptionalArgs#{
        <<"Username">>   => UserName,
        <<"UserPoolId">> => UserPoolId
    },
    request(Config, "AdminCreateUser", Body).

-spec admin_delete_user(binary(), binary()) -> ok | {error, any()}.
admin_delete_user(UserName, UserPoolId) ->
    Config = erlcloud_aws:default_config(),
    admin_delete_user(UserName, UserPoolId, Config).

-spec admin_delete_user(binary(), binary(), aws_config()) -> ok | {error, any()}.
admin_delete_user(UserName, UserPoolId, Config) ->
    Body = #{
        <<"Username">>   => UserName,
        <<"UserPoolId">> => UserPoolId
    },
    request_no_resp(Config, "AdminDeleteUser", Body).

-spec admin_add_user_to_group(binary(), binary(), binary()) ->
    ok | {error, any()}.
admin_add_user_to_group(GroupName, UserName, UserPoolId) ->
    Config = erlcloud_aws:default_config(),
    admin_add_user_to_group(GroupName, UserName, UserPoolId, Config).

-spec admin_add_user_to_group(binary(), binary(), binary(), aws_config()) ->
    ok | {error, any()}.
admin_add_user_to_group(GroupName, UserName, UserPoolId, Config) ->
    Body = #{
        <<"Username">>   => UserName,
        <<"GroupName">>  => GroupName,
        <<"UserPoolId">> => UserPoolId
    },
    request_no_resp(Config, "AdminAddUserToGroup", Body).

-spec admin_remove_user_from_group(binary(), binary(), binary()) ->
    ok | {error, any()}.
admin_remove_user_from_group(GroupName, UserName, UserPoolId) ->
    Config = erlcloud_aws:default_config(),
    admin_remove_user_from_group(GroupName, UserName, UserPoolId, Config).

-spec admin_remove_user_from_group(binary(), binary(), binary(), aws_config()) ->
    ok | {error, any()}.
admin_remove_user_from_group(GroupName, UserName, UserPoolId, Config) ->
    Body = #{
        <<"Username">>   => UserName,
        <<"GroupName">>  => GroupName,
        <<"UserPoolId">> => UserPoolId
    },
    request_no_resp(Config, "AdminRemoveUserFromGroup", Body).

-spec create_group(binary(), binary()) -> {ok, map()} | {error, any()}.
create_group(GroupName, UserPoolId) ->
    create_group(GroupName, UserPoolId, undefined, undefined, undefined).

-spec create_group(binary(), binary(), aws_config()) ->
    {ok, map()} | {error, any()}.
create_group(GroupName, UserPoolId, Config) ->
    create_group(GroupName, UserPoolId, undefined, undefined, undefined, Config).

-spec create_group(binary(), binary(), binary() | undefined,
                   number() | undefined, binary() | undefined) ->
    {ok, map()} | {error, any()}.
create_group(GroupName, UserPoolId, Description, Precedence, RoleArn) ->
    Config = erlcloud_aws:default_config(),
    create_group(GroupName, UserPoolId, Description, Precedence, RoleArn, Config).

-spec create_group(binary(), binary(), binary() | undefined,
                   number() | undefined, binary() | undefined, aws_config()) ->
    {ok, map()} | {error, any()}.
create_group(GroupName, UserPoolId, Description, Precedence, RoleArn, Config) ->
    Body0 = #{
        <<"GroupName">>   => GroupName,
        <<"UserPoolId">>  => UserPoolId,
        <<"Description">> => Description,
        <<"Precedence">>  => Precedence,
        <<"RoleArn">>     => RoleArn
    },

    Body = erlcloud_util:filter_undef(Body0),
    request(Config, "CreateGroup", Body).

-spec delete_group(binary(), binary()) -> ok | {error, any()}.
delete_group(GroupName, UserPoolId) ->
    Config = erlcloud_aws:default_config(),
    delete_group(GroupName, UserPoolId, Config).

-spec delete_group(binary(), binary(), aws_config()) -> ok | {error, any()}.
delete_group(GroupName, UserPoolId, Config) ->
    Body = #{
        <<"GroupName">>  => unicode:characters_to_binary(GroupName),
        <<"UserPoolId">> => unicode:characters_to_binary(UserPoolId)
    },
    request_no_resp(Config, "DeleteGroup", Body).

-spec admin_reset_user_password(binary(), binary()) ->
    ok| {error, any()}.
admin_reset_user_password(UserName, UserPoolId) ->
    admin_reset_user_password(UserName, UserPoolId, undefined).

-spec admin_reset_user_password(binary(), binary(), map() | undefined) ->
    ok | {error, any()}.
admin_reset_user_password(UserName, UserPoolId, MetaData) ->
    Config = erlcloud_aws:default_config(),
    admin_reset_user_password(UserName, UserPoolId, MetaData, Config).

-spec admin_reset_user_password(binary(), binary(),
                                map() | undefined, aws_config()) ->
    ok | {error, any()}.
admin_reset_user_password(UserName, UserPoolId, MetaData, Config) ->
    BaseBody = #{
        <<"Username">>       => UserName,
        <<"UserPoolId">>     => UserPoolId,
        <<"ClientMetaData">> => MetaData
    },
    Body = erlcloud_util:filter_undef(BaseBody),
    request_no_resp(Config, "AdminResetUserPassword", Body).

-spec admin_update_user_attributes(binary(), binary(), [map()]) ->
    ok | {error, any()}.
admin_update_user_attributes(UserName, UserPoolId, Attributes) ->
    admin_update_user_attributes(UserName, UserPoolId, Attributes, undefined).

-spec admin_update_user_attributes(binary(), binary(), [map()],
                                   map() | undefined) ->
    ok | {error, any()}.
admin_update_user_attributes(UserName, UserPoolId, Attributes, MetaData) ->
    Config = erlcloud_aws:default_config(),
    admin_update_user_attributes(UserName, UserPoolId, Attributes, MetaData, Config).

-spec admin_update_user_attributes(binary(), binary(), [map()],
                                   map() | undefined, aws_config()) ->
    ok | {error, any()}.
admin_update_user_attributes(UserName, UserPoolId, Attributes, MetaData, Config) ->
    BaseBody = #{
        <<"Username">>       => UserName,
        <<"UserPoolId">>     => UserPoolId,
        <<"UserAttributes">> => Attributes,
        <<"ClientMetaData">> => MetaData
    },
    Body = erlcloud_util:filter_undef(BaseBody),
    request_no_resp(Config, "AdminUpdateUserAttributes", Body).

-spec change_password(binary(), binary(), binary()) ->
    ok | {error, any()}.
change_password(OldPass, NewPass, AccessToken) ->
    Config = erlcloud_aws:default_config(),
    change_password(OldPass, NewPass, AccessToken, Config).

-spec change_password(binary(), binary(), binary(), aws_config()) ->
    ok | {error, any()}.
change_password(OldPass, NewPass, AccessToken, Config) ->
    Body = #{
        <<"AccessToken">>      => AccessToken,
        <<"PreviousPassword">> => OldPass,
        <<"ProposedPassword">> => NewPass
    },
    request_no_resp(Config, "ChangePassword", Body).

-spec list_user_pools() -> {ok, map()} | {error, any()}.
list_user_pools() ->
    list_user_pools(?MAX_RESULTS, undefined).

-spec list_user_pools(integer()) -> {ok, map()} | {error, any()}.
list_user_pools(MaxResult) ->
    list_user_pools(MaxResult, undefined).

-spec list_user_pools(integer(), binary() | undefined) ->
    {ok, map()} | {error, any()}.
list_user_pools(MaxResult, NextToken) ->
    Config = erlcloud_aws:default_config(),
    list_user_pools(MaxResult, NextToken, Config).

-spec list_user_pools(integer(), binary() | undefined, aws_config()) ->
    {ok, map()} | {error, any()}.
list_user_pools(MaxResult, NextToken, Config) ->
    Body0 = #{
        <<"MaxResults">> => MaxResult,
        <<"NextToken">>  => NextToken
    },
    Body = erlcloud_util:filter_undef(Body0),
    request(Config, "ListUserPools", Body).

-spec list_all_user_pools() -> {ok, map()} | {error, any()}.
list_all_user_pools() ->
    Config = erlcloud_aws:default_config(),
    list_all_user_pools(Config).

-spec list_all_user_pools(aws_config()) -> {ok, map()} | {error, any()}.
list_all_user_pools(Config) ->
    Fun = fun list_user_pools/3,
    list_all(Fun, [], Config, <<"UserPools">>, <<"NextToken">>).

-spec admin_set_user_password(binary(), binary(), binary()) ->
    {ok, map()} | {error, any()}.
admin_set_user_password(UserId, UserPoolId, Password) ->
    admin_set_user_password(UserId, UserPoolId, Password, false).

-spec admin_set_user_password(binary(), binary(), binary(), boolean()) ->
    ok | {error, any()}.
admin_set_user_password(UserId, UserPoolId, Password, Permanent) ->
    Config = erlcloud_aws:default_config(),
    admin_set_user_password(UserId, UserPoolId, Password, Permanent, Config).

-spec admin_set_user_password(binary(), binary(), binary(), boolean(),
                              aws_config()) ->
    ok | {error, any()}.
admin_set_user_password(UserId, UserPoolId, Password, Permanent, Config) ->
    Body = #{
        <<"Password">>   => Password,
        <<"Username">>   => UserId,
        <<"UserPoolId">> => UserPoolId,
        <<"Permanent">>  => Permanent
    },
    request_no_resp(Config, "AdminSetUserPassword", Body).

-spec describe_user_pool(binary()) -> {ok, map()} | {error, any()}.
describe_user_pool(UserPoolId) ->
    Config = erlcloud_aws:default_config(),
    describe_user_pool(UserPoolId, Config).

-spec describe_user_pool(binary(), aws_config()) -> {ok, map()} | {error, any()}.
describe_user_pool(UserPoolId, Config) ->
    Body = #{
        <<"UserPoolId">> => UserPoolId
    },
    request(Config, "DescribeUserPool", Body).

-spec get_user_pool_mfa_config(binary()) -> {ok, map()} | {error, any()}.
get_user_pool_mfa_config(UserPoolId) ->
    Config = erlcloud_aws:default_config(),
    get_user_pool_mfa_config(UserPoolId, Config).

-spec get_user_pool_mfa_config(binary(), aws_config()) ->
    {ok, map()} | {error, any()}.
get_user_pool_mfa_config(UserPoolId, Config) ->
    Body = #{
        <<"UserPoolId">> => UserPoolId
    },
    request(Config, "GetUserPoolMfaConfig", Body).

-spec list_identity_providers(binary()) -> {ok, map()} | {error, any()}.
list_identity_providers(UserPoolId) ->
    list_identity_providers(UserPoolId, ?MAX_RESULTS, undefined).

-spec list_identity_providers(binary(), integer(), binary() | undefined) ->
    {ok, map()} | {error, any()}.
list_identity_providers(UserPoolId, MaxResults, NextToken) ->
    Config = erlcloud_aws:default_config(),
    list_identity_providers(UserPoolId, MaxResults, NextToken, Config).

-spec list_identity_providers(binary(),
                              integer(),
                              binary() | undefined,
                              aws_config()) ->
    {ok, map()} | {error, any()}.
list_identity_providers(UserPoolId, MaxResults, NextToken, Config) ->
    Body0 = #{
        <<"UserPoolId">> => UserPoolId,
        <<"NextToken">>  => NextToken,
        <<"MaxResults">> => MaxResults
    },
    Body = erlcloud_util:filter_undef(Body0),
    request(Config, "ListIdentityProviders", Body).

-spec list_all_identity_providers(binary()) ->
    {ok, map()} | {error, any()}.
list_all_identity_providers(UserPoolId) ->
    Config = erlcloud_aws:default_config(),
    list_all_identity_providers(UserPoolId, Config).

-spec list_all_identity_providers(binary(), aws_config()) ->
    {ok, map()} | {error, any()}.
list_all_identity_providers(UserPoolId, Config) ->
    Fun  = fun list_identity_providers/4,
    Args = [UserPoolId],
    list_all(Fun, Args, Config, <<"Providers">>, <<"NextToken">>).

-spec describe_identity_provider(binary(), binary()) ->
    {ok, map()} | {error, any()}.
describe_identity_provider(UserPoolId, ProviderName) ->
    Config = erlcloud_aws:default_config(),
    describe_identity_provider(UserPoolId, ProviderName, Config).

-spec describe_identity_provider(binary(), binary(), aws_config()) ->
    {ok, map()} | {error, any()}.
describe_identity_provider(UserPoolId, ProviderName, Config) ->
    Body = #{
        <<"ProviderName">> => ProviderName,
        <<"UserPoolId">>   => UserPoolId
    },
    request(Config, "DescribeIdentityProvider", Body).

-spec describe_user_pool_client(binary(), binary()) ->
    {ok, map()} | {error, any()}.
describe_user_pool_client(UserPoolId, ClientId) ->
    Config = erlcloud_aws:default_config(),
    describe_user_pool_client(UserPoolId, ClientId, Config).

describe_user_pool_client(UserPoolId, ClientId, Config) ->
    Body = #{
        <<"ClientId">>   => ClientId,
        <<"UserPoolId">> => UserPoolId
    },
    request(Config, "DescribeUserPoolClient", Body).

-spec list_user_pool_clients(binary()) -> {ok, map()} | {error, any()}.
list_user_pool_clients(UserPoolId) ->
    list_user_pool_clients(UserPoolId, ?MAX_RESULTS, undefined).

-spec list_user_pool_clients(binary(), non_neg_integer(), binary() | undefined) ->
    {ok, map()} | {error, any()}.
list_user_pool_clients(UserPoolId, MaxResults, NextToken) ->
    Config = erlcloud_aws:default_config(),
    list_user_pool_clients(UserPoolId, MaxResults, NextToken, Config).

-spec list_user_pool_clients(binary(), non_neg_integer(), binary() | undefined,
                             aws_config()) ->
    {ok, map()} | {error, any()}.
list_user_pool_clients(UserPoolId, MaxResults, NextToken, Config) ->
    Body0 = #{
        <<"UserPoolId">> => UserPoolId,
        <<"NextToken">>  => NextToken,
        <<"MaxResults">> => MaxResults
    },
    Body = erlcloud_util:filter_undef(Body0),
    request(Config, "ListUserPoolClients", Body).

-spec list_all_user_pool_clients(binary()) ->
    {ok, map()} | {error, any()}.
list_all_user_pool_clients(UserPoolId) ->
    Config = erlcloud_aws:default_config(),
    list_all_user_pool_clients(UserPoolId, Config).

-spec list_all_user_pool_clients(binary(), aws_config()) ->
    {ok, map()} | {error, any()}.
list_all_user_pool_clients(UserPoolId, Config) ->
    Fun  = fun list_user_pool_clients/4,
    Args = [UserPoolId],
    list_all(Fun, Args, Config, <<"UserPoolClients">>, <<"NextToken">>).

-spec admin_list_devices(binary(), binary()) -> {ok, map()} | {error, any()}.
admin_list_devices(UserPoolId, Username) ->
    Config = erlcloud_aws:default_config(),
    admin_list_devices(UserPoolId, Username, Config).

-spec admin_list_devices(binary(), binary(), aws_config()) ->
    {ok, map()} | {error, any()}.
admin_list_devices(UserPoolId, Username, Config) ->
    admin_list_devices(UserPoolId, Username, ?MAX_RESULTS, undefined, Config).

-spec admin_list_devices(binary(), binary(), integer(), binary() | undefined,
                         aws_config()) ->
    {ok, map()} | {error, any()}.
admin_list_devices(UserPoolId, Username, Limit, PaginationToken, Config) ->
    Body0 = #{
        <<"UserPoolId">>      => UserPoolId,
        <<"Username">>        => Username,
        <<"Limit">>           => Limit,
        <<"PaginationToken">> => PaginationToken
    },
    Body = erlcloud_util:filter_undef(Body0),
    request(Config, "AdminListDevices", Body).

-spec admin_list_all_devices(binary(), binary()) ->
    {ok, map()} | {error, any()}.
admin_list_all_devices(UserPoolId, Username) ->
    Config = erlcloud_aws:default_config(),
    admin_list_all_devices(UserPoolId, Username, Config).

-spec admin_list_all_devices(binary(), binary(), aws_config()) ->
    {ok, map()} | {error, any()}.
admin_list_all_devices(UserPoolId, Username, Config) ->
    Fun  = fun admin_list_devices/5,
    Args = [UserPoolId, Username],
    list_all(Fun, Args, Config, <<"Devices">>, <<"PaginationToken">>).

-spec admin_forget_device(binary(), binary(), binary()) ->
    ok | {error, any()}.
admin_forget_device(UserPoolId, Username, DeviceKey) ->
    Config = erlcloud_aws:default_config(),
    admin_forget_device(UserPoolId, Username, DeviceKey, Config).

-spec admin_forget_device(binary(), binary(), binary(), aws_config()) ->
    ok | {error, any()}.
admin_forget_device(UserPoolId, Username, DeviceKey, Config) ->
    Body = #{
        <<"UserPoolId">> => UserPoolId,
        <<"Username">>   => Username,
        <<"DeviceKey">>  => DeviceKey
    },
    request_no_resp(Config, "AdminForgetDevice", Body).

-spec admin_confirm_signup(binary(), binary()) ->
    {ok, map()} | {error, any()}.
admin_confirm_signup(UserPoolId, Username) ->
    admin_confirm_signup(UserPoolId, Username, #{}).

-spec admin_confirm_signup(binary(), binary(), maps:map()) ->
    {ok, map()} | {error, any()}.
admin_confirm_signup(UserPoolId, Username, ClientMetadata) ->
    Config = erlcloud_aws:default_config(),
    admin_confirm_signup(UserPoolId, Username, ClientMetadata, Config).

-spec admin_confirm_signup(binary(), binary(), maps:map(), aws_config()) ->
    {ok, map()} | {error, any()}.
admin_confirm_signup(UserPoolId, Username, ClientMetadata, Config) ->
    Body = #{
        <<"UserPoolId">>     => UserPoolId,
        <<"Username">>       => Username,
        <<"ClientMetadata">> => ClientMetadata
    },
    request(Config, "AdminConfirmSignUp", Body).

-spec admin_initiate_auth(binary(), binary(), binary(), maps:map()) ->
    {ok, map()} | {error, any()}.
admin_initiate_auth(PoolId, ClientId, AuthFlow, AuthParams) ->
    Cfg = erlcloud_aws:default_config(),
    admin_initiate_auth(PoolId, ClientId, AuthFlow, AuthParams, Cfg).

-spec admin_initiate_auth(binary(), binary(), binary(),
                          maps:map(), aws_config()) ->
    {ok, map()} | {error, any()}.
admin_initiate_auth(PoolId, ClientId, AuthFlow, AuthParams, Cfg) ->
    admin_initiate_auth(PoolId, ClientId, AuthFlow, AuthParams,
                        #{}, #{}, #{}, Cfg).

-spec admin_initiate_auth(binary(), binary(), binary(), maps:map(),
                          maps:map(), maps:map(), maps:map(), aws_config()) ->
    {ok, map()} | {error, any()}.
admin_initiate_auth(PoolId, ClientId, AuthFlow, AuthParams,
                    AnalyticsMeta, ClientMeta, ContextData, Cfg) ->
    Mandatory = #{
        <<"AuthFlow">>          => AuthFlow,
        <<"ClientId">>          => ClientId,
        <<"UserPoolId">>        => PoolId
    },
    Optional = #{
        <<"AnalyticsMetadata">> => AnalyticsMeta,
        <<"AuthParameters">>    => AuthParams,
        <<"ClientMetadata">>    => ClientMeta,
        <<"ContextData">>       => ContextData
    },
    request(Cfg, "AdminInitiateAuth", make_request_body(Mandatory, Optional)).

-spec respond_to_auth_challenge(binary(), binary(), maps:map(), binary()) ->
    {ok, map()} | {error, any()}.
respond_to_auth_challenge(ClientId, ChallengeName, ChallengeResponses, Session) ->
    Cfg = erlcloud_aws:default_config(),
    respond_to_auth_challenge(ClientId, ChallengeName, ChallengeResponses,
                              Session, Cfg).

-spec respond_to_auth_challenge(binary(), binary(), maps:map(), binary(),
                                aws_config()) ->
    {ok, map()} | {error, any()}.
respond_to_auth_challenge(ClientId, ChallengeName, ChallengeResponses,
                          Session, Cfg) ->
    respond_to_auth_challenge(ClientId, ChallengeName, ChallengeResponses,
                              Session, #{}, #{}, #{}, Cfg).

-spec respond_to_auth_challenge(binary(), binary(), maps:map(), binary(),
                                maps:map(), maps:map(), maps:map(),
                                aws_config()) ->
    {ok, map()} | {error, any()}.
respond_to_auth_challenge(ClientId, ChallengeName, ChallengeResponses,
                          Session, AnalyticsMeta, ClientMeta, ContextData, Cfg) ->
    Mandatory = #{
        <<"ChallengeName">>      => ChallengeName,
        <<"ChallengeResponses">> => ChallengeResponses,
        <<"ClientId">>           => ClientId
    },
    Optional = #{
        <<"AnalyticsMetadata">>  => AnalyticsMeta,
        <<"ClientMetadata">>     => ClientMeta,
        <<"Session">>            => Session,
        <<"UserContextData">>    => ContextData
    },
    request(Cfg, "RespondToAuthChallenge", make_request_body(Mandatory, Optional)).

-spec create_identity_provider(binary(), binary(), binary(), map()) ->
    {ok, map()} | {error, any()}.
create_identity_provider(UserPoolId, ProviderName, ProviderType,
                         ProviderDetails) ->
  create_identity_provider(UserPoolId, ProviderName, ProviderType,
                           ProviderDetails, #{}).

-spec create_identity_provider(binary(), binary(), binary(), map(), map()) ->
    {ok, map()} | {error, any()}.
create_identity_provider(UserPoolId, ProviderName, ProviderType,
                         ProviderDetails, AttributeMapping) ->
  create_identity_provider(UserPoolId, ProviderName, ProviderType,
                           ProviderDetails, AttributeMapping, []).

-spec create_identity_provider(binary(), binary(), binary(),
                               map(), map(), list()) ->
    {ok, map()} | {error, any()}.
create_identity_provider(UserPoolId, ProviderName, ProviderType,
                         ProviderDetails, AttributeMapping, IdpIdentifiers) ->
    Config = erlcloud_aws:default_config(),
    create_identity_provider(UserPoolId, ProviderName, ProviderType,
                            ProviderDetails, AttributeMapping, IdpIdentifiers, Config).

-spec create_identity_provider(binary(), binary(), binary(), map(), map(),
                               list(), aws_config()) ->
    {ok, map()} | {error, any()}.
create_identity_provider(UserPoolId, ProviderName, ProviderType, ProviderDetails,
                         AttributeMapping, IdpIdentifiers, Config) ->
    Mandatory = #{
      <<"UserPoolId">> => UserPoolId,
      <<"ProviderName">> => ProviderName,
      <<"ProviderType">> => ProviderType,
      <<"ProviderDetails">> => ProviderDetails
    },
    Optional = #{
      <<"AttributeMapping">> => AttributeMapping,
      <<"IdpIdentifiers">> => IdpIdentifiers
    },
  request(Config, "CreateIdentityProvider", make_request_body(Mandatory, Optional)).

-spec delete_identity_provider(binary(), binary()) ->
    ok | {error, any()}.
delete_identity_provider(UserPoolId, ProviderName) ->
    Config = erlcloud_aws:default_config(),
    delete_identity_provider(UserPoolId, ProviderName, Config).

-spec delete_identity_provider(binary(), binary(), aws_config()) ->
    ok | {error, any()}.
delete_identity_provider(UserPoolId, ProviderName, Config) ->
    Body = #{
        <<"UserPoolId">>   => UserPoolId,
        <<"ProviderName">> => ProviderName
    },
    request_no_resp(Config, "DeleteIdentityProvider", Body).

-spec update_identity_provider(binary(), binary()) ->
    {ok, map()} | {error, any()}.
update_identity_provider(UserPoolId, ProviderName) ->
    update_identity_provider(UserPoolId, ProviderName, #{}).

-spec update_identity_provider(binary(), binary(), map()) ->
    {ok, map()} | {error, any()}.
update_identity_provider(UserPoolId, ProviderName, ProviderDetails) ->
    update_identity_provider(UserPoolId, ProviderName, ProviderDetails, #{}).

-spec update_identity_provider(binary(), binary(), map(), map()) ->
    {ok, map()} | {error, any()}.
update_identity_provider(UserPoolId, ProviderName,
                         ProviderDetails, AttributeMapping) ->
    update_identity_provider(UserPoolId, ProviderName,
                             ProviderDetails, AttributeMapping, []).

-spec update_identity_provider(binary(), binary(), map(), map(), list()) ->
    {ok, map()} | {error, any()}.
update_identity_provider(UserPoolId, ProviderName,
                         ProviderDetails, AttributeMapping, IdpIdentifiers) ->
    Config = erlcloud_aws:default_config(),
    update_identity_provider(UserPoolId, ProviderName, ProviderDetails,
                             AttributeMapping, IdpIdentifiers, Config).

-spec update_identity_provider(binary(), binary(), map(), map(),
                               list(), aws_config()) ->
    {ok, map()} | {error, any()}.
update_identity_provider(UserPoolId, ProviderName, ProviderDetails,
                         AttributeMapping, IdpIdentifiers, Config) ->
    Mandatory = #{
        <<"UserPoolId">>       => UserPoolId,
        <<"ProviderName">>     => ProviderName
    },
    Optional = #{
        <<"ProviderDetails">>  => ProviderDetails,
        <<"AttributeMapping">> => AttributeMapping,
        <<"IdpIdentifiers">>   => IdpIdentifiers
    },
    request(Config, "UpdateIdentityProvider", make_request_body(Mandatory, Optional)).

%%------------------------------------------------------------------------------
%% Internal Functions
%%------------------------------------------------------------------------------
request(Config, Request) ->
    Result = erlcloud_retry:request(Config, Request, fun handle_result/1),
    case erlcloud_aws:request_to_return(Result) of
        {ok, {_, <<>>}}     -> {ok, #{}};
        {ok, {_, RespBody}} -> {ok, jsx:decode(RespBody, [return_maps])};
        {error, _} = Error  -> Error
    end.

request(Config0, OperationName, Request) ->
    case erlcloud_aws:update_config(Config0) of
        {ok, Config} ->
            Body       = jsx:encode(Request),
            Operation  = "AWSCognitoIdentityProviderService." ++ OperationName,
            Headers    = get_headers(Config, Operation, Body),
            AwsRequest = #aws_request{service         = 'cognito-idp',
                                      uri             = get_url(Config),
                                      method          = post,
                                      request_headers = Headers,
                                      request_body    = Body},
            request(Config, AwsRequest);
        {error, Reason} ->
            {error, Reason}
    end.

request_no_resp(Config, OperationName, Request) ->
    case request(Config, OperationName, Request) of
        {ok, _} -> ok;
        Error   -> Error
    end.

make_request_body(Mandatory, Optional) ->
    maps:merge(Mandatory, erlcloud_util:filter_empty_map(Optional)).

get_headers(#aws_config{cognito_user_pools_host = Host} = Config, Operation, Body) ->
    Headers = [{"host", Host},
               {"x-amz-target", Operation},
               {"version", ?API_VERSION},
               {"content-type", "application/x-amz-json-1.1"}],
    Region = erlcloud_aws:aws_region_from_host(Host),
    erlcloud_aws:sign_v4_headers(Config, Headers, Body, Region, "cognito-idp").

handle_result(#aws_request{response_type = ok} = Request) ->
    Request;
handle_result(#aws_request{response_type   = error,
                           error_type      = aws,
                           response_status = Status} = Request)
  when Status >= 500 ->
    Request#aws_request{should_retry = true};
handle_result(#aws_request{response_type = error,
                           error_type    = aws} = Request) ->
    Request#aws_request{should_retry = false}.

get_url(#aws_config{cognito_user_pools_scheme = Scheme,
                    cognito_user_pools_host   = Host}) ->
    Scheme ++ Host.

list_all(Fun, Args, Config, Key, TokenAlias) ->
    list_all(Fun, Args, Config, Key, TokenAlias, undefined, []).

list_all(Fun, Args, Config, Key, TokenAlias, NextToken, Acc) ->
    UpdArgs = Args ++ [?MAX_RESULTS, NextToken, Config],
    case erlang:apply(Fun, UpdArgs) of
        {ok, Map} ->
            UpdAcc   = Acc ++ maps:get(Key, Map),
            NewToken = maps:get(TokenAlias, Map, undefined),
            case NewToken of
                undefined ->
                    {ok, #{Key => UpdAcc}};
                _ ->
                    list_all(Fun, Args, Config, Key, TokenAlias, NewToken, UpdAcc)
            end;
        Error ->
            Error
    end.
