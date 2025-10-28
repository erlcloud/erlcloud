-module(erlcloud_sm).
-author("joshua@halloapp.com").

-include("erlcloud.hrl").
-include("erlcloud_aws.hrl").

%%% Library initialization.
-export([new/2, new/3, new/4]).

%%% API
-export([
    batch_get_secret_value/2, batch_get_secret_value/3,
    cancel_rotate_secret/1, cancel_rotate_secret/2,
    create_secret/3, create_secret/4, create_secret/5,
    create_secret_binary/3, create_secret_binary/4, create_secret_binary/5,
    create_secret_string/3, create_secret_string/4, create_secret_string/5,
    delete_resource_policy/1, delete_resource_policy/2,
    delete_secret/1, delete_secret/2, delete_secret/3,
    describe_secret/1, describe_secret/2,
    get_random_password/0, get_random_password/1, get_random_password/2,
    get_resource_policy/1, get_resource_policy/2,
    get_secret_value/2, get_secret_value/3,
    list_secrets/0, list_secrets/1, list_secrets/2,
    list_secret_version_ids/1, list_secret_version_ids/2, list_secret_version_ids/3,
    put_resource_policy/2, put_resource_policy/3, put_resource_policy/4,
    put_secret_binary/3, put_secret_binary/4, put_secret_binary/5,
    put_secret_string/3, put_secret_string/4, put_secret_string/5,
    put_secret_value/3, put_secret_value/4, put_secret_value/5,
    remove_regions_from_replication/2, remove_regions_from_replication/3,
    replicate_secret_to_regions/2, replicate_secret_to_regions/3, replicate_secret_to_regions/4,
    restore_secret/1, restore_secret/2,
    rotate_secret/2, rotate_secret/3, rotate_secret/4,
    stop_replication_to_replica/1, stop_replication_to_replica/2,
    tag_resource/2, tag_resource/3,
    untag_resource/2, untag_resource/3,
    update_secret/2, update_secret/3, update_secret/4,
    update_secret_version_stage/2, update_secret_version_stage/3, update_secret_version_stage/4,
    validate_resource_policy/1, validate_resource_policy/2, validate_resource_policy/3
]).

%%%------------------------------------------------------------------------------
%%% Shared types
%%%------------------------------------------------------------------------------

-type sm_response() :: {ok, proplists:proplist()} | {error, term()}.

-type get_secret_value_option() :: {version_id | version_stage, binary()}.
-type get_secret_value_options() :: [get_secret_value_option()].

%% replica region is expected to be a proplist of the following tuples:
%% [{<<"KmsKeyId">>, binary()}, {<<"Region">>, binary()}]
-type replica_region() :: [proplist()].
-type replica_regions() :: [replica_region()].

%% batch get secret value types
-type secret_value_filter() :: {key, binary(), values, [binary()]}.
-type secret_value_filters() :: [secret_value_filter()].

-type batch_get_secret_value_param() :: {filters, secret_value_filters()} | {secret_id_list, [binary()]}.

-type batch_get_secret_value_option() :: {max_results, pos_integer()}
                                       | {next_token, binary()}.
-type batch_get_secret_value_options() :: [batch_get_secret_value_option()].

-type create_secret_option() :: {add_replica_regions, replica_regions()}
                              | {client_request_token, binary()}
                              | {description, binary()}
                              | {force_overwrite_replica_secret, boolean()}
                              | {kms_key_id, binary()}
                              | {secret_binary, binary()} %% Note AWS accepts either SecretBinary or SecretString,
                              | {secret_string, binary()} %% not both at the same time
                              | {tags, proplist()}.
-type create_secret_options() :: [create_secret_option()].

-type delete_secret_option() :: {force_delete_without_recovery, boolean()} %% Note you can't use both this parameter and RecoveryWindowInDays.
                              | {recovery_window_in_days, pos_integer()}.  %% If none of these two options are specified then SM defaults to 30 day recovery window
-type delete_secret_options() :: [delete_secret_option()].

%% get random password options types
-type get_random_password_option() :: {exclude_characters, binary()}
                                   | {exclude_lowercase, boolean()}
                                   | {exclude_numbers, boolean()}
                                   | {exclude_punctuation, boolean()}
                                   | {exclude_uppercase, boolean()}
                                   | {include_space, boolean()}
                                   | {password_length, pos_integer()}
                                   | {require_each_included_type, boolean()}.
-type get_random_password_options() :: [get_random_password_option()].

%% list secrets options types
-type list_secrets_option() :: {filters, secret_value_filters()}
                             | {max_results, pos_integer()}
                             | {include_planned_deletion, boolean()}
                             | {next_token, binary()}
                             | {sort_order, binary()}.
-type list_secrets_options() :: [list_secrets_option()].

% list secret version ids options types
-type list_secret_version_ids_option() :: {include_deprecated, boolean()}
                                      | {max_results, pos_integer()}
                                      | {next_token, binary()}.
-type list_secret_version_ids_options() :: [list_secret_version_ids_option()].

-type put_resource_policy_option() :: {block_public_policy, boolean()}.
-type put_resource_policy_options() :: [put_resource_policy_option()].

-type secret_value() :: {secret_binary, binary()} | {secret_string, binary()}.
-type put_secret_value_option() :: {client_request_token, binary()}
                                 | {secret_binary, binary()}
                                 | {secret_string, binary()}
                                 | {version_stages, [binary()]}.
-type put_secret_value_options() :: [put_secret_value_option()].

-type rotation_rules() :: [rotate_rule()].
-type rotate_rule() :: {automatically_after_days, pos_integer()}
                     | {duration, binary()}
                     | {schedule_expression, binary()}.

-type rotate_secret_options() :: [rotate_secret_option()].
-type rotate_secret_option() :: {rotate_immediately, boolean()}
                              | {rotation_lambda_arn, binary()}
                              | {rotation_rules, proplist()}.

-type replicate_secret_to_regions_option() :: {force_overwrite_replica_secret, boolean()}.
-type replicate_secret_to_regions_options() :: [replicate_secret_to_regions_option()].

-type update_secret_options() :: [update_secret_option()].
-type update_secret_option() :: {description, binary()}
                              | {kms_key_id, binary()}
                              | {secret_binary, binary()}
                              | {secret_string, binary()}.

-type update_secret_version_stage_option() :: {move_to_version_id, binary()}
                                            | {remove_from_version_id, binary()}.
-type update_secret_version_stage_options() :: [update_secret_version_stage_option()].

-type validate_resource_policy_option() :: {secret_id, binary()}.
-type validate_resource_policy_options() :: [validate_resource_policy_option()].


%%%------------------------------------------------------------------------------
%%% Library initialization.
%%%------------------------------------------------------------------------------

-spec new(string(), string()) -> aws_config().
new(AccessKeyID, SecretAccessKey) ->
    #aws_config{
        access_key_id = AccessKeyID,
        secret_access_key = SecretAccessKey
    }.


-spec new(string(), string(), string()) -> aws_config().
new(AccessKeyID, SecretAccessKey, Host) ->
    #aws_config{
        access_key_id = AccessKeyID,
        secret_access_key = SecretAccessKey,
        sm_host = Host
    }.


-spec new(string(), string(), string(), non_neg_integer()) -> aws_config().
new(AccessKeyID, SecretAccessKey, Host, Port) ->
    #aws_config{
        access_key_id = AccessKeyID,
        secret_access_key = SecretAccessKey,
        sm_host = Host,
        sm_port = Port
    }.


%%------------------------------------------------------------------------------
%% BatchGetSecretValue
%%------------------------------------------------------------------------------
%% @doc
%% Retrieves the contents of the encrypted fields SecretString or SecretBinary for up to 20 secrets.
%% To retrieve a single secret, call GetSecretValue. You must include Filters or SecretIdList, but not both.
%% SM API:
%% [https://docs.aws.amazon.com/secretsmanager/latest/apireference/API_BatchGetSecretValue.html]
%% @end
%%------------------------------------------------------------------------------

-spec batch_get_secret_value(FiltersOrSecretIdList :: batch_get_secret_value_param(),
                             Opts :: batch_get_secret_value_options()) -> sm_response().
batch_get_secret_value(FiltersOrSecretIdList, Opts) ->
    batch_get_secret_value(FiltersOrSecretIdList, Opts, erlcloud_aws:default_config()).

-spec batch_get_secret_value(FiltersOrSecretIdList :: batch_get_secret_value_param(),
                             Opts :: batch_get_secret_value_options(),
                             Config :: aws_config()) -> sm_response().
batch_get_secret_value(FiltersOrSecretIdList, Opts, Config) ->
    BatchGetSecretParam = case FiltersOrSecretIdList of
        {filters, Filter} -> {<<"Filters">>, Filter};
        {secret_id_list, IdList} -> {<<"SecretIdList">>, IdList}
    end,
    Json = lists:map(
        fun
            ({max_results, Val}) -> {<<"MaxResults">>, Val};
            ({next_token, Val}) -> {<<"NextToken">>, Val};
            (Other) -> Other
        end,
    [BatchGetSecretParam | Opts]),
    sm_request(Config, "secretsmanager.BatchGetSecretValue", Json).

%%------------------------------------------------------------------------------
%% CancelRotateSecret
%%------------------------------------------------------------------------------
%% @doc
%% Turns off automatic rotation, and if a rotation is currently in progress, cancels the rotation.
%%
%% SM API:
%% [https://docs.aws.amazon.com/secretsmanager/latest/apireference/API_CancelRotateSecret.html]
%% @end
%%------------------------------------------------------------------------------

-spec cancel_rotate_secret(SecretId :: binary()) -> sm_response().
cancel_rotate_secret(SecretId) ->
    cancel_rotate_secret(SecretId, erlcloud_aws:default_config()).

-spec cancel_rotate_secret(SecretId :: binary(), Config :: aws_config()) -> sm_response().
cancel_rotate_secret(SecretId, Config) ->
    Json = [{<<"SecretId">>, SecretId}],
    sm_request(Config, "secretsmanager.CancelRotateSecret", Json).

%%------------------------------------------------------------------------------
%% CreateSecret - CreateSecret
%%------------------------------------------------------------------------------
%% @doc
%% Creates a new secret.A secret can be a password, a set of credentials such as
%% a user name and password, an OAuth token, or other secret information that you
%% store in an encrypted form in Secrets Manager. The secret also includes the connection
%% information to access a database or other service, which Secrets Manager doesn't encrypt.
%% A secret in Secrets Manager consists of both the protected secret data and the important
%% information needed to manage the secret.
%%
%% ClientRequestToken is used by AWS for secret versioning purposes.
%% It is recommended to be a UUID type value, and is required to be between
%% 32 and 64 characters.
%%
%% Note: Use create_secret, as create_secret_binary and create_secret_string functions are kept for backward compatibility.
%%
%% SM API:
%% [https://docs.aws.amazon.com/secretsmanager/latest/apireference/API_CreateSecret.html]
%%
%% @end
%%------------------------------------------------------------------------------

-spec create_secret(Name :: binary(), ClientRequestToken :: binary(),
                    Secret :: secret_value()) -> sm_response().
create_secret(Name, ClientRequestToken, Secret) ->
    create_secret(Name, ClientRequestToken, Secret, []).

-spec create_secret(Name :: binary(), ClientRequestToken :: binary(),
                    Secret :: secret_value(),
                    Opts :: create_secret_options()) -> sm_response().
create_secret(Name, ClientRequestToken, Secret, Opts) ->
    create_secret(Name, ClientRequestToken, Secret, Opts, erlcloud_aws:default_config()).

-spec create_secret(Name :: binary(), ClientRequestToken :: binary(),
                    Secret :: secret_value(), Opts :: create_secret_options(),
                    Config :: aws_config()) -> sm_response().
create_secret(Name, ClientRequestToken, Secret, Opts, Config) ->
    SecretValue = case Secret of
        {secret_binary, Val} -> {secret_binary, base64:encode(Val)};
        {secret_string, Val} -> {secret_string, Val}
    end,
    create_secret_call(Name, ClientRequestToken, [SecretValue | Opts], Config).

%%------------------------------------------------------------------------------
%% CreateSecret - SecretBinary
%%------------------------------------------------------------------------------
%% @doc
%%
%% Note: Use create_secret, as create_secret_binary and create_secret_string functions are kept for backward compatibility.
%%
%% Creates a new secret binary. The function internally base64-encodes the binary
%% as it is expected by the AWS SecretManager API, so raw blob is expected
%% to be passed as an attribute.
%%
%% ClientRequestToken is used by AWS for secret versioning purposes.
%% It is recommended to be a UUID type value, and is required to be between
%% 32 and 64 characters.
%%
%% To store a text secret use CreateSecret - SecretString version of the function
%% instead.
%%
%% SM API:
%% [https://docs.aws.amazon.com/secretsmanager/latest/apireference/API_CreateSecret.html]
%%
%% Example:
%% Name = <<"my-secret-binary">>,
%% ClientRequestToken = <<"7537a353-0de0-4b98-bf55-f8365821ed36">>,
%% %% some binary to store (say, an RSA private key's exponent)
%% {[_E, _Pub], [_E, _N, Priv, _P1, _P2, _E1, _E2, _C]} = crypto:generate_key(rsa, {2048,65537}),
%% erlcloud_sm:create_secret_binary(Name, ClientRequestToken, Priv).
%% @end
%%------------------------------------------------------------------------------

-spec create_secret_binary(Name :: binary(), ClientRequestToken :: binary(),
                           SecretBinary :: binary()) -> sm_response().
create_secret_binary(Name, ClientRequestToken, SecretBinary) ->
    create_secret_binary(Name, ClientRequestToken, SecretBinary, []).

-spec create_secret_binary(Name :: binary(), ClientRequestToken :: binary(),
                           SecretBinary :: binary(),
                           Opts :: create_secret_options()) -> sm_response().
create_secret_binary(Name, ClientRequestToken, SecretBinary, Opts) ->
    create_secret_binary(Name, ClientRequestToken, SecretBinary, Opts, erlcloud_aws:default_config()).

-spec create_secret_binary(Name :: binary(), ClientRequestToken :: binary(),
                           SecretBinary :: binary(), Opts :: create_secret_options(),
                           Config :: aws_config()) -> sm_response().
create_secret_binary(Name, ClientRequestToken, SecretBinary, Opts, Config) ->
    Secret = {secret_binary, base64:encode(SecretBinary)},
    create_secret_call(Name, ClientRequestToken, [Secret | Opts], Config).

%%------------------------------------------------------------------------------
%% CreateSecret - SecretString
%%------------------------------------------------------------------------------
%% @doc
%%
%% Note: Use create_secret, as create_secret_binary and create_secret_string functions are kept for backward compatibility.
%%
%% Creates a new secret string. The API expects SecretString is a text data to
%% encrypt and store in the SecretManager. It is recommended a JSON structure
%% of key/value pairs is used for the secret value.
%%
%% ClientRequestToken is used by AWS for secret versioning purposes.
%% It is recommended to be a UUID type value, and is required to be between
%% 32 and 64 characters.
%%
%% To store a binary (which will be base64 encoded by the library, as it is
%% expected by AWS SecretManager API), use CreateSecret - SecretBinary version
%% of the function.
%%
%% SM API:
%% [https://docs.aws.amazon.com/secretsmanager/latest/apireference/API_CreateSecret.html]
%%
%% Example:
%% Name = <<"my-secret-string">>,
%% ClientRequestToken = <<"7537a353-0de0-4b98-bf55-f8365821ed37">>,
%% %% some user/password json to store
%% Secret = jsx:encode(#{<<"user">> => <<"my-user">>, <<"password">> => <<"superSecretPassword">>}),
%% erlcloud_sm:create_secret_string(Name, ClientRequestToken, Secret).
%%
%% @end
%%------------------------------------------------------------------------------

-spec create_secret_string(Name :: binary(), ClientRequestToken :: binary(),
                           SecretString :: binary()) -> sm_response().
create_secret_string(Name, ClientRequestToken, SecretString) ->
    create_secret_string(Name, ClientRequestToken, SecretString, []).

-spec create_secret_string(Name :: binary(), ClientRequestToken :: binary(),
                           SecretString :: binary(), Opts :: create_secret_options()) -> sm_response().
create_secret_string(Name, ClientRequestToken, SecretString, Opts) ->
    create_secret_string(Name, ClientRequestToken, SecretString, Opts, erlcloud_aws:default_config()).


-spec create_secret_string(Name :: binary(), ClientRequestToken :: binary(),
                           SecretString :: binary(), Opts :: create_secret_options(),
                           Config :: aws_config()) -> sm_response().
create_secret_string(Name, ClientRequestToken, SecretString, Opts, Config) ->
    Secret = {secret_string, SecretString},
    create_secret_call(Name, ClientRequestToken, [Secret | Opts], Config).

%%------------------------------------------------------------------------------
%% DeleteResourcePolicy
%%------------------------------------------------------------------------------
%% @doc
%% SM API:
%% [https://docs.aws.amazon.com/secretsmanager/latest/apireference/API_DeleteResourcePolicy.html]
%% @end
%%------------------------------------------------------------------------------

-spec delete_resource_policy(SecretId :: binary()) -> sm_response().
delete_resource_policy(SecretId) ->
    delete_resource_policy(SecretId, erlcloud_aws:default_config()).

-spec delete_resource_policy(SecretId :: binary(), Config :: aws_config()) -> sm_response().
delete_resource_policy(SecretId, Config) ->
    Json = [{<<"SecretId">>, SecretId}],
    sm_request(Config, "secretsmanager.DeleteResourcePolicy", Json).

%%------------------------------------------------------------------------------
%% DeleteSecret
%%------------------------------------------------------------------------------
%% @doc
%% SM API:
%% [https://docs.aws.amazon.com/secretsmanager/latest/apireference/API_DeleteSecret.html]
%% @end
%%------------------------------------------------------------------------------

-spec delete_secret(SecretId :: binary()) -> sm_response().
delete_secret(SecretId) ->
    delete_secret(SecretId, []).

-spec delete_secret(SecretId :: binary(), Opts :: delete_secret_options()) -> sm_response().
delete_secret(SecretId, Opts) ->
    delete_secret(SecretId, Opts, erlcloud_aws:default_config()).

-spec delete_secret(SecretId :: binary(), Opts :: delete_secret_options(),
        Config :: aws_config()) -> sm_response().
delete_secret(SecretId, Opts, Config) ->
    Json = lists:map(
        fun
            ({force_delete_without_recovery, Val}) -> {<<"ForceDeleteWithoutRecovery">>, Val};
            ({recovery_window_in_days, Val}) -> {<<"RecoveryWindowInDays">>, Val};
            (Other) -> Other
        end,
        [{<<"SecretId">>, SecretId} | Opts]),
    sm_request(Config, "secretsmanager.DeleteSecret", Json).

%%------------------------------------------------------------------------------
%% DescribeSecret
%%------------------------------------------------------------------------------
%% @doc
%% SM API:
%% [https://docs.aws.amazon.com/secretsmanager/latest/apireference/API_DescibeSecret.html]
%% @end
%%------------------------------------------------------------------------------

-spec describe_secret(SecretId :: binary()) -> sm_response().
describe_secret(SecretId) ->
    describe_secret(SecretId, erlcloud_aws:default_config()).

-spec describe_secret(SecretId :: binary(), Config :: aws_config()) -> sm_response().
describe_secret(SecretId, Config) ->
    Json = [{<<"SecretId">>, SecretId}],
    sm_request(Config, "secretsmanager.DescribeSecret", Json).

%%------------------------------------------------------------------------------
%% GetRandomPassword
%%------------------------------------------------------------------------------
%% @doc
%% Generates a random password. We recommend that you specify the maximum length and include
%% every character type that the system you are generating a password for can support.
%% By default, Secrets Manager uses uppercase and lowercase letters, numbers, and the
%% following characters in passwords: !\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~
%%
%% SM API:
%% [https://docs.aws.amazon.com/secretsmanager/latest/apireference/API_GetRandomPassword.html]
%% @end
%%------------------------------------------------------------------------------

-spec get_random_password() -> sm_response().
get_random_password() ->
    get_random_password([]).

-spec get_random_password(Opts :: get_random_password_options()) -> sm_response().
get_random_password(Opts) ->
    get_random_password(Opts, erlcloud_aws:default_config()).

-spec get_random_password(Opts :: get_random_password_options(), Config :: aws_config()) -> sm_response().
get_random_password(Opts, Config) ->
    Json = lists:map(
        fun
            ({exclude_characters, Val}) -> {<<"ExcludeCharacters">>, Val};
            ({exclude_lowercase, Val}) -> {<<"ExcludeLowercase">>, Val};
            ({exclude_numbers, Val}) -> {<<"ExcludeNumbers">>, Val};
            ({exclude_punctuation, Val}) -> {<<"ExcludePunctuation">>, Val};
            ({exclude_uppercase, Val}) -> {<<"ExcludeUppercase">>, Val};
            ({include_space, Val}) -> {<<"IncludeSpace">>, Val};
            ({password_length, Val}) -> {<<"PasswordLength">>, Val};
            ({require_each_included_type, Val}) -> {<<"RequireEachIncludedType">>, Val};
            (Other) -> Other
        end,
        Opts),
    sm_request(Config, "secretsmanager.GetRandomPassword", Json).

%%------------------------------------------------------------------------------
%% GetResourcePolicy
%%------------------------------------------------------------------------------
%% @doc
%% SM API:
%% [https://docs.aws.amazon.com/secretsmanager/latest/apireference/API_GetResourcePolicy.html]
%% @end
%%------------------------------------------------------------------------------

-spec get_resource_policy(SecretId :: binary()) -> sm_response().
get_resource_policy(SecretId) ->
    get_resource_policy(SecretId, erlcloud_aws:default_config()).

-spec get_resource_policy(SecretId :: binary(), Config :: aws_config()) -> sm_response().
get_resource_policy(SecretId, Config) ->
    Json = [{<<"SecretId">>, SecretId}],
    sm_request(Config, "secretsmanager.GetResourcePolicy", Json).

%%------------------------------------------------------------------------------
%% GetSecretValue
%%------------------------------------------------------------------------------
%% @doc
%% SM API:
%% [https://docs.aws.amazon.com/secretsmanager/latest/apireference/API_GetSecretValue.html]
%% @end
%%------------------------------------------------------------------------------

-spec get_secret_value(SecretId :: binary(), Opts :: get_secret_value_options()) -> sm_response().
get_secret_value(SecretId, Opts) ->
    get_secret_value(SecretId, Opts, erlcloud_aws:default_config()).

-spec get_secret_value(SecretId :: binary(), Opts :: get_secret_value_options(),
        Config :: aws_config()) -> sm_response().
get_secret_value(SecretId, Opts, Config) ->
    Json = lists:map(
        fun
            ({version_id, Val}) -> {<<"VersionId">>, Val};
            ({version_stage, Val}) -> {<<"VersionStage">>, Val};
            (Other) -> Other
        end,
        [{<<"SecretId">>, SecretId} | Opts]),
    sm_request(Config, "secretsmanager.GetSecretValue", Json).

%%------------------------------------------------------------------------------
%% ListSecrets
%%------------------------------------------------------------------------------
%% @doc
%% Lists the secrets that are stored by Secrets Manager in the AWS account, not including
%% secrets that are marked for deletion. To see secrets marked for deletion, use the Secrets Manager console.
%%
%% All Secrets Manager operations are eventually consistent. ListSecrets might not reflect changes
%% from the last five minutes. You can get more recent information for a specific secret by calling DescribeSecret.
%%
%% SM API:
%% [https://docs.aws.amazon.com/secretsmanager/latest/apireference/API_ListSecrets.html]
%% @end
%%------------------------------------------------------------------------------

-spec list_secrets() -> sm_response().
list_secrets() ->
    list_secrets([]).

-spec list_secrets(Opts :: list_secrets_options()) -> sm_response().
list_secrets(Opts) ->
    list_secrets(Opts, erlcloud_aws:default_config()).

-spec list_secrets(Opts :: list_secrets_options(),
        Config :: aws_config()) -> sm_response().
list_secrets(Opts, Config) ->
    Json = lists:map(
        fun
            ({filters, Val}) -> {<<"Filters">>, Val};
            ({include_planned_deletion, Val}) -> {<<"IncludePlannedDeletion">>, Val};
            ({max_results, Val}) -> {<<"MaxResults">>, Val};
            ({next_token, Val}) -> {<<"NextToken">>, Val};
            ({sort_order, Val}) -> {<<"SortOrder">>, Val};
            (Other) -> Other
        end,
        Opts),
    sm_request(Config, "secretsmanager.ListSecrets", Json).

%%------------------------------------------------------------------------------
%% ListSecretVersionIds
%%------------------------------------------------------------------------------
%% @doc
%% Lists the versions of a secret.  Secrets Manager uses staging labels to indicate the different versions of a secret.
%%
%% SM API:
%% [https://docs.aws.amazon.com/secretsmanager/latest/apireference/API_ListSecretVersionIds.html]
%% @end
%%------------------------------------------------------------------------------

-spec list_secret_version_ids(SecretId :: binary()) -> sm_response().
list_secret_version_ids(SecretId) ->
    list_secret_version_ids(SecretId, []).

-spec list_secret_version_ids(SecretId :: binary(), Opts :: list_secret_version_ids_options()) -> sm_response().
list_secret_version_ids(SecretId, Opts) ->
    list_secret_version_ids(SecretId, Opts, erlcloud_aws:default_config()).

-spec list_secret_version_ids(SecretId :: binary(), Opts :: list_secret_version_ids_options(),
        Config :: aws_config()) -> sm_response().
list_secret_version_ids(SecretId, Opts, Config) ->
    Json = lists:map(
        fun
            ({include_deprecated, Val}) -> {<<"IncludeDeprecated">>, Val};
            ({max_results, Val}) -> {<<"MaxResults">>, Val};
            ({next_token, Val}) -> {<<"NextToken">>, Val};
            (Other) -> Other
        end,
        [{<<"SecretId">>, SecretId} | Opts]),
    sm_request(Config, "secretsmanager.ListSecretVersionIds", Json).

%%------------------------------------------------------------------------------
%% PutResourcePolicy
%%------------------------------------------------------------------------------
%% @doc
%% SM API:
%% [https://docs.aws.amazon.com/secretsmanager/latest/apireference/API_PutResourcePolicy.html]
%% @end
%%------------------------------------------------------------------------------

-spec put_resource_policy(SecretId :: binary(), ResourcePolicy :: binary()) -> sm_response().
put_resource_policy(SecretId, ResourcePolicy) ->
    put_resource_policy(SecretId, ResourcePolicy, []).

-spec put_resource_policy(SecretId :: binary(), ResourcePolicy :: binary(),
                          Opts :: put_resource_policy_options()) -> sm_response().
put_resource_policy(SecretId, ResourcePolicy, Opts) ->
    put_resource_policy(SecretId, ResourcePolicy, Opts, erlcloud_aws:default_config()).

-spec put_resource_policy(SecretId :: binary(), ResourcePolicy :: binary(),
                          Opts :: put_resource_policy_options(),
                          Config :: aws_config()) -> sm_response().
put_resource_policy(SecretId, ResourcePolicy, Opts, Config) ->
    Json = lists:map(
        fun
            ({block_public_policy, Val}) -> {<<"BlockPublicPolicy">>, Val};
            (Other) -> Other
        end,
        [{<<"SecretId">>, SecretId}, {<<"ResourcePolicy">>, ResourcePolicy} | Opts]),
    sm_request(Config, "secretsmanager.PutResourcePolicy", Json).

%%------------------------------------------------------------------------------
%% PutSecretValue - put_secret_value
%%------------------------------------------------------------------------------
%% @doc
%%
%% Note: Use put_secret_value as put_secret_string and put_secret_binary functions are kept for backward compatibility.
%%
%% Creates a new version of your secret by creating a new encrypted value and attaching it to the secret.
%% Version can contain a new SecretString value or a new SecretBinary value.
%%
%% Do not call PutSecretValue at a sustained rate of more than once every 10 minutes.
%% When you update the secret value, Secrets Manager creates a new version of the secret.
%% Secrets Manager keeps 100 of the most recent versions, but it keeps all secret versions created in the last 24 hours.
%% If you call PutSecretValue more than once every 10 minutes, you will create more versions than Secrets Manager removes,
%% and you will reach the quota for secret versions.
%%
%% You can specify the staging labels to attach to the new version in VersionStages.
%% If you don't include VersionStages, then Secrets Manager automatically moves the staging label AWSCURRENT to this version.
%% If this operation creates the first version for the secret, then Secrets Manager automatically attaches the staging label AWSCURRENT to it.
%% If this operation moves the staging label AWSCURRENT from another version to this version, then Secrets Manager also automatically moves
%% the staging label AWSPREVIOUS to the version that AWSCURRENT was removed from.
%%
%% This operation is idempotent. If you call this operation with a ClientRequestToken that matches an existing version's VersionId,
%% and you specify the same secret data, the operation succeeds but does nothing. However, if the secret data is different, then
%% the operation fails because you can't modify an existing version; you can only create new ones.
%%
%% SM API:
%% [https://docs.aws.amazon.com/secretsmanager/latest/apireference/API_PutSecretValue.html]
%% @end
%%------------------------------------------------------------------------------
-spec put_secret_value(SecretId :: binary(), ClientRequestToken :: binary(),
                       Secret :: secret_value()) -> sm_response().
put_secret_value(SecretId, ClientRequestToken, Secret) ->
    put_secret_value(SecretId, ClientRequestToken, Secret, []).

-spec put_secret_value(SecretId :: binary(), ClientRequestToken :: binary(),
                       Secret :: secret_value(),
                       Opts :: put_secret_value_options()) -> sm_response().
put_secret_value(SecretId, ClientRequestToken, Secret, Opts) ->
    put_secret_value(SecretId, ClientRequestToken, Secret, Opts, erlcloud_aws:default_config()).

-spec put_secret_value(SecretId :: binary(), ClientRequestToken :: binary(),
                       Secret :: secret_value(),
                       Opts :: put_secret_value_options(),
                       Config :: aws_config()) -> sm_response().
put_secret_value(SecretId, ClientRequestToken, Secret, Opts, Config) ->
    SecretValue = case Secret of
        {secret_binary, Val} -> {secret_binary, base64:encode(Val)};
        {secret_string, Val} -> {secret_string, Val}
    end,
    put_secret_call(SecretId, ClientRequestToken, [SecretValue | Opts], Config).


%%------------------------------------------------------------------------------
%% PutSecretValue
%%------------------------------------------------------------------------------
%% @doc
%% Note: Use put_secret_value as put_secret_string and put_secret_binary functions are kept for backward compatibility.
%% SM API:
%% [https://docs.aws.amazon.com/secretsmanager/latest/apireference/API_PutSecretValue.html]
%% @end
%%------------------------------------------------------------------------------
-spec put_secret_string(SecretId :: binary(), ClientRequestToken :: binary(),
                        SecretString :: binary()) -> sm_response().
put_secret_string(SecretId, ClientRequestToken, SecretString) ->
    put_secret_string(SecretId, ClientRequestToken, SecretString, []).

-spec put_secret_string(SecretId :: binary(), ClientRequestToken :: binary(),
                        SecretString :: binary(), Opts :: put_secret_value_options()) -> sm_response().
put_secret_string(SecretId, ClientRequestToken, SecretString, Opts) ->
    put_secret_string(SecretId, ClientRequestToken, SecretString, Opts, erlcloud_aws:default_config()).

-spec put_secret_string(SecretId :: binary(), ClientRequestToken :: binary(),
                        SecretString :: binary(), Opts :: put_secret_value_options(),
                        Config :: aws_config()) -> sm_response().
put_secret_string(SecretId, ClientRequestToken, SecretString, Opts, Config) ->
    Secret = {secret_string, SecretString},
    put_secret_call(SecretId, ClientRequestToken, [Secret | Opts], Config).

-spec put_secret_binary(SecretId :: binary(), ClientRequestToken :: binary(),
                        SecretBinary :: binary()) -> sm_response().
put_secret_binary(SecretId, ClientRequestToken, SecretBinary) ->
    put_secret_binary(SecretId, ClientRequestToken, SecretBinary, []).

-spec put_secret_binary(SecretId :: binary(), ClientRequestToken :: binary(),
                        SecretBinary :: binary(), Opts :: put_secret_value_options()) -> sm_response().
put_secret_binary(SecretId, ClientRequestToken, SecretBinary, Opts) ->
    put_secret_binary(SecretId, ClientRequestToken, SecretBinary, Opts, erlcloud_aws:default_config()).

-spec put_secret_binary(SecretId :: binary(), ClientRequestToken :: binary(),
                        SecretBinary :: binary(), Opts :: put_secret_value_options(),
                        Config :: aws_config()) -> sm_response().
put_secret_binary(SecretId, ClientRequestToken, SecretBinary, Opts, Config) ->
    Secret = {secret_binary, base64:encode(SecretBinary)},
    put_secret_call(SecretId, ClientRequestToken, [Secret | Opts], Config).

%%------------------------------------------------------------------------------
%% RemoveRegionsFromReplication
%%------------------------------------------------------------------------------
%% @doc
%% For a secret that is replicated to other Regions, deletes the secret replicas from the Regions you specify.
%% SM API:
%% [https://docs.aws.amazon.com/secretsmanager/latest/apireference/API_RemoveRegionsFromReplication.html]
%% @end
%%------------------------------------------------------------------------------

-spec remove_regions_from_replication(SecretId :: binary(),
                                      Regions :: list(binary())) -> sm_response().
remove_regions_from_replication(SecretId, Regions) ->
    remove_regions_from_replication(SecretId, Regions, erlcloud_aws:default_config()).

-spec remove_regions_from_replication(SecretId :: binary(),
                                      Regions :: list(binary()),
                                      Config :: aws_config()) -> sm_response().
remove_regions_from_replication(SecretId, Regions, Config) ->
    Json = [{<<"SecretId">>, SecretId}, {<<"RemoveReplicaRegions">>, Regions}],
    sm_request(Config, "secretsmanager.RemoveRegionsFromReplication", Json).

%%------------------------------------------------------------------------------
%% ReplicateSecretToRegions
%%------------------------------------------------------------------------------
%% @doc
%% Replicates the secret to a new Regions.
%% SM API:
%% [https://docs.aws.amazon.com/secretsmanager/latest/apireference/API_ReplicateSecretToRegions.html]
%% @end
%%------------------------------------------------------------------------------

-spec replicate_secret_to_regions(SecretId :: binary(),
                                  Regions :: replica_regions()) -> sm_response().
replicate_secret_to_regions(SecretId, Regions) ->
    replicate_secret_to_regions(SecretId, Regions, []).

-spec replicate_secret_to_regions(SecretId :: binary(),
                                  Regions :: replica_regions(),
                                  Opts :: replicate_secret_to_regions_options()) -> sm_response().
replicate_secret_to_regions(SecretId, Regions, Opts) ->
    replicate_secret_to_regions(SecretId, Regions, Opts, erlcloud_aws:default_config()).

-spec replicate_secret_to_regions(SecretId :: binary(),
                                  Regions :: replica_regions(),
                                  Opts :: replicate_secret_to_regions_options(),
                                  Config :: aws_config()) -> sm_response().
replicate_secret_to_regions(SecretId, Regions, Opts, Config) ->
    Json = lists:map(
        fun
            ({force_overwrite_replica_secret, Val}) -> {<<"ForceOverwriteReplicaSecret">>, Val};
            (Other) -> Other
        end,
        [{<<"SecretId">>, SecretId}, {<<"ReplicaRegions">>, format_replica_regions(Regions)} | Opts]),
    sm_request(Config, "secretsmanager.ReplicateSecretToRegions", Json).

%%------------------------------------------------------------------------------
%% RestoreSecret
%%------------------------------------------------------------------------------
%% @doc
%% Cancels the scheduled deletion of a secret by removing the DeletedDate time stamp.
%% You can access a secret again after it has been restored.
%% SM API:
%% [https://docs.aws.amazon.com/secretsmanager/latest/apireference/API_RestoreSecret.html]
%% @end
%%------------------------------------------------------------------------------

-spec restore_secret(SecretId :: binary()) -> sm_response().
restore_secret(SecretId) ->
    restore_secret(SecretId, erlcloud_aws:default_config()).

-spec restore_secret(SecretId :: binary(), Config :: aws_config()) -> sm_response().
restore_secret(SecretId, Config) ->
    Json = [{<<"SecretId">>, SecretId}],
    sm_request(Config, "secretsmanager.RestoreSecret", Json).

%%------------------------------------------------------------------------------
%% RotateSecret
%%------------------------------------------------------------------------------
%% @doc
%% Configures and starts the asynchronous process of rotating the secret.
%% SM API:
%% [https://docs.aws.amazon.com/secretsmanager/latest/apireference/API_RotateSecret.html]
%% @end
%%------------------------------------------------------------------------------

-spec rotate_secret(SecretId :: binary(), ClientRequestToken :: binary()) -> sm_response().
rotate_secret(SecretId, ClientRequestToken) ->
    rotate_secret(SecretId, ClientRequestToken, []).
-spec rotate_secret(SecretId :: binary(), ClientRequestToken :: binary(),
                     Opts :: rotate_secret_options()) -> sm_response().
rotate_secret(SecretId, ClientRequestToken, Opts) ->
    rotate_secret(SecretId, ClientRequestToken, Opts, erlcloud_aws:default_config()).
-spec rotate_secret(SecretId :: binary(), ClientRequestToken :: binary(),
                     Opts :: rotate_secret_options(),
                     Config :: aws_config()) -> sm_response().
rotate_secret(SecretId, ClientRequestToken, Opts, Config) ->
    Json = lists:map(
        fun
            ({rotate_immediately, Val}) -> {<<"RotateImmediately">>, Val};
            ({rotation_lambda_arn, Val}) -> {<<"RotationLambdaARN">>, Val};
            ({rotation_rules, Val}) -> {<<"RotationRules">>, format_rotation_rules(Val)};
            (Other) -> Other
        end,
        [{<<"SecretId">>, SecretId}, {<<"ClientRequestToken">>, ClientRequestToken} | Opts]),
    sm_request(Config, "secretsmanager.RotateSecret", Json).

%%------------------------------------------------------------------------------
%% StopReplicationToReplicate
%%------------------------------------------------------------------------------
%% @doc
%% Removes the link between the replica secret and the primary secret and promotes the replica to a primary secret in the replica Region.
%%
%% You must call this operation from the Region in which you want to promote the replica to a primary secret.
%% SM API:
%% [https://docs.aws.amazon.com/secretsmanager/latest/apireference/API_StopReplicationToReplica.html]
%% @end
%%------------------------------------------------------------------------------

-spec stop_replication_to_replica(SecretId :: binary()) -> sm_response().
stop_replication_to_replica(SecretId) ->
    stop_replication_to_replica(SecretId, erlcloud_aws:default_config()).
-spec stop_replication_to_replica(SecretId :: binary(), Config :: aws_config()) -> sm_response().
stop_replication_to_replica(SecretId, Config) ->
    Json = [{<<"SecretId">>, SecretId}],
    sm_request(Config, "secretsmanager.StopReplicationToReplica", Json).

%%------------------------------------------------------------------------------
%% TagResource
%%------------------------------------------------------------------------------
%% @doc
%% Attaches tags to a secret. Tags consist of a key name and a value.
%% Tags are part of the secret's metadata. They are not associated with specific versions
%% of the secret. This operation appends tags to the existing list of tags.
%% SM API:
%% [https://docs.aws.amazon.com/secretsmanager/latest/apireference/API_TagResource.html]
%% @end
%%------------------------------------------------------------------------------

-spec tag_resource(SecretId :: binary(), Tags :: list()) -> sm_response().
tag_resource(SecretId, Tags) ->
    tag_resource(SecretId, Tags, erlcloud_aws:default_config()).
-spec tag_resource(SecretId :: binary(), Tags :: list(), Config :: aws_config()) -> sm_response().
tag_resource(SecretId, Tags, Config) ->
    Json = [{<<"SecretId">>, SecretId}, {<<"Tags">>, format_tags(Tags)}],
    sm_request(Config, "secretsmanager.TagResource", Json).

%%------------------------------------------------------------------------------
%% UntagResource
%%------------------------------------------------------------------------------
%% @doc
%% Removes tags from a secret.
%% This operation is idempotent. If a requested tag is not attached to the secret, no error is returned and the secret metadata is unchanged.
%% %% SM API:
%% [https://docs.aws.amazon.com/secretsmanager/latest/apireference/API_UntagResource.html]
%% @end
%%------------------------------------------------------------------------------

-spec untag_resource(SecretId :: binary(), TagKeys :: list(binary())) -> sm_response().
untag_resource(SecretId, TagKeys) ->
    untag_resource(SecretId, TagKeys, erlcloud_aws:default_config()).
-spec untag_resource(SecretId :: binary(), TagKeys :: list(binary()),
                     Config :: aws_config()) -> sm_response().
untag_resource(SecretId, TagKeys, Config) ->
    Json = [{<<"SecretId">>, SecretId}, {<<"TagKeys">>, TagKeys}],
    sm_request(Config, "secretsmanager.UntagResource", Json).

%%------------------------------------------------------------------------------
%% UpdateSecret
%%------------------------------------------------------------------------------
%% @doc
%% Modifies the details of a secret, including metadata and the secret value. To change the secret value,
%% you can also use PutSecretValue.
%%
%% To change the rotation configuration of a secret, use RotateSecret instead.
%%
%% It is recommended to avoid calling UpdateSecret at a sustained rate of more than once every 10 minutes.
%% When you call UpdateSecret to update the secret value, Secrets Manager creates a new version of the secret.
%% Secrets Manager removes outdated versions when there are more than 100, but it does not remove versions
%% created less than 24 hours ago. If you update the secret value more than once every 10 minutes, you create
%% more versions than Secrets Manager removes, and you will reach the quota for secret versions.
%%
%% If you include SecretString or SecretBinary to create a new secret version, Secrets Manager automatically
%% moves the staging label AWSCURRENT to the new version. Then it attaches the label AWSPREVIOUS to the version
%% that AWSCURRENT was removed from.
%%
%% If you call this operation with a ClientRequestToken that matches an existing version's VersionId,
%% the operation results in an error. You can't modify an existing version, you can only create a new version.
%% To remove a version, remove all staging labels from it. See UpdateSecretVersionStage.
%%
%% %% SM API:
%% [https://docs.aws.amazon.com/secretsmanager/latest/apireference/API_UpdateSecret.html]
%% @end
%%------------------------------------------------------------------------------

-spec update_secret(SecretId :: binary(), ClientRequestToken :: binary()) -> sm_response().
update_secret(SecretId, ClientRequestToken) ->
    update_secret(SecretId, ClientRequestToken, []).
-spec update_secret(SecretId :: binary(),
                    ClientRequestToken :: binary(),
                    Opts :: update_secret_options()) -> sm_response().
update_secret(SecretId, ClientRequestToken, Opts) ->
    update_secret(SecretId, ClientRequestToken, Opts, erlcloud_aws:default_config()).
-spec update_secret(SecretId :: binary(),
                    ClientRequestToken :: binary(),
                    Opts :: update_secret_options(),
                    Config :: aws_config()) -> sm_response().
update_secret(SecretId, ClientRequestToken, Opts, Config) ->
    Json = lists:map(
        fun
            ({secret_binary, Val}) -> {<<"SecretBinary">>, base64:encode(Val)};
            ({secret_string, Val}) -> {<<"SecretString">>, Val};
            ({description, Val}) -> {<<"Description">>, Val};
            ({kms_key_id, Val}) -> {<<"KmsKeyId">>, Val};
            (Other) -> Other
        end,
        [{<<"SecretId">>, SecretId}, {<<"ClientRequestToken">>, ClientRequestToken} | Opts]),
    sm_request(Config, "secretsmanager.UpdateSecret", Json).

%%------------------------------------------------------------------------------
%% UpdateSecretVersionStage
%%------------------------------------------------------------------------------
%% @doc
%% Modifies the staging labels attached to a version of a secret. Secrets Manager uses
%% staging labels to track a version as it progresses through the secret rotation process.
%% Each staging label can be attached to only one version at a time. To add a staging
%% label to a version when it is already attached to another version, Secrets Manager
%% first removes it from the other version first and then attaches it to this one.
%%
%% The staging labels that you specify in the VersionStage parameter are added to the
%% existing list of staging labels for the version.
%%
%% %% SM API:
%% [https://docs.aws.amazon.com/secretsmanager/latest/apireference/API_UpdateSecretVersionStage.html]
%% @end
%%------------------------------------------------------------------------------

-spec update_secret_version_stage(SecretId :: binary(),
                                  VersionStage :: binary()) -> sm_response().
update_secret_version_stage(SecretId, VersionStage) ->
    update_secret_version_stage(SecretId, VersionStage, []).
-spec update_secret_version_stage(SecretId :: binary(),
                                  VersionStage :: binary(),
                                  Opts :: update_secret_version_stage_options()) -> sm_response().
update_secret_version_stage(SecretId, VersionStage, Opts) ->
    update_secret_version_stage(SecretId, VersionStage, Opts, erlcloud_aws:default_config()).
-spec update_secret_version_stage(SecretId :: binary(),
                                  VersionStage :: binary(),
                                  Opts :: update_secret_version_stage_options(),
                                  Config :: aws_config()) -> sm_response().
update_secret_version_stage(SecretId, VersionStage, Opts, Config) ->
    Json = lists:map(
        fun
            ({remove_from_version_id, Val}) -> {<<"RemoveFromVersionId">>, Val};
            ({move_to_version_id, Val}) -> {<<"MoveToVersionId">>, Val};
            (Other) -> Other
        end,
        [{<<"SecretId">>, SecretId}, {<<"VersionStage">>, VersionStage} | Opts]),
    sm_request(Config, "secretsmanager.UpdateSecretVersionStage", Json).

%%------------------------------------------------------------------------------
%% ValidateResourcePolicy
%%------------------------------------------------------------------------------
%% @doc
%% Modifies the resource policy attached to a secret. Secrets Manager uses resource
%% policies to manage access to secrets. This operation allows you to add or remove
%% permissions for specific principals (IAM users, roles, etc.) on a secret.
%%
%% %% SM API:
%% [https://docs.aws.amazon.com/secretsmanager/latest/apireference/API_ValidateResourcePolicy.html]
%% @end
%%------------------------------------------------------------------------------

-spec validate_resource_policy(ResourcePolicy :: binary()) -> sm_response().
validate_resource_policy(ResourcePolicy) ->
    validate_resource_policy(ResourcePolicy, []).
-spec validate_resource_policy(ResourcePolicy :: binary(),
                               Opts :: validate_resource_policy_options()) -> sm_response().
validate_resource_policy(ResourcePolicy, Opts) ->
    validate_resource_policy(ResourcePolicy, Opts, erlcloud_aws:default_config()).
-spec validate_resource_policy(ResourcePolicy :: binary(),
                               Opts :: validate_resource_policy_options(),
                               Config :: aws_config()) -> sm_response().
validate_resource_policy(ResourcePolicy, Opts, Config) ->
    Json = lists:map(
        fun
            ({secret_id, Val}) -> {<<"SecretId">>, Val};
            (Other) -> Other
        end,
        [{<<"ResourcePolicy">>, ResourcePolicy} | Opts]),
    sm_request(Config, "secretsmanager.ValidateResourcePolicy", Json).


%%%------------------------------------------------------------------------------
%%% Internal Functions
%%%------------------------------------------------------------------------------

create_secret_call(SecretName, ClientRequestToken, Opts, Config) ->
    Json = lists:map(
        fun
            ({add_replica_regions, Val}) -> {<<"AddReplicaRegions">>, format_replica_regions(Val)};
            ({description, Val}) -> {<<"Description">>, Val};
            ({force_overwrite_replica_secret, Val}) -> {<<"ForceOverwriteReplicaSecret">>, Val};
            ({kms_key_id, Val}) -> {<<"KmsKeyId">>, Val};
            ({secret_binary, Val}) -> {<<"SecretBinary">>, Val};
            ({secret_string, Val}) -> {<<"SecretString">>, Val};
            ({tags, Val}) -> {<<"Tags">>, format_tags(Val)};
            (Other) -> Other
        end,
        [{<<"Name">>, SecretName}, {<<"ClientRequestToken">>, ClientRequestToken} | Opts]),
    sm_request(Config, "secretsmanager.CreateSecret", Json).

put_secret_call(SecretId, ClientRequestToken, Opts, Config) ->
    Json = lists:map(
        fun
            ({rotation_token, Val}) -> {<<"RotationToken">>, Val};
            ({secret_binary, Val}) -> {<<"SecretBinary">>, Val};
            ({secret_string, Val}) -> {<<"SecretString">>, Val};
            ({version_stages, Val}) -> {<<"VersionStages">>, Val};
            (Other) -> Other
        end,
        [{<<"SecretId">>, SecretId}, {<<"ClientRequestToken">>, ClientRequestToken} | Opts]),
    sm_request(Config, "secretsmanager.PutSecretValue", Json).


-spec format_replica_regions(ReplicaRegions :: replica_regions()) -> list().
format_replica_regions(ReplicaRegions) ->
    lists:map(fun format_replica_region/1, ReplicaRegions).

-spec format_replica_region(ReplicaRegion :: replica_region()) -> list().
format_replica_region(ReplicaRegion) ->
    format_replica_region(ReplicaRegion, []).

-spec format_replica_region(ReplicaRegion :: replica_region(),
                            Acc :: list()) -> list().
format_replica_region([{region, Region} | Rest], Acc) ->
    format_replica_region(Rest, [{<<"Region">>, Region} | Acc]);
format_replica_region([{kms_key_id, KmsKeyId} | Rest], Acc) ->
    format_replica_region(Rest, [{<<"KmsKeyId">>, KmsKeyId} | Acc]);
format_replica_region([Val | Rest], Acc) ->
    format_replica_region(Rest, [Val | Acc]);
format_replica_region([], Acc) ->
    Acc.


-spec format_tags(Tags :: list(proplists:proplist())) -> list(proplists:proplist()).
format_tags(Tags) ->
    lists:map(fun format_tag/1, Tags).

-spec format_tag(Tags :: proplists:proplist()) -> list().
format_tag(Tags) ->
    format_tag(Tags, []).

-spec format_tag(Tags :: proplists:proplist(),
                 Acc :: list()) -> list().
format_tag([{key, Key} | Rest], Acc) ->
    format_tag(Rest, [{<<"Key">>, Key} | Acc]);
format_tag([{value, Value} | Rest], Acc) ->
    format_tag(Rest, [{<<"Value">>, Value} | Acc]);
format_tag([Val | Rest], Acc) ->
    format_tag(Rest, [Val | Acc]);
format_tag([], Acc) ->
    Acc.

-spec format_rotation_rules(RotationRules :: rotation_rules()) -> list().
format_rotation_rules(RotationRules) ->
    lists:map(
        fun
            ({automatically_after_days, Val}) -> {<<"AutomaticallyAfterDays">>, Val};
            ({duration, Val}) -> {<<"Duration">>, Val};
            ({schedule_expression, Val}) -> {<<"ScheduleExpression">>, Val};
            (Other) -> Other
        end,
        RotationRules).


sm_request(Config, Operation, Body) ->
    case erlcloud_aws:update_config(Config) of
        {ok, Config1} ->
            sm_request_no_update(Config1, Operation, Body);
        {error, Reason} ->
            {error, Reason}
    end.


sm_request_no_update(Config, Operation, Body) ->
    Payload = jsx:encode(Body),
    Headers = headers(Config, Operation, Payload),
    Request = #aws_request{service = sm,
        uri = uri(Config),
        method = post,
        request_headers = Headers,
        request_body = Payload},
    case erlcloud_aws:request_to_return(erlcloud_retry:request(Config, Request, fun sm_result_fun/1)) of
        {ok, {_RespHeaders, <<>>}} -> {ok, []};
        {ok, {_RespHeaders, RespBody}} -> {ok, jsx:decode(RespBody, [{return_maps, false}])};
        {error, _} = Error -> Error
    end.


headers(Config, Operation, Body) ->
    Headers = [{"host", Config#aws_config.sm_host},
        {"x-amz-target", Operation},
        {"content-type", "application/x-amz-json-1.1"}],
    Region = erlcloud_aws:aws_region_from_host(Config#aws_config.sm_host),
    erlcloud_aws:sign_v4_headers(Config, Headers, Body, Region, "secretsmanager").


uri(#aws_config{sm_scheme = Scheme, sm_host = Host} = Config) ->
    lists:flatten([Scheme, Host, port_spec(Config)]).


port_spec(#aws_config{sm_port = 443}) ->
    "";
port_spec(#aws_config{sm_port = Port}) ->
    [":", erlang:integer_to_list(Port)].


-spec sm_result_fun(Request :: aws_request()) -> aws_request().
sm_result_fun(#aws_request{response_type = ok} = Request) ->
    Request;
sm_result_fun(#aws_request{response_type = error,
        error_type = aws, response_status = Status} = Request) when Status >= 500 ->
    Request#aws_request{should_retry = true};
sm_result_fun(#aws_request{response_type = error, error_type = aws} = Request) ->
    Request#aws_request{should_retry = false}.
