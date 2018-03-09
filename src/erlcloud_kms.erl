-module(erlcloud_kms).
-author('rorra@rorra.com.ar').
-author('zfox@alertlogic.com').

-include("erlcloud.hrl").
-include("erlcloud_aws.hrl").

-define(API_VERSION, "2014-11-01").

%%% Library initialization.
-export([configure/2, configure/3, configure/4, new/2, new/3, new/4, kms_result_fun/1]).

%%% KMS API
-export([create_alias/2, create_alias/3,
         create_grant/3, create_grant/4, create_grant/5,
         create_key/0, create_key/1, create_key/2,
         decrypt/1, decrypt/2, decrypt/3,
         delete_alias/1, delete_alias/2,
         describe_key/1, describe_key/2,
         disable_key/1, disable_key/2,
         disable_key_rotation/1, disable_key_rotation/2,
         enable_key/1, enable_key/2,
         enable_key_rotation/1, enable_key_rotation/2,
         encrypt/2, encrypt/3, encrypt/4,
         generate_data_key/2, generate_data_key/3,
         generate_data_key_without_plaintext/2, generate_data_key_without_plaintext/3,
         generate_random/1, generate_random/2,
         get_key_policy/2, get_key_policy/3,
         get_key_rotation_status/1, get_key_rotation_status/2,
         list_aliases/0, list_aliases/1, list_aliases/2,
         list_grants/1, list_grants/2, list_grants/3,
         list_key_policies/1, list_key_policies/2, list_key_policies/3,
         list_keys/0, list_keys/1, list_keys/2,
         put_key_policy/3, put_key_policy/4,
         re_encrypt/2, re_encrypt/3, re_encrypt/4,
         retire_grant/1, retire_grant/2,
         revoke_grant/2, revoke_grant/3,
         update_alias/2, update_alias/3,
         update_key_description/2, update_key_description/3]).


%%%------------------------------------------------------------------------------
%%% Shared types
%%%------------------------------------------------------------------------------
-type pagination_opts() :: [pagination_opt()].
-type pagination_opt() :: {limit, non_neg_integer()} | {marker, string()}.

-type return_val() :: {ok, proplists:proplist()} | {error, term()}.

-export_type([return_val/0]).

%%%------------------------------------------------------------------------------
%%% Library initialization.
%%%------------------------------------------------------------------------------

-spec new(string(), string()) -> aws_config().

new(AccessKeyID, SecretAccessKey) ->
    #aws_config{
       access_key_id=AccessKeyID,
       secret_access_key=SecretAccessKey
      }.

-spec new(string(), string(), string()) -> aws_config().

new(AccessKeyID, SecretAccessKey, Host) ->
    #aws_config{
       access_key_id=AccessKeyID,
       secret_access_key=SecretAccessKey,
       kms_host=Host
      }.

-spec new(string(), string(), string(), non_neg_integer()) -> aws_config().

new(AccessKeyID, SecretAccessKey, Host, Port) ->
    #aws_config{
       access_key_id=AccessKeyID,
       secret_access_key=SecretAccessKey,
       kms_host=Host,
       kms_port=Port
      }.

-spec configure(string(), string()) -> ok.

configure(AccessKeyID, SecretAccessKey) ->
    put(aws_config, new(AccessKeyID, SecretAccessKey)),
    ok.

-spec configure(string(), string(), string()) -> ok.

configure(AccessKeyID, SecretAccessKey, Host) ->
    put(aws_config, new(AccessKeyID, SecretAccessKey, Host)),
    ok.

-spec configure(string(), string(), string(), non_neg_integer()) -> ok.

configure(AccessKeyID, SecretAccessKey, Host, Port) ->
    put(aws_config, new(AccessKeyID, SecretAccessKey, Host, Port)),
    ok.

default_config() ->
    erlcloud_aws:default_config().

%%%------------------------------------------------------------------------------
%%% CreateAlias
%%%------------------------------------------------------------------------------

%%------------------------------------------------------------------------------
%% @doc
%% KMS API:
%% [http://docs.aws.amazon.com/kms/latest/APIReference/API_CreateAlias.html]
%%
%% ===Example===
%%
%%------------------------------------------------------------------------------
-spec create_alias
          (AliasName :: binary(),
           TargetKeyId :: binary()) ->
          return_val().
create_alias(AliasName, TargetKeyId) ->
    create_alias(AliasName, TargetKeyId, default_config()).


-spec create_alias
          (AliasName :: binary(),
           TargetKeyId :: binary(),
           Config :: aws_config()) ->
          return_val().
create_alias(AliasName, TargetKeyId, Config) ->
    Json = [{<<"AliasName">>, AliasName}, {<<"TargetKeyId">>, TargetKeyId}],
    kms_request(Config, "TrentService.CreateAlias", Json).


%%%------------------------------------------------------------------------------
%%% CreateGrant
%%%------------------------------------------------------------------------------

%%------------------------------------------------------------------------------
%% @doc
%% KMS API:
%% [http://docs.aws.amazon.com/kms/latest/APIReference/API_CreateGrant.html]
%%
%% ===Example===
%%
%%------------------------------------------------------------------------------
-type create_grant_opts() :: [create_grant_opt()].
-type create_grant_opt() :: {create_grant_opt_key(), term()}.
-type create_grant_opt_key() :: grant_tokens | constraints | retiring_principal.


-spec create_grant
          (GranteePrincipal :: binary(),
           KeyId :: binary(),
           Operations :: [binary() | atom()]) ->
          return_val().
create_grant(GranteePrincipal, KeyId, Operations) ->
    create_grant(GranteePrincipal, KeyId, Operations, []).


-spec create_grant
          (GranteePrincipal :: binary(),
           KeyId :: binary(),
           Operations :: [binary() | atom()],
           Options :: create_grant_opts()) ->
          return_val().
create_grant(GranteePrincipal, KeyId, Operations, Options) ->
    create_grant(GranteePrincipal, KeyId, Operations, Options, default_config()).


-spec create_grant
          (GranteePrincipal :: binary(),
           KeyId :: binary(),
           Operations :: [binary() | atom()],
           Options :: create_grant_opts(),
           Config :: aws_config()) ->
          return_val().
create_grant(GranteePrincipal, KeyId, Operations, Options, Config) ->
    OptJson = dynamize_options(Options),
    Json = [{<<"GranteePrincipal">>, GranteePrincipal},
            {<<"KeyId">>, KeyId},
            {<<"Operations">>, Operations}|OptJson],
    kms_request(Config, "TrentService.CreateGrant", Json).


%%%------------------------------------------------------------------------------
%%% CreateKey
%%%------------------------------------------------------------------------------

%%------------------------------------------------------------------------------
%% @doc
%% KMS API:
%% [http://docs.aws.amazon.com/kms/latest/APIReference/API_CreateKey.html]
%%
%% ===Example===
%%
%%------------------------------------------------------------------------------
-type create_key_opts() :: [create_key_opt()].
-type create_key_opt() :: [{create_key_opt_key(), term()}].
-type create_key_opt_key() :: description | key_usage | policy.


-spec create_key() -> return_val().
create_key() ->
    create_key([]).


-spec create_key
          (Options :: create_key_opts()) ->
          return_val().
create_key(Options) ->
    create_key(Options, default_config()).


-spec create_key
          (Options :: create_key_opts(),
           Config :: aws_config()) ->
          return_val().
create_key(Options, Config) ->
    Json = dynamize_options(Options),
    kms_request(Config, "TrentService.CreateKey", Json).


%%%------------------------------------------------------------------------------
%%% Decrypt
%%%------------------------------------------------------------------------------

%%------------------------------------------------------------------------------
%% @doc
%% KMS API:
%% [http://docs.aws.amazon.com/kms/latest/APIReference/API_Decrypt.html]
%%
%% ===Example===
%%
%%------------------------------------------------------------------------------
-type decrypt_opts() :: [decrypt_opt()].
-type decrypt_opt() :: {decrypt_opt_key(), term()}.
-type decrypt_opt_key() :: encryption_context | grant_tokens.


-spec decrypt
          (CiphertextBlob :: binary()) ->
          return_val().
decrypt(CiphertextBlob) ->
    decrypt(CiphertextBlob, []).


-spec decrypt
          (CiphertextBlob :: binary(),
           Options :: decrypt_opts()) ->
          return_val().
decrypt(CiphertextBlob, Options) ->
    decrypt(CiphertextBlob, Options, default_config()).


-spec decrypt
          (CiphertextBlob :: binary(),
           Options :: decrypt_opts(),
           Config :: aws_config()) ->
          return_val().
decrypt(CiphertextBlob, Options, Config) ->
    OptJson = dynamize_options(Options),
    Json = [{<<"CiphertextBlob">>, CiphertextBlob}|OptJson],
    kms_request(Config, "TrentService.Decrypt", Json).


%%%------------------------------------------------------------------------------
%%% DeleteAlias
%%%------------------------------------------------------------------------------

%%------------------------------------------------------------------------------
%% @doc
%% KMS API:
%% [http://docs.aws.amazon.com/kms/latest/APIReference/API_DeleteAlias.html]
%%
%% ===Example===
%%
%%------------------------------------------------------------------------------
-spec delete_alias
          (AliasName :: binary()) ->
          return_val().
delete_alias(AliasName) ->
    delete_alias(AliasName, default_config()).


-spec delete_alias
          (AliasName :: binary(),
           Config :: aws_config()) ->
          return_val().
delete_alias(AliasName, Config) ->
    Json = [{<<"AliasName">>, AliasName}],
    kms_request(Config, "TrentService.DeleteAlias", Json).


%%%------------------------------------------------------------------------------
%%% DescribeKey
%%%------------------------------------------------------------------------------

%%------------------------------------------------------------------------------
%% @doc
%% KMS API:
%% [http://docs.aws.amazon.com/kms/latest/APIReference/API_DescribeKey.html]
%%
%% ===Example===
%%
%%------------------------------------------------------------------------------
-spec describe_key
          (KeyId :: binary()) ->
          return_val().
describe_key(KeyId) ->
    describe_key(KeyId, default_config()).


-spec describe_key
          (KeyId :: binary(),
           Config :: aws_config()) ->
          return_val().
describe_key(KeyId, Config) ->
    Json = [{<<"KeyId">>, KeyId}],
    kms_request(Config, "TrentService.DescribeKey", Json).


%%%------------------------------------------------------------------------------
%%% DisableKey
%%%------------------------------------------------------------------------------

%%------------------------------------------------------------------------------
%% @doc
%% KMS API:
%% [http://docs.aws.amazon.com/kms/latest/APIReference/API_DisableKey.html]
%%
%% ===Example===
%%
%%------------------------------------------------------------------------------
-spec disable_key
          (KeyId :: binary()) ->
          return_val().
disable_key(KeyId) ->
    disable_key(KeyId, default_config()).


-spec disable_key
          (KeyId :: binary(),
           Config :: aws_config()) ->
          return_val().
disable_key(KeyId, Config) ->
    Json = [{<<"KeyId">>, KeyId}],
    kms_request(Config, "TrentService.DisableKey", Json).


%%%------------------------------------------------------------------------------
%%% DisableKeyRotation
%%%------------------------------------------------------------------------------

%%------------------------------------------------------------------------------
%% @doc
%% KMS API:
%% [http://docs.aws.amazon.com/kms/latest/APIReference/API_DisableKeyRotation.html]
%%
%% ===Example===
%%
%%------------------------------------------------------------------------------
-spec disable_key_rotation
          (KeyId :: binary()) ->
          return_val().
disable_key_rotation(KeyId) ->
    disable_key_rotation(KeyId, default_config()).


-spec disable_key_rotation
          (KeyId :: binary(),
           Config :: aws_config()) ->
          return_val().
disable_key_rotation(KeyId, Config) ->
    Json = [{<<"KeyId">>, KeyId}],
    kms_request(Config, "TrentService.DisableKey", Json).


%%%------------------------------------------------------------------------------
%%% EnableKey
%%%------------------------------------------------------------------------------

%%------------------------------------------------------------------------------
%% @doc
%% KMS API:
%% [http://docs.aws.amazon.com/kms/latest/APIReference/API_EnableKeyRotation.html]
%%
%% ===Example===
%%
%%------------------------------------------------------------------------------
-spec enable_key
          (KeyId :: binary()) ->
          return_val().
enable_key(KeyId) ->
    enable_key(KeyId, default_config()).

-spec enable_key
          (KeyId :: binary(),
           Config :: aws_config()) ->
          return_val().
enable_key(KeyId, Config) ->
    Json = [{<<"KeyId">>, KeyId}],
    kms_request(Config, "TrentService.EnableKey", Json).


%%%------------------------------------------------------------------------------
%%% EnableKeyRotation
%%%------------------------------------------------------------------------------

%%------------------------------------------------------------------------------
%% @doc
%% KMS API:
%% [http://docs.aws.amazon.com/kms/latest/APIReference/API_EnableKeyRotation.html]
%%
%% ===Example===
%%
%%------------------------------------------------------------------------------
-spec enable_key_rotation
          (KeyId :: binary()) ->
          return_val().
enable_key_rotation(KeyId) ->
    enable_key_rotation(KeyId, default_config()).


-spec enable_key_rotation
          (KeyId :: binary(),
           Config :: aws_config()) ->
          return_val().
enable_key_rotation(KeyId, Config) ->
    Json = [{<<"KeyId">>, KeyId}],
    kms_request(Config, "TrentService.EnableKeyRotation", Json).


%%%------------------------------------------------------------------------------
%%% Encrypt
%%%------------------------------------------------------------------------------

%%------------------------------------------------------------------------------
%% @doc
%% KMS API:
%% [http://docs.aws.amazon.com/kms/latest/APIReference/API_Encrypt.html]
%%
%% ===Example===
%%
%%------------------------------------------------------------------------------
-type encrypt_opts() :: [encrypt_opt()].
-type encrypt_opt() :: {encrypt_opt_key(), term()}.
-type encrypt_opt_key() :: encryption_context | grant_tokens.


-spec encrypt
          (KeyId :: binary(),
           Plaintext :: binary()) ->
          return_val().
encrypt(KeyId, Plaintext) ->
    encrypt(KeyId, Plaintext, []).


-spec encrypt
          (KeyId :: binary(),
           Plaintext :: binary(),
           Options :: encrypt_opts()) ->
          return_val().
encrypt(KeyId, Plaintext, Options) ->
    encrypt(KeyId, Plaintext, Options, default_config()).


-spec encrypt
          (KeyId :: binary(),
           Plaintext :: binary(),
           Options :: encrypt_opts(),
           Config :: aws_config()) ->
          return_val().
encrypt(KeyId, Plaintext, Options, Config) ->
    OptJson = dynamize_options(Options),
    Json = [{<<"KeyId">>, KeyId}, {<<"Plaintext">>, Plaintext}|OptJson],
    kms_request(Config, "TrentService.Encrypt", Json).


%%%------------------------------------------------------------------------------
%%% GenerateDataKey
%%%------------------------------------------------------------------------------

%%------------------------------------------------------------------------------
%% @doc
%% KMS API:
%% [http://docs.aws.amazon.com/kms/latest/APIReference/API_GenerateDataKey.html]
%%
%% ===Example===
%%
%%------------------------------------------------------------------------------
-type generate_data_key_opts() :: [generate_data_key_opt()].
-type generate_data_key_opt() :: {generate_data_key_opt_key(), term()}.
-type generate_data_key_opt_key() :: encryption_context | grant_tokens | key_spec | number_of_bytes.


-spec generate_data_key
          (KeyId :: binary(),
           Options :: generate_data_key_opts()) ->
          return_val().
generate_data_key(KeyId, Options) ->
    generate_data_key(KeyId, Options, default_config()).


-spec generate_data_key
          (KeyId :: binary(),
           Options :: generate_data_key_opts(),
           Config :: aws_config()) ->
          return_val().
generate_data_key(KeyId, Options, Config) ->
    OptJson = dynamize_options(Options),
    Json = [{<<"KeyId">>, KeyId}|OptJson],
    kms_request(Config, "TrentService.GenerateDataKey", Json).


%%%------------------------------------------------------------------------------
%%%------------------------------------------------------------------------------
%%% GenerateDataKeyWithoutPlaintext
%%%------------------------------------------------------------------------------

%%------------------------------------------------------------------------------
%% @doc
%% KMS API:
%% [http://docs.aws.amazon.com/kms/latest/APIReference/API_GenerateDataKeyWithoutPlaintext.html]
%%
%% ===Example===
%%
%%------------------------------------------------------------------------------
-spec generate_data_key_without_plaintext
          (KeyId :: binary(),
           Options :: generate_data_key_opts()) ->
          return_val().
generate_data_key_without_plaintext(KeyId, Options) ->
    generate_data_key_without_plaintext(KeyId, Options, default_config()).


-spec generate_data_key_without_plaintext
          (KeyId :: binary(),
           Options :: generate_data_key_opts(),
           Config :: aws_config()) ->
          return_val().
generate_data_key_without_plaintext(KeyId, Options, Config) ->
    OptJson = dynamize_options(Options),
    Json = [{<<"KeyId">>, KeyId}|OptJson],
    kms_request(Config, "TrentService.GenerateDataKeyWithoutPlaintext", Json).


%%%------------------------------------------------------------------------------
%%% GenerateRandom
%%%------------------------------------------------------------------------------

%%------------------------------------------------------------------------------
%% @doc
%% KMS API:
%% [http://docs.aws.amazon.com/kms/latest/APIReference/API_GenerateRandom.html]
%%
%% ===Example===
%%
%%------------------------------------------------------------------------------
-spec generate_random
          (NumberOfBytes :: non_neg_integer()) ->
          return_val().
generate_random(NumberOfBytes) ->
    generate_random(NumberOfBytes, default_config()).


-spec generate_random
          (NumberOfBytes :: non_neg_integer(),
           Config :: aws_config()) ->
          return_val().
generate_random(NumberOfBytes, Config) ->
    Json = [{<<"NumberOfBytes">>, NumberOfBytes}],
    kms_request(Config, "TrentService.GenerateRandom", Json).


%%%------------------------------------------------------------------------------
%%% GetKeyPolicy
%%%------------------------------------------------------------------------------

%%------------------------------------------------------------------------------
%% @doc
%% KMS API:
%% [http://docs.aws.amazon.com/kms/latest/APIReference/API_GetKeyPolicy.html]
%%
%% ===Example===
%%
%%------------------------------------------------------------------------------
-spec get_key_policy
          (KeyId :: binary(),
           PolicyName :: binary()) ->
          return_val().
get_key_policy(KeyId, PolicyName) ->
    get_key_policy(KeyId, PolicyName, default_config()).


-spec get_key_policy
          (KeyId :: binary(),
           PolicyName :: binary(),
           Config :: aws_config()) ->
          return_val().
get_key_policy(KeyId, PolicyName, Config) ->
    Json = [{<<"KeyId">>, KeyId}, {<<"PolicyName">>, PolicyName}],
    kms_request(Config, "TrentService.GetKeyPolicy", Json).


%%%------------------------------------------------------------------------------
%%% GetKeyRotationStatus
%%%------------------------------------------------------------------------------

%%------------------------------------------------------------------------------
%% @doc
%% KMS API:
%% [http://docs.aws.amazon.com/kms/latest/APIReference/API_GetKeyRotationStatus.html]
%%
%% ===Example===
%%
%%------------------------------------------------------------------------------
-spec get_key_rotation_status
          (KeyId :: binary()) ->
          return_val().
get_key_rotation_status(KeyId) ->
    get_key_rotation_status(KeyId, default_config()).


-spec get_key_rotation_status
          (KeyId :: binary(),
           Config :: aws_config()) ->
          return_val().
get_key_rotation_status(KeyId, Config) ->
    Json = [{<<"KeyId">>, KeyId}],
    kms_request(Config, "TrentService.GetKeyRotationStatus", Json).


%%%------------------------------------------------------------------------------
%%% ListAliases
%%%------------------------------------------------------------------------------

%%------------------------------------------------------------------------------
%% @doc
%% KMS API:
%% [http://docs.aws.amazon.com/kms/latest/APIReference/API_ListAliases.html]
%%
%% ===Example===
%%
%%------------------------------------------------------------------------------
-spec list_aliases
          () ->
          return_val().
list_aliases() ->
    list_aliases([]).


-spec list_aliases
          (Options :: pagination_opts()) ->
          return_val().
list_aliases(Options) ->
    list_aliases(Options, default_config()).


-spec list_aliases
          (Options :: pagination_opts(),
           Config :: aws_config()) ->
          return_val().
list_aliases(Options, Config) ->
    Json = dynamize_options(Options),
    kms_request(Config, "TrentService.ListAliases", Json).


%%%------------------------------------------------------------------------------
%%% ListGrants
%%%------------------------------------------------------------------------------

%%------------------------------------------------------------------------------
%% @doc
%% KMS API:
%% [http://docs.aws.amazon.com/kms/latest/APIReference/API_ListGrants.html]
%%
%% ===Example===
%%
%%------------------------------------------------------------------------------
-spec list_grants
          (KeyId :: binary()) ->
          return_val().
list_grants(KeyId) ->
    list_grants(KeyId, []).


-spec list_grants
          (KeyId :: binary(),
           Options :: pagination_opts()) ->
          return_val().
list_grants(KeyId, Options) ->
    list_grants(KeyId, Options, default_config()).


-spec list_grants
          (KeyId :: binary(),
           Options :: pagination_opts(),
           Config :: aws_config()) ->
          return_val().
list_grants(KeyId, Options, Config) ->
    OptJson = dynamize_options(Options),
    Json = [{<<"KeyId">>, KeyId}|OptJson],
    kms_request(Config, "TrentService.ListGrants", Json).


%%%------------------------------------------------------------------------------
%%% ListKeyPolicies
%%%------------------------------------------------------------------------------

%%------------------------------------------------------------------------------
%% @doc
%% KMS API:
%% [http://docs.aws.amazon.com/kms/latest/APIReference/API_ListKeyPolicies.html]
%%
%% ===Example===
%%
%%------------------------------------------------------------------------------
-spec list_key_policies
          (KeyId :: binary()) ->
          return_val().
list_key_policies(KeyId) ->
    list_key_policies(KeyId, []).


-spec list_key_policies
          (KeyId :: binary(),
           Options :: pagination_opts()) ->
          return_val().
list_key_policies(KeyId, Options) ->
    list_key_policies(KeyId, Options, default_config()).


-spec list_key_policies
          (KeyId :: binary(),
           Options :: pagination_opts(),
           Config :: aws_config()) ->
          return_val().
list_key_policies(KeyId, Options, Config) ->
    OptJson = dynamize_options(Options),
    Json = [{<<"KeyId">>, KeyId}|OptJson],
    kms_request(Config, "TrentService.ListKeyPolicies", Json).


%%%------------------------------------------------------------------------------
%%% ListKeys
%%%------------------------------------------------------------------------------

%%------------------------------------------------------------------------------
%% @doc
%% KMS API:
%% [http://docs.aws.amazon.com/kms/latest/APIReference/API_ListKeys.html]
%%
%% ===Example===
%%
%%------------------------------------------------------------------------------
-spec list_keys
          () ->
          return_val().
list_keys() ->
    list_keys([]).


-spec list_keys
          (Options :: pagination_opts()) ->
          return_val().
list_keys(Options) ->
    list_keys(Options, default_config()).


-spec list_keys
          (Options :: pagination_opts(),
           Config :: aws_config()) ->
          return_val().
list_keys(Options, Config) ->
    Json = dynamize_options(Options),
    kms_request(Config, "TrentService.ListKeys", Json).


%%%------------------------------------------------------------------------------
%%% PutKeyPolicy
%%%------------------------------------------------------------------------------

%%------------------------------------------------------------------------------
%% @doc
%% KMS API:
%% [http://docs.aws.amazon.com/kms/latest/APIReference/API_PutKeyPolicy.html]
%%
%% ===Example===
%%
%%------------------------------------------------------------------------------
-spec put_key_policy
          (KeyId :: binary(),
           Policy :: binary(),
           PolicyName :: binary()) ->
          return_val().
put_key_policy(KeyId, Policy, PolicyName) ->
    put_key_policy(KeyId, Policy, PolicyName, default_config()).


-spec put_key_policy
          (KeyId :: binary(),
           Policy :: binary(),
           PolicyName :: binary(),
           Config :: aws_config()) ->
          return_val().
put_key_policy(KeyId, Policy, PolicyName, Config) ->
    Json = [{<<"KeyId">>, KeyId}, {<<"Policy">>, Policy}, {<<"PolicyName">>, PolicyName}],
    kms_request(Config, "TrentService.PutKeyPolicy", Json).


%%%------------------------------------------------------------------------------
%%% ReEncrypt
%%%------------------------------------------------------------------------------

%%------------------------------------------------------------------------------
%% @doc
%% KMS API:
%% [http://docs.aws.amazon.com/kms/latest/APIReference/API_ReEncrypt.html]
%%
%% ===Example===
%%
%%------------------------------------------------------------------------------
-type re_encrypt_opts() :: [re_encrypt_opt()].
-type re_encrypt_opt() :: {re_encrypt_opt_key(), term()}.
-type re_encrypt_opt_key() :: destination_encryption_context | grant_tokens | source_encryption_context.


-spec re_encrypt
          (CiphertextBlob :: binary(),
           DestinationKeyId :: binary()) ->
          return_val().
re_encrypt(CiphertextBlob, DestinationKeyId) ->
    re_encrypt(CiphertextBlob, DestinationKeyId, []).


-spec re_encrypt
          (CiphertextBlob :: binary(),
           DestinationKeyId :: binary(),
           Options :: re_encrypt_opts()) ->
          return_val().
re_encrypt(CiphertextBlob, DestinationKeyId, Options) ->
    re_encrypt(CiphertextBlob, DestinationKeyId, Options, default_config()).


-spec re_encrypt
          (CiphertextBlob :: binary(),
           DestinationKeyId :: binary(),
           Options :: re_encrypt_opts(),
           Config :: aws_config()) ->
          return_val().
re_encrypt(CiphertextBlob, DestinationKeyId, Options, Config) ->
    OptJson = dynamize_options(Options),
    Json = [{<<"CiphertextBlob">>, CiphertextBlob}, {<<"DestinationKeyId">>, DestinationKeyId}|OptJson],
    kms_request(Config, "TrentService.ReEncrypt", Json).


%%%------------------------------------------------------------------------------
%%% RetireGrant
%%%------------------------------------------------------------------------------

%%------------------------------------------------------------------------------
%% @doc
%% KMS API:
%% [http://docs.aws.amazon.com/kms/latest/APIReference/API_RetireGrant.html]
%%
%% ===Example===
%%
%%------------------------------------------------------------------------------
-type retire_grant_opts() :: [retire_grant_opt()].
-type retire_grant_opt() :: {retire_grant_opt_key(), term()}.
-type retire_grant_opt_key() :: key_id | grant_id | grant_token.

-spec retire_grant
          (Options :: retire_grant_opts()) ->
          return_val().
retire_grant(Options) ->
    retire_grant(Options, default_config()).


-spec retire_grant
          (Options :: retire_grant_opts(),
           Config :: aws_config()) ->
          return_val().
retire_grant(Options, Config) ->
    Json = dynamize_options(Options),
    kms_request(Config, "TrentService.RetireGrant", Json).


%%%------------------------------------------------------------------------------
%%% RevokeGrant
%%%------------------------------------------------------------------------------

%%------------------------------------------------------------------------------
%% @doc
%% KMS API:
%% [http://docs.aws.amazon.com/kms/latest/APIReference/API_RevokeGrant.html]
%%
%% ===Example===
%%
%%------------------------------------------------------------------------------
-spec revoke_grant
          (GrantId :: binary(),
           KeyId :: binary()) ->
          return_val().
revoke_grant(GrantId, KeyId) ->
    revoke_grant(GrantId, KeyId, default_config()).


-spec revoke_grant
          (GrantId :: binary(),
           KeyId :: binary(),
           Config :: aws_config()) ->
          return_val().
revoke_grant(GrantId, KeyId, Config) ->
    Json = [{<<"GrantId">>, GrantId}, {<<"KeyId">>, KeyId}],
    kms_request(Config, "TrentService.RevokeGrant", Json).

%%%------------------------------------------------------------------------------
%%% UpdateAlias
%%%------------------------------------------------------------------------------

%%------------------------------------------------------------------------------
%% @doc
%% KMS API:
%% [http://docs.aws.amazon.com/kms/latest/APIReference/API_UpdateAlias.html]
%%
%% ===Example===
%%
%%------------------------------------------------------------------------------
-spec update_alias
          (AliasName :: binary(),
           TargetKeyId :: binary()) -> return_val().
update_alias(AliasName, TargetKeyId) ->
    update_alias(AliasName, TargetKeyId, default_config()).


-spec update_alias
          (AliasName :: binary(),
           TargetKeyId :: binary(),
           Config :: aws_config()) -> return_val().
update_alias(AliasName, TargetKeyId, Config) ->
    Json = [{<<"AliasName">>, AliasName}, {<<"TargetKeyId">>, TargetKeyId}],
    kms_request(Config, "TrentService.UpdateAlias", Json).


%%%------------------------------------------------------------------------------
%%% UpdateKeyDescription
%%%------------------------------------------------------------------------------

%%------------------------------------------------------------------------------
%% @doc
%% KMS API:
%% [http://docs.aws.amazon.com/kms/latest/APIReference/API_UpdateKeyDescription.html]
%%
%% ===Example===
%%
%%------------------------------------------------------------------------------
-spec update_key_description
          (KeyId :: binary(),
           Description :: binary()) ->
          return_val().
update_key_description(KeyId, Description) ->
    update_key_description(KeyId, Description, default_config()).


-spec update_key_description
          (KeyId :: binary(),
           Description :: binary(),
           Config :: aws_config()) ->
          return_val().
update_key_description(KeyId, Description, Config) ->
    Json = [{<<"KeyId">>, KeyId}, {<<"Description">>, Description}],
    kms_request(Config, "TrentService.UpdateKeyDescription", Json).


-spec kms_result_fun
          (Request :: aws_request()) -> aws_request().
kms_result_fun(#aws_request{response_type = ok} = Request) ->
    Request;
kms_result_fun(#aws_request{response_type = error,
                           error_type = aws,
                           response_status = Status} = Request)
  when Status >= 500 ->
    Request#aws_request{should_retry = true};
kms_result_fun(#aws_request{response_type = error, error_type = aws} = Request) ->
    Request#aws_request{should_retry = false}.


%%%------------------------------------------------------------------------------
%%% Internal Functions
%%%------------------------------------------------------------------------------

kms_request(Config, Operation, Body) ->
    case erlcloud_aws:update_config(Config) of
        {ok, Config1} ->
            kms_request_no_update(Config1, Operation, Body);
        {error, Reason} ->
            {error, Reason}
    end.

kms_request_no_update(Config, Operation, Body) ->
    Payload = case Body of
               [] -> <<"{}">>;
               _ -> jsx:encode(Body)
           end,
    Headers = headers(Config, Operation, Payload),
    Request = #aws_request{service = kms,
                           uri = uri(Config),
                           method = post,
                           request_headers = Headers,
                           request_body = Payload},
    case erlcloud_aws:request_to_return(erlcloud_retry:request(Config, Request, fun kms_result_fun/1)) of
        {ok, {_RespHeaders, <<>>}} -> {ok, []};
        {ok, {_RespHeaders, RespBody}} -> {ok, jsx:decode(RespBody)};
        {error, _} = Error-> Error
    end.


headers(Config, Operation, Body) ->
    Headers = [{"host", Config#aws_config.kms_host},
               {"x-amz-target", Operation},
               {"content-type", "application/x-amz-json-1.1"}],
    Region = erlcloud_aws:aws_region_from_host(Config#aws_config.kms_host),
    erlcloud_aws:sign_v4_headers(Config, Headers, Body, Region, "kms").


uri(#aws_config{kms_scheme = Scheme, kms_host = Host} = Config) ->
    lists:flatten([Scheme, Host, port_spec(Config)]).


port_spec(#aws_config{kms_port=80}) ->
    "";
port_spec(#aws_config{kms_port=Port}) ->
    [":", erlang:integer_to_list(Port)].


dynamize_options(List) ->
    dynamize_options(List, []).


dynamize_options([{Key, Value}|T], Acc) ->
    DynamizedKey = dynamize_option_key(Key),
    dynamize_options(T, [{DynamizedKey, Value}|Acc]);
dynamize_options([], Acc) ->
    Acc.


dynamize_option_key(Key) when is_atom(Key) ->
    F = fun([A|B], Acc) ->
                C = list_to_binary([string:to_upper(A)|B]),
                <<Acc/binary, C/binary>>
        end,
    lists:foldl(F, <<>>, string:tokens(atom_to_list(Key), "_")).
