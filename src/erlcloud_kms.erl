-module(erlcloud_kms).
-author('rorra@rorra.com.ar').

-include("erlcloud.hrl").
-include("erlcloud_aws.hrl").

-define(API_VERSION, "2014-11-01").

%%% Library initialization.
-export([configure/2, configure/3, configure/4, new/2, new/3, new/4]).

%%% KMS API
-export([create_alias/2, create_grant/6, create_key/3, decrypt/3, delete_alias/1, describe_key/1, disable_key/1, disable_key_rotation/1,
	 enable_key/1, enable_key_rotation/1, encrypt/4, generate_data_key/5, generate_data_key_without_plaintext/5, generate_random/1,
	 get_key_policy/2, get_key_rotation_status/1, list_aliases/2, list_grants/3, list_key_policies/3, list_keys/2, put_key_policy/3,
	 re_encrypt/5, retire_grant/3, revoke_grant/2, update_alias/2, update_key_description/2]).


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
       s3_host=Host
      }.

-spec new(string(), string(), string(), non_neg_integer()) -> aws_config().

new(AccessKeyID, SecretAccessKey, Host, Port) ->
    #aws_config{
       access_key_id=AccessKeyID,
       secret_access_key=SecretAccessKey,
       s3_host=Host,
       s3_port=Port
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

-spec create_alias(string(), string()) -> any().

create_alias(AliasName, TargetKeyId) ->
    Json = [{<<"AliasName">>, AliasName}, {<<"TargetKeyId">>, TargetKeyId}],
    erlcloud_kms_impl:request(default_config(), "TrentService.CreateAlias", Json).


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

-spec create_grant(string(), string(), tuple() | 'undefined', [string()] | 'undefined', [string()] | 'undefined', string() | 'undefined') -> any().

create_grant(GranteePrincipal, KeyId, undefined, undefined, undefined, undefined) ->
    Json = [{<<"GranteePrincipal">>, GranteePrincipal}, {<<"KeyId">>, KeyId}],
    erlcloud_kms_impl:request(default_config(), "TrentService.CreateGrant", Json);
create_grant(GranteePrincipal, KeyId, Constraints, undefined, undefined, undefined) ->
    Json = [{<<"GranteePrincipal">>, GranteePrincipal}, {<<"KeyId">>, KeyId}, {<<"Constraints">>, Constraints}],
    erlcloud_kms_impl:request(default_config(), "TrentService.CreateGrant", Json);
create_grant(GranteePrincipal, KeyId, undefined, GrantTokens, undefined, undefined) ->
    Json = [{<<"GranteePrincipal">>, GranteePrincipal}, {<<"KeyId">>, KeyId}, {<<"GrantTokens">>, GrantTokens}],
    erlcloud_kms_impl:request(default_config(), "TrentService.CreateGrant", Json);
create_grant(GranteePrincipal, KeyId, undefined, undefined, Operations, undefined) ->
    Json = [{<<"GranteePrincipal">>, GranteePrincipal}, {<<"KeyId">>, KeyId}, {<<"Operations">>, Operations}],
    erlcloud_kms_impl:request(default_config(), "TrentService.CreateGrant", Json);
create_grant(GranteePrincipal, KeyId, undefined, undefined, undefined, RetiringPrincipal) ->
    Json = [{<<"GranteePrincipal">>, GranteePrincipal}, {<<"KeyId">>, KeyId}, {<<"RetiringPrincipal">>, RetiringPrincipal}],
    erlcloud_kms_impl:request(default_config(), "TrentService.CreateGrant", Json);
create_grant(GranteePrincipal, KeyId, undefined, undefined, Operations, RetiringPrincipal) ->
    Json = [{<<"GranteePrincipal">>, GranteePrincipal}, {<<"KeyId">>, KeyId}, {<<"Operations">>, Operations}, {<<"RetiringPrincipal">>, RetiringPrincipal}],
    erlcloud_kms_impl:request(default_config(), "TrentService.CreateGrant", Json);
create_grant(GranteePrincipal, KeyId, undefined, GrantTokens, undefined, RetiringPrincipal) ->
    Json = [{<<"GranteePrincipal">>, GranteePrincipal}, {<<"KeyId">>, KeyId}, {<<"GrantTokens">>, GrantTokens}, {<<"RetiringPrincipal">>, RetiringPrincipal}],
    erlcloud_kms_impl:request(default_config(), "TrentService.CreateGrant", Json);
create_grant(GranteePrincipal, KeyId, undefined, GrantTokens, Operations, undefined) ->
    Json = [{<<"GranteePrincipal">>, GranteePrincipal}, {<<"KeyId">>, KeyId}, {<<"GrantTokens">>, GrantTokens}, {<<"Operations">>, Operations}],
    erlcloud_kms_impl:request(default_config(), "TrentService.CreateGrant", Json);
create_grant(GranteePrincipal, KeyId, Constraints, undefined, undefined, RetiringPrincipal) ->
    Json = [{<<"GranteePrincipal">>, GranteePrincipal}, {<<"KeyId">>, KeyId}, {<<"Constraints">>, Constraints}, {<<"RetiringPrincipal">>, RetiringPrincipal}],
    erlcloud_kms_impl:request(default_config(), "TrentService.CreateGrant", Json);
create_grant(GranteePrincipal, KeyId, Constraints, undefined, Operations, undefined) ->
    Json = [{<<"GranteePrincipal">>, GranteePrincipal}, {<<"KeyId">>, KeyId}, {<<"Constraints">>, Constraints}, {<<"Operations">>, Operations}],
    erlcloud_kms_impl:request(default_config(), "TrentService.CreateGrant", Json);
create_grant(GranteePrincipal, KeyId, Constraints, GrantTokens, undefined, undefined) ->
    Json = [{<<"GranteePrincipal">>, GranteePrincipal}, {<<"KeyId">>, KeyId}, {<<"Constraints">>, Constraints}, {<<"GrantTokens">>, GrantTokens}],
    erlcloud_kms_impl:request(default_config(), "TrentService.CreateGrant", Json);
create_grant(GranteePrincipal, KeyId, undefined, GrantTokens, Operations, RetiringPrincipal) ->
    Json = [{<<"GranteePrincipal">>, GranteePrincipal}, {<<"KeyId">>, KeyId}, {<<"GrantTokens">>, GrantTokens}, {<<"Operations">>, Operations}, {<<"RetiringPrincipal">>, RetiringPrincipal}],
    erlcloud_kms_impl:request(default_config(), "TrentService.CreateGrant", Json);
create_grant(GranteePrincipal, KeyId, Constraints, undefined, Operations, RetiringPrincipal) ->
    Json = [{<<"GranteePrincipal">>, GranteePrincipal}, {<<"KeyId">>, KeyId}, {<<"Constraints">>, Constraints}, {<<"Operations">>, Operations}, {<<"RetiringPrincipal">>, RetiringPrincipal}],
    erlcloud_kms_impl:request(default_config(), "TrentService.CreateGrant", Json);
create_grant(GranteePrincipal, KeyId, Constraints, GrantTokens, undefined, RetiringPrincipal) ->
    Json = [{<<"GranteePrincipal">>, GranteePrincipal}, {<<"KeyId">>, KeyId}, {<<"Constraints">>, Constraints}, {<<"GrantTokens">>, GrantTokens}, {<<"RetiringPrincipal">>, RetiringPrincipal}],
    erlcloud_kms_impl:request(default_config(), "TrentService.CreateGrant", Json);
create_grant(GranteePrincipal, KeyId, Constraints, GrantTokens, Operations, undefined) ->
    Json = [{<<"GranteePrincipal">>, GranteePrincipal}, {<<"KeyId">>, KeyId}, {<<"Constraints">>, Constraints}, {<<"GrantTokens">>, GrantTokens}, {<<"Operations">>, Operations}],
    erlcloud_kms_impl:request(default_config(), "TrentService.CreateGrant", Json);
create_grant(GranteePrincipal, KeyId, Constraints, GrantTokens, Operations, RetiringPrincipal) ->
    Json = [{<<"GranteePrincipal">>, GranteePrincipal}, {<<"KeyId">>, KeyId}, {<<"Constraints">>, Constraints}, {<<"GrantTokens">>, GrantTokens}, {<<"Operations">>, Operations},
	    {<<"RetiringPrincipal">>, RetiringPrincipal}],
    erlcloud_kms_impl:request(default_config(), "TrentService.CreateGrant", Json).

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

-spec create_key(string() | 'undefined', string() | 'undefined', string() | 'undefined') -> any().

create_key(undefined, undefined, undefined) ->
    Json = [],
    erlcloud_kms_impl:request(default_config(), "TrentService.CreateKey", Json);
create_key(Description, undefined, undefined) ->
    Json = [{<<"Description">>, Description}],
    erlcloud_kms_impl:request(default_config(), "TrentService.CreateKey", Json);
create_key(undefined, KeyUsage, undefined) ->
    Json = [{<<"KeyUsage">>, KeyUsage}],
    erlcloud_kms_impl:request(default_config(), "TrentService.CreateKey", Json);
create_key(undefined, undefined, Policy) ->
    Json = [{<<"Policy">>, Policy}],
    erlcloud_kms_impl:request(default_config(), "TrentService.CreateKey", Json);
create_key(Description, KeyUsage, undefined) ->
    Json = [{<<"Description">>, Description}, {<<"KeyUsage">>, KeyUsage}],
    erlcloud_kms_impl:request(default_config(), "TrentService.CreateKey", Json);
create_key(Description, undefined, Policy) ->
    Json = [{<<"Description">>, Description}, {<<"Policy">>, Policy}],
    erlcloud_kms_impl:request(default_config(), "TrentService.CreateKey", Json);
create_key(undefined, KeyUsage, Policy) ->
    Json = [{<<"KeyUsage">>, KeyUsage}, {<<"Policy">>, Policy}],
    erlcloud_kms_impl:request(default_config(), "TrentService.CreateKey", Json);
create_key(Description, KeyUsage, Policy) ->
    Json = [{<<"Description">>, Description}, {<<"KeyUsage">>, KeyUsage}, {<<"Policy">>, Policy}],
    erlcloud_kms_impl:request(default_config(), "TrentService.CreateKey", Json).

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

-spec decrypt(string(), tuple() | 'undefined', [string()] | 'undefined') -> any().

decrypt(CiphertextBlob, undefined, undefined) ->
    Json = [{<<"CiphertextBlob">>, CiphertextBlob}],
    erlcloud_kms_impl:request(default_config(), "TrentService.Decrypt", Json);
decrypt(CiphertextBlob, undefined, GrantTokens) ->
    Json = [{<<"CiphertextBlob">>, CiphertextBlob}, {<<"GrantTokens">>, GrantTokens}],
    erlcloud_kms_impl:request(default_config(), "TrentService.Decrypt", Json);
decrypt(CiphertextBlob, EncryptionContext, undefined) ->
    Json = [{<<"CiphertextBlob">>, CiphertextBlob}, {<<"EncryptionContext">>, EncryptionContext}],
    erlcloud_kms_impl:request(default_config(), "TrentService.Decrypt", Json);
decrypt(CiphertextBlob, EncryptionContext, GrantTokens) ->
    Json = [{<<"CiphertextBlob">>, CiphertextBlob}, {<<"EncryptionContext">>, EncryptionContext}, {<<"GrantTokens">>, GrantTokens}],
    erlcloud_kms_impl:request(default_config(), "TrentService.Decrypt", Json).

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

-spec delete_alias(string()) -> any().

delete_alias(AliasName) ->
    Json = [{<<"AliasName">>, AliasName}],
    erlcloud_kms_impl:request(default_config(), "TrentService.DeleteAlias", Json).

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

-spec describe_key(string()) -> any().

describe_key(KeyId) ->
    Json = [{<<"KeyId">>, KeyId}],
    erlcloud_kms_impl:request(default_config(), "TrentService.DescribeKey", Json).

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

-spec disable_key(string()) -> any().

disable_key(KeyId) ->
    Json = [{<<"KeyId">>, KeyId}],
    erlcloud_kms_impl:request(default_config(), "TrentService.DisableKey", Json).

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

-spec disable_key_rotation(string()) -> any().

disable_key_rotation(KeyId) ->
    Json = [{<<"KeyId">>, KeyId}],
    erlcloud_kms_impl:request(default_config(), "TrentService.DisableKey", Json).

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

-spec enable_key(string()) -> any().

enable_key(KeyId) ->
    Json = [{<<"KeyId">>, KeyId}],
    erlcloud_kms_impl:request(default_config(), "TrentService.EnableKey", Json).

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

-spec enable_key_rotation(string()) -> any().

enable_key_rotation(KeyId) ->
    Json = [{<<"KeyId">>, KeyId}],
    erlcloud_kms_impl:request(default_config(), "TrentService.EnableKeyRotation", Json).

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

-spec encrypt(string(), string(), tuple() | 'undefined', [string()] | 'undefined') -> any().

encrypt(KeyId, PlainText, undefined, undefined) ->
    Json = [{<<"KeyId">>, KeyId}, {<<"PlainText">>, PlainText}],
    erlcloud_kms_impl:request(default_config(), "TrentService.Encrypt", Json);
encrypt(KeyId, PlainText, undefined, GrantTokens) ->
    Json = [{<<"KeyId">>, KeyId}, {<<"PlainText">>, PlainText}, {<<"GrantTokens">>, GrantTokens}],
    erlcloud_kms_impl:request(default_config(), "TrentService.Encrypt", Json);
encrypt(KeyId, PlainText, EncryptionContent, undefined) ->
    Json = [{<<"KeyId">>, KeyId}, {<<"PlainText">>, PlainText}, {<<"EncryptionContent">>, EncryptionContent}],
    erlcloud_kms_impl:request(default_config(), "TrentService.Encrypt", Json);
encrypt(KeyId, PlainText, EncryptionContent, GrantTokens) ->
    Json = [{<<"KeyId">>, KeyId}, {<<"PlainText">>, PlainText}, {<<"EncryptionContent">>, EncryptionContent}, {<<"GrantTokens">>, GrantTokens}],
    erlcloud_kms_impl:request(default_config(), "TrentService.Encrypt", Json).

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

-spec generate_data_key(string(), string() | 'undefined', non_neg_integer() | 'undefined', tuple() | 'undefined', [string()] | 'undefined') -> any().

generate_data_key(KeyId, undefined, undefined, undefined, undefined) ->
    Json = [{<<"KeyId">>, KeyId}],
    erlcloud_kms_impl:request(default_config(), "TrentService.GenerateDataKey", Json);
generate_data_key(KeyId, KeySpec, undefined, undefined, undefined) ->
    Json = [{<<"KeyId">>, KeyId}, {<<"KeySpec">>, KeySpec}],
    erlcloud_kms_impl:request(default_config(), "TrentService.GenerateDataKey", Json);
generate_data_key(KeyId, undefined, NumberOfBytes, undefined, undefined) ->
    Json = [{<<"KeyId">>, KeyId}, {<<"NumberOfBytes">>, NumberOfBytes}],
    erlcloud_kms_impl:request(default_config(), "TrentService.GenerateDataKey", Json);
generate_data_key(KeyId, undefined, undefined, EncryptionContext, undefined) ->
    Json = [{<<"KeyId">>, KeyId}, {<<"EncryptionContext">>, EncryptionContext}],
    erlcloud_kms_impl:request(default_config(), "TrentService.GenerateDataKey", Json);
generate_data_key(KeyId, undefined, undefined, undefined, GrantTokens) ->
    Json = [{<<"KeyId">>, KeyId}, {<<"GrantTokens">>, GrantTokens}],
    erlcloud_kms_impl:request(default_config(), "TrentService.GenerateDataKey", Json);
generate_data_key(KeyId, undefined, undefined, EncryptionContext, GrantTokens) ->
    Json = [{<<"KeyId">>, KeyId}, {<<"EncryptionContext">>, EncryptionContext}, {<<"GrantTokens">>, GrantTokens}],
    erlcloud_kms_impl:request(default_config(), "TrentService.GenerateDataKey", Json);
generate_data_key(KeyId, undefined, NumberOfBytes, undefined, GrantTokens) ->
    Json = [{<<"KeyId">>, KeyId}, {<<"NumberOfBytes">>, NumberOfBytes}, {<<"GrantTokens">>, GrantTokens}],
    erlcloud_kms_impl:request(default_config(), "TrentService.GenerateDataKey", Json);
generate_data_key(KeyId, undefined, NumberOfBytes, EncryptionContext, undefined) ->
    Json = [{<<"KeyId">>, KeyId}, {<<"NumberOfBytes">>, NumberOfBytes}, {<<"EncryptionContext">>, EncryptionContext}],
    erlcloud_kms_impl:request(default_config(), "TrentService.GenerateDataKey", Json);
generate_data_key(KeyId, KeySpec, undefined, undefined, GrantTokens) ->
    Json = [{<<"KeyId">>, KeyId}, {<<"KeySpec">>, KeySpec}, {<<"GrantTokens">>, GrantTokens}],
    erlcloud_kms_impl:request(default_config(), "TrentService.GenerateDataKey", Json);
generate_data_key(KeyId, KeySpec, undefined, EncryptionContext, undefined) ->
    Json = [{<<"KeyId">>, KeyId}, {<<"KeySpec">>, KeySpec}, {<<"EncryptionContext">>, EncryptionContext}],
    erlcloud_kms_impl:request(default_config(), "TrentService.GenerateDataKey", Json);
generate_data_key(KeyId, KeySpec, NumberOfBytes, undefined, undefined) ->
    Json = [{<<"KeyId">>, KeyId}, {<<"KeySpec">>, KeySpec}, {<<"NumberOfBytes">>, NumberOfBytes}],
    erlcloud_kms_impl:request(default_config(), "TrentService.GenerateDataKey", Json);
generate_data_key(KeyId, undefined, NumberOfBytes, EncryptionContext, GrantTokens) ->
    Json = [{<<"KeyId">>, KeyId}, {<<"NumberOfBytes">>, NumberOfBytes}, {<<"EncryptionContext">>, EncryptionContext}, {<<"GrantTokens">>, GrantTokens}],
    erlcloud_kms_impl:request(default_config(), "TrentService.GenerateDataKey", Json);
generate_data_key(KeyId, KeySpec, undefined, EncryptionContext, GrantTokens) ->
    Json = [{<<"KeyId">>, KeyId}, {<<"KeySpec">>, KeySpec}, {<<"EncryptionContext">>, EncryptionContext}, {<<"GrantTokens">>, GrantTokens}],
    erlcloud_kms_impl:request(default_config(), "TrentService.GenerateDataKey", Json);
generate_data_key(KeyId, KeySpec, NumberOfBytes, undefined, GrantTokens) ->
    Json = [{<<"KeyId">>, KeyId}, {<<"KeySpec">>, KeySpec}, {<<"NumberOfBytes">>, NumberOfBytes}, {<<"GrantTokens">>, GrantTokens}],
    erlcloud_kms_impl:request(default_config(), "TrentService.GenerateDataKey", Json);
generate_data_key(KeyId, KeySpec, NumberOfBytes, EncryptionContext, undefined) ->
    Json = [{<<"KeyId">>, KeyId}, {<<"KeySpec">>, KeySpec}, {<<"NumberOfBytes">>, NumberOfBytes}, {<<"EncryptionContext">>, EncryptionContext}],
    erlcloud_kms_impl:request(default_config(), "TrentService.GenerateDataKey", Json);
generate_data_key(KeyId, KeySpec, NumberOfBytes, EncryptionContext, NumberOfBytes) ->
    Json = [{<<"KeyId">>, KeyId}, {<<"KeySpec">>, KeySpec}, {<<"NumberOfBytes">>, NumberOfBytes}, {<<"EncryptionContext">>, EncryptionContext},
	    {<<"NumberOfBytes">>, NumberOfBytes}],
    erlcloud_kms_impl:request(default_config(), "TrentService.GenerateDataKey", Json).

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

-spec generate_data_key_without_plaintext(string(), string() | 'undefined', non_neg_integer() | 'undefined', tuple() | 'undefined', [string()] | 'undefined') -> any().

generate_data_key_without_plaintext(KeyId, undefined, undefined, undefined, undefined) ->
    Json = [{<<"KeyId">>, KeyId}],
    erlcloud_kms_impl:request(default_config(), "TrentService.GenerateDataKeyWithoutPlaintext", Json);
generate_data_key_without_plaintext(KeyId, KeySpec, undefined, undefined, undefined) ->
    Json = [{<<"KeyId">>, KeyId}, {<<"KeySpec">>, KeySpec}],
    erlcloud_kms_impl:request(default_config(), "TrentService.GenerateDataKeyWithoutPlaintext", Json);
generate_data_key_without_plaintext(KeyId, undefined, NumberOfBytes, undefined, undefined) ->
    Json = [{<<"KeyId">>, KeyId}, {<<"NumberOfBytes">>, NumberOfBytes}],
    erlcloud_kms_impl:request(default_config(), "TrentService.GenerateDataKeyWithoutPlaintext", Json);
generate_data_key_without_plaintext(KeyId, undefined, undefined, EncryptionContext, undefined) ->
    Json = [{<<"KeyId">>, KeyId}, {<<"EncryptionContext">>, EncryptionContext}],
    erlcloud_kms_impl:request(default_config(), "TrentService.GenerateDataKeyWithoutPlaintext", Json);
generate_data_key_without_plaintext(KeyId, undefined, undefined, undefined, GrantTokens) ->
    Json = [{<<"KeyId">>, KeyId}, {<<"GrantTokens">>, GrantTokens}],
    erlcloud_kms_impl:request(default_config(), "TrentService.GenerateDataKeyWithoutPlaintext", Json);
generate_data_key_without_plaintext(KeyId, undefined, undefined, EncryptionContext, GrantTokens) ->
    Json = [{<<"KeyId">>, KeyId}, {<<"EncryptionContext">>, EncryptionContext}, {<<"GrantTokens">>, GrantTokens}],
    erlcloud_kms_impl:request(default_config(), "TrentService.GenerateDataKeyWithoutPlaintext", Json);
generate_data_key_without_plaintext(KeyId, undefined, NumberOfBytes, undefined, GrantTokens) ->
    Json = [{<<"KeyId">>, KeyId}, {<<"NumberOfBytes">>, NumberOfBytes}, {<<"GrantTokens">>, GrantTokens}],
    erlcloud_kms_impl:request(default_config(), "TrentService.GenerateDataKeyWithoutPlaintext", Json);
generate_data_key_without_plaintext(KeyId, undefined, NumberOfBytes, EncryptionContext, undefined) ->
    Json = [{<<"KeyId">>, KeyId}, {<<"NumberOfBytes">>, NumberOfBytes}, {<<"EncryptionContext">>, EncryptionContext}],
    erlcloud_kms_impl:request(default_config(), "TrentService.GenerateDataKeyWithoutPlaintext", Json);
generate_data_key_without_plaintext(KeyId, KeySpec, undefined, undefined, GrantTokens) ->
    Json = [{<<"KeyId">>, KeyId}, {<<"KeySpec">>, KeySpec}, {<<"GrantTokens">>, GrantTokens}],
    erlcloud_kms_impl:request(default_config(), "TrentService.GenerateDataKeyWithoutPlaintext", Json);
generate_data_key_without_plaintext(KeyId, KeySpec, undefined, EncryptionContext, undefined) ->
    Json = [{<<"KeyId">>, KeyId}, {<<"KeySpec">>, KeySpec}, {<<"EncryptionContext">>, EncryptionContext}],
    erlcloud_kms_impl:request(default_config(), "TrentService.GenerateDataKeyWithoutPlaintext", Json);
generate_data_key_without_plaintext(KeyId, KeySpec, NumberOfBytes, undefined, undefined) ->
    Json = [{<<"KeyId">>, KeyId}, {<<"KeySpec">>, KeySpec}, {<<"NumberOfBytes">>, NumberOfBytes}],
    erlcloud_kms_impl:request(default_config(), "TrentService.GenerateDataKeyWithoutPlaintext", Json);
generate_data_key_without_plaintext(KeyId, undefined, NumberOfBytes, EncryptionContext, GrantTokens) ->
    Json = [{<<"KeyId">>, KeyId}, {<<"NumberOfBytes">>, NumberOfBytes}, {<<"EncryptionContext">>, EncryptionContext}, {<<"GrantTokens">>, GrantTokens}],
    erlcloud_kms_impl:request(default_config(), "TrentService.GenerateDataKeyWithoutPlaintext", Json);
generate_data_key_without_plaintext(KeyId, KeySpec, undefined, EncryptionContext, GrantTokens) ->
    Json = [{<<"KeyId">>, KeyId}, {<<"KeySpec">>, KeySpec}, {<<"EncryptionContext">>, EncryptionContext}, {<<"GrantTokens">>, GrantTokens}],
    erlcloud_kms_impl:request(default_config(), "TrentService.GenerateDataKeyWithoutPlaintext", Json);
generate_data_key_without_plaintext(KeyId, KeySpec, NumberOfBytes, undefined, GrantTokens) ->
    Json = [{<<"KeyId">>, KeyId}, {<<"KeySpec">>, KeySpec}, {<<"NumberOfBytes">>, NumberOfBytes}, {<<"GrantTokens">>, GrantTokens}],
    erlcloud_kms_impl:request(default_config(), "TrentService.GenerateDataKeyWithoutPlaintext", Json);
generate_data_key_without_plaintext(KeyId, KeySpec, NumberOfBytes, EncryptionContext, undefined) ->
    Json = [{<<"KeyId">>, KeyId}, {<<"KeySpec">>, KeySpec}, {<<"NumberOfBytes">>, NumberOfBytes}, {<<"EncryptionContext">>, EncryptionContext}],
    erlcloud_kms_impl:request(default_config(), "TrentService.GenerateDataKeyWithoutPlaintext", Json);
generate_data_key_without_plaintext(KeyId, KeySpec, NumberOfBytes, EncryptionContext, NumberOfBytes) ->
    Json = [{<<"KeyId">>, KeyId}, {<<"KeySpec">>, KeySpec}, {<<"NumberOfBytes">>, NumberOfBytes}, {<<"EncryptionContext">>, EncryptionContext},
	    {<<"NumberOfBytes">>, NumberOfBytes}],
    erlcloud_kms_impl:request(default_config(), "TrentService.GenerateDataKeyWithoutPlaintext", Json).

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

-spec generate_random(non_neg_integer()) -> any().

generate_random(NumberOfBytes) ->
    Json = [{<<"NumberOfBytes">>, NumberOfBytes}],
    erlcloud_kms_impl:request(default_config(), "TrentService.GenerateRandom", Json).

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

-spec get_key_policy(string(), string()) -> any().

get_key_policy(KeyId, PolicyName) ->
    Json = [{<<"KeyId">>, KeyId}, {<<"PolicyName">>, PolicyName}],
    erlcloud_kms_impl:request(default_config(), "TrentService.GetKeyPolicy", Json).

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

-spec get_key_rotation_status(string()) -> any().

get_key_rotation_status(KeyId) ->
    Json = [{<<"KeyId">>, KeyId}],
    erlcloud_kms_impl:request(default_config(), "TrentService.GetKeyRotationStatus", Json).

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

-spec list_aliases(non_neg_integer() | 'undefined', string() | 'undefined') -> any().

list_aliases(undefined, undefined) ->
    Json = [],
    erlcloud_kms_impl:request(default_config(), "TrentService.ListAliases", Json);
list_aliases(undefined, Marker) ->
    Json = [{<<"Marker">>, Marker}],
    erlcloud_kms_impl:request(default_config(), "TrentService.ListAliases", Json);
list_aliases(Limit, undefined) ->
    Json = [{<<"Limit">>, Limit}],
    erlcloud_kms_impl:request(default_config(), "TrentService.ListAliases", Json);
list_aliases(Limit, Marker) ->
    Json = [{<<"Limit">>, Limit}, {<<"Marker">>, Marker}],
    erlcloud_kms_impl:request(default_config(), "TrentService.ListAliases", Json).

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

-spec list_grants(string(), non_neg_integer() | 'undefined', string() | 'undefined') -> any().

list_grants(KeyId, undefined, undefined) ->
    Json = [{<<"KeyId">>, KeyId}],
    erlcloud_kms_impl:request(default_config(), "TrentService.ListGrants", Json);
list_grants(KeyId, undefined, Marker) ->
    Json = [{<<"KeyId">>, KeyId}, {<<"Marker">>, Marker}],
    erlcloud_kms_impl:request(default_config(), "TrentService.ListGrants", Json);
list_grants(KeyId, Limit, undefined) ->
    Json = [{<<"KeyId">>, KeyId}, {<<"Limit">>, Limit}],
    erlcloud_kms_impl:request(default_config(), "TrentService.ListGrants", Json);
list_grants(KeyId, Limit, Marker) ->
    Json = [{<<"KeyId">>, KeyId}, {<<"Limit">>, Limit}, {<<"Marker">>, Marker}],
    erlcloud_kms_impl:request(default_config(), "TrentService.ListGrants", Json).

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

-spec list_key_policies(string(), non_neg_integer() | 'undefined', string() | 'undefined') -> any().

list_key_policies(KeyId, undefined, undefined) ->
    Json = [{<<"KeyId">>, KeyId}],
    erlcloud_kms_impl:request(default_config(), "TrentService.ListKeyPolicies", Json);
list_key_policies(KeyId, undefined, Marker) ->
    Json = [{<<"KeyId">>, KeyId}, {<<"Marker">>, Marker}],
    erlcloud_kms_impl:request(default_config(), "TrentService.ListKeyPolicies", Json);
list_key_policies(KeyId, Limit, undefined) ->
    Json = [{<<"KeyId">>, KeyId}, {<<"Limit">>, Limit}],
    erlcloud_kms_impl:request(default_config(), "TrentService.ListKeyPolicies", Json);
list_key_policies(KeyId, Limit, Marker) ->
    Json = [{<<"KeyId">>, KeyId}, {<<"Limit">>, Limit}, {<<"Marker">>, Marker}],
    erlcloud_kms_impl:request(default_config(), "TrentService.ListKeyPolicies", Json).

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

-spec list_keys(non_neg_integer() | 'undefined', string() | 'undefined') -> any().

list_keys(undefined, undefined) ->
    Json = [],
    erlcloud_kms_impl:request(default_config(), "TrentService.ListKeys", Json);
list_keys(undefined, Marker) ->
    Json = [{<<"Marker">>, Marker}],
    erlcloud_kms_impl:request(default_config(), "TrentService.ListKeys", Json);
list_keys(Limit, undefined) ->
    Json = [{<<"Limit">>, Limit}],
    erlcloud_kms_impl:request(default_config(), "TrentService.ListKeys", Json);
list_keys(Limit, Marker) ->
    Json = [{<<"Limit">>, Limit}, {<<"Marker">>, Marker}],
    erlcloud_kms_impl:request(default_config(), "TrentService.ListKeys", Json).

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

-spec put_key_policy(string(), string(), string()) -> any().

put_key_policy(KeyId, Policy, PolicyName) ->
    Json = [{<<"KeyId">>, KeyId}, {<<"Policy">>, Policy}, {<<"PolicyName">>, PolicyName}],
    erlcloud_kms_impl:request(default_config(), "TrentService.PutKeyPolicy", Json).

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

-spec re_encrypt(binary(), string(), [string()] | 'undefined', tuple() | 'undefined', tuple() | 'undefined') -> any().

re_encrypt(CiphertextBlob, DestinationKeyId, undefined, undefined, undefined) ->
    Json = [{<<"CiphertextBlob">>, CiphertextBlob}, {<<"DestinationKeyId">>, DestinationKeyId}],
    erlcloud_kms_impl:request(default_config(), "TrentService.ReEncrypt", Json);
re_encrypt(CiphertextBlob, DestinationKeyId, GrantTokens, undefined, undefined) ->
    Json = [{<<"CiphertextBlob">>, CiphertextBlob}, {<<"DestinationKeyId">>, DestinationKeyId}, {<<"GrantTokens">>, GrantTokens}],
    erlcloud_kms_impl:request(default_config(), "TrentService.ReEncrypt", Json);
re_encrypt(CiphertextBlob, DestinationKeyId, undefined, SourceEncryptionContext, undefined) ->
    Json = [{<<"CiphertextBlob">>, CiphertextBlob}, {<<"DestinationKeyId">>, DestinationKeyId}, {<<"SourceEncryptionContext">>, SourceEncryptionContext}],
    erlcloud_kms_impl:request(default_config(), "TrentService.ReEncrypt", Json);
re_encrypt(CiphertextBlob, DestinationKeyId, undefined, undefined, DestinationEncryptionContext) ->
    Json = [{<<"CiphertextBlob">>, CiphertextBlob}, {<<"DestinationKeyId">>, DestinationKeyId}, {<<"DestinationEncryptionContext">>, DestinationEncryptionContext}],
    erlcloud_kms_impl:request(default_config(), "TrentService.ReEncrypt", Json);
re_encrypt(CiphertextBlob, DestinationKeyId, undefined, SourceEncryptionContext, DestinationEncryptionContext) ->
    Json = [{<<"CiphertextBlob">>, CiphertextBlob}, {<<"DestinationKeyId">>, DestinationKeyId}, {<<"SourceEncryptionContext">>, SourceEncryptionContext}, {<<"DestinationEncryptionContext">>, DestinationEncryptionContext}],
    erlcloud_kms_impl:request(default_config(), "TrentService.ReEncrypt", Json);
re_encrypt(CiphertextBlob, DestinationKeyId, GrantTokens, undefined, DestinationEncryptionContext) ->
    Json = [{<<"CiphertextBlob">>, CiphertextBlob}, {<<"DestinationKeyId">>, DestinationKeyId}, {<<"GrantTokens">>, GrantTokens}, {<<"DestinationEncryptionContext">>, DestinationEncryptionContext}],
    erlcloud_kms_impl:request(default_config(), "TrentService.ReEncrypt", Json);
re_encrypt(CiphertextBlob, DestinationKeyId, GrantTokens, SourceEncryptionContext, undefined) ->
    Json = [{<<"CiphertextBlob">>, CiphertextBlob}, {<<"DestinationKeyId">>, DestinationKeyId}, {<<"GrantTokens">>, GrantTokens}, {<<"SourceEncryptionContext">>, SourceEncryptionContext}],
    erlcloud_kms_impl:request(default_config(), "TrentService.ReEncrypt", Json);
re_encrypt(CiphertextBlob, DestinationKeyId, GrantTokens, SourceEncryptionContext, DestinationEncryptionContext) ->
    Json = [{<<"CiphertextBlob">>, CiphertextBlob}, {<<"DestinationKeyId">>, DestinationKeyId}, {<<"GrantTokens">>, GrantTokens}, {<<"SourceEncryptionContext">>, SourceEncryptionContext}, {<<"DestinationEncryptionContext">>, DestinationEncryptionContext}],
    erlcloud_kms_impl:request(default_config(), "TrentService.ReEncrypt", Json).

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

-spec retire_grant(string() | 'undefined', string() | 'undefined', string() | 'undefined') -> any().

retire_grant(undefined, undefined, undefined) ->
    Json = [],
    erlcloud_kms_impl:request(default_config(), "TrentService.RetireGrant", Json);
retire_grant(GrantId, undefined, undefined) ->
    Json = [{<<"GrantId">>, GrantId}],
    erlcloud_kms_impl:request(default_config(), "TrentService.RetireGrant", Json);
retire_grant(undefined, GrantToken, undefined) ->
    Json = [{<<"GrantToken">>, GrantToken}],
    erlcloud_kms_impl:request(default_config(), "TrentService.RetireGrants", Json);
retire_grant(undefined, undefined, KeyId) ->
    Json = [{<<"KeyId">>, KeyId}],
    erlcloud_kms_impl:request(default_config(), "TrentService.RetireGrants", Json);
retire_grant(GrantId, GrantToken, undefined) ->
    Json = [{<<"GrantId">>, GrantId}, {<<"GrantToken">>, GrantToken}],
    erlcloud_kms_impl:request(default_config(), "TrentService.RetireGrants", Json);
retire_grant(GrantId, undefined, KeyId) ->
    Json = [{<<"GrantId">>, GrantId}, {<<"KeyId">>, KeyId}],
    erlcloud_kms_impl:request(default_config(), "TrentService.RetireGrant", Json);
retire_grant(undefined, GrantToken, KeyId) ->
    Json = [{<<"GrantToken">>, GrantToken}, {<<"KeyId">>, KeyId}],
    erlcloud_kms_impl:request(default_config(), "TrentService.RetireGrants", Json);
retire_grant(GrantId, GrantToken, KeyId) ->
    Json = [{<<"GrantId">>, GrantId}, {<<"GrantToken">>, GrantToken}, {<<"KeyId">>, KeyId}],
    erlcloud_kms_impl:request(default_config(), "TrentService.RetireGrants", Json).

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

-spec revoke_grant(string(), string()) -> any().

revoke_grant(GrantId, KeyId) ->
    Json = [{<<"GrantId">>, GrantId}, {<<"KeyId">>, KeyId}],
    erlcloud_kms_impl:request(default_config(), "TrentService.RevokeGrant", Json).

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

-spec update_alias(string(), string()) -> any().

update_alias(AliasName, TargetKeyId) ->
    Json = [{<<"AliasName">>, AliasName}, {<<"TargetKeyId">>, TargetKeyId}],
    erlcloud_kms_impl:request(default_config(), "TrentService.UpdateAlias", Json).

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

-spec update_key_description(string(), string()) -> any().

update_key_description(Description, KeyId) ->
    Json = [{<<"Description">>, Description}, {<<"KeyId">>, KeyId}],
    erlcloud_kms_impl:request(default_config(), "TrentService.UpdateKeyDescription", Json).
