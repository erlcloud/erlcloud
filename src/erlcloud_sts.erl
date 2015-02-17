-module(erlcloud_sts).

-include_lib("erlcloud/include/erlcloud.hrl").
-include_lib("erlcloud/include/erlcloud_aws.hrl").

-export([assume_role/4, assume_role/5,
		get_federation_token/3,
		get_federation_token/4]).

-define(API_VERSION, "2011-06-15").


assume_role(AwsConfig, RoleArn, RoleSessionName, DurationSeconds) ->
    assume_role(AwsConfig, RoleArn, RoleSessionName, DurationSeconds, undefined).


% See http://docs.aws.amazon.com/STS/latest/APIReference/API_AssumeRole.html
-spec assume_role/5 :: (#aws_config{}, string(), string(), 900..3600, undefined | string()) -> {#aws_config{}, proplist()}.
assume_role(AwsConfig, RoleArn, RoleSessionName, DurationSeconds, ExternalId)
    when length(RoleArn) >= 20,
         length(RoleSessionName) >= 2, length(RoleSessionName) =< 32,
         DurationSeconds >= 900, DurationSeconds =< 3600 ->

    Params =
        [
            {"RoleArn", RoleArn},
            {"RoleSessionName", RoleSessionName},
            {"DurationSeconds", DurationSeconds}
        ],
    ExternalIdPart =
        case ExternalId of
            undefined -> [];
            _ when length(ExternalId) >= 2, length(ExternalId) =< 96 -> [{"ExternalId", ExternalId}]
        end,

    Xml = sts_query(AwsConfig, "AssumeRole", Params ++ ExternalIdPart),

    Creds = erlcloud_xml:decode(
        [
            {access_key_id    , "AssumeRoleResult/Credentials/AccessKeyId"    , text},
            {secret_access_key, "AssumeRoleResult/Credentials/SecretAccessKey", text},
            {session_token    , "AssumeRoleResult/Credentials/SessionToken"   , text},
            {expiration       , "AssumeRoleResult/Credentials/Expiration"     , time}
        ],
        Xml),

    AssumedConfig =
        AwsConfig#aws_config {
            access_key_id     = proplists:get_value(access_key_id, Creds),
            secret_access_key = proplists:get_value(secret_access_key, Creds),
            security_token    = proplists:get_value(session_token, Creds)
        },

    {AssumedConfig, Creds}.


get_federation_token(AwsConfig, DurationSeconds, Name) ->
	get_federation_token(AwsConfig, DurationSeconds, Name, undefined).

% See http://docs.aws.amazon.com/STS/latest/APIReference/API_GetFederationToken.html
-spec get_federation_token/4 :: (#aws_config{}, 900..129600, string(), undefined | string()) -> {#aws_config{}, proplist()}.
get_federation_token(AwsConfig, DurationSeconds, Name, Policy)
  when length(Name) >= 2, length(Name) =< 32,
  	   DurationSeconds >= 900, DurationSeconds =< 129600 ->

  	Params =
  	[
	 {"DurationSeconds", DurationSeconds},
	 {"Name", Name}
	],

	PolicyList =
	case Policy of
		undefined -> [];
		_ -> [{"Policy", Policy}]
	end,
	Xml = sts_query(AwsConfig, "GetFederationToken", Params ++ PolicyList),

	Creds = erlcloud_xml:decode(
			  [
			   {access_key_id		, "GetFederationTokenResult/Credentials/AccessKeyId", text},
			   {secret_access_key	, "GetFederationTokenResult/Credentials/SecretAccessKey", text},
			   {federated_user_arn  , "GetFederationTokenResult/FederatedUser/Arn", text},
			   {federated_user_id   , "GetFederationTokenResult/FederatedUser/FederatedUserId", text}
			  ],
			  Xml),

	FederatedConfig =
	AwsConfig#aws_config {
            access_key_id     = proplists:get_value(access_key_id, Creds),
            secret_access_key = proplists:get_value(secret_access_key, Creds),
            security_token    = proplists:get_value(session_token, Creds)
	 },
	
	{FederatedConfig, Creds}.

sts_query(AwsConfig, Action, Params) ->
    sts_query(AwsConfig, Action, Params, ?API_VERSION).


sts_query(AwsConfig, Action, Params, ApiVersion) ->
    erlcloud_aws:aws_request_xml(post,
        AwsConfig#aws_config.sts_host,
        "/",
        [{"Action", Action}, {"Version", ApiVersion} | Params],
        AwsConfig
    ).
