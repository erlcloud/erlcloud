-module(erlcloud_sts).

-include_lib("erlcloud/include/erlcloud.hrl").
-include_lib("erlcloud/include/erlcloud_aws.hrl").

-export([assume_role/4]).

-define(API_VERSION, "2011-06-15").


% See http://docs.aws.amazon.com/STS/latest/APIReference/API_AssumeRole.html
-spec assume_role/4 :: (#aws_config{}, string(), string(), 900..3600) -> {#aws_config{}, proplist()}.
assume_role(AwsConfig, RoleArn, RoleSessionName, DurationSeconds)
    when length(RoleArn) >= 20, length(RoleSessionName) >= 2, length(RoleSessionName) =< 32 ->
    Xml = sts_query(AwsConfig, "AssumeRole",
        [
            {"RoleArn", RoleArn},
            {"RoleSessionName", RoleSessionName},
            {"DurationSeconds", DurationSeconds}
        ]),

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


sts_query(AwsConfig, Action, Params) ->
    sts_query(AwsConfig, Action, Params, ?API_VERSION).


sts_query(AwsConfig, Action, Params, ApiVersion) ->
    erlcloud_aws:aws_request_xml(post,
        AwsConfig#aws_config.sts_host,
        "/",
        [{"Action", Action}, {"Version", ApiVersion} | Params],
        AwsConfig
    ).
