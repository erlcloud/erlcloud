-module(erlcloud_ec2_meta).

-include("erlcloud.hrl").
-include("erlcloud_aws.hrl").

-export([generate_session_token/1, generate_session_token/2,
        get_instance_metadata/0, get_instance_metadata/1, get_instance_metadata/2, get_instance_metadata/3,
        get_instance_user_data/0, get_instance_user_data/1, get_instance_user_data/2,
        get_instance_dynamic_data/0, get_instance_dynamic_data/1, get_instance_dynamic_data/2, get_instance_dynamic_data/3]).


-spec generate_session_token(DurationSecs :: non_neg_integer()) -> {ok, binary()} | {error, erlcloud_aws:httpc_result_error()}.
generate_session_token(DurationSecs) ->
   generate_session_token(DurationSecs, erlcloud_aws:default_config()).

%%%---------------------------------------------------------------------------
-spec generate_session_token(DurationSecs :: non_neg_integer(), Config:: aws_config()) -> {ok, binary()} | {error, erlcloud_aws:httpc_result_error()}.
%%%---------------------------------------------------------------------------
%% @doc Generate session token Will fail if not an EC2 instance.
%%
%% This convenience function will generate the IMDSv2 session token from the AWS metadata available at
%% http://<host:port>/latest/latest/api/token
%% ItemPath allows fetching specific pieces of metadata.
%% <host:port> defaults to 169.254.169.254
generate_session_token(DurationSecs, Config) ->
   Header = [{"X-aws-ec2-metadata-token-ttl-seconds", integer_to_binary(DurationSecs)}],
   MetaDataPath = "http://" ++ ec2_meta_host_port() ++ "/latest/api/token",
   erlcloud_aws:http_body(erlcloud_httpc:request(MetaDataPath, put, Header, <<>>, erlcloud_aws:get_timeout(Config), Config)).


-spec get_instance_metadata() -> {ok, binary()} | {error, erlcloud_aws:httpc_result_error()}.
get_instance_metadata() ->
   get_instance_metadata(erlcloud_aws:default_config()).

-spec get_instance_metadata(Config :: aws_config() ) -> {ok, binary()} | {error, erlcloud_aws:httpc_result_error()}.
get_instance_metadata(Config) ->
   get_instance_metadata("", Config).

-spec get_instance_metadata( ItemPath :: string(), Config :: aws_config() ) -> {ok, binary()} | {error, erlcloud_aws:httpc_result_error()}.
get_instance_metadata(ItemPath, Config) ->
   get_instance_metadata(ItemPath, Config, undefined).

%%%---------------------------------------------------------------------------
-spec get_instance_metadata( ItemPath :: string(), Config :: aws_config(), Token :: undefined | binary()) -> {ok, binary()} | {error, erlcloud_aws:httpc_result_error()}.
%%%---------------------------------------------------------------------------
%% @doc Retrieve the instance meta data for the instance this code is running on. Will fail if not an EC2 instance.
%%
%% This convenience function will retrieve the instance id from the AWS metadata available at
%% http://<host:port>/latest/meta-data/*
%% ItemPath allows fetching specific pieces of metadata.
%% <host:port> defaults to 169.254.169.254
%%
%%
get_instance_metadata(ItemPath, Config, Token) ->
    MetaDataPath = "http://" ++ ec2_meta_host_port() ++ "/latest/meta-data/" ++ ItemPath,
    Header = maybe_token_header(Token),
    erlcloud_aws:http_body(erlcloud_httpc:request(MetaDataPath, get, Header, <<>>, erlcloud_aws:get_timeout(Config), Config)).


-spec get_instance_user_data() -> {ok, binary()} | {error, erlcloud_aws:httpc_result_error()}.
get_instance_user_data() ->
   get_instance_user_data(erlcloud_aws:default_config()).

-spec get_instance_user_data( Config :: aws_config() ) -> {ok, binary()} | {error, erlcloud_aws:httpc_result_error()}.
get_instance_user_data(Config) ->
   get_instance_user_data(Config, undefined).

%%%---------------------------------------------------------------------------
-spec get_instance_user_data( Config :: aws_config(), Token :: undefined | binary() ) -> {ok, binary()} | {error, erlcloud_aws:httpc_result_error()}.
%%%---------------------------------------------------------------------------
%% @doc Retrieve the user data for the instance this code is running on. Will fail if not an EC2 instance.
%%
%% This convenience function will retrieve the user data the instance was started with, i.e. what's available at
%% http://<host:port>/latest/user-data
%% <host:port> defaults to 169.254.169.254
%%
%%
get_instance_user_data(Config, Token) ->
    UserDataPath = "http://" ++ ec2_meta_host_port() ++ "/latest/user-data/",
    Header = maybe_token_header(Token),
    erlcloud_aws:http_body(erlcloud_httpc:request(UserDataPath, get, Header, <<>>, erlcloud_aws:get_timeout(Config), Config)).


-spec get_instance_dynamic_data() -> {ok, binary()} | {error, erlcloud_aws:httpc_result_error()}.
get_instance_dynamic_data() ->
   get_instance_dynamic_data(erlcloud_aws:default_config()).

-spec get_instance_dynamic_data(Config :: aws_config() ) -> {ok, binary()} | {error, erlcloud_aws:httpc_result_error()}.
get_instance_dynamic_data(Config) ->
   get_instance_dynamic_data("", Config).

-spec get_instance_dynamic_data( ItemPath :: string(), Config :: aws_config() ) -> {ok, binary()} | {error, erlcloud_aws:httpc_result_error()}.
get_instance_dynamic_data(ItemPath, Config) ->
   get_instance_dynamic_data(ItemPath, Config, undefined).

%%%---------------------------------------------------------------------------
-spec get_instance_dynamic_data( ItemPath :: string(), Config :: aws_config(), Token :: undefined | binary() ) -> {ok, binary()} | {error, erlcloud_aws:httpc_result_error()}.
%%%---------------------------------------------------------------------------

get_instance_dynamic_data(ItemPath, Config, Token) ->
    DynamicDataPath = "http://" ++ ec2_meta_host_port() ++ "/latest/dynamic/" ++ ItemPath,
    Header = maybe_token_header(Token),
    erlcloud_aws:http_body(erlcloud_httpc:request(DynamicDataPath, get, Header, <<>>, erlcloud_aws:get_timeout(Config), Config)).

%%%------------------------------------------------------------------------------
%%% Internal functions.
%%%------------------------------------------------------------------------------

ec2_meta_host_port() ->
    {ok, EC2MetaHostPort} = application:get_env(erlcloud, ec2_meta_host_port),
    EC2MetaHostPort.

maybe_token_header(undefined) ->
   [];
maybe_token_header(Token) ->
   [{"X-aws-ec2-metadata-token", Token}].
