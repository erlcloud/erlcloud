-module(erlcloud_ec2_meta).

-include("erlcloud.hrl").
-include("erlcloud_aws.hrl").

-export([get_instance_metadata/0, get_instance_metadata/1, get_instance_metadata/2,
        get_instance_user_data/0, get_instance_user_data/1,
        get_instance_dynamic_data/0, get_instance_dynamic_data/1, get_instance_dynamic_data/2]).




-spec get_instance_metadata() -> {ok, binary()} | {error, erlcloud_aws:httpc_result_error()}.
get_instance_metadata() ->
   get_instance_metadata(erlcloud_aws:default_config()).

-spec get_instance_metadata(Config :: aws_config() ) -> {ok, binary()} | {error, erlcloud_aws:httpc_result_error()}.
get_instance_metadata(Config) ->
   get_instance_metadata("", Config).


%%%---------------------------------------------------------------------------
-spec get_instance_metadata( ItemPath :: string(), Config :: aws_config() ) -> {ok, binary()} | {error, erlcloud_aws:httpc_result_error()}.
%%%---------------------------------------------------------------------------
%% @doc Retrieve the instance meta data for the instance this code is running on. Will fail if not an EC2 instance.
%%
%% This convenience function will retrieve the instance id from the AWS metadata available at 
%% http://169.254.169.254/latest/meta-data/*
%% ItemPath allows fetching specific pieces of metadata.
%%
%%
get_instance_metadata(ItemPath, Config) ->
    MetaDataPath = "http://169.254.169.254/latest/meta-data/" ++ ItemPath,
    erlcloud_aws:http_body(erlcloud_httpc:request(MetaDataPath, get, [], <<>>, erlcloud_aws:get_timeout(Config), Config)).


-spec get_instance_user_data() -> {ok, binary()} | {error, erlcloud_aws:httpc_result_error()}.
get_instance_user_data() ->
   get_instance_user_data(erlcloud_aws:default_config()).

%%%---------------------------------------------------------------------------
-spec get_instance_user_data( Config :: aws_config() ) -> {ok, binary()} | {error, erlcloud_aws:httpc_result_error()}.
%%%---------------------------------------------------------------------------
%% @doc Retrieve the user data for the instance this code is running on. Will fail if not an EC2 instance.
%%
%% This convenience function will retrieve the user data the instance was started with, i.e. what's available at 
%% http://169.254.169.254/latest/user-data
%%
%%
get_instance_user_data(Config) ->
    UserDataPath = "http://169.254.169.254/latest/user-data/",
    erlcloud_aws:http_body(erlcloud_httpc:request(UserDataPath, get, [], <<>>, erlcloud_aws:get_timeout(Config), Config)).


-spec get_instance_dynamic_data() -> {ok, binary()} | {error, erlcloud_aws:httpc_result_error()}.
get_instance_dynamic_data() ->
   get_instance_dynamic_data(erlcloud_aws:default_config()).

-spec get_instance_dynamic_data(Config :: aws_config() ) -> {ok, binary()} | {error, erlcloud_aws:httpc_result_error()}.
get_instance_dynamic_data(Config) ->
   get_instance_dynamic_data("", Config).


%%%---------------------------------------------------------------------------
-spec get_instance_dynamic_data( ItemPath :: string(), Config :: aws_config() ) -> {ok, binary()} | {error, erlcloud_aws:httpc_result_error()}.
%%%---------------------------------------------------------------------------

get_instance_dynamic_data(ItemPath, Config) ->
    DynamicDataPath = "http://169.254.169.254/latest/dynamic/" ++ ItemPath,
    erlcloud_aws:http_body(erlcloud_httpc:request(DynamicDataPath, get, [], <<>>, erlcloud_aws:get_timeout(Config), Config)).

