-module(erlcloud_ec2_meta).

-include("erlcloud.hrl").
-include("erlcloud_aws.hrl").

-export([get_instance_metadata/0, get_instance_metadata/1, get_instance_metadata/2,
        get_instance_user_data/0, get_instance_user_data/1,
        get_instance_dynamic_data/0, get_instance_dynamic_data/1, get_instance_dynamic_data/2]).

-define(DEFAULT_EC2_METADATA_URI, "http://169.254.169.254").

-spec get_instance_metadata() -> {ok, binary()} | {error, tuple()}.
get_instance_metadata() ->
   get_instance_metadata(erlcloud_aws:default_config()).

-spec get_instance_metadata(Config :: aws_config() ) -> {ok, binary()} | {error, tuple()}.
get_instance_metadata(Config) ->
   get_instance_metadata("", Config).


%%%---------------------------------------------------------------------------
-spec get_instance_metadata( ItemPath :: string(), Config :: aws_config() ) -> {ok, binary()} | {error, tuple()}.
%%%---------------------------------------------------------------------------
%% @doc Retrieve the instance meta data for the instance this code is running on. Will fail if not an EC2 instance.
%%
%% This convenience function will retrieve the instance id from the AWS metadata available at 
%% http://169.254.169.254/latest/meta-data/*
%% ItemPath allows fetching specific pieces of metadata.
%%
%%
get_instance_metadata(ItemPath, Config) ->
    MetaDataPath = unicode:characters_to_list([get_ec2_metadata_uri(),  "/latest/meta-data/", ItemPath]),
    erlcloud_aws:http_body(erlcloud_httpc:request(MetaDataPath, get, [], <<>>, erlcloud_aws:get_timeout(Config), Config)).


-spec get_instance_user_data() -> {ok, binary()} | {error, tuple()}.
get_instance_user_data() ->
   get_instance_user_data(erlcloud_aws:default_config()).

%%%---------------------------------------------------------------------------
-spec get_instance_user_data( Config :: aws_config() ) -> {ok, binary()} | {error, tuple()}.
%%%---------------------------------------------------------------------------
%% @doc Retrieve the user data for the instance this code is running on. Will fail if not an EC2 instance.
%%
%% This convenience function will retrieve the user data the instance was started with, i.e. what's available at 
%% http://169.254.169.254/latest/user-data
%%
%%
get_instance_user_data(Config) ->
    UserDataPath = unicode:characters_to_list([get_ec2_metadata_uri(), "/latest/user-data/"]),
    erlcloud_aws:http_body(erlcloud_httpc:request(UserDataPath, get, [], <<>>, erlcloud_aws:get_timeout(Config), Config)).


-spec get_instance_dynamic_data() -> {ok, binary()} | {error, tuple()}.
get_instance_dynamic_data() ->
   get_instance_dynamic_data(erlcloud_aws:default_config()).

-spec get_instance_dynamic_data(Config :: aws_config() ) -> {ok, binary()} | {error, tuple()}.
get_instance_dynamic_data(Config) ->
   get_instance_dynamic_data("", Config).


%%%---------------------------------------------------------------------------
-spec get_instance_dynamic_data( ItemPath :: string(), Config :: aws_config() ) -> {ok, binary()} | {error, tuple()}.
%%%---------------------------------------------------------------------------

get_instance_dynamic_data(ItemPath, Config) ->
    DynamicDataPath = unicode:characters_to_list([get_ec2_metadata_uri(), "/latest/dynamic/", ItemPath]),
    erlcloud_aws:http_body(erlcloud_httpc:request(DynamicDataPath, get, [], <<>>, erlcloud_aws:get_timeout(Config), Config)).


-spec get_ec2_metadata_uri() -> string().
get_ec2_metadata_uri() ->
    case application:get_env(?APP, ec2_metadata_url) of
        {ok, Val} ->
            Val;
        undefined ->
            ?DEFAULT_EC2_METADATA_URI
    end.