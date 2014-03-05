%% Amazon Kinesis Service (Kinesis)

-module(erlcloud_kinesis).

%%% Library initialization.
-export([configure/2, configure/3, configure/4,  new/2, new/3]).

-export([list_streams/0, list_streams/1
        ]).

-include_lib("erlcloud/include/erlcloud.hrl").
-include_lib("erlcloud/include/erlcloud_aws.hrl").

-define(API_VERSION, "20131202").
-define(KINESIS_TIMEOUT, 10000).

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
       kinesis_host=Host
      }.


-spec new(string(), string(), string(), non_neg_integer()) -> aws_config().

new(AccessKeyID, SecretAccessKey, Host, Port) ->
    #aws_config{
       access_key_id=AccessKeyID,
       secret_access_key=SecretAccessKey,
       kinesis_host=Host,
       kinesis_port=Port
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

default_config() -> erlcloud_aws:default_config().



-spec list_streams/0 :: () -> proplist().

list_streams() ->
   list_streams(default_config()).

-spec list_streams/1 :: (aws_config()) -> proplist().

list_streams(Config) when is_record(Config, aws_config) ->
   Json = [],
   erlcloud_kinesis_impl:request(Config, "Kinesis_20131202.ListStreams", Json).

