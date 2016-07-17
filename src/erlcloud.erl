-module(erlcloud).
-export([start/0]).

-export([
    new/2, new/3,
    configure/2, configure/3
]).

-include("erlcloud_aws.hrl").
-define(APP, erlcloud).

start() ->
    application:load(?APP),
    {ok, Apps} = application:get_key(?APP, applications),
    [application:start(App) || App <- Apps],
    application:start(?APP).


%%
%% create new erlcloud configuration 
-spec new(string(), string()) -> aws_config().

new(AccessKeyID, SecretAccessKey) ->
    #aws_config{
       access_key_id=AccessKeyID,
       secret_access_key=SecretAccessKey
      }.

-spec new(string(), string(), string()) -> aws_config().

new(AccessKeyID, SecretAccessKey, SecurityToken) ->
    #aws_config{
       access_key_id=AccessKeyID,
       secret_access_key=SecretAccessKey,
       security_token=SecurityToken
      }.

%%
%% global erlcloud configuration
-spec configure(string(), string()) -> ok.

configure(AccessKeyID, SecretAccessKey) ->
    case get(aws_config) of
        undefined ->
            put(aws_config, new(AccessKeyID, SecretAccessKey)),
            ok;
        #aws_config{} = Conf ->
            put(aws_config, 
                Conf#aws_config{
                    access_key_id=AccessKeyID,
                    secret_access_key=SecretAccessKey
                }
            ),
            ok
    end.

-spec configure(string(), string(), string()) -> ok.

configure(AccessKeyID, SecretAccessKey, SecurityToken) ->
    case get(aws_config) of
        undefined ->
            put(aws_config, new(AccessKeyID, SecretAccessKey, SecurityToken)),
            ok;
        #aws_config{} = Conf ->
            put(aws_config, 
                Conf#aws_config{
                    access_key_id=AccessKeyID,
                    secret_access_key=SecretAccessKey,
                    security_token=SecurityToken
                }
            ),
            ok
    end.


