-module(erlcloud_ecs_container_credentials).

-include("erlcloud.hrl").
-include("erlcloud_aws.hrl").

-export([task_credentials_available/0, get_container_credentials/1]).

%%%---------------------------------------------------------------------------
-spec task_credentials_available() -> ok | error.
%%%---------------------------------------------------------------------------
%% @doc Returns whether task metadata is available or not.
%%
%% This convenience function will check if the required enviroment variables are
%% set or not; it will not perform any requests to validate them.
%%
%%
task_credentials_available() ->
    RelativeUri = os:getenv("AWS_CONTAINER_CREDENTIALS_RELATIVE_URI"),
    case RelativeUri of
      false -> error;
      _ -> ok
    end.

%%%---------------------------------------------------------------------------
-spec get_container_credentials( Config :: aws_config() ) -> {ok, binary()} | {error, tuple()}.
%%%---------------------------------------------------------------------------
%% @doc Retrieve the container credentials. Will fail if not an ECS task.
%%
%% This convenience function will retrieve the credentials from the AWS metadata available at
%% http://169.254.170.2${AWS_CONTAINER_CREDENTIALS_RELATIVE_URI}
%%
%%
get_container_credentials(Config) ->
    RelativeUri = os:getenv("AWS_CONTAINER_CREDENTIALS_RELATIVE_URI"),
    CredentialsPath = "http://169.254.170.2" ++ RelativeUri,
    erlcloud_aws:http_body(erlcloud_httpc:request(CredentialsPath, get, [], <<>>, erlcloud_aws:get_timeout(Config), Config)).
