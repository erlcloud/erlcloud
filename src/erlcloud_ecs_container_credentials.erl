-module(erlcloud_ecs_container_credentials).

-include("erlcloud.hrl").
-include("erlcloud_aws.hrl").

-export([get_container_credentials/1]).

%%%---------------------------------------------------------------------------
-spec get_container_credentials( Config :: aws_config() ) -> {ok, binary()} | {error, container_credentials_unavailable | erlcloud_aws:httpc_result_error()}.
%%%---------------------------------------------------------------------------
%% @doc Retrieve the container credentials. Will fail if not an ECS task.
%%
%% This convenience function will retrieve the credentials from the AWS metadata available at
%% http://169.254.170.2${AWS_CONTAINER_CREDENTIALS_RELATIVE_URI}
%%
%%
get_container_credentials(Config) ->
    RelativeUri = os:getenv("AWS_CONTAINER_CREDENTIALS_RELATIVE_URI"),
    case RelativeUri of
      false -> {error, container_credentials_unavailable};
      _ ->
        CredentialsPath = "http://169.254.170.2" ++ RelativeUri,
        erlcloud_aws:http_body(erlcloud_httpc:request(CredentialsPath, get, [], <<>>, erlcloud_aws:get_timeout(Config), Config))
    end.
