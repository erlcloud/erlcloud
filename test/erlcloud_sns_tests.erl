-module(erlcloud_sns_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("erlcloud/include/erlcloud_aws.hrl").

sns_publish_test_() ->
    {foreach,
     fun start/0,
     fun stop/1,
     [fun defaults_to_http/1,
      fun supports_explicit_http/1,
      fun supports_https/1,
      fun is_case_insensitive/1,
      fun doesnt_support_gopher/1,
      fun doesnt_accept_non_strings/1
     ]}.

start() ->
    meck:new(erlcloud_httpc),
    meck:expect(erlcloud_httpc, request,
                 fun(_,_,_,_,_,_) -> mock_httpc_response() end),
    erlcloud_sns:configure(string:copies("A", 20), string:copies("a", 40)).

stop(_) ->
    meck:unload(erlcloud_httpc).

defaults_to_http(_) ->
    Config = erlcloud_aws:default_config(),
    erlcloud_sns:publish_to_topic("topicarn", "message", "subject", Config),
    ?_assertMatch({"http://sns.amazonaws.com/", _, _, _, _, Config}, request_params()).

supports_explicit_http(_) ->
    Config = (erlcloud_aws:default_config())#aws_config{sns_scheme="http://"},
    erlcloud_sns:publish_to_topic("topicarn", "message", "subject", Config),
    ?_assertMatch({"http://sns.amazonaws.com/", _, _, _, _, Config}, request_params()).

supports_https(_) ->
    Config = (erlcloud_aws:default_config())#aws_config{sns_scheme="https://"},
    erlcloud_sns:publish_to_topic("topicarn", "message", "subject", Config),
    ?_assertMatch({"https://sns.amazonaws.com/", _, _, _, _, Config}, request_params()).

is_case_insensitive(_) ->
    Config = (erlcloud_aws:default_config())#aws_config{sns_scheme="HTTPS://"},
    erlcloud_sns:publish_to_topic("topicarn", "message", "subject", Config),
    ?_assertMatch({"https://sns.amazonaws.com/", _, _, _, _, Config}, request_params()).

doesnt_support_gopher(_) ->
    Config = (erlcloud_aws:default_config())#aws_config{sns_scheme="gopher://"},
    ?_assertError({sns_error, {unsupported_scheme,"gopher://"}},
                  erlcloud_sns:publish_to_topic("topicarn", "message", "subject", Config)).

doesnt_accept_non_strings(_) ->
    Config = (erlcloud_aws:default_config())#aws_config{sns_scheme=https},
    ?_assertError({sns_error, badarg},
                  erlcloud_sns:publish_to_topic("topicarn", "message", "subject", Config)).


% ==================
% Internal functions
% ==================

get_values_from_history(Plist) ->
    [Call1] = [ Params || {_, {erlcloud_httpc, request, Params}, _} <- Plist ],
    list_to_tuple(Call1).

request_params() ->
    get_values_from_history(meck:history(erlcloud_httpc)).

mock_httpc_response() ->
    {ok, {{200, "ok"}, [], response_body()}}.

response_body() ->
    <<"<PublishResponse xmlns='http://sns.amazonaws.com/doc/2010-03-31/'>
  <PublishResult>
    <MessageId>94f20ce6-13c5-43a0-9a9e-ca52d816e90b</MessageId>
  </PublishResult>
  <ResponseMetadata>
    <RequestId>f187a3c1-376f-11df-8963-01868b7c937a</RequestId>
  </ResponseMetadata>
</PublishResponse>">>.
