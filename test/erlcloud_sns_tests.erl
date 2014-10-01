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
    meck:new(erlcloud_aws, [passthrough]),
    meck:expect(erlcloud_aws, aws_request_xml2,
                fun(_,_,_,_,_,_,_) -> mock_response() end).

stop(_) ->
    meck:unload(erlcloud_aws).

defaults_to_http(_) ->
    Config = erlcloud_aws:default_config(),
    erlcloud_sns:publish_to_topic("topicarn", "message", "subject", Config),
    ?_assertMatch({_, "http", _, _, _, _, Config},
                  get_values_from_history(meck:history(erlcloud_aws))).

supports_explicit_http(_) ->
    Config = (erlcloud_aws:default_config())#aws_config{sns_scheme="http://"},
    erlcloud_sns:publish_to_topic("topicarn", "message", "subject", Config),
    ?_assertMatch({_, "http", _, _, _, _, Config},
                  get_values_from_history(meck:history(erlcloud_aws))).

supports_https(_) ->
    Config = (erlcloud_aws:default_config())#aws_config{sns_scheme="https://"},
    erlcloud_sns:publish_to_topic("topicarn", "message", "subject", Config),
    ?_assertMatch({_, "https", _, _, _, _, Config},
                  get_values_from_history(meck:history(erlcloud_aws))).

is_case_insensitive(_) ->
    Config = (erlcloud_aws:default_config())#aws_config{sns_scheme="HTTPS://"},
    erlcloud_sns:publish_to_topic("topicarn", "message", "subject", Config),
    ?_assertMatch({_, "https", _, _, _, _, Config},
                  get_values_from_history(meck:history(erlcloud_aws))).

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
    [Call1] = [ Params || {_, {erlcloud_aws, aws_request_xml2, Params}, _} <- Plist ],
    list_to_tuple(Call1).

mock_response() ->
   R = <<"<PublishResponse xmlns='http://sns.amazonaws.com/doc/2010-03-31/'>
  <PublishResult>
    <MessageId>94f20ce6-13c5-43a0-9a9e-ca52d816e90b</MessageId>
  </PublishResult>
  <ResponseMetadata>
    <RequestId>f187a3c1-376f-11df-8963-01868b7c937a</RequestId>
  </ResponseMetadata>
</PublishResponse>">>,
    {ok, element(1, xmerl_scan:string(binary_to_list(R)))}.
