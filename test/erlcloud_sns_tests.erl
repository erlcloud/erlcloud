-module(erlcloud_sns_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("erlcloud/include/erlcloud_aws.hrl").

request_test_() ->
    {foreach,
     fun start/0,
     fun stop/1,
     [fun sns_publish_defaults_to_http/1,
      fun sns_publish_supports_https/1
     ]}.

start() ->
    meck:new(erlcloud_aws, [passthrough]),
    meck:expect(erlcloud_aws, aws_request_xml2,
                fun(_,_,_,_,_,_,_) -> mock_response() end).

stop(_) ->
    meck:unload(erlcloud_aws).

sns_publish_defaults_to_http(_) ->
    Config = erlcloud_aws:default_config(),
    catch(erlcloud_sns:publish_to_topic("topicarn", "message", "subject", Config)),
    ?_assertMatch({post, "http", "sns.amazonaws.com", _, _, _, Config},
                  get_values_from_history(meck:history(erlcloud_aws))).

sns_publish_supports_https(_) ->
    Config = (erlcloud_aws:default_config())#aws_config{sns_protocol="https"},
    erlcloud_sns:publish_to_topic("topicarn", "message", "subject", Config),
    ?_assertMatch({post, "https", "sns.amazonaws.com", _, _, _, Config},
                  get_values_from_history(meck:history(erlcloud_aws))).


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
