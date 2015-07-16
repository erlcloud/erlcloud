%% -*- mode: erlang;erlang-indent-level: 4;indent-tabs-mode: nil -*-
-module(erlcloud_ses_tests).
-include_lib("eunit/include/eunit.hrl").
-include("erlcloud.hrl").

%% Unit tests for ses.
%% These tests work by using meck to mock erlcloud_httpc.

%%%===================================================================
%%% Test entry points
%%%===================================================================

operation_test_() ->
    {foreach,
     fun start/0,
     fun stop/1,
     [fun delete_identity_tests/1,
      fun get_identity_dkim_attributes_tests/1,
      fun get_identity_notification_attributes_tests/1,
      fun get_identity_verification_attributes_tests/1,
      fun get_send_quota_tests/1,
      fun get_send_statistics_tests/1,
      fun list_identities_tests/1,
      fun send_email_tests/1,
      fun set_identity_dkim_enabled_tests/1,
      fun set_identity_feedback_forwarding_enabled_tests/1,
      fun set_identity_notification_topic_tests/1,
      fun verify_domain_dkim_tests/1,
      fun verify_domain_identity_tests/1,
      fun verify_email_identity_tests/1
     ]}.

start() ->
    meck:new(erlcloud_httpc),
    ok.

stop(_) ->
    meck:unload(erlcloud_httpc).

%%%===================================================================
%%% Test helpers
%%%===================================================================

-type expected_body() :: string().

-spec validate_body(binary(), expected_body()) -> ok.
validate_body(Body, Expected) ->
    Want = list_to_binary(Expected),
    case Body =:= Want of
        true -> ok;
        false ->
            ?debugFmt("~nEXPECTED~n~p~nACTUAL~n~p~n", [Want, Body])
    end,
    ?assertEqual(Want, Body).

%% returns the mock of the erlcloud_httpc function input tests expect to be called.
%% Validates the request body and responds with the provided response.
-spec input_expect(string(), expected_body()) -> fun().
input_expect(Response, Expected) ->
    fun(_Url, post, _Headers, Body, _Timeout, _Config) -> 
            validate_body(Body, Expected),
            {ok, {{200, "OK"}, [], list_to_binary(Response)}} 
    end.

configure() ->
    erlcloud_ses:configure(string:copies("A", 20), string:copies("a", 40)).

%%%===================================================================
%%% Actual test specifiers
%%%===================================================================

delete_identity_tests(_) ->
    [fun() ->
        configure(),
        Expected = "Action=DeleteIdentity&Version=2010-12-01&Identity=domain.com",
        Response =
"<DeleteIdentityResponse xmlns=\"http://ses.amazonaws.com/doc/2010-12-01/\">
  <DeleteIdentityResult/>
  <ResponseMetadata>
    <RequestId>d96bd874-9bf2-11e1-8ee7-c98a0037a2b6</RequestId>
  </ResponseMetadata>
</DeleteIdentityResponse>",
        meck:expect(erlcloud_httpc, request, input_expect(Response, Expected)),
        ?assertEqual(ok, erlcloud_ses:delete_identity(<<"domain.com">>))
     end
    ].

get_identity_dkim_attributes_tests(_) ->
    [fun() ->
        configure(),
        Expected = "Action=GetIdentityDkimAttributes&Version=2010-12-01&Identities.member.1=example.com",
        Response =
"<GetIdentityDkimAttributesResponse xmlns=\"http://ses.amazonaws.com/doc/2010-12-01/\">
  <GetIdentityDkimAttributesResult>
    <DkimAttributes>
      <entry>
        <key>example.com</key>
      <value>
        <DkimEnabled>true</DkimEnabled>
        <DkimVerificationStatus>Success</DkimVerificationStatus>
        <DkimTokens>
          <member>vvjuipp74whm76gqoni7qmwwn4w4qusjiainivf6f</member>
          <member>3frqe7jn4obpuxjpwpolz6ipb3k5nvt2nhjpik2oy</member>
          <member>wrqplteh7oodxnad7hsl4mixg2uavzneazxv5sxi2</member>
        </DkimTokens>
      </value>
    </entry>
    </DkimAttributes>
  </GetIdentityDkimAttributesResult>
  <ResponseMetadata>
    <RequestId>bb5a105d-c468-11e1-82eb-dff885ccc06a</RequestId>
  </ResponseMetadata>
</GetIdentityDkimAttributesResponse>",
        meck:expect(erlcloud_httpc, request, input_expect(Response, Expected)),
        ?assertEqual({ok, [{dkim_attributes, [{"example.com",
                                               [{dkim_enabled, true},
                                                {dkim_verification_status, success},
                                                {dkim_tokens,["vvjuipp74whm76gqoni7qmwwn4w4qusjiainivf6f",
                                                              "3frqe7jn4obpuxjpwpolz6ipb3k5nvt2nhjpik2oy",
                                                              "wrqplteh7oodxnad7hsl4mixg2uavzneazxv5sxi2"]}]}]}]},
                     erlcloud_ses:get_identity_dkim_attributes(<<"example.com">>))
     end
    ].

get_identity_notification_attributes_tests(_) ->
    [fun() ->
        configure(),
        Expected = "Action=GetIdentityNotificationAttributes&Version=2010-12-01&Identities.member.1=example.com",
        Response =
"<GetIdentityNotificationAttributesResponse xmlns=\"http://ses.amazonaws.com/doc/2010-12-01/\">
  <GetIdentityNotificationAttributesResult>
    <NotificationAttributes>
      <entry>
        <key>user@example.com</key>
        <value>
          <ForwardingEnabled>true</ForwardingEnabled>
          <BounceTopic>arn:aws:sns:us-east-1:123456789012:example</BounceTopic>
          <ComplaintTopic>arn:aws:sns:us-east-1:123456789012:example</ComplaintTopic>
          <DeliveryTopic>arn:aws:sns:us-east-1:123456789012:example</DeliveryTopic>
        </value>
      </entry>
    </NotificationAttributes>
  </GetIdentityNotificationAttributesResult>
  <ResponseMetadata>
    <RequestId>e038e509-b72a-11e1-901f-1fbd90e8104f</RequestId>
  </ResponseMetadata>
</GetIdentityNotificationAttributesResponse>",
        meck:expect(erlcloud_httpc, request, input_expect(Response, Expected)),
        ?assertEqual({ok, [{notification_attributes, [{"user@example.com",
                                                       [{forwarding_enabled, true},
                                                        {bounce_topic, "arn:aws:sns:us-east-1:123456789012:example"},
                                                        {complaint_topic, "arn:aws:sns:us-east-1:123456789012:example"},
                                                        {delivery_topic, "arn:aws:sns:us-east-1:123456789012:example"}]}]}]},
                     erlcloud_ses:get_identity_notification_attributes(<<"example.com">>))
     end
    ].

get_identity_verification_attributes_tests(_) ->
    [fun() ->
        configure(),
        Expected = "Action=GetIdentityVerificationAttributes&Version=2010-12-01&Identities.member.1=user%40domain.com&Identities.member.2=domain.com",
        Response =
"<GetIdentityVerificationAttributesResponse xmlns=\"http://ses.amazonaws.com/doc/2010-12-01/\">
  <GetIdentityVerificationAttributesResult>
    <VerificationAttributes>
      <entry>
        <key>domain.com</key>
        <value>
          <VerificationStatus>Pending</VerificationStatus>
          <VerificationToken>QTKknzFg2J4ygwa+XvHAxUl1hyHoY0gVfZdfjIedHZ0=</VerificationToken>
        </value>
      </entry>
      <entry>
        <key>user@domain.com</key>
        <value>
          <VerificationStatus>Pending</VerificationStatus>
        </value>
      </entry>
    </VerificationAttributes>
  </GetIdentityVerificationAttributesResult>
  <ResponseMetadata>
    <RequestId>1d0c29f1-9bf3-11e1-8ee7-c98a0037a2b6</RequestId>
  </ResponseMetadata>
</GetIdentityVerificationAttributesResponse>",
        meck:expect(erlcloud_httpc, request, input_expect(Response, Expected)),
        ?assertEqual({ok, [{verification_attributes, [{"domain.com",
                                                       [{verification_status, pending},
                                                        {verification_token, "QTKknzFg2J4ygwa+XvHAxUl1hyHoY0gVfZdfjIedHZ0="}]},
                                                      {"user@domain.com",
                                                       [{verification_status, pending}]}]}]},
                     erlcloud_ses:get_identity_verification_attributes(["user@domain.com", "domain.com"]))
     end
    ].

get_send_quota_tests(_) ->
    [fun() ->
        configure(),
        Expected = "Action=GetSendQuota&Version=2010-12-01",
        Response =
"<GetSendQuotaResponse xmlns=\"http://ses.amazonaws.com/doc/2010-12-01/\">
  <GetSendQuotaResult>
    <SentLast24Hours>127.0</SentLast24Hours>
    <Max24HourSend>200.0</Max24HourSend>
    <MaxSendRate>1.0</MaxSendRate>
  </GetSendQuotaResult>
  <ResponseMetadata>
    <RequestId>273021c6-c866-11e0-b926-699e21c3af9e</RequestId>
  </ResponseMetadata>
</GetSendQuotaResponse>",
        meck:expect(erlcloud_httpc, request, input_expect(Response, Expected)),
        ?assertEqual({ok, [{sent_last_24_hours, 127.0},
                           {max_24_hour_send, 200.0},
                           {max_send_rate, 1.0}]},
                     erlcloud_ses:get_send_quota())
     end
    ].

get_send_statistics_tests(_) ->
    [fun() ->
        configure(),
        Expected = "Action=GetSendStatistics&Version=2010-12-01",
        Response =
"<GetSendStatisticsResponse xmlns=\"http://ses.amazonaws.com/doc/2010-12-01/\">
  <GetSendStatisticsResult>
    <SendDataPoints>
      <member>
        <DeliveryAttempts>8</DeliveryAttempts>
        <Timestamp>2011-08-03T19:23:00Z</Timestamp>
        <Rejects>0</Rejects>
        <Bounces>0</Bounces>
        <Complaints>0</Complaints>
      </member>
      <member>
        <DeliveryAttempts>7</DeliveryAttempts>
        <Timestamp>2011-08-03T06:53:00Z</Timestamp>
        <Rejects>0</Rejects>
        <Bounces>0</Bounces>
        <Complaints>0</Complaints>
      </member>
    </SendDataPoints>
  </GetSendStatisticsResult>
  <ResponseMetadata>
    <RequestId>c2b66ee5-c866-11e0-b17f-cddb0ab334db</RequestId>
  </ResponseMetadata>
</GetSendStatisticsResponse>",
        meck:expect(erlcloud_httpc, request, input_expect(Response, Expected)),
        ?assertEqual({ok,[{send_data_points,
                           [[{delivery_attempts, 8},
                             {timestamp, "2011-08-03T19:23:00Z"},
                             {rejects, 0},
                             {bounces, 0},
                             {complaints, 0}],
                            [{delivery_attempts, 7},
                             {timestamp, "2011-08-03T06:53:00Z"},
                             {rejects, 0},
                             {bounces, 0},
                             {complaints, 0}]]}]},
                     erlcloud_ses:get_send_statistics())
     end
    ].

list_identities_tests(_) ->
    [fun() ->
        configure(),
        Expected = "Action=ListIdentities&Version=2010-12-01",
        Response =
"<ListIdentitiesResponse xmlns=\"http://ses.amazonaws.com/doc/2010-12-01/\">
  <ListIdentitiesResult>
    <Identities>
      <member>example.com</member>
      <member>user@example.com</member>
    </Identities>
  </ListIdentitiesResult>
  <ResponseMetadata>
    <RequestId>cacecf23-9bf1-11e1-9279-0100e8cf109a</RequestId>
  </ResponseMetadata>
</ListIdentitiesResponse>",
        meck:expect(erlcloud_httpc, request, input_expect(Response, Expected)),
        ?assertEqual({ok, [{identities, ["example.com", "user@example.com"]}]},
                     erlcloud_ses:list_identities())
     end,
     fun() ->
        configure(),
        Expected = "Action=ListIdentities&Version=2010-12-01&MaxItems=1&IdentityType=Domain&NextToken=next_token",
        Response =
"<ListIdentitiesResponse xmlns=\"http://ses.amazonaws.com/doc/2010-12-01/\">
  <ListIdentitiesResult>
    <Identities>
      <member>example.com</member>
    </Identities>
    <NextToken>next_token</NextToken>
  </ListIdentitiesResult>
  <ResponseMetadata>
    <RequestId>cacecf23-9bf1-11e1-9279-0100e8cf109a</RequestId>
  </ResponseMetadata>
</ListIdentitiesResponse>",
        meck:expect(erlcloud_httpc, request, input_expect(Response, Expected)),
        ?assertEqual({ok, [{identities, ["example.com"]},
                           {next_token, "next_token"}]},
                     erlcloud_ses:list_identities([{max_items,1},{identity_type, domain},{next_token, "next_token"}]))
     end
    ].

send_email_tests(_) ->
    [
     fun() ->
         configure(),
         Expected = "Action=SendEmail&Version=2010-12-01&Destination.ToAddresses.member.1=a%40to.com&Message.Body.Text.Data=Email%20Body&Message.Subject.Data=Subject&Source=b%40from.com",
         Response =
"<SendEmailResponse xmlns=\"http://ses.amazonaws.com/doc/2010-12-01/\">
  <SendEmailResult>
    <MessageId>00000131d51d2292-159ad6eb-077c-46e6-ad09-ae7c05925ed4-000000</MessageId>
  </SendEmailResult>
  <ResponseMetadata>
    <RequestId>d5964849-c866-11e0-9beb-01a62d68c57f</RequestId>
  </ResponseMetadata>
</SendEmailResponse>",
         meck:expect(erlcloud_httpc, request, input_expect(Response, Expected)),
         ?assertEqual({ok, [{message_id, "00000131d51d2292-159ad6eb-077c-46e6-ad09-ae7c05925ed4-000000"}]},
                      erlcloud_ses:send_email(<<"a@to.com">>, <<"Email Body">>, <<"Subject">>,
                                              <<"b@from.com">>, []))
     end,
     fun() ->
         configure(),
         Expected = "Action=SendEmail&Version=2010-12-01&Destination.BccAddresses.member.1=a%40bcc.com&Destination.BccAddresses.member.2=b%40bcc.com&Destination.CcAddresses.member.1=c%40cc.com&Destination.ToAddresses.member.1=d%40to.com&Message.Body.Html.Charset=html%20charset&Message.Body.Html.Data=html%20data&Message.Body.Text.Charset=text%20charset&Message.Body.Text.Data=text%20data&Message.Subject.Charset=subject%20charset&Message.Subject.Data=subject%20data&Source=e%40from.com&ReplyToAddresses.member.1=f%40reply.com&ReplyToAddresses.member.2=g%40reply.com&ReturnPath=return%20path",
         Response =
"<SendEmailResponse xmlns=\"http://ses.amazonaws.com/doc/2010-12-01/\">
<SendEmailResult>
<MessageId>00000131d51d2292-159ad6eb-077c-46e6-ad09-ae7c05925ed4-000000</MessageId>
</SendEmailResult>
<ResponseMetadata>
<RequestId>d5964849-c866-11e0-9beb-01a62d68c57f</RequestId>
</ResponseMetadata>
</SendEmailResponse>",
         meck:expect(erlcloud_httpc, request, input_expect(Response, Expected)),
         ?assertEqual({ok, [{message_id, "00000131d51d2292-159ad6eb-077c-46e6-ad09-ae7c05925ed4-000000"}]},
                      erlcloud_ses:send_email([{bcc_addresses, [<<"a@bcc.com">>, "b@bcc.com"]},
                                               {cc_addresses, [<<"c@cc.com">>]},
                                               {to_addresses, ["d@to.com"]}],
                                              [{html, [{charset, "html charset"},
                                                       {data, "html data"}]},
                                               {text, [{charset, "text charset"},
                                                       {data, "text data"}]}],
                                              [{charset, "subject charset"},
                                               {data, "subject data"}],
                                              "e@from.com",
                                              [{reply_to_addresses, [<<"f@reply.com">>, "g@reply.com"]},
                                               {return_path, "return path"}]))
     end
    ].

set_identity_dkim_enabled_tests(_) ->
    [fun() ->
        configure(),
        Expected = "Action=SetIdentityDkimEnabled&Version=2010-12-01&Identity=user%40example.com&DkimEnabled=true",
        Response =
"<SetIdentityDkimEnabledResponse xmlns=\"http://ses.amazonaws.com/doc/2010-12-01/\">
  <SetIdentityDkimEnabledResult/>
  <ResponseMetadata>
    <RequestId>7aa61362-c469-11e1-aee5-6bbb4608fbcc</RequestId>
  </ResponseMetadata>
</SetIdentityDkimEnabledResponse>",
        meck:expect(erlcloud_httpc, request, input_expect(Response, Expected)),
        ?assertEqual(ok, erlcloud_ses:set_identity_dkim_enabled("user@example.com", true))
     end
    ].

set_identity_feedback_forwarding_enabled_tests(_) ->
    [fun() ->
        configure(),
        Expected = "Action=SetIdentityFeedbackForwardingEnabled&Version=2010-12-01&Identity=user%40example.com&ForwardingEnabled=true",
        Response =
"<SetIdentityFeedbackForwardingEnabledResponse xmlns=\"http://ses.amazonaws.com/doc/2010-12-01/\">
  <SetIdentityFeedbackForwardingEnabledResult/>
  <ResponseMetadata>
    <RequestId>299f4af4-b72a-11e1-901f-1fbd90e8104f</RequestId>
  </ResponseMetadata>
</SetIdentityFeedbackForwardingEnabledResponse>",
        meck:expect(erlcloud_httpc, request, input_expect(Response, Expected)),
        ?assertEqual(ok, erlcloud_ses:set_identity_feedback_forwarding_enabled("user@example.com", true))
     end
    ].

set_identity_notification_topic_tests(_) ->
    [fun() ->
        configure(),
        Expected = "Action=SetIdentityNotificationTopic&Version=2010-12-01&Identity=user%40example.com&NotificationType=Bounce&SnsTopic=arn%3Aaws%3Asns%3Aus-east-1%3A123456789012%3Aexample",
        Response =
"<SetIdentityNotificationTopicResponse xmlns=\"http://ses.amazonaws.com/doc/2010-12-01/\">
  <SetIdentityNotificationTopicResult/>
  <ResponseMetadata>
    <RequestId>299f4af4-b72a-11e1-901f-1fbd90e8104f</RequestId>
  </ResponseMetadata>
</SetIdentityNotificationTopicResponse>",
        meck:expect(erlcloud_httpc, request, input_expect(Response, Expected)),
        ?assertEqual(ok, erlcloud_ses:set_identity_notification_topic(<<"user@example.com">>, bounce, <<"arn:aws:sns:us-east-1:123456789012:example">>))
     end
    ].

verify_domain_dkim_tests(_) ->
    [fun() ->
        configure(),
        Expected = "Action=VerifyDomainDkim&Version=2010-12-01&Domain=example.com",
        Response =
"<VerifyDomainDkimResponse xmlns=\"http://ses.amazonaws.com/doc/2010-12-01/\">
  <VerifyDomainDkimResult>
    <DkimTokens>
      <member>vvjuipp74whm76gqoni7qmwwn4w4qusjiainivf6sf</member>
      <member>3frqe7jn4obpuxjpwpolz6ipb3k5nvt2nhjpik2oy</member>
      <member>wrqplteh7oodxnad7hsl4mixg2uavzneazxv5sxi2</member>
    </DkimTokens>
    </VerifyDomainDkimResult>
    <ResponseMetadata>
      <RequestId>9662c15b-c469-11e1-99d1-797d6ecd6414</RequestId>
    </ResponseMetadata>
</VerifyDomainDkimResponse>",
        meck:expect(erlcloud_httpc, request, input_expect(Response, Expected)),
        ?assertEqual({ok, [{dkim_tokens, ["vvjuipp74whm76gqoni7qmwwn4w4qusjiainivf6sf",
                                          "3frqe7jn4obpuxjpwpolz6ipb3k5nvt2nhjpik2oy",
                                          "wrqplteh7oodxnad7hsl4mixg2uavzneazxv5sxi2"]}]},
                     erlcloud_ses:verify_domain_dkim(<<"example.com">>))
     end
    ].

verify_domain_identity_tests(_) ->
    [fun() ->
        configure(),
        Expected = "Action=VerifyDomainIdentity&Version=2010-12-01&Domain=example.com",
        Response =
"<VerifyDomainIdentityResponse xmlns=\"http://ses.amazonaws.com/doc/2010-12-01/\">
  <VerifyDomainIdentityResult>
    <VerificationToken>QTKknzFg2J4ygwa+XvHAxUl1hyHoY0gVfZdfjIedHZ0=</VerificationToken>
  </VerifyDomainIdentityResult>
  <ResponseMetadata>
    <RequestId>94f6368e-9bf2-11e1-8ee7-c98a0037a2b6</RequestId>
  </ResponseMetadata>
</VerifyDomainIdentityResponse>",
        meck:expect(erlcloud_httpc, request, input_expect(Response, Expected)),
        ?assertEqual({ok, [{verification_token, "QTKknzFg2J4ygwa+XvHAxUl1hyHoY0gVfZdfjIedHZ0="}]},
                     erlcloud_ses:verify_domain_identity(<<"example.com">>))
     end
    ].

verify_email_identity_tests(_) ->
    [fun() ->
        configure(),
        Expected = "Action=VerifyEmailIdentity&Version=2010-12-01&EmailAddress=user%40example.com",
        Response =
"<VerifyEmailIdentityResponse xmlns=\"http://ses.amazonaws.com/doc/2010-12-01/\">
  <VerifyEmailIdentityResult/>
  <ResponseMetadata>
    <RequestId>47e0ef1a-9bf2-11e1-9279-0100e8cf109a</RequestId>
  </ResponseMetadata>
</VerifyEmailIdentityResponse>",
        meck:expect(erlcloud_httpc, request, input_expect(Response, Expected)),
        ?assertEqual(ok, erlcloud_ses:verify_email_identity(<<"user@example.com">>))
     end
    ].
