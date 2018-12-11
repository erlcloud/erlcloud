-module(erlcloud_sqs_tests).
-include_lib("eunit/include/eunit.hrl").

-export([start/0]).
-export([stop/1]).
-export([send_message_batch/1]).
-export([send_message_with_message_opts/1]).
-export([send_message_with_message_attributes/1]).
-export([receive_messages_with_message_attributes/1]).
-export([send_message_batch_with_message_attributes/1]).

-define(_sqs_test(T), {?LINE, T}).
-define(_f(F), fun() -> F end).

erlcloud_api_test_() ->
    {foreach,
     fun start/0,
     fun stop/1,
     [
      fun set_queue_attributes/1,
      fun send_message_with_message_opts/1,
      fun send_message_with_message_attributes/1,
      fun receive_messages_with_message_attributes/1,
      fun send_message_batch/1,
      fun send_message_batch_with_message_attributes/1,
      fun receive_messages_with_unknown_attributes/1
     ]}.

start() ->
    meck:new(erlcloud_httpc),
    ok.

stop(_) ->
    meck:unload(erlcloud_httpc).

%%%===================================================================
%%% Input test helpers
%%%===================================================================

%% common_params returns the list of parameters that are not validated by these tests.
%% They should be checked by lower level unit tests.
-spec common_params() -> [string()].
common_params() ->
    ["AWSAccessKeyId",
     "SignatureMethod",
     "SignatureVersion",
     "Timestamp",
     "Version",
     "Signature"].

%% validate_param checks that the query parameter is either a common param or expected
%% by the test case. If expected, returns expected with the param deleted to be used in
%% subsequent calls.
-type expected_param() :: {string(), string()}.
-spec validate_param(string(), [expected_param()]) -> [expected_param()].
validate_param(Param, Expected) ->
    [Key, Value] = case string:tokens(Param, "=") of
                        [K, V] ->
                            [K, V];
                        [K] ->
                            [K, ""]
                    end,
    case lists:member(Key, common_params()) of
        true ->
            Expected;
        false ->
            Expected1 = lists:delete({Key, Value}, Expected),
            case length(Expected) - 1 =:= length(Expected1) of
                true -> ok;
                false ->
                    ?debugFmt("Parameter not expected: ~p", [{Key, Value}])
            end,
            ?assertEqual(length(Expected) - 1, length(Expected1)),
            Expected1
    end.

-spec validate_params(binary(), [expected_param()]) -> ok.
validate_params(Body, Expected) ->
    ParamList = string:tokens(binary_to_list(Body), "&"),
    Remain = lists:foldl(fun validate_param/2, Expected, ParamList),
    io:format("Remain: ~p", [Remain]),
    ?assertEqual([], Remain).

%% returns the mock of the httpc function input tests expect to be called.
%% Validates the query body and responds with the provided response.
-spec input_expect(string(), [expected_param()]) -> fun().
input_expect(Response, Expected) ->
    fun(_Url, post, _Headers, Body, _Timeout, _Config) ->
            validate_params(Body, Expected),
            {ok, {{200, "OK"}, [], list_to_binary(Response)}}
    end.

%% input_test converts an input_test specifier into an eunit test generator
-type input_test_spec() :: {pos_integer(), {fun(), [expected_param()]} | {string(), fun(), [expected_param()]}}.
-spec input_test(string(), input_test_spec()) -> tuple().
input_test(Response, {Line, {Description, Fun, Params}})
  when is_list(Description) ->
    {Description,
     {Line,
      fun() ->
              meck:expect(erlcloud_httpc, request, input_expect(Response, Params)),
              erlcloud_sns:configure(string:copies("A", 20), string:copies("a", 40)),
              Fun()
      end}}.

%% input_tests converts a list of input_test specifiers into an eunit test generator
-spec input_tests(string(), [input_test_spec()]) -> [tuple()].
input_tests(Response, Tests) ->
    [input_test(Response, Test) || Test <- Tests].

%%%===================================================================
%%% Output test helpers
%%%===================================================================

%% returns the mock of the erlcloud_httpc function output tests expect to be called.
-spec output_expect(string()) -> fun().
output_expect(Response) ->
    fun(_Url, post, _Headers, _Body, _Timeout, _Config) ->
            {ok, {{200, "OK"}, [], list_to_binary(Response)}}
    end.

%% output_test converts an output_test specifier into an eunit test generator
-type output_test_spec() :: {pos_integer(), {string(), term()} | {string(), string(), term()}}.
-spec output_test(fun(), output_test_spec()) -> tuple().
output_test(Fun, {Line, {Description, Response, Result}}) ->
    {Description,
     {Line,
      fun() ->
              meck:expect(erlcloud_httpc, request, output_expect(Response)),
              erlcloud_ec2:configure(string:copies("A", 20), string:copies("a", 40)),
              Actual = Fun(),
              ?assertEqual(Result, Actual)
      end}}.
%% output_test(Fun, {Line, {Response, Result}}) ->
%%     output_test(Fun, {Line, {"", Response, Result}}).

%% output_tests converts a list of output_test specifiers into an eunit test generator
-spec output_tests(fun(), [output_test_spec()]) -> [term()].
output_tests(Fun, Tests) ->
    [output_test(Fun, Test) || Test <- Tests].

set_queue_attributes(_) ->
    Attributes = [{kms_master_key_id, "some/id"},
                  {kms_data_key_reuse_period_seconds, 200}],
    Expected = [
        {"Action", "SetQueueAttributes"},
        {"Attribute.1.Name", "KmsMasterKeyId"},
        {"Attribute.1.Value", "some%2Fid"},
        {"Attribute.2.Name", "KmsDataKeyReusePeriodSeconds"},
        {"Attribute.2.Value", "200"}
    ],
    Tests =
        [?_sqs_test(
            {"Test queue attributes setting.",
             ?_f(erlcloud_sqs:set_queue_attributes("Queue", Attributes)),
             Expected})],
    Response = "
<SetQueueAttributesResponse>
   <ResponseMetadata>
      <RequestId>40945605-b328-53b5-aed4-1cc24a7240e8</RequestId>
   </ResponseMetadata>
</SetQueueAttributesResponse>",
    input_tests(Response, Tests).

send_message_with_message_opts(_) ->
    MessageBody = "Hello",
    MessageOpts = [
        {message_group_id, "GroupId"},
        {message_deduplication_id, "DedupId"}
    ],
    Expected = [
        {"Action", "SendMessage"},
        {"MessageGroupId", "GroupId"},
        {"MessageDeduplicationId", "DedupId"},
        {"MessageBody", MessageBody}
    ],
    Tests =
        [?_sqs_test(
            {"Test sends a message with message options.",
                ?_f(erlcloud_sqs:send_message("Queue.fifo", MessageBody, MessageOpts,
                    erlcloud_aws:default_config())),
                Expected})],
    Response = "
<SendMessageResponse>
    <SendMessageResult>
        <MD5OfMessageBody>
            fafb00f5732ab283681e124bf8747ed1
        </MD5OfMessageBody>
        <MessageId>
            5fea7756-0ea4-451a-a703-a558b933e274
        </MessageId>
    </SendMessageResult>
    <ResponseMetadata>
        <RequestId>
            27daac76-34dd-47df-bd01-1f6e873584a0
        </RequestId>
    </ResponseMetadata>
</SendMessageResponse>",
    input_tests(Response, Tests).

send_message_with_message_attributes(_) ->
    MessageBody = "Hello",
    MessageAttributes = [{"first", "value"},
                         {"second", 1},
                         {"third", 3.14159265359},
                         {"fourth", <<"binary">>},
                         {"uuid", {"uuid", <<"db3bf1fc-0cac-4cf8-8d2c-5c307ad4ac3a">>}}],
    Expected = [
                {"Action", "SendMessage"},
                {"MessageAttribute.1.Name", "first"},
                {"MessageAttribute.1.Value.StringValue", "value"},
                {"MessageAttribute.1.Value.DataType", "String"},
                {"MessageAttribute.2.Name", "second"},
                {"MessageAttribute.2.Value.StringValue", "1"},
                {"MessageAttribute.2.Value.DataType", "Number.int"},
                {"MessageAttribute.3.Name", "third"},
                {"MessageAttribute.3.Value.StringValue", "3.14159265359"},
                {"MessageAttribute.3.Value.DataType", "Number.float"},
                {"MessageAttribute.4.Name", "fourth"},
                {"MessageAttribute.4.Value.BinaryValue", "binary"},
                {"MessageAttribute.4.Value.DataType", "Binary"},
                {"MessageAttribute.5.Name", "uuid"},
                {"MessageAttribute.5.Value.DataType", "Binary.uuid"},
                {"MessageAttribute.5.Value.BinaryValue", "db3bf1fc-0cac-4cf8-8d2c-5c307ad4ac3a"},
                {"MessageBody", MessageBody}
               ],
    Tests =
        [?_sqs_test(
            {"Test sends a message with message attributes.",
             ?_f(erlcloud_sqs:send_message("Queue", MessageBody, none, MessageAttributes,
                                           erlcloud_aws:default_config())),
             Expected})],
    Response = "
<SendMessageResponse>
    <SendMessageResult>
        <MD5OfMessageBody>
            fafb00f5732ab283681e124bf8747ed1
        </MD5OfMessageBody>
        <MD5OfMessageAttributes>
            3ae8f24a165a8cedc005670c81a27295
        </MD5OfMessageAttributes>
        <MessageId>
            5fea7756-0ea4-451a-a703-a558b933e274
        </MessageId>
    </SendMessageResult>
    <ResponseMetadata>
        <RequestId>
            27daac76-34dd-47df-bd01-1f6e873584a0
        </RequestId>
    </ResponseMetadata>
</SendMessageResponse>",
    input_tests(Response, Tests).

receive_messages_with_message_attributes(_) ->
    MessageResponse = "
<ReceiveMessageResponse>
  <ReceiveMessageResult>
    <Message>
      <MessageId>5fea7756-0ea4-451a-a703-a558b933e274</MessageId>
      <ReceiptHandle>MbZj6wDWli+JvwwJaBV+3dcjk2YW2vA3+STFFljTM8tJJg6HRG6PYSasuWXPJB+CwLj1FjgXUv1uSj1gUPAWV66FU/WeR4mq2OKpEGYWbnLmpRCJVAyeMjeU5ZBdtcQ+QEauMZc8ZRv37sIW2iJKq3M9MFx1YvV11A2x/KSbkJ0=</ReceiptHandle>
      <MD5OfBody>fafb00f5732ab283681e124bf8747ed1</MD5OfBody>
      <Body>This is a test message</Body>
      <Attribute>
        <Name>SenderId</Name>
        <Value>195004372649</Value>
      </Attribute>
      <Attribute>
        <Name>MessageGroupId</Name>
        <Value>GroupId</Value>
      </Attribute>
      <Attribute>
        <Name>MessageDeduplicationId</Name>
        <Value>DedupId</Value>
      </Attribute>
      <Attribute>
        <Name>SentTimestamp</Name>
        <Value>1238099229000</Value>
      </Attribute>
      <Attribute>
        <Name>ApproximateReceiveCount</Name>
        <Value>5</Value>
      </Attribute>
      <Attribute>
        <Name>ApproximateFirstReceiveTimestamp</Name>
        <Value>1250700979248</Value>
      </Attribute>
      <MD5OfMessageAttributes>ea4cd23cddb3081504655c7720a0975f</MD5OfMessageAttributes>
      <MessageAttribute>
        <Name>content-type</Name>
        <Value>
          <DataType>String</DataType>
          <StringValue>application/json</StringValue>
        </Value>
      </MessageAttribute>
      <MessageAttribute>
        <Name>float</Name>
        <Value>
          <DataType>Number.float</DataType>
          <StringValue>3.1415926</StringValue>
        </Value>
      </MessageAttribute>
      <MessageAttribute>
        <Name>integer</Name>
        <Value>
          <DataType>Number.int</DataType>
          <StringValue>42</StringValue>
        </Value>
      </MessageAttribute>
    <MessageAttribute>
        <Name>number</Name>
        <Value>
            <DataType>Number</DataType>
            <StringValue>56</StringValue>
        </Value>
    </MessageAttribute>
      <MessageAttribute>
        <Name>binary</Name>
        <Value>
          <DataType>Binary</DataType>
          <BinaryValue>Binary string</BinaryValue>
        </Value>
      </MessageAttribute>
      <MessageAttribute>
        <Name>uuid</Name>
        <Value>
          <DataType>Binary.uuid</DataType>
          <BinaryValue>db3bf1fc-0cac-4cf8-8d2c-5c307ad4ac3a</BinaryValue>
        </Value>
      </MessageAttribute>
    </Message>
  </ReceiveMessageResult>
  <ResponseMetadata>
    <RequestId>
      b6633655-283d-45b4-aee4-4e84e0ae6afa
    </RequestId>
  </ResponseMetadata>
</ReceiveMessageResponse>",
    Expected = [{messages, [[{body, "This is a test message"},
                             {md5_of_body, "fafb00f5732ab283681e124bf8747ed1"},
                             {message_id, "5fea7756-0ea4-451a-a703-a558b933e274"},
                             {receipt_handle, "MbZj6wDWli+JvwwJaBV+3dcjk2YW2vA3+STFFljTM8tJJg6HRG6PYSasuWXPJB+CwLj1FjgXUv1uSj1gUPAWV66FU/WeR4mq2OKpEGYWbnLmpRCJVAyeMjeU5ZBdtcQ+QEauMZc8ZRv37sIW2iJKq3M9MFx1YvV11A2x/KSbkJ0="},
                             {attributes, [
                                           {sender_id, "195004372649"},
                                           {message_group_id, "GroupId"},
                                           {message_deduplication_id, "DedupId"},
                                           {sent_timestamp, 1238099229000},
                                           {approximate_receive_count, 5},
                                           {approximate_first_receive_timestamp, 1250700979248}]},
                             {message_attributes, [
                                                   {"content-type", "application/json"},
                                                   {"float", 3.1415926},
                                                   {"integer", 42},
                                                   {"number", 56},
                                                   {"binary", <<"Binary string">>},
                                                   {"uuid", {"uuid", <<"db3bf1fc-0cac-4cf8-8d2c-5c307ad4ac3a">>}}
                                                  ]}
                            ]]}],
    Tests =
        [?_sqs_test(
            {"Test receives a message with message attributes.",
             MessageResponse, Expected})],
    output_tests(?_f(erlcloud_sqs:receive_message("Queue", all, 1, 30, none, all, erlcloud_aws:default_config())), Tests).

    receive_messages_with_unknown_attributes(_) ->
            MessageResponse = "
        <ReceiveMessageResponse>
          <ReceiveMessageResult>
            <Message>
              <MessageId>5fea7756-0ea4-451a-a703-a558b933e274</MessageId>
              <ReceiptHandle>MbZj6wDWli+JvwwJaBV+3dcjk2YW2vA3+STFFljTM8tJJg6HRG6PYSasuWXPJB+CwLj1FjgXUv1uSj1gUPAWV66FU/WeR4mq2OKpEGYWbnLmpRCJVAyeMjeU5ZBdtcQ+QEauMZc8ZRv37sIW2iJKq3M9MFx1YvV11A2x/KSbkJ0=</ReceiptHandle>
              <MD5OfBody>fafb00f5732ab283681e124bf8747ed1</MD5OfBody>
              <Body>This is a test message</Body>
            <MessageAttribute>
            <Name>unknown</Name>
            <Value>
                <DataType>Unknown</DataType>
                <StringValue>invalid</StringValue>
            </Value>
            </MessageAttribute>
            </Message>
          </ReceiveMessageResult>
          <ResponseMetadata>
            <RequestId>
              b6633655-283d-45b4-aee4-4e84e0ae6afa
            </RequestId>
          </ResponseMetadata>
        </ReceiveMessageResponse>",

        F = fun() ->
                ?assertException(error,decode_message_attribute_value_error, 
                    erlcloud_sqs:receive_message("Queue", all, 1, 30, none, all, erlcloud_aws:default_config())),
            decode_message_attribute_value_error
        end,

        Tests =
                [?_sqs_test(
                    {"Test receives a message with message unknown attribute.",
                     MessageResponse, decode_message_attribute_value_error})],
                     
        output_tests(F, Tests).

send_message_batch(_) ->
    Batch = [{"batch-message-01", "Hello", [], [{message_group_id, "GroupId"}]},
             {"batch-message-02", "World", [], [{message_deduplication_id, "DedupId"}]}],
    Expected = [
                {"Action", "SendMessageBatch"},
                {"SendMessageBatchRequestEntry.1.Id", "batch-message-02"},
                {"SendMessageBatchRequestEntry.1.MessageBody", "World"},
                {"SendMessageBatchRequestEntry.1.MessageDeduplicationId", "DedupId"},
                {"SendMessageBatchRequestEntry.2.Id", "batch-message-01"},
                {"SendMessageBatchRequestEntry.2.MessageBody", "Hello"},
                {"SendMessageBatchRequestEntry.2.MessageGroupId", "GroupId"}
               ],
    Tests =
        [?_sqs_test(
            {"Test sends messages in a batch.",
             ?_f(erlcloud_sqs:send_message_batch("Queue", Batch, none,
                                                 erlcloud_aws:default_config())),
             Expected})],
    Response = "
<SendMessageBatchResponse>
    <SendMessageBatchResult>
        <SendMessageBatchResultEntry>
            <Id>batch-message-01</Id>
            <MessageId>0a5231c7-8bff-4955-be2e-8dc7c50a25fa</MessageId>
            <MD5OfMessageBody>8b1a9953c4611296a827abf8c47804d7</MD5OfMessageBody>
        </SendMessageBatchResultEntry>
        <SendMessageBatchResultEntry>
            <Id>batch-message-02</Id>
            <MessageId>15ee1ed3-87e7-40c1-bdaa-2e49968ea7e9</MessageId>
            <MD5OfMessageBody>f5a7924e621e84c9280a9a27e1bcb7f6</MD5OfMessageBody>
        </SendMessageBatchResultEntry>
    </SendMessageBatchResult>
    <ResponseMetadata>
        <RequestId>
            27daac76-34dd-47df-bd01-1f6e873584a0
        </RequestId>
    </ResponseMetadata>
</SendMessageBatchResponse>",
    input_tests(Response, Tests).

send_message_batch_with_message_attributes(_) ->
    Batch = [
             {"batch-message-01", "Hello", [{"correlationId", "1234"},
                                            {"content-type", "application/json"}]},
             {"batch-message-02", "World", [{"correlationId", "5678"},
                                            {"content-type", "text/xml"}]}
            ],
    Expected = [
                {"Action", "SendMessageBatch"},
                {"SendMessageBatchRequestEntry.1.Id", "batch-message-02"},
                {"SendMessageBatchRequestEntry.1.MessageBody", "World"},
                {"SendMessageBatchRequestEntry.1.MessageAttribute.1.Name", "correlationId"},
                {"SendMessageBatchRequestEntry.1.MessageAttribute.1.Value.StringValue", "5678"},
                {"SendMessageBatchRequestEntry.1.MessageAttribute.1.Value.DataType", "String"},
                {"SendMessageBatchRequestEntry.1.MessageAttribute.2.Name", "content-type"},
                {"SendMessageBatchRequestEntry.1.MessageAttribute.2.Value.StringValue", "text%2Fxml"},
                {"SendMessageBatchRequestEntry.1.MessageAttribute.2.Value.DataType", "String"},
                {"SendMessageBatchRequestEntry.2.Id", "batch-message-01"},
                {"SendMessageBatchRequestEntry.2.MessageBody", "Hello"},
                {"SendMessageBatchRequestEntry.2.MessageAttribute.1.Name", "correlationId"},
                {"SendMessageBatchRequestEntry.2.MessageAttribute.1.Value.StringValue", "1234"},
                {"SendMessageBatchRequestEntry.2.MessageAttribute.1.Value.DataType", "String"},
                {"SendMessageBatchRequestEntry.2.MessageAttribute.2.Name", "content-type"},
                {"SendMessageBatchRequestEntry.2.MessageAttribute.2.Value.StringValue", "application%2Fjson"},
                {"SendMessageBatchRequestEntry.2.MessageAttribute.2.Value.DataType", "String"}
               ],
    Tests =
        [?_sqs_test(
            {"Test sends messages in a batch.",
             ?_f(erlcloud_sqs:send_message_batch("Queue", Batch, none,
                                                 erlcloud_aws:default_config())),
             Expected})],
    Response = "
<SendMessageBatchResponse>
    <SendMessageBatchResult>
        <SendMessageBatchResultEntry>
            <Id>batch-message-01</Id>
            <MessageId>0a5231c7-8bff-4955-be2e-8dc7c50a25fa</MessageId>
            <MD5OfMessageBody>8b1a9953c4611296a827abf8c47804d7</MD5OfMessageBody>
        </SendMessageBatchResultEntry>
        <SendMessageBatchResultEntry>
            <Id>batch-message-02</Id>
            <MessageId>15ee1ed3-87e7-40c1-bdaa-2e49968ea7e9</MessageId>
            <MD5OfMessageBody>f5a7924e621e84c9280a9a27e1bcb7f6</MD5OfMessageBody>
        </SendMessageBatchResultEntry>
    </SendMessageBatchResult>
    <ResponseMetadata>
        <RequestId>
            27daac76-34dd-47df-bd01-1f6e873584a0
        </RequestId>
    </ResponseMetadata>
</SendMessageBatchResponse>",
    input_tests(Response, Tests).
