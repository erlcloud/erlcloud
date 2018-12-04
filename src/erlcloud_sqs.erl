%% Amazon Simple Queue Service (SQS)

-module(erlcloud_sqs).

-export([configure/2, configure/3, new/2, new/3]).

-export([
         add_permission/3, add_permission/4,
         change_message_visibility/3, change_message_visibility/4,
         create_queue/1, create_queue/2, create_queue/3,
         create_fifo_queue/1, create_fifo_queue/2, create_fifo_queue/3,
         delete_message/2, delete_message/3,
         delete_queue/1, delete_queue/2,
         purge_queue/1, purge_queue/2,
         get_queue_attributes/1, get_queue_attributes/2, get_queue_attributes/3,
         list_queues/0, list_queues/1, list_queues/2,
         receive_message/1, receive_message/2, receive_message/3, receive_message/4,
         receive_message/5, receive_message/6, receive_message/7,
         remove_permission/2, remove_permission/3,
         send_message/2, send_message/3, send_message/4, send_message/5,
         set_queue_attributes/2, set_queue_attributes/3,
         send_message_batch/2, send_message_batch/3, send_message_batch/4,
         delete_message_batch/2, delete_message_batch/3,
         change_message_visibility_batch/3, change_message_visibility_batch/4
        ]).

-include("erlcloud.hrl").
-include("erlcloud_aws.hrl").

-define(API_VERSION, "2012-11-05").

-define(SEND_BATCH_FIELD(N, F, V), {"SendMessageBatchRequestEntry." ++ N ++ F, V}).
-define(DELETE_BATCH_FIELD(N, F, V), {"DeleteMessageBatchRequestEntry." ++ N ++ F, V}).
-define(CHANGE_VISIBILITY_BATCH_FIELD(N, F, V), {"ChangeMessageVisibilityBatchRequestEntry." ++ N ++ F, V}).

-type(sqs_permission() :: all | send_message | receive_message | delete_message |
                          change_message_visibility | get_queue_attributes).
-type(sqs_acl() :: [{string(), sqs_permission()}]).
-type(sqs_msg_attribute_name() :: all | sender_id | sent_timestamp |
                                  approximate_receive_count |
                                  approximate_first_receive_timestamp |
                                  wait_time_seconds |
                                  receive_message_wait_time_seconds).
-type(sqs_queue_attribute_name() :: all | approximate_number_of_messages |
                                    kms_master_key_id | kms_data_key_reuse_period_seconds |
                                    approximate_number_of_messages_not_visible | visibility_timeout |
                                    created_timestamp | last_modified_timestamp | policy |
                                    queue_arn).

-type(batch_entry() ::   {string(), string()}
                       | {string(), string(), [message_attribute()]}
                       | {string(), string(), [message_attribute()], proplists:proplist()}).
-type(message_attribute() :: {string(), string() | integer() | float() | binary()}).

-spec new(string(), string()) -> aws_config().
new(AccessKeyID, SecretAccessKey) ->
    #aws_config{access_key_id=AccessKeyID,
                secret_access_key=SecretAccessKey}.

-spec new(string(), string(), string()) -> aws_config().
new(AccessKeyID, SecretAccessKey, Host) ->
    #aws_config{access_key_id=AccessKeyID,
                secret_access_key=SecretAccessKey,
                sqs_host=Host}.

-spec configure(string(), string()) -> ok.
configure(AccessKeyID, SecretAccessKey) ->
    put(aws_config, new(AccessKeyID, SecretAccessKey)),
    ok.

-spec configure(string(), string(), string()) -> ok.
configure(AccessKeyID, SecretAccessKey, Host) ->
    put(aws_config, new(AccessKeyID, SecretAccessKey, Host)),
    ok.


-spec add_permission(string(), string(), sqs_acl()) -> ok | no_return().
add_permission(QueueName, Label, Permissions) ->
    add_permission(QueueName, Label, Permissions, default_config()).

-spec add_permission(string(), string(), sqs_acl(), aws_config()) -> ok | no_return().
add_permission(QueueName, Label, Permissions, Config)
  when is_list(QueueName),
       is_list(Label), length(Label) =< 80,
       is_list(Permissions) ->
    sqs_simple_request(Config, QueueName, "AddPermission",
                       [{"Label", Label}|erlcloud_aws:param_list(encode_permissions(Permissions), {"AWSAccountId", "ActionName"})]).

encode_permissions(Permissions) ->
    [encode_permission(P) || P <- Permissions].

encode_permission({AccountId, Permission}) ->
    {AccountId,
     case Permission of
         all -> "*";
         send_message -> "SendMessage";
         receive_message -> "ReceiveMessage";
         delete_message -> "DeleteMessage";
         change_message_visibility -> "ChangeMessageVisibility";
         get_queue_attributes -> "GetQueueAttributes"
     end}.

-spec change_message_visibility(string(), string(), 0..43200) -> ok | no_return().
change_message_visibility(QueueName, ReceiptHandle, VisibilityTimeout) ->
    change_message_visibility(QueueName, ReceiptHandle, VisibilityTimeout,
                              default_config()).

-spec change_message_visibility(string(), string(), 0..43200, aws_config()) -> ok | no_return().
change_message_visibility(QueueName, ReceiptHandle, VisibilityTimeout, Config) ->
    sqs_simple_request(Config, QueueName, "ChangeMessageVisibility",
                       [{"ReceiptHandle", ReceiptHandle}, {"VisibilityTimeout", VisibilityTimeout}]).

-spec create_queue(string()) -> proplist() | no_return().
create_queue(QueueName) ->
    create_queue(QueueName, default_config()).

-spec create_queue(string(), 0..43200 | none | aws_config()) -> proplist() | no_return().
create_queue(QueueName, Config)
  when is_record(Config, aws_config) ->
    create_queue(QueueName, none, Config);
create_queue(QueueName, DefaultVisibilityTimeout) ->
    create_queue(QueueName, DefaultVisibilityTimeout, default_config()).

-spec create_queue(string(), 0..43200 | none, aws_config()) -> proplist() | no_return().
create_queue(QueueName, DefaultVisibilityTimeout, Config) ->
    create_queue_impl(QueueName, DefaultVisibilityTimeout, Config, []).

-spec create_fifo_queue(string()) -> proplist() | no_return().
create_fifo_queue(QueueName) ->
    create_fifo_queue(QueueName, default_config()).

-spec create_fifo_queue(string(), 0..43200 | none | aws_config()) -> proplist() | no_return().
create_fifo_queue(QueueName, Config)
  when is_record(Config, aws_config) ->
    create_fifo_queue(QueueName, none, Config);
create_fifo_queue(QueueName, DefaultVisibilityTimeout) ->
    create_fifo_queue(QueueName, DefaultVisibilityTimeout, default_config()).

-spec create_fifo_queue(string(), 0..43200 | none, aws_config()) -> proplist() | no_return().
create_fifo_queue(QueueName, DefaultVisibilityTimeout, Config) ->
    Attributes = erlcloud_aws:param_list([[{"Name", "FifoQueue"}, {"Value", true}]], "Attribute"),
    create_queue_impl(QueueName, DefaultVisibilityTimeout, Config, Attributes).

-spec create_queue_impl(string(), 0..43200 | none, aws_config(), proplists:proplist()) -> proplist() | no_return().
create_queue_impl(QueueName, DefaultVisibilityTimeout, Config, Attributes)
  when is_list(QueueName),
       (is_integer(DefaultVisibilityTimeout) andalso
        DefaultVisibilityTimeout >= 0 andalso
        DefaultVisibilityTimeout =< 43200) orelse
       DefaultVisibilityTimeout =:= none ->
    Doc = sqs_xml_request(Config, "/", "CreateQueue",
                          [{"QueueName", QueueName},
                           {"DefaultVisibilityTimeout", DefaultVisibilityTimeout} | Attributes]),
    erlcloud_xml:decode(
      [
       {queue_url, "CreateQueueResult/QueueUrl", text}
      ],
      Doc
     ).

-spec delete_message(string(), string()) -> ok | no_return().
delete_message(QueueName, ReceiptHandle) ->
    delete_message(QueueName, ReceiptHandle, default_config()).

-spec delete_message(string(), string(), aws_config()) -> ok | no_return().
delete_message(QueueName, ReceiptHandle, Config)
  when is_list(QueueName), is_list(ReceiptHandle), is_record(Config, aws_config) ->
    sqs_simple_request(Config, QueueName, "DeleteMessage",
                       [{"ReceiptHandle", ReceiptHandle}]).

-spec delete_queue(string()) -> ok | no_return().
delete_queue(QueueName) ->
    delete_queue(QueueName, default_config()).

-spec delete_queue(string(), aws_config()) -> ok | no_return().
delete_queue(QueueName, Config)
  when is_list(QueueName), is_record(Config, aws_config) ->
    sqs_simple_request(Config, QueueName, "DeleteQueue", []).

-spec purge_queue(string()) -> ok | no_return().
purge_queue(QueueName) ->
    purge_queue(QueueName, default_config()).

-spec purge_queue(string(), aws_config()) -> ok | no_return().
purge_queue(QueueName, Config)
  when is_list(QueueName), is_record(Config, aws_config) ->
    sqs_simple_request(Config, QueueName, "PurgeQueue", []).

-spec get_queue_attributes(string()) -> proplist() | no_return().
get_queue_attributes(QueueName) ->
    get_queue_attributes(QueueName, all).

-spec get_queue_attributes(string(), all | [sqs_queue_attribute_name()] | aws_config()) -> proplist() | no_return().
get_queue_attributes(QueueName, Config)
  when is_record(Config, aws_config) ->
    get_queue_attributes(QueueName, all, Config);
get_queue_attributes(QueueName, AttributeNames) ->
    get_queue_attributes(QueueName, AttributeNames, default_config()).

-spec get_queue_attributes(string(), all | [sqs_queue_attribute_name()], aws_config()) -> proplist() | no_return().
get_queue_attributes(QueueName, all, Config) when is_record(Config, aws_config) ->
    get_queue_attributes(QueueName, [all], Config);
get_queue_attributes(QueueName, AttributeNames, Config)
  when is_list(QueueName), is_list(AttributeNames), is_record(Config, aws_config) ->
    Doc = sqs_xml_request(Config, QueueName, "GetQueueAttributes",
                          erlcloud_aws:param_list([encode_attribute_name(N) || N <- AttributeNames], "AttributeName")),
    Attrs = decode_attributes(xmerl_xpath:string("GetQueueAttributesResult/Attribute", Doc)),
    [{decode_attribute_name(Name), decode_attribute_value(Name, Value)} || {Name, Value} <- Attrs].


encode_attribute_name(message_retention_period) -> "MessageRetentionPeriod";
encode_attribute_name(queue_arn) -> "QueueArn";
encode_attribute_name(maximum_message_size) -> "MaximumMessageSize";
encode_attribute_name(visibility_timeout) -> "VisibilityTimeout";
encode_attribute_name(approximate_number_of_messages) -> "ApproximateNumberOfMessages";
encode_attribute_name(approximate_number_of_messages_not_visible) -> "ApproximateNumberOfMessagesNotVisible";
encode_attribute_name(approximate_number_of_messages_delayed) -> "ApproximateNumberOfMessagesDelayed";
encode_attribute_name(last_modified_timestamp) -> "LastModifiedTimestamp";
encode_attribute_name(created_timestamp) -> "CreatedTimestamp";
encode_attribute_name(delay_seconds) -> "DelaySeconds";
encode_attribute_name(receive_message_wait_time_seconds) -> "ReceiveMessageWaitTimeSeconds";
encode_attribute_name(policy) -> "Policy";
encode_attribute_name(redrive_policy) -> "RedrivePolicy";
encode_attribute_name(kms_master_key_id) -> "KmsMasterKeyId";
encode_attribute_name(kms_data_key_reuse_period_seconds) -> "KmsDataKeyReusePeriodSeconds";
encode_attribute_name(all) -> "All".


decode_attribute_name("MessageRetentionPeriod") -> message_retention_period;
decode_attribute_name("QueueArn") -> queue_arn;
decode_attribute_name("MaximumMessageSize") -> maximum_message_size;
decode_attribute_name("VisibilityTimeout") -> visibility_timeout;
decode_attribute_name("ApproximateNumberOfMessages") -> approximate_number_of_messages;
decode_attribute_name("ApproximateNumberOfMessagesNotVisible") -> approximate_number_of_messages_not_visible;
decode_attribute_name("ApproximateNumberOfMessagesDelayed") -> approximate_number_of_messages_delayed;
decode_attribute_name("LastModifiedTimestamp") -> last_modified_timestamp;
decode_attribute_name("CreatedTimestamp") -> created_timestamp;
decode_attribute_name("DelaySeconds") -> delay_seconds;
decode_attribute_name("ReceiveMessageWaitTimeSeconds") -> receive_message_wait_time_seconds;
decode_attribute_name("Policy") -> policy;
decode_attribute_name("RedrivePolicy") -> redrive_policy;
decode_attribute_name("ContentBasedDeduplication") -> content_based_deduplication;
decode_attribute_name("KmsMasterKeyId") -> kms_master_key_id;
decode_attribute_name("KmsDataKeyReusePeriodSeconds") -> kms_data_key_reuse_period_seconds;
decode_attribute_name("FifoQueue") -> fifo_queue.


decode_attribute_value("Policy", Value) -> Value;
decode_attribute_value("QueueArn", Value) -> Value;
decode_attribute_value("RedrivePolicy", Value) -> Value;
decode_attribute_value("KmsMasterKeyId", Value) -> Value;
decode_attribute_value(_, "true") -> true;
decode_attribute_value(_, "false") -> false;
decode_attribute_value(_, Value) -> list_to_integer(Value).


-spec list_queues() -> [string()] | no_return().
list_queues() ->
    list_queues("").

-spec list_queues(string() | aws_config()) -> [string()] | no_return().
list_queues(Config)
  when is_record(Config, aws_config) ->
    list_queues("", Config);
list_queues(QueueNamePrefix) ->
    list_queues(QueueNamePrefix, default_config()).

-spec list_queues(string(), aws_config()) -> [string()] | no_return().
list_queues(QueueNamePrefix, Config)
  when is_list(QueueNamePrefix), is_record(Config, aws_config) ->
    Doc = sqs_xml_request(Config, "/", "ListQueues",
                          [{"QueueNamePrefix", QueueNamePrefix}]),
    erlcloud_xml:get_list("ListQueuesResult/QueueUrl", Doc).

-spec receive_message(string()) -> proplist() | no_return().
receive_message(QueueName) ->
    receive_message(QueueName, default_config()).

-spec receive_message(string(), [sqs_msg_attribute_name()] | all | aws_config()) -> proplist() | no_return().
receive_message(QueueName, Config)
  when is_record(Config, aws_config) ->
    receive_message(QueueName, [], Config);
receive_message(QueueName, AttributeNames) ->
    receive_message(QueueName, AttributeNames, default_config()).

-spec receive_message(string(), [sqs_msg_attribute_name()] | all, 1..10 | aws_config()) -> proplist() | no_return().
receive_message(QueueName, AttributeNames, Config)
  when is_record(Config, aws_config) ->
    receive_message(QueueName, AttributeNames, 1, Config);
receive_message(QueueName, AttributeNames, MaxNumberOfMessages) ->
    receive_message(QueueName, AttributeNames, MaxNumberOfMessages, default_config()).

-spec receive_message(string(), [sqs_msg_attribute_name()] | all, 1..10, 0..43200 | none | aws_config()) -> proplist() | no_return().
receive_message(QueueName, AttributeNames, MaxNumberOfMessages, Config)
  when is_record(Config, aws_config) ->
    receive_message(QueueName, AttributeNames, MaxNumberOfMessages, none, Config);
receive_message(QueueName, AttributeNames, MaxNumberOfMessages, VisibilityTimeout) ->
    receive_message(QueueName, AttributeNames, MaxNumberOfMessages,
                    VisibilityTimeout, default_config()).

-spec receive_message(string(), [sqs_msg_attribute_name()] | all, 1..10,
                      0..43200 | none, 0..20 | none | aws_config()) -> proplist() | no_return().
receive_message(QueueName, AttributeNames, MaxNumberOfMessages,
                VisibilityTimeout, Config)
  when is_record(Config, aws_config) ->
    receive_message(QueueName, AttributeNames, MaxNumberOfMessages,
                    VisibilityTimeout, none, Config);
receive_message(QueueName, AttributeNames, MaxNumberOfMessages,
                VisibilityTimeout, WaitTimeSeconds) ->
    receive_message(QueueName, AttributeNames, MaxNumberOfMessages,
                    VisibilityTimeout, WaitTimeSeconds, default_config()).

-spec receive_message(string(), [sqs_msg_attribute_name()] | all, 1..10,
                      0..43200 | none, 0..20 | none, aws_config()) -> proplist() | no_return().
receive_message(QueueName, all, MaxNumberOfMessages, VisibilityTimeout,
                WaitTimeoutSeconds, Config)  when is_record(Config, aws_config) ->
    receive_message(QueueName, [all], MaxNumberOfMessages,
                    VisibilityTimeout, WaitTimeoutSeconds, Config);
receive_message(QueueName, AttributeNames, MaxNumberOfMessages,
                VisibilityTimeout, WaitTimeSeconds, Config) ->
    receive_message(QueueName, AttributeNames, MaxNumberOfMessages, VisibilityTimeout,
                    WaitTimeSeconds, [all], Config).

receive_message(QueueName, AttributeNames, MaxNumberOfMessages,
                VisibilityTimeout, WaitTimeSeconds, MessageAttributeNames, Config)
  when is_record(Config, aws_config), (is_list(AttributeNames) orelse AttributeNames =:= all),
       MaxNumberOfMessages >= 1, MaxNumberOfMessages =< 10,
       (VisibilityTimeout >= 0 andalso VisibilityTimeout =< 43200) orelse
       VisibilityTimeout =:= none,
       (WaitTimeSeconds >= 0 andalso WaitTimeSeconds =< 20) orelse
       WaitTimeSeconds =:= none,
       (is_list(MessageAttributeNames) orelse MessageAttributeNames =:= all) ->
    InitialTimeout = erlcloud_aws:get_timeout(Config),
    TotalTimeout = if
       (WaitTimeSeconds =/= none andalso WaitTimeSeconds >= 0 andalso InitialTimeout =/= infinity) ->
           InitialTimeout + (WaitTimeSeconds * 1000) ;
       true -> InitialTimeout
    end,
    Doc = sqs_xml_request(Config#aws_config{timeout=TotalTimeout}, QueueName, "ReceiveMessage",
                          [
                           {"MaxNumberOfMessages", MaxNumberOfMessages},
                           {"VisibilityTimeout", VisibilityTimeout},
                           {"WaitTimeSeconds", WaitTimeSeconds} |
                           erlcloud_aws:param_list(encode_msg_attribute_names(MessageAttributeNames), "MessageAttributeName") ++
                               erlcloud_aws:param_list(encode_msg_attribute_names(AttributeNames), "AttributeName")
                          ]
                         ),
    erlcloud_xml:decode(
      [
       {messages, "ReceiveMessageResult/Message", fun decode_messages/1}
      ],
      Doc
     ).


encode_msg_attribute_names(all) ->
    [encode_msg_attribute_name(all)];
encode_msg_attribute_names(Names) when is_list(Names) ->
    [encode_msg_attribute_name(Name) || Name <- Names].

encode_msg_attribute_name(all) -> "All";
encode_msg_attribute_name(sender_id) -> "SenderId";
encode_msg_attribute_name(sent_timestamp) -> "SentTimestamp";
encode_msg_attribute_name(message_group_id) -> "MessageGroupId";
encode_msg_attribute_name(message_deduplication_id) -> "MessageDeduplicationId";
encode_msg_attribute_name(approximate_receive_count) -> "ApproximateReceiveCount";
encode_msg_attribute_name(approximate_first_receive_timestamp) -> "ApproximateFirstReceiveTimestamp";
encode_msg_attribute_name(Name) when is_list(Name) -> Name.

decode_msg_attribute_name("SenderId") -> sender_id;
decode_msg_attribute_name("SentTimestamp") -> sent_timestamp;
decode_msg_attribute_name("MessageGroupId") -> message_group_id;
decode_msg_attribute_name("MessageDeduplicationId") -> message_deduplication_id;
decode_msg_attribute_name("ApproximateReceiveCount") -> approximate_receive_count;
decode_msg_attribute_name("ApproximateFirstReceiveTimestamp") -> approximate_first_receive_timestamp;
decode_msg_attribute_name(Name) when is_list(Name) -> Name.

decode_msg_attribute_value("SenderId", Value) -> Value;
decode_msg_attribute_value("MessageGroupId", Value) -> Value;
decode_msg_attribute_value("MessageDeduplicationId", Value) -> Value;
decode_msg_attribute_value(_Name, Value) -> list_to_integer(Value).

decode_messages(Messages) ->
    [decode_message(Message) || Message <- Messages].

decode_message(Message) ->
    erlcloud_xml:decode(
      [
       {body, "Body", text},
       {md5_of_body, "MD5OfBody", text},
       {message_id, "MessageId", text},
       {receipt_handle, "ReceiptHandle", text},
       {attributes, "Attribute", fun decode_msg_attributes/1},
       {message_attributes, "MessageAttribute", fun decode_message_attributes/1}
      ],
      Message
     ).

decode_message_attributes(Attributes) ->
    [{decode_msg_attribute_name(Name), decode_message_attribute_value(DataType, StringValue)} ||
        {Name, DataType, StringValue} <- decode_message_attribute(Attributes)].

decode_message_attribute(Attributes) ->
    F = fun(Attr) ->
                Name = erlcloud_xml:get_text("Name", Attr),
                DataType = erlcloud_xml:get_text("Value/DataType", Attr),
                Value = case string:rstr(DataType, "Binary") of
                            1 ->
                                erlcloud_xml:get_text("Value/BinaryValue", Attr);
                            _ ->
                                erlcloud_xml:get_text("Value/StringValue", Attr)
                        end,
                {Name, DataType, Value}
        end,
    [F(Attr) || Attr <- Attributes].

decode_message_attribute_value("Number", Value) ->
    list_to_integer(Value);
decode_message_attribute_value(["Number", "int"], Value) ->
    list_to_integer(Value);
decode_message_attribute_value(["Number", "int", CustomType], Value) ->
    {CustomType, list_to_integer(Value)};
decode_message_attribute_value(["Number", "float"], Value) ->
    list_to_float(Value);
decode_message_attribute_value(["Number", "float", CustomType], Value) ->
    {CustomType, list_to_float(Value)};
decode_message_attribute_value("String", Value) ->
    Value;
decode_message_attribute_value(["String", CustomType], Value) ->
    {CustomType, Value};
decode_message_attribute_value("Binary", Value) ->
    list_to_binary(Value);
decode_message_attribute_value(["Binary", CustomType], Value) ->
    {CustomType, list_to_binary(Value)};
decode_message_attribute_value(DataType, Value) ->
    case string:tokens(DataType, ".") of
        [_Other] -> % check if datatype is something not handled above by Number/String/Binary
            erlang:error(decode_message_attribute_value_error, [DataType, Value]);
        Parsed ->
            decode_message_attribute_value(Parsed, Value)
    end.

decode_msg_attributes(Attrs)  ->
    [{decode_msg_attribute_name(Name),
      decode_msg_attribute_value(Name, Value)} || {Name, Value} <- decode_attributes(Attrs)].

decode_attributes(Attrs) ->
    [{erlcloud_xml:get_text("Name", Attr), erlcloud_xml:get_text("Value", Attr)} ||
        Attr <- Attrs].

-spec remove_permission(string(), string()) -> ok | no_return().
remove_permission(QueueName, Label) ->
    remove_permission(QueueName, Label, default_config()).

-spec remove_permission(string(), string(), aws_config()) -> ok | no_return().
remove_permission(QueueName, Label, Config)
  when is_list(QueueName), is_list(Label), is_record(Config, aws_config) ->
    sqs_simple_request(Config, QueueName, "RemovePermission",
                       [{"Label", Label}]).

-spec send_message(string(), string()) -> proplist() | no_return().
send_message(QueueName, MessageBody) ->
    send_message(QueueName, MessageBody, default_config()).

-spec send_message(string(), string(), proplists:proplist() | 0..900 | none | aws_config()) -> proplist() | no_return().
send_message(QueueName, MessageBody, #aws_config{} = Config) ->
    send_message(QueueName, MessageBody, none, Config);
send_message(QueueName, MessageBody, DelaySeconds) 
  when ((DelaySeconds >= 0 andalso DelaySeconds =< 900) orelse DelaySeconds =:= none) ->
    send_message(QueueName, MessageBody, DelaySeconds, default_config());
send_message(QueueName, MessageBody, Opts) when is_list(Opts) ->
    send_message(QueueName, MessageBody, Opts, default_config()).

-spec send_message(string(), string(), proplists:proplist() | 0..900 | none, aws_config()) -> proplist() | no_return().
send_message(QueueName, MessageBody, DelaySeconds, Config)
  when ((DelaySeconds >= 0 andalso DelaySeconds =< 900) orelse DelaySeconds =:= none) ->
    send_message(QueueName, MessageBody, [{delay_seconds, DelaySeconds}], [], Config);
send_message(QueueName, MessageBody, Opts, Config) when is_list(Opts) ->
    send_message(QueueName, MessageBody, Opts, [], Config).

-spec send_message(string(), string(), proplists:proplist() | 0..900 | none, [message_attribute()], aws_config()) -> proplist() | no_return().
send_message(QueueName, MessageBody, DelaySeconds, MessageAttributes, #aws_config{}=Config)
  when ((DelaySeconds >= 0 andalso DelaySeconds =< 900) orelse DelaySeconds =:= none) ->
    send_message(QueueName, MessageBody, [{delay_seconds, DelaySeconds}], MessageAttributes, Config);
send_message(QueueName, MessageBody, Opts, MessageAttributes, #aws_config{}=Config)
  when is_list(Opts) andalso is_list(MessageAttributes) ->
    EncodedMessageAttributes = encode_message_attributes(MessageAttributes),
    EncodedMessageOpts = [{encode_send_msg_attribute_name(N), V} || {N, V} <- Opts],
    Doc = sqs_xml_request(Config, QueueName, "SendMessage",
                          [{"MessageBody", MessageBody} | EncodedMessageOpts] ++ EncodedMessageAttributes),
    erlcloud_xml:decode(
      [
       {message_id, "SendMessageResult/MessageId", text},
       {md5_of_message_body, "SendMessageResult/MD5OfMessageBody", text}
      ],
      Doc
     ).

-spec set_queue_attributes(string(), proplists:proplist()) -> ok | no_return().
set_queue_attributes(QueueName, Attributes) ->
    set_queue_attributes(QueueName, Attributes, default_config()).

-spec set_queue_attributes(string(), proplists:proplist(), aws_config()) -> ok | no_return().
set_queue_attributes(QueueName, Attributes, Config)
  when is_list(QueueName), is_list(Attributes), is_record(Config, aws_config) ->
    Params = erlcloud_aws:param_list([
        [{"Name", encode_attribute_name(Name)},
         {"Value", Value}] || {Name, Value} <- Attributes], "Attribute"),
    sqs_simple_request(Config, QueueName, "SetQueueAttributes", Params).

-spec send_message_batch(string(), [batch_entry()]) -> proplist() | no_return().
send_message_batch(QueueName, BatchMessages) ->
    send_message_batch(QueueName, BatchMessages, default_config()).

-spec send_message_batch(string(), [batch_entry()], 0..900 | none | aws_config()) -> proplist() | no_return().
send_message_batch(QueueName, BatchMessages, Config)
    when is_record(Config, aws_config) ->
    send_message_batch(QueueName, BatchMessages, none, Config);
send_message_batch(QueueName, BatchMessages, DelaySeconds) ->
    send_message_batch(QueueName, BatchMessages, DelaySeconds, default_config()).

-spec send_message_batch(string(), [batch_entry()], 0..900 | none, aws_config()) -> proplist() | no_return().
send_message_batch(QueueName, BatchMessages, DelaySeconds, Config)
    when is_list(QueueName), is_record(Config, aws_config),
         (DelaySeconds >= 0 andalso DelaySeconds =< 900) orelse
         DelaySeconds =:= none ->
    {_, BatchRequestEntries} =
    lists:foldr(fun(Message, {N, Acc}) ->
                    {N + 1, [mk_send_batch_entry(N, Message, DelaySeconds)| Acc]}
                end, {1, []}, BatchMessages),

    Doc = sqs_xml_request(Config, QueueName, "SendMessageBatch",
                          lists:flatten(BatchRequestEntries)),

    BatchResponse =
    [{successful, "SendMessageBatchResult/SendMessageBatchResultEntry", fun decode_send_batch_successful/1},
     {failed, "SendMessageBatchResult/BatchResultErrorEntry", fun decode_batch_result_error/1}],

    erlcloud_xml:decode(BatchResponse, Doc).

-spec delete_message_batch(string(), [batch_entry()]) -> proplists:proplist() | no_return().
delete_message_batch(QueueName, BatchReceiptHandles) ->
    delete_message_batch(QueueName, BatchReceiptHandles, default_config()).

-spec delete_message_batch(string(), [batch_entry()], aws_config()) -> proplists:proplist() | no_return().
delete_message_batch(QueueName, [{Id, Handle}|_]=BatchReceiptHandles, Config)
    when is_list(QueueName), is_list(Id), is_list(Handle),
         is_record(Config, aws_config) ->

    {_, BatchRequestEntries} =
    lists:foldr(fun({BatchId, MessageBody}, {N, Acc}) ->
                    {N + 1, [mk_delete_batch_entry(N, BatchId, MessageBody)| Acc]}
                end, {1, []}, BatchReceiptHandles),

    Doc = sqs_xml_request(Config, QueueName, "DeleteMessageBatch",
                          lists:flatten(BatchRequestEntries)),

    BatchResponse =
    [{successful, "DeleteMessageBatchResult/DeleteMessageBatchResultEntry", fun decode_id_batch_successful/1},
     {failed, "DeleteMessageBatchResult/BatchResultErrorEntry", fun decode_batch_result_error/1}],
    erlcloud_xml:decode(BatchResponse, Doc).

-spec change_message_visibility_batch(string(), [batch_entry()], 0..43200) -> proplists:proplist() | no_return().
change_message_visibility_batch(QueueName, BatchReceiptHandles, VisibilityTimeout) ->
    change_message_visibility_batch(QueueName, BatchReceiptHandles, VisibilityTimeout,
                              default_config()).

-spec change_message_visibility_batch(string(), [batch_entry()], 0..43200, aws_config()) -> proplists:proplist() | no_return().
change_message_visibility_batch(QueueName, [{Id, Handle}|_]=BatchReceiptHandles, VisibilityTimeout, Config)
    when is_list(QueueName), is_list(Id), is_list(Handle), is_record(Config, aws_config) ->

    {_, BatchRequestEntries} =
    lists:foldr(fun({BatchId, MessageBody}, {N, Acc}) ->
                    {N + 1, [mk_ch_visibility_batch_entry(N, BatchId, MessageBody, VisibilityTimeout)| Acc]}
                end, {1, []}, BatchReceiptHandles),

    Doc = sqs_xml_request(Config, QueueName, "ChangeMessageVisibilityBatch",
                          lists:flatten(BatchRequestEntries)),

    BatchResponse =
    [{successful, "ChangeMessageVisibilityBatchResult/ChangeMessageVisibilityBatchResultEntry", fun decode_id_batch_successful/1},
     {failed, "ChangeMessageVisibilityBatchResult/BatchResultErrorEntry", fun decode_batch_result_error/1}],
    erlcloud_xml:decode(BatchResponse, Doc).

-spec mk_send_batch_entry(integer(), batch_entry(), 0..900 | none) -> [{string(), integer() | string()}].
mk_send_batch_entry(N, {MessageId, MessageBody}, DelaySeconds) ->
    mk_send_batch_entry(N, {MessageId, MessageBody, [], []}, DelaySeconds);
mk_send_batch_entry(N, {MessageId, MessageBody, MessageAttributes}, DelaySeconds) ->
    mk_send_batch_entry(N, {MessageId, MessageBody, MessageAttributes, []}, DelaySeconds);
mk_send_batch_entry(N, {MessageId, MessageBody, MessageAttributes, Opts}, DelaySeconds)
  when is_list(MessageId), is_list(MessageBody), is_list(MessageAttributes), is_list(Opts) ->
    N0 = integer_to_list(N),
    EncodedOpts = [
        ?SEND_BATCH_FIELD(N0, [$., encode_send_msg_attribute_name(Field)], Value)
        || {Field, Value} <- Opts],
    Base = [?SEND_BATCH_FIELD(N0, ".Id", MessageId),
            ?SEND_BATCH_FIELD(N0, ".MessageBody", MessageBody),
            ?SEND_BATCH_FIELD(N0, ".DelaySeconds", DelaySeconds) | EncodedOpts],
    lists:foldl(fun({Field, Value}, Acc) ->
                        [?SEND_BATCH_FIELD(N0, [$., Field], Value) | Acc]
                end, Base, encode_message_attributes(MessageAttributes)).

-spec mk_delete_batch_entry(integer(), string(), string()) -> [{string(), string()}].
mk_delete_batch_entry(N, MessageId, ReceiptHandle) ->
    N0 = integer_to_list(N),
    [
        ?DELETE_BATCH_FIELD(N0, ".Id", MessageId),
        ?DELETE_BATCH_FIELD(N0, ".ReceiptHandle", ReceiptHandle)
    ].

-spec mk_ch_visibility_batch_entry(integer(), string(), string(), 0..43200) -> [{string(), integer() | string()}].
mk_ch_visibility_batch_entry(N, MessageId, ReceiptHandle, VisibilityTimeout) ->
    N0 = integer_to_list(N),
    [
        ?CHANGE_VISIBILITY_BATCH_FIELD(N0, ".Id", MessageId),
        ?CHANGE_VISIBILITY_BATCH_FIELD(N0, ".ReceiptHandle", ReceiptHandle),
        ?CHANGE_VISIBILITY_BATCH_FIELD(N0, ".VisibilityTimeout", VisibilityTimeout)
    ].

decode_send_batch_successful(Nodes) ->
    [erlcloud_xml:decode([
                             {id, "Id", text},
                             {message_id, "MessageId", text},
                             {md5_of_message_body, "MD5OfMessageBody", text}
                         ], Node) || Node <- Nodes].

decode_id_batch_successful(Nodes) ->
    [erlcloud_xml:decode([{id, "Id", text}], Node) || Node <- Nodes].

decode_batch_result_error(Nodes) ->
    [erlcloud_xml:decode([
                             {id, "Id", text},
                             {sender_fault, "SenderFault", boolean},
                             {code, "Code", text},
                             {message, "Message", text}
                         ], Node) || Node <- Nodes].

default_config() -> erlcloud_aws:default_config().

sqs_simple_request(Config, QueueName, Action, Params) ->
    sqs_request(Config, QueueName, Action, Params),
    ok.

sqs_xml_request(Config, QueueName, Action, Params) ->
    case erlcloud_aws:aws_request_xml4(post, Config#aws_config.sqs_protocol,
                                  Config#aws_config.sqs_host, Config#aws_config.sqs_port,
                                  queue_path(QueueName),
                                  [{"Action", Action}, {"Version", ?API_VERSION}|Params],
                                  "sqs",Config)
    of
        {ok, Body} ->
            Body;
        {error, Reason} ->
            erlang:error({aws_error, Reason})
    end.

sqs_request(Config, QueueName, Action, Params) ->
    case erlcloud_aws:aws_request4(post, Config#aws_config.sqs_protocol,
                              Config#aws_config.sqs_host, Config#aws_config.sqs_port,
                              queue_path(QueueName),
                              [{"Action", Action}, {"Version", ?API_VERSION}|Params],
                              "sqs", Config)
    of
        {ok, Body} ->
            Body;
        {error, Reason} ->
            erlang:error({aws_error, Reason})
    end.

queue_path([$/|QueueName]) -> [$/ |erlcloud_http:url_encode(QueueName)];
queue_path([$h,$t,$t,$p|_] = URL) ->
    re:replace(URL, "^https?://[^/]*", "", [{return, list}]);
queue_path(QueueName) -> [$/ | erlcloud_http:url_encode(QueueName)].

encode_send_msg_attribute_name(delay_seconds) -> "DelaySeconds";
encode_send_msg_attribute_name(message_group_id) -> "MessageGroupId";
encode_send_msg_attribute_name(message_deduplication_id) -> "MessageDeduplicationId".

encode_message_attributes(Attributes) ->
    Map = fun(Attribute, Acc) ->
                  [encode_message_attribute(Attribute) | Acc]
          end,
    erlcloud_aws:param_list(lists:reverse(lists:foldl(Map, [], Attributes)), "MessageAttribute").

encode_message_attribute({Key, Value}) ->
    [
      {"Value.DataType", encode_message_attribute_type(Value)},
     encode_message_attribute_value(Value),
      {"Name", Key}
    ].

encode_message_attribute_value({_CustomType, Value}) ->
    encode_message_attribute_value(Value);
encode_message_attribute_value(Value) when is_integer(Value) ->
    {"Value.StringValue", integer_to_list(Value)};
encode_message_attribute_value(Value) when is_float(Value) ->
    {"Value.StringValue", float_to_list(Value, [{decimals, 12}, compact])};
encode_message_attribute_value(Value) when is_list(Value) ->
    {"Value.StringValue", Value};
encode_message_attribute_value(Value) when is_binary(Value) ->
    {"Value.BinaryValue", binary_to_list(Value)}.

encode_message_attribute_type({CustomType, Value})
  when is_list(CustomType) andalso is_integer(Value) ->
    ["Number.int.", CustomType];
encode_message_attribute_type({CustomType, Value})
  when is_list(CustomType) andalso is_float(Value) ->
    ["Number.float.", CustomType];
encode_message_attribute_type({CustomType, Value})
  when is_list(CustomType) andalso is_list(Value) ->
    ["String.", CustomType];
encode_message_attribute_type({CustomType, Value})
  when is_list(CustomType) andalso is_binary(Value) ->
    ["Binary.", CustomType];
encode_message_attribute_type(Value) when is_integer(Value) ->
    "Number.int";
encode_message_attribute_type(Value) when is_float(Value) ->
    "Number.float";
encode_message_attribute_type(Value) when is_list(Value) ->
    "String";
encode_message_attribute_type(Value) when is_binary(Value) ->
    "Binary".
