%% Amazon Simple Notification Service (SNS)

-module(erlcloud_sns).

-include_lib("erlcloud/include/erlcloud.hrl").
-include_lib("erlcloud/include/erlcloud_aws.hrl").
-include_lib("erlcloud/include/erlcloud_sns.hrl").
-include_lib("xmerl/include/xmerl.hrl").

-export([configure/2, configure/3, new/2, new/3]).

%% SNS API Functions
-export([
    %% Users
    create_topic/1, create_topic/2, 
    delete_topic/1, delete_topic/2,
    set_topic_attributes/3, set_topic_attributes/4,
    subscribe/3, subscribe/4
]).

-import(erlcloud_xml, [get_text/1, get_text/2, get_text/3, get_bool/2, get_list/2, get_integer/2]).

-define(API_VERSION, "2010-03-31").

-spec(new/2 :: (string(), string()) -> aws_config()).
new(AccessKeyID, SecretAccessKey) ->
    #aws_config{access_key_id=AccessKeyID,
                secret_access_key=SecretAccessKey}.

-spec(new/3 :: (string(), string(), string()) -> aws_config()).
new(AccessKeyID, SecretAccessKey, Host) ->
    #aws_config{access_key_id=AccessKeyID,
                secret_access_key=SecretAccessKey,
                ec2_host=Host}.

-spec(configure/2 :: (string(), string()) -> ok).
configure(AccessKeyID, SecretAccessKey) ->
    put(aws_config, new(AccessKeyID, SecretAccessKey)),
    ok.

-spec(configure/3 :: (string(), string(), string()) -> ok).
configure(AccessKeyID, SecretAccessKey, Host) ->
    put(aws_config, new(AccessKeyID, SecretAccessKey, Host)),
    ok.


%%
%%
-spec(create_topic/1 :: (string() | aws_config()) -> {ok, Arn::string()} | {error, Reason::term()}).
create_topic(TopicName) ->
    create_topic(TopicName, default_config()).

-spec(create_topic/2 :: (string(), aws_config()) -> {ok, Arn::string()} | {error, Reason::term()}).
create_topic(TopicName, Config) 
    when is_record(Config, aws_config) ->
        case sns_query(Config, "CreateTopic", [{"Name", TopicName}]) of
            {ok, Doc} ->
                Node = hd(xmerl_xpath:string("/CreateTopicResponse/CreateTopicResult", Doc)),
                Topic = get_text("TopicArn", Node),
                {ok, Topic};
            {error, _} = Error ->
                Error
        end.

-spec(delete_topic/1 :: (string() | aws_config()) -> proplist()).
delete_topic(TopicArn) ->
    delete_topic(TopicArn, default_config()).

-spec(delete_topic/2 :: (string(), aws_config()) -> proplist()).
delete_topic(TopicArn, Config) 
    when is_record(Config, aws_config) ->
        sns_simple_query(Config, "DeleteTopic", [{"TopicArn", TopicArn}]).

-spec(set_topic_attributes/3 :: (sns_topic_attribute_name(), string(), string()) -> proplist()).
set_topic_attributes(AttributeName, AttributeValue, TopicArn) ->
    set_topic_attributes(AttributeName, AttributeValue, TopicArn, default_config()).

-spec(set_topic_attributes/4 :: (sns_topic_attribute_name(), string(), string(), aws_config()) -> proplist()).
set_topic_attributes(AttributeName, AttributeValue, TopicArn, Config)
    when is_record(Config, aws_config) ->
        sns_simple_query(Config, "SetTopicAttributes", [
            {"AttributeName", AttributeName},
            {"AttributeValue", AttributeValue},
            {"TopicArn", TopicArn}]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Subscribe
%% 
%% Description
%% 
%% Prepares to subscribe an endpoint by sending the endpoint a confirmation message. 
%% To actually create a subscription, the endpoint owner must call the ConfirmSubscription action with the token from the confirmation message. 
%% Confirmation tokens are valid for three days.
%% 
%% Parameters
%%  Endpoint
%%      The endpoint that you want to receive notifications. Endpoints vary by protocol:
%%      
%%      For the http protocol, the endpoint is an URL beginning with "http://"
%%      For the https protocol, the endpoint is a URL beginning with "https://"
%%      For the email protocol, the endpoint is an email address
%%      For the email-json protocol, the endpoint is an email address
%%      For the sms protocol, the endpoint is a phone number of an SMS-enabled device
%%      For the sqs protocol, the endpoint is the ARN of an Amazon SQS queue
%%      For the application protocol, the endpoint is the EndpointArn of a mobile app and device.
%%      Type: String
%%      
%%      Required: No
%%      
%%  Protocol
%%      The protocol you want to use. Use sns_subscribe_protocol_type() defined atoms.
%%      Supported protocols include:
%%      
%%      http -- delivery of JSON-encoded message via HTTP POST
%%      https -- delivery of JSON-encoded message via HTTPS POST
%%      email -- delivery of message via SMTP
%%      'email-json' -- delivery of JSON-encoded message via SMTP
%%      sms -- delivery of message via SMS
%%      sqs -- delivery of JSON-encoded message to an Amazon SQS queue
%%      application -- delivery of JSON-encoded message to an EndpointArn for a mobile app and device.
%%      Type: String
%%      
%%      Required: Yes
%%      
%%  TopicArn
%%      The ARN of the topic you want to subscribe to.
%%      
%%      Type: String
%%      
%%      Required: Yes
%% Returns:
%%  on success: {ok, [{arn, SubscriptionArn}]}

-spec(subscribe/3 :: (string(), sns_subscribe_protocol_type(), string()) -> {ok, Arn::string()} | {error, Reason::term()}).
subscribe(Endpoint, Protocol, TopicArn) ->
    subscribe(Endpoint, Protocol, TopicArn, default_config()).

-spec(subscribe/4 :: (string(), sns_subscribe_protocol_type(), string(), aws_config()) -> {ok, Arn::string()} | {error, Reason::term()}).
subscribe(Endpoint, Protocol, TopicArn, Config)
    when is_record(Config, aws_config) ->
         case sns_query(Config, "Subscribe", [
                {"Endpoint", Endpoint},
                {"Protocol", atom_to_list(Protocol)},
                {"TopicArn", TopicArn}]) of
            {ok, Doc} ->
                Node= hd(xmerl_xpath:string("/SubscribeResponse/SubscribeResult", Doc)),
                Topic = get_text("SubscriptionArn", Node),
                {ok, Topic};
            {error, _} = Error ->
                Error
        end.

sns_simple_query(Config, Action, Params) ->
    case sns_query(Config, Action, Params) of
        {ok, _Doc} -> ok;
        {error, Reason} -> {error, Reason}
    end
.
    
sns_query(Config, Action, Params) ->
    sns_query(Config, Action, Params, ?API_VERSION).

sns_query(Config, Action, Params, ApiVersion) ->
    QParams = [{"Action", Action}, {"Version", ApiVersion}|Params],
    erlcloud_aws:aws_request_xml2(post, Config#aws_config.sns_host,
                                  "/", QParams, Config).

default_config() -> erlcloud_aws:default_config().


