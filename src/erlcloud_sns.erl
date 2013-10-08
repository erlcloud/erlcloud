%%% @doc Amazon SNS
%%% @todo add all the missing functions for the different actions
-module(erlcloud_sns).
-author('elbrujohalcon@inaka.net').

-export([add_permission/3, add_permission/4,
         create_platform_endpoint/2, create_platform_endpoint/3,
         create_platform_endpoint/4, create_platform_endpoint/5,
         create_platform_endpoint/6,
         publish_to_topic/2, publish_to_topic/3, publish_to_topic/4,
         publish_to_topic/5, publish_to_target/2, publish_to_target/3,
         publish_to_target/4, publish_to_target/5, publish/5
         ]).

-include("erlcloud.hrl").
-include("erlcloud_aws.hrl").
-define(API_VERSION, "2010-03-31").

-type sns_endpoint_attribute() :: custom_user_data
                                | enabled
                                | token.

-type sns_permission() :: all
                        | add_permission
                        | confirm_subscription
                        | create_platform_application
                        | create_platform_endpoint
                        | create_topic
                        | delete_endpoint
                        | delete_platform_application
                        | delete_topic
                        | get_endpoint_attributes
                        | get_platform_application_attributes
                        | get_subscription_attributes
                        | get_topic_attributes
                        | list_endpoints_by_platform_application
                        | list_platform_applications
                        | list_subscriptions
                        | list_subscriptions_by_topic
                        | list_topics
                        | publish
                        | remove_permission
                        | set_endpoint_attributes
                        | set_platform_application_attributes
                        | set_subscription_attributes
                        | set_topic_attributes
                        | subscribe
                        | unsubscribe.
-type sns_acl() :: [{string(), sns_permission()}].

-type sns_message() :: string() | jsx:json_term().

-export_type([sns_acl/0, sns_endpoint_attribute/0]).

-spec add_permission/3 :: (string(), string(), sns_acl()) -> ok.
-spec add_permission/4 :: (string(), string(), sns_acl(), aws_config()) -> ok.

add_permission(TopicArn, Label, Permissions) ->
    add_permission(TopicArn, Label, Permissions, default_config()).

add_permission(TopicArn, Label, Permissions, Config)
  when is_list(TopicArn),
       is_list(Label), length(Label) =< 80,
       is_list(Permissions) ->
    sns_simple_request(Config, "AddPermission",
                       [{"Label", Label}, {"TopicArn", TopicArn}
                       | erlcloud_aws:param_list(
                            encode_permissions(Permissions),
                            {"AWSAccountId.member", "ActionName.member"})]).



-spec create_platform_endpoint/2 :: (string(), string()) -> string().
-spec create_platform_endpoint/3 :: (string(), string(), string()) -> string().
-spec create_platform_endpoint/4 :: (string(), string(), string(), [{sns_endpoint_attribute(), string()}]) -> string().
-spec create_platform_endpoint/5 :: (string(), string(), string(), [{sns_endpoint_attribute(), string()}], aws_config()) -> string().
-spec create_platform_endpoint/6 :: (string(), string(), string(), [{sns_endpoint_attribute(), string()}], string(), string()) -> string().

create_platform_endpoint(PlatformApplicationArn, Token) ->
    create_platform_endpoint(PlatformApplicationArn, Token, "").

create_platform_endpoint(PlatformApplicationArn, Token, CustomUserData) ->
    create_platform_endpoint(PlatformApplicationArn, Token, CustomUserData, []).

create_platform_endpoint(PlatformApplicationArn, Token, CustomUserData, Attributes) ->
    create_platform_endpoint(PlatformApplicationArn, Token, CustomUserData, Attributes, default_config()).

create_platform_endpoint(PlatformApplicationArn, Token, CustomUserData, Attributes, Config) ->
    Doc =
        sns_xml_request(
            Config, "CreatePlatformEndpoint",
            [{"PlatformApplicationArn", PlatformApplicationArn},
             {"Token",                  Token},
             {"CustomUserData",         CustomUserData}
             | encode_attributes(Attributes)
             ]),
    erlcloud_xml:get_text(
        "/CreatePlatformEndpointResponse/CreatePlatformEndpointResult/EndpointArn", Doc).

create_platform_endpoint(PlatformApplicationArn, Token, CustomUserData, Attributes, AccessKeyID, SecretAccessKey) ->
    create_platform_endpoint(PlatformApplicationArn, Token, CustomUserData, Attributes, new_config(AccessKeyID, SecretAccessKey)).



-spec publish_to_topic/2 :: (string(), sns_message()) -> string().
-spec publish_to_topic/3 :: (string(), sns_message(), undefined|string()) -> string().
-spec publish_to_topic/4 :: (string(), sns_message(), undefined|string(), aws_config()) -> string().
-spec publish_to_topic/5 :: (string(), sns_message(), undefined|string(), string(), string()) -> string().
-spec publish_to_target/2 :: (string(), sns_message()) -> string().
-spec publish_to_target/3 :: (string(), sns_message(), undefined|string()) -> string().
-spec publish_to_target/4 :: (string(), sns_message(), undefined|string(), aws_config()) -> string().
-spec publish_to_target/5 :: (string(), sns_message(), undefined|string(), string(), string()) -> string().
-spec publish/5 :: (topic|target, string(), sns_message(), undefined|string(), aws_config()) -> string().

publish_to_topic(TopicArn, Message) ->
    publish_to_topic(TopicArn, Message, undefined).
publish_to_topic(TopicArn, Message, Subject) ->
    publish_to_topic(TopicArn, Message, Subject, default_config()).
publish_to_topic(TopicArn, Message, Subject, Config) ->
    publish(topic, TopicArn, Message, Subject, Config).
publish_to_topic(TopicArn, Message, Subject, AccessKeyID, SecretAccessKey) ->
    publish_to_topic(TopicArn, Message, Subject, new_config(AccessKeyID, SecretAccessKey)).
publish_to_target(TargetArn, Message) ->
    publish_to_target(TargetArn, Message, undefined).
publish_to_target(TargetArn, Message, Subject) ->
    publish_to_target(TargetArn, Message, Subject, default_config()).
publish_to_target(TargetArn, Message, Subject, Config) ->
    publish(target, TargetArn, Message, Subject, Config).
publish_to_target(TargetArn, Message, Subject, AccessKeyID, SecretAccessKey) ->
    publish_to_target(TargetArn, Message, Subject, new_config(AccessKeyID, SecretAccessKey)).
publish(Type, RecipientArn, Message, Subject, Config) ->
    RecipientParam =
        case Type of
            topic -> [{"TopicArn", RecipientArn}];
            target -> [{"TargetArn", RecipientArn}]
        end,
    MessageParams =
        case Message of
            [{_,_} |_] ->
                EncodedMessage = jsx:encode(Message),
                [{"Message",            EncodedMessage},
                 {"MessageStructure",   "json"}];
            Message ->
                [{"Message", Message}]
        end,
    SubjectParam =
        case Subject of
            undefined -> [];
            Subject -> [{"Subject", Subject}]
        end,
    Doc =
        sns_xml_request(
            Config, "Publish",
            RecipientParam ++ MessageParams ++ SubjectParam),
    erlcloud_xml:get_text(
        "/PublishResponse/PublishResult/MessageId", Doc).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% PRIVATE
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

encode_attributes(Attributes) ->
    lists:foldl(
        fun({Index, {Key, Value}}, Acc) ->
            Prefix = "Attributes.entry." ++ integer_to_list(Index),
            [{Prefix ++ ".key",     encode_attribute_name(Key)},
             {Prefix ++ ".value",   Value} | Acc]
        end, [], lists:zip(lists:seq(1, length(Attributes)), Attributes)).

encode_attribute_name(custom_user_data) -> "CustomUserData";
encode_attribute_name(enabled) -> "Enabled";
encode_attribute_name(token) -> "Token".

encode_permissions(Permissions) ->
    [encode_permission(P) || P <- Permissions].

encode_permission({AccountId, Permission}) ->
    {AccountId,
     case Permission of
         all -> "*";
         add_permission -> "AddPermission";
         confirm_subscription -> "ConfirmSubscription";
         create_platform_application -> "CreatePlatformApplication";
         create_platform_endpoint -> "CreatePlatformEndpoint";
         create_topic -> "CreateTopic";
         delete_endpoint -> "DeleteEndpoint";
         delete_platform_application -> "DeletePlatformApplication";
         delete_topic -> "DeleteTopic";
         get_endpoint_attributes -> "GetEndpointAttributes";
         get_platform_application_attributes -> "GetPlatformApplicationAttributes";
         get_subscription_attributes -> "GetSubscriptionAttributes";
         get_topic_attributes -> "GetTopicAttributes";
         list_endpoints_by_platform_application -> "ListEndpointsByPlatformApplication";
         list_platform_applications -> "ListPlatformApplications";
         list_subscriptions -> "ListSubscriptions";
         list_subscriptions_by_topic -> "ListSubscriptionsByTopic";
         list_topics -> "ListTopics";
         publish -> "Publish";
         remove_permission -> "RemovePermission";
         set_endpoint_attributes -> "SetEndpointAttributes";
         set_platform_application_attributes -> "SetPlatformApplicationAttributes";
         set_subscription_attributes -> "SetSubscriptionAttributes";
         set_topic_attributes -> "SetTopicAttributes";
         subscribe -> "Subscribe";
         unsubscribe -> "Unsubscribe"
      end}.



default_config() -> erlcloud_aws:default_config().

new_config(AccessKeyID, SecretAccessKey) ->
    #aws_config{
       access_key_id=AccessKeyID,
       secret_access_key=SecretAccessKey
      }.

sns_simple_request(Config, Action, Params) ->
    sns_request(Config, Action, Params),
    ok.

sns_xml_request(Config, Action, Params) ->
    erlcloud_aws:aws_request_xml(
        post, "http", Config#aws_config.sns_host, undefined, "/",
        [{"Action", Action}, {"Version", ?API_VERSION} | Params],
        Config).

sns_request(Config, Action, Params) ->
    erlcloud_aws:aws_request(
        post, "http", Config#aws_config.sns_host, undefined, "/",
        [{"Action", Action}, {"Version", ?API_VERSION} | Params],
        Config).
