%% -*- mode: erlang;erlang-indent-level: 4;indent-tabs-mode: nil -*-

%% @author Ransom Richardson <ransom@ransomr.net>
%% @author Nicholas Lundgaard <nalundgaard@gmail.com>
%% @doc
%% An Erlang interface to Amazon's Simple Email Service (SES)
%%
%% Methods not implemented:
%%  * DeleteIdentityPolicy
%%  * DeleteVerifiedEmailAddress (deprecated; use DeleteIdentity)
%%  * GetIdentityPolicies
%%  * ListIdentityPolicies
%%  * ListVerifiedEmailAddresses (deprecated; use ListIdentities)
%%  * PutIdentityPolicy
%%  * VerifyEmailAddress (deprecated; use VerifyEmailIdentity)
%%
%% @end


-module(erlcloud_ses).

-export([configure/2, configure/3, new/2, new/3]).
-export([create_custom_verification_email_template/6, create_custom_verification_email_template/7]).

-export([delete_custom_verification_email_template/1, delete_custom_verification_email_template/2]).

-export([delete_identity/1, delete_identity/2]).

-export([get_custom_verification_email_template/1, get_custom_verification_email_template/2]).

-export([get_identity_dkim_attributes/1, get_identity_dkim_attributes/2]).
-export([get_identity_notification_attributes/1, get_identity_notification_attributes/2]).
-export([get_identity_verification_attributes/1, get_identity_verification_attributes/2]).

-export([get_send_quota/0, get_send_quota/1]).
-export([get_send_statistics/0, get_send_statistics/1]).

-export([list_custom_verification_email_templates/0, list_custom_verification_email_templates/1]).

-export([list_identities/0, list_identities/1, list_identities/2]).

-export([send_custom_verification_email/2, send_custom_verification_email/3]).

-export([send_email/4, send_email/5, send_email/6]).

-export([send_raw_email/1, send_raw_email/2, send_raw_email/3]).

-export([set_identity_dkim_enabled/2, set_identity_dkim_enabled/3]).
-export([set_identity_feedback_forwarding_enabled/2, set_identity_feedback_forwarding_enabled/3]).
-export([set_identity_notification_topic/3, set_identity_notification_topic/4]).
-export([set_identity_headers_in_notifications_enabled/3, set_identity_headers_in_notifications_enabled/4]).

-export([update_custom_verification_email_template/2, update_custom_verification_email_template/3]).

-export([verify_domain_dkim/1, verify_domain_dkim/2]).
-export([verify_email_identity/1, verify_email_identity/2]).
-export([verify_domain_identity/1, verify_domain_identity/2]).


-include("erlcloud.hrl").
-include("erlcloud_aws.hrl").

-define(API_VERSION, "2010-12-01").

%%%------------------------------------------------------------------------------
%%% Common types
%%%------------------------------------------------------------------------------

-type custom_template_attribute_names() :: template_name | from_email_address | template_subject | template_content | success_redirect_url | failure_redirect_url .
-type custom_template_attributes() :: [{custom_template_attribute_names(), string()}].

-type identity() :: string() | binary().

%% identities (as input) can be a single identity or a list of them.
-type identities() :: identity() | [identity()].

-type email() :: string() | binary().

%% emails (as input) can be a single email or a list of them.
-type emails() :: email() | [email()].

-type domain() :: string() | binary().

-type verification_status() :: pending | success | failed | temporary_failure | not_started.

-type notification_type() :: bounce | complaint | delivery.

-export_type([custom_template_attribute_names/0, custom_template_attributes/0,
              identity/0, identities/0,
              email/0, emails/0,
              domain/0,
              verification_status/0,
              notification_type/0]).

%%%------------------------------------------------------------------------------
%%% Library initialization
%%%------------------------------------------------------------------------------

-spec new(string(), string()) -> aws_config().
new(AccessKeyID, SecretAccessKey) ->
    #aws_config{access_key_id=AccessKeyID,
                secret_access_key=SecretAccessKey}.

-spec new(string(), string(), string()) -> aws_config().
new(AccessKeyID, SecretAccessKey, Host) ->
    #aws_config{access_key_id=AccessKeyID,
                secret_access_key=SecretAccessKey,
                ses_host=Host}.

-spec configure(string(), string()) -> ok.
configure(AccessKeyID, SecretAccessKey) ->
    put(aws_config, new(AccessKeyID, SecretAccessKey)),
    ok.

-spec configure(string(), string(), string()) -> ok.
configure(AccessKeyID, SecretAccessKey, Host) ->
    put(aws_config, new(AccessKeyID, SecretAccessKey, Host)),
    ok.

default_config() -> erlcloud_aws:default_config().

%%------------------------------------------------------------------------------
%% @doc
%% SES API:
%% [https://docs.aws.amazon.com/ses/latest/APIReference/API_CreateCustomVerificationEmailTemplate.html]
%%
%% ===Example===
%%
%% Creates a new custom verification email template.
%%
%% `
%%  ok = erlcloud_ses:create_custom_verification_email_template_result(
%%          "templaneName",
%%          "support@example.com",
%%          "Welcome to support",
%%          "limited html content",
%%          "https://www.example.com/success",
%%          "https://www.example.com/failure" ).
%% '
%%
%% Please consult the following for what is and not allowed in the HTML content parameter
%% [https://docs.aws.amazon.com/ses/latest/DeveloperGuide/custom-verification-emails.html#custom-verification-emails-faq]
%% @end
%%------------------------------------------------------------------------------

-type create_custom_verification_email_template_result() :: ok | { error, term() }.

-spec create_custom_verification_email_template( string(), string(), string(), string(), string(), string() ) -> create_custom_verification_email_template_result().
create_custom_verification_email_template(TemplateName, FromEmailAddress, TemplateSubject, TemplateContent, SuccessRedirectionURL, FailureRedirectionURL) ->
  create_custom_verification_email_template(TemplateName, FromEmailAddress, TemplateSubject, TemplateContent, SuccessRedirectionURL, FailureRedirectionURL ,default_config()).

-spec create_custom_verification_email_template(string(), string(), string(), string(), string(), string(), aws_config()) ->
  create_custom_verification_email_template_result().
create_custom_verification_email_template(TemplateName, FromEmailAddress, TemplateSubject, TemplateContent, SuccessRedirectionURL, FailureRedirectionURL, Config) ->
  Params = encode_params([{template_name, TemplateName},
    {from_email_address, FromEmailAddress},
    {template_subject, TemplateSubject},
    {template_content, TemplateContent},
    {success_redirect_url, SuccessRedirectionURL},
    {failure_redirect_url, FailureRedirectionURL}]),
  case ses_request(Config, "CreateCustomVerificationEmailTemplate", Params) of
    {ok, _Doc} -> ok;
    {error, Reason} -> {error, Reason}
  end.

%%------------------------------------------------------------------------------
%% @doc
%% SES API:
%% [https://docs.aws.amazon.com/ses/latest/APIReference/API_DeleteCustomVerificationEmailTemplate.html]
%%
%% ===Example===
%%
%% Deletes an existing custom verification email template.
%%
%% `
%%  ok = erlcloud_ses:delete_custom_verification_email_template("templaneName").
%% '
%%
%% @end
%%------------------------------------------------------------------------------

-type delete_custom_verification_email_template_result() :: ok | {error,term()}.

-spec delete_custom_verification_email_template(string()) -> delete_custom_verification_email_template_result().
delete_custom_verification_email_template(TemplateName) ->
  delete_custom_verification_email_template(TemplateName, default_config()).

-spec delete_custom_verification_email_template(string(), aws_config()) -> delete_custom_verification_email_template_result().
delete_custom_verification_email_template(TemplateName, Config) ->
  Params = encode_params([{template_name, TemplateName }]),
  case ses_request(Config, "DeleteCustomVerificationEmailTemplate", Params) of
    {ok, _Doc} -> ok;
    {error, Reason} -> {error, Reason}
  end.


%%%------------------------------------------------------------------------------
%%% DeleteIdentity
%%%------------------------------------------------------------------------------

-type delete_identity_result() :: ok | {error, term()}.

delete_identity(Identity) ->
    delete_identity(Identity, default_config()).

%%------------------------------------------------------------------------------
%% @doc
%% SES API:
%% [http://docs.aws.amazon.com/ses/2010-12-01/APIReference/API_DeleteIdentity.html]
%%
%% ===Example===
%%
%% Delete an identity.
%%
%% `
%%  ok = erlcloud_ses:delete_identity(<<"user@example.com">>).
%% '
%%
%% @end
%%------------------------------------------------------------------------------

-spec delete_identity(identity(), aws_config()) -> delete_identity_result().
delete_identity(Identity, Config) ->
    Params = encode_params([{identity, Identity}]),
    case ses_request(Config, "DeleteIdentity", Params) of
        {ok, _Doc} -> ok;
        {error, Reason} -> {error, Reason}
    end.

%%------------------------------------------------------------------------------
%% @doc
%% SES API:
%% [https://docs.aws.amazon.com/ses/latest/APIReference/API_GetCustomVerificationEmailTemplate.html]
%%
%% ===Example===
%%
%% Returns the custom email verification template for the template name you specify.
%%
%% `
%%  {ok,[{template_name,"templateName"},
%%     {from_email_address,"support@example.com"},
%%     {template_subject,"Welcome to Example.com"},
%%     {template_content,"<html>\n<head></head>\n <body style=\"font-family:sans-serif;\">\n<h1 style=
%%          \"text-align:center\">Ready to start with Example.com</h1>\n<p>Example.com is very happy to
%%          welcome you to the ACME system Please click\non the link below to activate
%%          your account.</p>\n</body>\n</html>"},
%%     {success_redirect_url,"https://www.example.com/success"},
%%     {failure_redirect_url,"http://example.arilia.com"}]} =
%%        erlcloud_ses:delete_custom_verification_email_template("templaneName").
%% '
%%
%% @end
%%------------------------------------------------------------------------------

-type get_custom_verification_email_template_result() :: {ok, custom_template_attributes()} | {error, term()}.

-spec get_custom_verification_email_template(string()) -> get_custom_verification_email_template_result().
get_custom_verification_email_template(TemplateName) ->
  get_custom_verification_email_template(TemplateName, default_config()).

-spec get_custom_verification_email_template( string() , aws_config() ) -> get_custom_verification_email_template_result().
get_custom_verification_email_template(TemplateName, Config) ->
  Params = encode_params([{template_name, TemplateName}]),
  case ses_request(Config, "GetCustomVerificationEmailTemplate", Params) of
    { ok , Doc } ->
      {ok, erlcloud_xml:decode(
        [{template_name, "GetCustomVerificationEmailTemplateResult/TemplateName", text},
          {from_email_address, "GetCustomVerificationEmailTemplateResult/FromEmailAddress", text},
          {template_subject, "GetCustomVerificationEmailTemplateResult/TemplateSubject", text},
          {template_content, "GetCustomVerificationEmailTemplateResult/TemplateContent", text},
          {success_redirect_url, "GetCustomVerificationEmailTemplateResult/SuccessRedirectionURL", text},
          {failure_redirect_url, "GetCustomVerificationEmailTemplateResult/FailureRedirectionURL", text}],
        Doc)};
    {error, Reason} -> {error, Reason}
  end.

%%%------------------------------------------------------------------------------
%%% GetIdentityDkimAttributes
%%%------------------------------------------------------------------------------

-type dkim_attributes() :: [{email(), [{dkim_enabled, boolean()} |
                                       {dkim_verification_status, verification_status()} |
                                       {dkim_tokens,[string()]}]}].

-type get_identity_dkim_attributes_result() :: {ok, [{dkim_attributes, dkim_attributes()}]} |
                                               {error, term()}.

get_identity_dkim_attributes(Identities) ->
    get_identity_dkim_attributes(Identities, default_config()).

%%------------------------------------------------------------------------------
%% @doc
%% SES API:
%% [http://docs.aws.amazon.com/ses/2010-12-01/APIReference/API_GetIdentityDkimAttributes.html]
%%
%% ===Example===
%%
%% Get DKIM attributes for an identity.
%%
%% `
%%  {ok, [{dkim_attributes, [{"amazon.com",
%%                            [{dkim_enabled, true},
%%                             {dkim_verification_status, success},
%%                             {dkim_tokens,["vvjuipp74whm76gqoni7qmwwn4w4qusjiainivf6f",
%%                                           "3frqe7jn4obpuxjpwpolz6ipb3k5nvt2nhjpik2oy",
%%                                           "wrqplteh7oodxnad7hsl4mixg2uavzneazxv5sxi2"]}]}]}]} =
%% erlcloud_ses:get_identity_dkim_attributes(["amazon.com"]).
%% '
%%
%% @end
%%------------------------------------------------------------------------------

-spec get_identity_dkim_attributes(identities(), aws_config()) ->
        get_identity_dkim_attributes_result().
get_identity_dkim_attributes(Identities, Config) ->
    Params = encode_params([{identities, Identities}]),
    case ses_request(Config, "GetIdentityDkimAttributes", Params) of
        {ok, Doc} ->
            {ok, erlcloud_xml:decode([{dkim_attributes, "GetIdentityDkimAttributesResult/DkimAttributes/entry", fun decode_dkim_attributes/1}],
                                     Doc)};
        {error, Reason} -> {error, Reason}
    end.


%%%------------------------------------------------------------------------------
%%% GetIdentityNotificationAttributes
%%%------------------------------------------------------------------------------

-type notification_attributes() :: [{email(),
                                     [{forwarding_enabled, boolean()} |
                                      {bounce_topic, string()} |
                                      {complaint_topic, string()} |
                                      {delivery_topic, string()}]}].

-type get_identity_notification_attributes_result() :: {ok, [{notification_attributes, notification_attributes()}]} |
                                                       {error, term()}.

get_identity_notification_attributes(Identities) ->
    get_identity_notification_attributes(Identities, default_config()).

%%------------------------------------------------------------------------------
%% @doc
%% SES API:
%% [http://docs.aws.amazon.com/ses/2010-12-01/APIReference/API_GetIdentityNotificationAttributes.html]
%%
%% ===Example===
%%
%% Get notification attributes for an identity.
%%
%% `
%%  {ok, [{notification_attributes, [{"user@example.com",
%%                                    [{forwarding_enabled, true},
%%                                     {bounce_topic, "arn:aws:sns:us-east-1:123456789012:example"},
%%                                     {complaint_topic, "arn:aws:sns:us-east-1:123456789012:example"},
%%                                     {delivery_topic, "arn:aws:sns:us-east-1:123456789012:example"}]}]}]} = 
%% erlcloud_ses:get_identity_notification_attributes(["user@example.com"]).
%% '
%%
%% @end
%%------------------------------------------------------------------------------

-spec get_identity_notification_attributes(identities(), aws_config()) ->
        get_identity_notification_attributes_result().
get_identity_notification_attributes(Identities, Config) ->
    Params = encode_params([{identities, Identities}]),
    case ses_request(Config, "GetIdentityNotificationAttributes", Params) of
        {ok, Doc} ->
            {ok, erlcloud_xml:decode([{notification_attributes, "GetIdentityNotificationAttributesResult/NotificationAttributes/entry", fun decode_notification_attributes/1}],
                                     Doc)};
        {error, Reason} -> {error, Reason}
    end.


%%%------------------------------------------------------------------------------
%%% GetIdentityVerificationAttributes
%%%------------------------------------------------------------------------------

-type verification_attributes() :: [{email(),
                                     [{verification_status, verification_status()} |
                                      {verification_token, string()}]}].

-type get_identity_verification_attributes_result() :: {ok, [{verification_attributes, verification_attributes()}]} |
                                                       {error, term()}.

get_identity_verification_attributes(Identities) ->
    get_identity_verification_attributes(Identities, default_config()).

%%------------------------------------------------------------------------------
%% @doc
%% SES API:
%% [http://docs.aws.amazon.com/ses/2010-12-01/APIReference/API_GetIdentityVerificationAttributes.html]
%%
%% ===Example===
%%
%% Get verification attributes for an identity.
%%
%% `
%%  {ok, [{verification_attributes, [{"domain.com",
%%                                    [{verification_status, pending},
%%                                     {verification_token, "QTKknzFg2J4ygwa+XvHAxUl1hyHoY0gVfZdfjIedHZ0="}]},
%%                                   {"user@domain.com",
%%                                    [{verification_status, pending}]}]}]} =
%% erlcloud_ses:get_identity_verification_attributes(["user@domain.com", "domain.com"]).
%% '
%%
%% @end
%%------------------------------------------------------------------------------

-spec get_identity_verification_attributes(identities(), aws_config()) ->
        get_identity_verification_attributes_result().
get_identity_verification_attributes(Identities, Config) ->
    Params = encode_params([{identities, Identities}]),
    case ses_request(Config, "GetIdentityVerificationAttributes", Params) of
        {ok, Doc} ->
            {ok, erlcloud_xml:decode([{verification_attributes, "GetIdentityVerificationAttributesResult/VerificationAttributes/entry", fun decode_verification_attributes/1}],
                                     Doc)};
        {error, Reason} -> {error, Reason}
    end.


%%%------------------------------------------------------------------------------
%%% GetSendQuota
%%%------------------------------------------------------------------------------

-type get_send_quota_result() :: {ok, [{sent_last_24_hours, float()} |
                                       {max_24_hour_send, float()} |
                                       {max_send_rate, float()}]} |
                                 {error, term()}.

get_send_quota() ->
    get_send_quota(default_config()).

%%------------------------------------------------------------------------------
%% @doc
%% SES API:
%% [http://docs.aws.amazon.com/ses/2010-12-01/APIReference/API_GetSendQuota.html]
%%
%% ===Example===
%%
%% Get the send quota.
%%
%% `
%%  {ok, [{sent_last_24_hours, 127.0},
%%        {max_24_hour_send, 200.0},
%%        {max_send_rate, 1.0}]} = 
%% erlcloud_ses:get_send_quota().
%% '
%%
%% @end
%%------------------------------------------------------------------------------

-spec get_send_quota(aws_config()) -> get_send_quota_result().
get_send_quota(Config) ->
    case ses_request(Config, "GetSendQuota", []) of
        {ok, Doc} ->
            {ok, erlcloud_xml:decode([{sent_last_24_hours, "GetSendQuotaResult/SentLast24Hours", float},
                                      {max_24_hour_send, "GetSendQuotaResult/Max24HourSend", float},
                                      {max_send_rate, "GetSendQuotaResult/MaxSendRate", float}],
                                     Doc)};
        {error, Reason} -> {error, Reason}
    end.


%%%------------------------------------------------------------------------------
%%% GetSendStatistics
%%%------------------------------------------------------------------------------

-type send_data_point() :: [{delivery_attempts, integer()} |
                            {timestamp, string()} |
                            {rejects, integer()} |
                            {bounces, integer()} |
                            {complaints, integer()}].

-type get_send_statistics_result() :: {ok, [{send_data_points, send_data_point()}]} |
                                      {error, term()}.

get_send_statistics() ->
    get_send_statistics(default_config()).

%%------------------------------------------------------------------------------
%% @doc
%% SES API:
%% [http://docs.aws.amazon.com/ses/2010-12-01/APIReference/API_GetSendStatistics.html]
%%
%% ===Example===
%%
%% Get send statistics.
%%
%% `
%%  {ok,[{send_data_points, [[{delivery_attempts,7},
%%                            {timestamp,"2011-08-03T06:53:00Z"},
%%                            {rejects,0},
%%                            {bounces,0},
%%                            {complaints,0}],
%%                           ...
%%                          ]}]} = 
%% erlcloud_ses:get_send_statistics().
%% '
%%
%% @end
%%------------------------------------------------------------------------------

-spec get_send_statistics(aws_config()) -> get_send_statistics_result().
get_send_statistics(Config) ->
    case ses_request(Config, "GetSendStatistics", []) of
        {ok, Doc} ->
            {ok, erlcloud_xml:decode([{send_data_points, "GetSendStatisticsResult/SendDataPoints/member", fun decode_send_data_points/1}],
                                     Doc)};
        {error, Reason} -> {error, Reason}
    end.

%%------------------------------------------------------------------------------
%% @doc
%% SES API:
%% [https://docs.aws.amazon.com/ses/latest/APIReference/API_ListCustomVerificationEmailTemplates.html]
%%
%% ===Example===
%%
%% Lists the existing custom verification email templates for your account in the current AWS Region.
%%
%% `
%%  {ok,[{custom_templates,[[{template_name,"template1"},
%%                         {from_email_address,"support@example.com"},
%%                         {template_subject,"Welcome to support"},
%%                         {success_redirect_url,"https://www.example.com/success"},
%%                         {failure_redirect_url,"https://www.example.com/failure"}],
%%                        [{template_name,"template2"},
%%                         {from_email_address,"applications@example.com"},
%%                         {template_subject,"Welcome to Applications"},
%%                         {success_redirect_url,"https://www.example.com/success"},
%%                         {failure_redirect_url,"https://www.example.com/failure"}]]}]} =
%%                  erlcloud_ses:list_custom_verification_email_templates().
%% '
%%
%% @end
%%------------------------------------------------------------------------------

-type list_custom_verification_email_templates_result() :: {ok, [{custom_templates, [custom_template_attributes()]}]} | {error , term()}.

-spec list_custom_verification_email_templates() -> list_custom_verification_email_templates_result().
list_custom_verification_email_templates() ->
  list_custom_verification_email_templates(default_config()).

-spec list_custom_verification_email_templates(aws_config()) -> list_custom_verification_email_templates_result().
list_custom_verification_email_templates(Config) ->
  Params = [{"MaxResults",50}],
  case ses_request(Config, "ListCustomVerificationEmailTemplates", Params) of
    { ok , Doc } ->
      {ok, erlcloud_xml:decode([{custom_templates, "ListCustomVerificationEmailTemplatesResult/CustomVerificationEmailTemplates/member", fun decode_custom_template_entry/1},
        {next_token, "ListCustomVerificationEmailTemplatesResult/NextToken", optional_text}],
        Doc)};
    {error, Reason } -> {error, Reason}
  end.

%%%------------------------------------------------------------------------------
%%% ListIdentities
%%%------------------------------------------------------------------------------

-type identity_type() :: email_address | domain.

-type list_identities_opt() :: {identity_type, identity_type()} |
                               {max_items, pos_integer()} |
                               {next_token, string() | binary()}.
-type list_identities_opts() :: [list_identities_opt()].

-type list_identities_result() :: {ok, [{identities, [string()]} |
                                        {next_token, string()}]} |
                                  {error, term()}.

list_identities() ->
    list_identities([], default_config()).

list_identities(#aws_config{} = Config) ->
    list_identities([], Config);
list_identities(Opts) ->
    list_identities(Opts, default_config()).

%%------------------------------------------------------------------------------
%% @doc
%% SES API:
%% [http://docs.aws.amazon.com/ses/2010-12-01/APIReference/API_ListIdentities.html]
%%
%% ===Example===
%%
%% List identities.
%%
%% `
%%  {ok, [{identities, ["example.com", "user@example.com"]}]} =
%% erlcloud_ses:list_identities().
%% '
%%
%% All supported inputs.
%%
%% `
%%  {ok, [{identities, ["example.com"]},
%%        {next_token, "..."}]} = 
%% erlcloud_ses:list_identities([{identity_type, domain},
%%                               {max_items, 1},
%%                               {next_token, "..."}]).
%% '
%% @end
%%------------------------------------------------------------------------------

-spec list_identities(list_identities_opts(),
                      aws_config()) ->
        list_identities_result().
list_identities(Opts, Config) ->
    Params = encode_params(Opts),
    case ses_request(Config, "ListIdentities", Params) of
        {ok, Doc} ->
            {ok, erlcloud_xml:decode([{identities, "ListIdentitiesResult/Identities/member", list},
                                      {next_token, "ListIdentitiesResult/NextToken", optional_text}],
                                     Doc)};
        {error, Reason} -> {error, Reason}
    end.

%%------------------------------------------------------------------------------
%% @doc
%% SES API:
%% [https://docs.aws.amazon.com/ses/latest/APIReference/API_SendCustomVerificationEmail.html]
%%
%% ===Example===
%%
%% Send a custom verification email.
%%
%% `
%%  {ok, [{message_id, "abcdefghijkllkjdlkj"}]} =
%%            erlcloud_ses:send_custom_verification_email_result("newclient@newco.com", "templateName").
%% '
%%
%% @end
%%------------------------------------------------------------------------------

-type send_custom_verification_email_result() :: {ok, term()} | {error, term()}.

-spec send_custom_verification_email(string(), string()) -> send_custom_verification_email_result().
send_custom_verification_email(EmailAddress, TemplateName) ->
  send_custom_verification_email(EmailAddress, TemplateName, default_config()).

-spec send_custom_verification_email(string(), string(), aws_config()) -> send_custom_verification_email_result().
send_custom_verification_email(EmailAddress, TemplateName, Config) ->
  Params = encode_params([{email_address, EmailAddress},
    {template_name, TemplateName}]),
  case ses_request(Config, "SendCustomVerificationEmail", Params) of
    {ok, Doc} ->
      {ok, erlcloud_xml:decode([{message_id, "SendCustomVerificationEmailResult/MessageId", text}], Doc)};
    {error, Reason} -> {error, Reason}
  end.

%%%------------------------------------------------------------------------------
%%% SendEmail
%%%------------------------------------------------------------------------------

-type send_email_destination_pair() :: {to_addresses, emails()} |
                                       {cc_addresses, emails()} |
                                       {bcc_addresses, emails()}.

-type send_email_destination() :: email() | [send_email_destination_pair()].

-type send_email_content() :: string() | binary() |
                              [{ Type :: data | charset, string() | binary()}].

-type send_email_body() :: string() | binary() |
                           [{Type :: text | html, send_email_content()}].

-type send_email_subject() :: send_email_content().

-type send_email_source() :: email().

-type send_email_opt() :: {configuration_set_name, string() | binary()} |
                          {reply_to_addresses, emails()} |
                          {return_path, email()} |
                          {tags, [{string() | binary(), string() | binary()}]}.

-type send_email_opts() :: [send_email_opt()].

-type send_email_result() :: {ok, [{message_id, string()}]} | {error, term()}.


send_email(Destination, Body, Subject, Source) ->
    send_email(Destination, Body, Subject, Source, [], default_config()).
                                                       
send_email(Destination, Body, Subject, Source, #aws_config{} = Config) ->
    send_email(Destination, Body, Subject, Source, [], Config);
send_email(Destination, Body, Subject, Source, Opts) ->
    send_email(Destination, Body, Subject, Source, Opts, default_config()).

%%------------------------------------------------------------------------------
%% @doc
%% SES API:
%% [http://docs.aws.amazon.com/ses/2010-12-01/APIReference/API_SendEmail.html]
%%
%% ===Example===
%%
%% Send a simple email.
%%
%% `
%% {ok, [{message_id, "00000131d51d2292-159ad6eb-077c-46e6-ad09-ae7c05925ed4-000000"}]} =
%%     erlcloud_ses:send_email(<<"a@to.com">>, <<"Email Body">>, <<"Subject">>,
%%                             <<"b@from.com">>, []).
%% '
%%
%% All supported inputs.
%%
%% `
%%  {ok, [{message_id, "00000131d51d2292-159ad6eb-077c-46e6-ad09-ae7c05925ed4-000000"}]} =
%% erlcloud_ses:send_email([{bcc_addresses, [<<"a@bcc.com">>, "b@bcc.com"]},
%%                          {cc_addresses, [<<"c@cc.com">>]},
%%                          {to_addresses, ["d@to.com"]}],
%%                         [{html, [{charset, "html charset"},
%%                                  {data, "html data"}]},
%%                          {text, [{charset, "text charset"},
%%                                  {data, "text data"}]}],
%%                         [{charset, "subject charset"},
%%                          {data, "subject data"}],
%%                         "e@from.com",
%%                         [{reply_to_addresses, [<<"f@reply.com">>, "g@reply.com"]},
%%                          {return_path, "return path"}]).
%% '
%% @end
%%------------------------------------------------------------------------------

-spec send_email(send_email_destination(),
                 send_email_body(),
                 send_email_subject(),
                 send_email_source(),
                 send_email_opts(),
                 aws_config()) ->
        send_email_result().
send_email(Destination, Body, Subject, Source, Opts, Config) ->
    Params = encode_params([{destination, Destination},
                            {body, Body},
                            {subject, Subject},
                            {source, Source},
                            {send_email_opts, Opts}]),
    case ses_request(Config, "SendEmail", Params) of
        {ok, Doc} ->
            {ok, erlcloud_xml:decode([{message_id, "SendEmailResult/MessageId", text}], Doc)};
        {error, Reason} -> {error, Reason}
    end.

%%%------------------------------------------------------------------------------
%%% SendRawEmail
%%%------------------------------------------------------------------------------

-type send_raw_email_message() :: binary() | string().

-type send_raw_email_opt() :: {source, email()} |
                              {destinations, emails()}.

-type send_raw_email_opts() :: [send_raw_email_opt()].

-type send_raw_email_result() :: {ok, [{message_id, string()}]} | {error, term()}.


send_raw_email(RawMessage) ->
    send_raw_email(RawMessage, [], default_config()).

send_raw_email(RawMessage, #aws_config{} = Config) ->
    send_raw_email(RawMessage, [], Config);
send_raw_email(RawMessage, Opts) ->
    send_raw_email(RawMessage, Opts, default_config()).

%%------------------------------------------------------------------------------
%% @doc
%% SES API:
%% [https://docs.aws.amazon.com/ses/latest/APIReference/API_SendRawEmail.html]
%%
%% ===Example===
%%
%% Send a raw email.
%%
%% `
%% {ok, [{message_id, "00000131d51d2292-159ad6eb-077c-46e6-ad09-ae7c05925ed4-000000"}]} =
%%     erlcloud_ses:send_raw_email(<<"From: b@from.com\nTo: a@to.com\nSubject: Subject\nMIME-Version: 1.0\nContent-type: Multipart/Mixed; boundary=\"NextPart\"\n\n--NextPart\nContent-Type: text/plain\n\nEmail Body\n\n--NextPart--">>, []).
%% '
%%
%% All supported inputs.
%%
%% `
%%  {ok, [{message_id, "00000131d51d2292-159ad6eb-077c-46e6-ad09-ae7c05925ed4-000000"}]} =
%% erlcloud_ses:send_email(<<"To: d@to.com\nCC: c@cc.com\nBCC: a@bcc.com, b@bcc.com\nSubject: Subject\nMIME-Version: 1.0\nContent-type: Multipart/Mixed; boundary=\"NextPart\"\n\n--NextPart\nContent-Type: text/plain\n\nEmail Body\n\n--NextPart--">>,
%%                         [{destinations, [<<"a@bcc.com">>, "b@bcc.com", <<"c@cc.com">>, "d@to.com"]},
%%                          {source, "e@from.com"}].
%% '
%% @end
%%------------------------------------------------------------------------------

-spec send_raw_email(send_raw_email_message(),
                     send_raw_email_opts(),
                     aws_config()) ->
        send_raw_email_result().
send_raw_email(RawMessage, Opts, Config) ->
    Params = encode_params([{raw_message, RawMessage},
                            {send_raw_email_opts, Opts}]),
    case ses_request(Config, "SendRawEmail", Params) of
        {ok, Doc} ->
            {ok, erlcloud_xml:decode([{message_id, "SendRawEmailResult/MessageId", text}], Doc)};
        {error, Reason} -> {error, Reason}
    end.


%%%------------------------------------------------------------------------------
%%% SetIdentityDkimEnabled
%%%------------------------------------------------------------------------------

-type set_identity_dkim_enabled_result() :: ok | {error, term()}.


set_identity_dkim_enabled(Identity, DkimEnabled) ->
    set_identity_dkim_enabled(Identity, DkimEnabled, default_config()).

%%------------------------------------------------------------------------------
%% @doc
%% SES API:
%% [http://docs.aws.amazon.com/ses/2010-12-01/APIReference/API_SetIdentityDkimEnabled.html]
%%
%% ===Example===
%%
%% Enable DKIM for an identity.
%%
%% `
%%  ok = erlcloud_ses:set_identity_dkim_enabled(<<"user@example.com">>, true).
%% '
%%
%% @end
%%------------------------------------------------------------------------------

-spec set_identity_dkim_enabled(identity(), boolean(), aws_config()) ->
        set_identity_dkim_enabled_result().
set_identity_dkim_enabled(Identity, DkimEnabled, Config) ->
   Params = encode_params([{identity, Identity},
                            {dkim_enabled, DkimEnabled}]),
    case ses_request(Config, "SetIdentityDkimEnabled", Params) of
        {ok, _Doc} -> ok;
        {error, Reason} -> {error, Reason}
    end.


%%%------------------------------------------------------------------------------
%%% SetIdentityFeedbackForwardingEnabled
%%%------------------------------------------------------------------------------

-type set_identity_feedback_forwarding_enabled_result() :: ok | {error, term()}.


set_identity_feedback_forwarding_enabled(Identity, ForwardingEnabled) ->
    set_identity_feedback_forwarding_enabled(Identity, ForwardingEnabled, default_config()).

%%------------------------------------------------------------------------------
%% @doc
%% SES API:
%% [http://docs.aws.amazon.com/ses/2010-12-01/APIReference/API_SetIdentityFeedbackForwardingEnabled.html]
%%
%% ===Example===
%%
%% Enable feedback forwarding for an identity.
%%
%% `
%%  ok = erlcloud_ses:set_identity_feedback_forwarding_enabled(<<"user@example.com">>, true).
%% '
%%
%% @end
%%------------------------------------------------------------------------------

-spec set_identity_feedback_forwarding_enabled(identity(), boolean(), aws_config()) ->
        set_identity_feedback_forwarding_enabled_result().
set_identity_feedback_forwarding_enabled(Identity, ForwardingEnabled, Config) ->
    Params = encode_params([{identity, Identity},
                            {forwarding_enabled, ForwardingEnabled}]),
    case ses_request(Config, "SetIdentityFeedbackForwardingEnabled", Params) of
        {ok, _Doc} -> ok;
        {error, Reason} -> {error, Reason}
    end.


%%%------------------------------------------------------------------------------
%%% SetIdentityNotificationTopic
%%%------------------------------------------------------------------------------

-type sns_topic() :: string() | binary().

-type set_identity_notification_topic_result() :: ok | {error, term()}.

set_identity_notification_topic(Identity, NotificationType, SnsTopic) ->
    set_identity_notification_topic(Identity, NotificationType, SnsTopic, default_config()).

%%------------------------------------------------------------------------------
%% @doc
%% SES API:
%% [http://docs.aws.amazon.com/ses/2010-12-01/APIReference/API_SetIdentityFeedbackForwardingEnabled.html]
%%
%% ===Example===
%%
%% Set the bounce notification topic for an identity.
%%
%% `
%%  ok = erlcloud_ses:set_identity_notification_topic(<<"user@example.com">>,
%%                                                     bounce,
%%                                                     <<"arn:aws:sns:us-east-1:123456789012:example">>).
%% '
%%
%% @end
%%------------------------------------------------------------------------------

-spec set_identity_notification_topic(identity(),
                                      notification_type(),
                                      sns_topic(),
                                      aws_config()) ->
        set_identity_notification_topic_result().
set_identity_notification_topic(Identity, NotificationType, SnsTopic, Config) ->
    Params = encode_params([{identity, Identity},
                            {notification_type, NotificationType},
                            {sns_topic, SnsTopic}]),
    case ses_request(Config, "SetIdentityNotificationTopic", Params) of
        {ok, _Doc} -> ok;
        {error, Reason} -> {error, Reason}
    end.


%%%------------------------------------------------------------------------------
%%% SetIdentityHeadersInNotificationsEnabled
%%%------------------------------------------------------------------------------

set_identity_headers_in_notifications_enabled(Identity, NotificationType, Enabled) ->
    set_identity_headers_in_notifications_enabled(Identity, NotificationType, Enabled, default_config()).

%%------------------------------------------------------------------------------
%% @doc
%% SES API:
%% [https://docs.aws.amazon.com/ses/latest/APIReference/API_SetIdentityHeadersInNotificationsEnabled.html]
%%
%% ===Example===
%%
%% Enable headers in notifications for an identity.
%%
%% `
%%  ok = erlcloud_ses:set_identity_headers_in_notifications_enabled(<<"user@example.com">>, bounce, true).
%% '
%%
%% @end
%%------------------------------------------------------------------------------

-spec set_identity_headers_in_notifications_enabled(identity(),
                                                    notification_type(),
                                                    boolean(),
                                                    aws_config()) ->
    ok | {error, term()}.
set_identity_headers_in_notifications_enabled(Identity, NotificationType, Enabled, Config) ->
    Params = encode_params([{identity, Identity},
                            {notification_type, NotificationType},
                            {enabled, Enabled}]),
    case ses_request(Config, "SetIdentityHeadersInNotificationsEnabled", Params) of
        {ok, _Doc} -> ok;
        {error, Reason} -> {error, Reason}
    end.

%%------------------------------------------------------------------------------
%% @doc
%% SES API:
%% [https://docs.aws.amazon.com/ses/latest/APIReference/API_UpdateCustomVerificationEmailTemplate.html]
%%
%%  Template attributes
%%   { template_name , string() }
%%   { from_email_address , string() }
%%   { template_subject , string() }
%%   { template_content , string() }
%%   { success_redirect_url , string() }
%%   { failure_redirect_url , string() }
%%
%% ===Example===
%%
%% Updates an existing custom verification email template.
%%
%% `
%%  ok = erlcloud_ses:update_custom_verification_email_template("templateName",
%%                                                     [{template_subject, "New subject"},
%%                                                      {from_email_address, "support2@example.com"}]).
%% '
%% Please consult the following for what is and not allowed in the HTML content parameter
%% [https://docs.aws.amazon.com/ses/latest/DeveloperGuide/custom-verification-emails.html#custom-verification-emails-faq]
%% @end
%%------------------------------------------------------------------------------

-type update_custom_verification_email_template_result() :: ok | { error, term() }.

-spec update_custom_verification_email_template(string(), [{atom(), string()}]) -> update_custom_verification_email_template_result().
update_custom_verification_email_template(TemplateName, Attributes) ->
  update_custom_verification_email_template(TemplateName, Attributes,default_config()).

-spec update_custom_verification_email_template(string(), [{atom(), string()}], aws_config()) -> update_custom_verification_email_template_result().
update_custom_verification_email_template(TemplateName, Attributes, Config) ->
  Params = encode_params([{template_name, TemplateName} | Attributes]),
  case ses_request(Config, "UpdateCustomVerificationEmailTemplate", Params) of
    {ok, _Doc} -> ok ;
    {error, Reason} -> {error, Reason}
  end.

%%%------------------------------------------------------------------------------
%%% VerifyDomainDkim
%%%------------------------------------------------------------------------------

-type verify_domain_dkim_result() :: {ok, [{dkim_tokens, [string()]}]} | 
                                     {error, term()}.

verify_domain_dkim(Domain) ->
    verify_domain_dkim(Domain, default_config()).

%%------------------------------------------------------------------------------
%% @doc
%% SES API:
%% [http://docs.aws.amazon.com/ses/2010-12-01/APIReference/API_VerifyDomainIdentity.html]
%%
%% ===Example===
%%
%% Get DKIM tokens for a domain.
%%
%% `
%%  {ok, [{dkim_tokens, ["vvjuipp74whm76gqoni7qmwwn4w4qusjiainivf6sf",
%%                       "3frqe7jn4obpuxjpwpolz6ipb3k5nvt2nhjpik2oy",
%%                       "wrqplteh7oodxnad7hsl4mixg2uavzneazxv5sxi2"]}]} =
%% erlcloud_ses:verify_domain_dkim(<<"example.com">>).
%% '
%%
%% @end
%%------------------------------------------------------------------------------

-spec verify_domain_dkim(domain(), aws_config()) -> verify_domain_dkim_result().
verify_domain_dkim(Domain, Config) ->
    Params = encode_params([{domain, Domain}]),
    case ses_request(Config, "VerifyDomainDkim", Params) of
        {ok, Doc} ->
            {ok, erlcloud_xml:decode([{dkim_tokens, "VerifyDomainDkimResult/DkimTokens/member", list}], Doc)};
        {error, Reason} -> {error, Reason}
    end.


%%%------------------------------------------------------------------------------
%%% VerifyDomainIdentity
%%%------------------------------------------------------------------------------

-type verify_domain_identity_result() :: {ok, [{verification_token, string()}]} | 
                                         {error, term()}.

verify_domain_identity(Domain) ->
    verify_domain_identity(Domain, default_config()).

%%------------------------------------------------------------------------------
%% @doc
%% SES API:
%% [http://docs.aws.amazon.com/ses/2010-12-01/APIReference/API_VerifyDomainIdentity.html]
%%
%% ===Example===
%%
%% Verify a domain.
%%
%% `
%%  {ok, [{verification_token, "QTKknzFg2J4ygwa+XvHAxUl1hyHoY0gVfZdfjIedHZ0="}] =
%% erlcloud_ses:verify_domain_identity(<<"example.com">>).
%% '
%%
%% @end
%%------------------------------------------------------------------------------

-spec verify_domain_identity(domain(), aws_config()) -> verify_domain_identity_result().
verify_domain_identity(Domain, Config) ->
    Params = encode_params([{domain, Domain}]),
    case ses_request(Config, "VerifyDomainIdentity", Params) of
        {ok, Doc} ->
            {ok, erlcloud_xml:decode([{verification_token, "VerifyDomainIdentityResult/VerificationToken", text}], Doc)};
        {error, Reason} -> {error, Reason}
    end.

%%%------------------------------------------------------------------------------
%%% VerifyEmailIdentity
%%%------------------------------------------------------------------------------

-type verify_email_identity_result() :: ok | {error, term()}.

verify_email_identity(EmailAddress) ->
    verify_email_identity(EmailAddress, default_config()).

%%------------------------------------------------------------------------------
%% @doc
%% SES API:
%% [http://docs.aws.amazon.com/ses/2010-12-01/APIReference/API_VerifyEmailIdentity.html]
%%
%% ===Example===
%%
%% Verify an email address.
%%
%% `
%%  ok = erlcloud_ses:verify_email_identity(<<"user@example.com">>).
%% '
%%
%% @end
%%------------------------------------------------------------------------------

-spec verify_email_identity(email(), aws_config()) -> verify_email_identity_result().
verify_email_identity(EmailAddress, Config) ->
    Params = encode_params([{email_address, EmailAddress}]),
    case ses_request(Config, "VerifyEmailIdentity", Params) of
        {ok, _Doc} -> ok;
        {error, Reason} -> {error, Reason}
    end.


%%%------------------------------------------------------------------------------
%%% Encoders
%%%------------------------------------------------------------------------------

encode_params(Params) ->
    encode_params(Params, []).

encode_params([], Acc) ->
    lists:reverse(Acc);
encode_params([{body, Body} | T], Acc) ->
    encode_params(T, encode_body(Body, Acc));
encode_params([{destination, Destination} | T], Acc) ->
    encode_params(T, encode_destination(Destination, Acc));
encode_params([{email_address, EmailAddress} | T], Acc) when is_list(EmailAddress); is_binary(EmailAddress) ->
    encode_params(T, [{"EmailAddress", EmailAddress} | Acc]);
encode_params([{enabled, Enabled} | T], Acc) when is_boolean(Enabled) ->
    encode_params(T, [{"Enabled", Enabled} | Acc]);
encode_params([{dkim_enabled, DkimEnabled} | T], Acc) when is_boolean(DkimEnabled) ->
    encode_params(T, [{"DkimEnabled", DkimEnabled} | Acc]);
encode_params([{domain, Domain} | T], Acc) when is_list(Domain); is_binary(Domain) ->
    encode_params(T, [{"Domain", Domain} | Acc]);
encode_params([{forwarding_enabled, ForwardingEnabled} | T], Acc) when is_boolean(ForwardingEnabled) ->
    encode_params(T, [{"ForwardingEnabled", ForwardingEnabled} | Acc]);
encode_params([{identities, Identities} | T], Acc) ->
    encode_params(T, encode_list("Identities", Identities, Acc));
encode_params([{identity, Identity} | T], Acc) ->
    encode_params(T, [{"Identity", Identity} | Acc]);
encode_params([{identity_type, IdentityType} | T], Acc) ->
    encode_params(T, encode_identity_type(IdentityType, Acc));
encode_params([{max_items, MaxItems} | T], Acc) when is_integer(MaxItems), MaxItems > 0 ->
    encode_params(T, [{"MaxItems", integer_to_list(MaxItems)} | Acc]);
encode_params([{next_token, NextToken} | T], Acc) when is_list(NextToken); is_binary(NextToken) ->
    encode_params(T, [{"NextToken", NextToken} | Acc]);
encode_params([{notification_type, NotificationType} | T], Acc) ->
    encode_params(T, encode_notification_type(NotificationType, Acc));
encode_params([{raw_message, RawMessage} | T], Acc) ->
    encode_params(T,  [{"RawMessage.Data", base64:encode(RawMessage)} | Acc]);
encode_params([{send_email_opts, Opts} | T], Acc) ->
    encode_params(T, encode_opts(Opts, Acc));
encode_params([{send_raw_email_opts, Opts} | T], Acc) ->
    encode_params(T, encode_raw_opts(Opts, Acc));
encode_params([{sns_topic, SnsTopic} | T], Acc) when is_list(SnsTopic); is_binary(SnsTopic) ->
    encode_params(T, [{"SnsTopic", SnsTopic} | Acc]);
encode_params([{source, Source} | T], Acc) when is_list(Source); is_binary(Source) ->
    encode_params(T, [{"Source", Source} | Acc]);
encode_params([{subject, Subject} | T], Acc) ->
    encode_params(T, encode_content("Message.Subject", Subject, Acc));
encode_params([{template_name, TemplateName} | T], Acc)
    when is_list(TemplateName); is_binary(TemplateName) ->
      encode_params(T, [{"TemplateName", TemplateName} | Acc]);
encode_params([{from_email_address, FromEmailAddress} | T], Acc)
    when is_list(FromEmailAddress); is_binary(FromEmailAddress) ->
      encode_params(T, [{"FromEmailAddress", FromEmailAddress} | Acc]);
encode_params([{template_subject, TemplateSubject} | T], Acc)
    when is_list(TemplateSubject); is_binary(TemplateSubject) ->
      encode_params(T, [{"TemplateSubject", TemplateSubject} | Acc]);
encode_params([{template_content, TemplateContent} | T], Acc)
    when is_list(TemplateContent); is_binary(TemplateContent) ->
      encode_params(T, [{"TemplateContent", TemplateContent} | Acc]);
encode_params([{success_redirect_url, SuccessRedirectionURL} | T], Acc)
    when is_list(SuccessRedirectionURL); is_binary(SuccessRedirectionURL) ->
      encode_params(T, [{"SuccessRedirectionURL", SuccessRedirectionURL} | Acc]);
encode_params([{failure_redirect_url, FailureRedirectionURL} | T], Acc)
    when is_list(FailureRedirectionURL); is_binary(FailureRedirectionURL) ->
      encode_params(T, [{"FailureRedirectionURL", FailureRedirectionURL} | Acc]);
encode_params([Option | _], _Acc) ->
    error({erlcloud_ses, {invalid_parameter, Option}}).

encode_list(Prefix, List, Acc) ->
    encode_list(Prefix, List, 1, Acc).

encode_list(_, [], _, Acc) ->
    Acc;
encode_list(Prefix, [{Name, Value} | T], N, Acc) when is_list(Name) orelse is_binary(Name), 
                                                      is_list(Value) orelse is_binary(Value) ->
    encode_list(Prefix, T, N + 1,
     [{encode_param_index(Prefix, N) ++ ".Name", Name},
      {encode_param_index(Prefix, N) ++ ".Value", Value} | Acc]);
encode_list(Prefix, [H | T], N, Acc) when is_list(H); is_binary(H) ->
    encode_list(Prefix, T, N + 1, [{encode_param_index(Prefix, N), H} | Acc]);
encode_list(Prefix, V, N, Acc) when is_list(V); is_binary(V) ->
    encode_list(Prefix, [V], N, Acc).
  
encode_param_index(Prefix, N) -> Prefix ++ ".member." ++ integer_to_list(N).
  
encode_destination_pairs([], Acc) ->
    Acc;
encode_destination_pairs([{bcc_addresses, List} | T], Acc) ->
    encode_destination_pairs(T, encode_list("Destination.BccAddresses", List, Acc));
encode_destination_pairs([{cc_addresses, List} | T], Acc) ->
    encode_destination_pairs(T, encode_list("Destination.CcAddresses", List, Acc));
encode_destination_pairs([{to_addresses, List} | T], Acc) ->
    encode_destination_pairs(T, encode_list("Destination.ToAddresses", List, Acc)).

encode_destination([{_,_} | _] = Destination, Acc) ->
    %% List of pairs
    encode_destination_pairs(Destination, Acc);
encode_destination([ [_|_] | _ ] = ToAddresses, Acc) ->
    %% List of strings
    encode_destination_pairs([{to_addresses, ToAddresses}], Acc);
encode_destination(ToAddress, Acc) when is_list(ToAddress); is_binary(ToAddress) ->
    %% Single entry
    encode_destination_pairs([{to_addresses, [ToAddress]}], Acc).

encode_destinations([Dest | _T] = Destinations, Acc) when is_list(Dest); is_binary(Dest) ->
    encode_list("Destinations", Destinations, Acc);
encode_destinations(ToAddress, Acc) when is_list(ToAddress); is_binary(ToAddress) ->
    %% Single entry
    encode_destinations([ToAddress], Acc).

encode_content_pairs(_, [], Acc) ->
    Acc;
encode_content_pairs(Prefix, [{charset, Charset} | T], Acc) ->
    encode_content_pairs(Prefix, T, [{Prefix ++ ".Charset", Charset} | Acc]);
encode_content_pairs(Prefix, [{data, Data} | T], Acc) ->
    encode_content_pairs(Prefix, T, [{Prefix ++ ".Data", Data} | Acc]).

encode_content(Prefix, [{_,_} | _] = Content, Acc) ->
    %% List of pairs
    encode_content_pairs(Prefix, Content, Acc);
encode_content(Prefix, Data, Acc) when is_list(Data); is_binary(Data) ->
    %% Single entry
    encode_content_pairs(Prefix, [{data, Data}], Acc).

encode_body_pairs([], Acc) ->
    Acc;
encode_body_pairs([{html, Content} | T], Acc) ->
    encode_body_pairs(T, encode_content("Message.Body.Html", Content, Acc));
encode_body_pairs([{text, Content} | T], Acc) ->
    encode_body_pairs(T, encode_content("Message.Body.Text", Content, Acc)).

encode_body([{_,_} | _] = Body, Acc) ->
    %% List of pairs
    encode_body_pairs(Body, Acc);
encode_body(Body, Acc) when is_list(Body); is_binary(Body) ->
    %% Single entry
    encode_body_pairs([{text, Body}], Acc).
    
encode_opts([], Acc) ->
    Acc;
encode_opts([{configuration_set_name, ConfigurationSet} | T], Acc) ->
    encode_opts(T,  [{"ConfigurationSetName", ConfigurationSet} | Acc]);
encode_opts([{reply_to_addresses, List} | T], Acc) ->
    encode_opts(T, encode_list("ReplyToAddresses", List, Acc));
encode_opts([{return_path, ReturnPath} | T], Acc) ->
    encode_opts(T, [{"ReturnPath", ReturnPath} | Acc]);
encode_opts([{tags, Tags} | T], Acc) ->
    encode_opts(T, encode_list("Tags", Tags, Acc)).

encode_raw_opts([], Acc) ->
    Acc;
encode_raw_opts([{source, Source} | T], Acc) when is_list(Source); is_binary(Source) ->
    encode_raw_opts(T, [{"Source", Source} | Acc]);
encode_raw_opts([{destinations, Destination} | T], Acc) ->
    encode_raw_opts(T, encode_destinations(Destination, Acc)).

encode_identity_type(email_address, Acc) ->
    [{"IdentityType", "EmailAddress"} | Acc];
encode_identity_type(domain, Acc) ->
    [{"IdentityType", "Domain"} | Acc];
encode_identity_type(IdentityType, _Acc) ->
    error({erlcloud_ses, {invalid_parameter, {identity_type, IdentityType}}}).

encode_notification_type(bounce, Acc) ->
    [{"NotificationType", "Bounce"} | Acc];
encode_notification_type(complaint, Acc) ->
    [{"NotificationType", "Complaint"} | Acc];
encode_notification_type(delivery, Acc) ->
    [{"NotificationType", "Delivery"} | Acc];
encode_notification_type(NotificationType, _Acc) ->
    error({erlcloud_ses, {invalid_parameter, {notification_type, NotificationType}}}).

%%%------------------------------------------------------------------------------
%%% Decoders
%%%------------------------------------------------------------------------------

-spec decode_verification_status(string()) -> verification_status().
decode_verification_status("Pending") -> pending; 
decode_verification_status("Success") -> success; 
decode_verification_status("Failed") -> failed; 
decode_verification_status("TemporaryFailure") -> temporary_failure; 
decode_verification_status("NotStarted") -> not_started.


decode_dkim_attributes(DkimAttributesDoc) ->
    [{erlcloud_xml:get_text("key", Entry),
      erlcloud_xml:decode([{dkim_enabled, "value/DkimEnabled", boolean},
                           {dkim_verification_status, "value/DkimVerificationStatus", {value, fun decode_verification_status/1}},
                           {dkim_tokens, "value/DkimTokens/member", list}],
                          Entry)}
        || Entry <- DkimAttributesDoc].


decode_notification_attributes(NotificationAttributesDoc) ->
    [{erlcloud_xml:get_text("key", Entry),
      erlcloud_xml:decode([{forwarding_enabled, "value/ForwardingEnabled", boolean},
                           {headers_in_bounce_notifications_enabled,
                            "value/HeadersInBounceNotificationsEnabled",
                            boolean},
                           {headers_in_complaint_notifications_enabled,
                            "value/HeadersInComplaintNotificationsEnabled",
                            boolean},
                           {headers_in_delivery_notifications_enabled,
                            "value/HeadersInDeliveryNotificationsEnabled",
                            boolean},
                           {bounce_topic, "value/BounceTopic", optional_text},
                           {complaint_topic, "value/ComplaintTopic", optional_text},
                           {delivery_topic, "value/DeliveryTopic", optional_text}],
                          Entry)}
        || Entry <- NotificationAttributesDoc].


decode_verification_attributes(VerificationAttributesDoc) ->
    [{erlcloud_xml:get_text("key", Entry),
      erlcloud_xml:decode([{verification_status, "value/VerificationStatus", {value, fun decode_verification_status/1}},
                           {verification_token, "value/VerificationToken", optional_text}],
                          Entry)}
        || Entry <- VerificationAttributesDoc].


decode_send_data_points(SendDataPointsDoc) ->
    [erlcloud_xml:decode([{delivery_attempts, "DeliveryAttempts", integer},
                          {timestamp, "Timestamp", text},
                          {rejects, "Rejects", integer},
                          {bounces, "Bounces", integer},
                          {complaints, "Complaints", integer}],
                         Entry)
        || Entry <- SendDataPointsDoc].

%% Added for decoding a custom template entry with the ListCustomVerificationEmailTemplates command
%% Notice that the ListCustomVerificationEmailTemplates API does not return the TemplateContent
%% You must use GetCustomVerificationEmailTemplate to retrieve TemplateContent
decode_custom_template_entry(CustomTemplatesDoc) ->
  [ erlcloud_xml:decode([
    { template_name , "TemplateName" , text } ,
    { from_email_address , "FromEmailAddress" , text  } ,
    { template_subject , "TemplateSubject" , text  } ,
    { success_redirect_url , "SuccessRedirectionURL" , text } ,
    { failure_redirect_url , "FailureRedirectionURL" , text } ],
    Entry)
    || Entry <- CustomTemplatesDoc ].

decode_error_code("IncompleteSignature") -> incomplete_signature;
decode_error_code("InternalFailure") -> internal_failure;
decode_error_code("InvalidAction") -> invalid_action;
decode_error_code("InvalidClientTokenId") -> invalid_client_token_id;
decode_error_code("InvalidParameterCombination") -> invalid_parameter_combination;
decode_error_code("InvalidParameterValue") -> invalid_parameter_value;
decode_error_code("InvalidQueryParameter") -> invalid_query_parameter;
decode_error_code("MalformedQueryString") -> malformed_query_string;
decode_error_code("MessageRejected") -> message_rejected;
decode_error_code("MissingAction") -> missing_action;
decode_error_code("MissingAuthenticationToken") -> missing_authentication_token;
decode_error_code("MissingParameter") -> missing_parameter;
decode_error_code("OptInRequired") -> opt_in_required;
decode_error_code("RequestExpired") -> request_expired;
decode_error_code("ServiceUnavailable") -> service_unavailable;
decode_error_code("Throttling") -> throttling;
decode_error_code("ValidationError") -> validation_error;
decode_error_code("LimitExceeded") -> limit_exceeded;
decode_error_code("ConfigurationSetDoesNotExist") -> configuration_set_does_not_exist;
decode_error_code("CustomVerificationEmailTemplateDoesNotExist") -> custom_verification_email_template_does_not_exist;
decode_error_code("ProductionAccessNotGranted") -> production_access_not_granted;
decode_error_code("CustomVerificationEmailInvalidContent") -> custom_verification_email_invalid_content;
decode_error_code("FromEmailAddressNotVerified") -> from_email_address_not_verified;
decode_error_code("CustomVerificationEmailTemplateAlreadyExists") -> custom_verification_email_template_already_exists;
decode_error_code("AccountSendingPaused") -> account_sending_paused;
decode_error_code("ConfigurationSetSendingPaused") -> configuration_set_sending_paused;
decode_error_code("MailFromDomainNotVerified") -> mail_from_domain_not_verified.


decode_error(Doc) ->
    {erlcloud_ses, {decode_error_code(erlcloud_xml:get_text("Error/Code", Doc)),
                    erlcloud_xml:get_text("Error/Message", Doc)}}.


%%%------------------------------------------------------------------------------
%%% Internal Functions
%%%------------------------------------------------------------------------------

ses_request(Config, Action, Params) ->
    QParams = [{"Action", Action},
               {"Version", ?API_VERSION} |
               Params],
    case erlcloud_aws:aws_request4(post, "https", Config#aws_config.ses_host, 443, "/", QParams, "ses", Config) of
        {ok, Body} ->
            {ok, element(1, xmerl_scan:string(binary_to_list(Body)))};
        {error, {http_error, Code, _, ErrBody}} when Code >= 400; Code =< 599 ->
            ErrDoc = element(1, xmerl_scan:string(binary_to_list(ErrBody))),
            {error, decode_error(ErrDoc)};
        {error, Reason} -> 
            {error, Reason}
    end.
