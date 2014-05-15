%% -*- mode: erlang;erlang-indent-level: 4;indent-tabs-mode: nil -*-

%% @author Ransom Richardson <ransom@ransomr.net>
%% @doc
%% An Erlang interface to Amazon's Simple Email Service (SES)
%%
%% SendEmail is the only method implemented.
%%
%% @end


-module(erlcloud_ses).

-export([configure/2, configure/3, new/2, new/3]).

-export([
         send_email/4, send_email/5, send_email/6
        ]).

-include("erlcloud.hrl").
-include("erlcloud_aws.hrl").

-define(API_VERSION, "2010-12-01").

%%%------------------------------------------------------------------------------
%%% Library initialization.
%%%------------------------------------------------------------------------------

-spec(new/2 :: (string(), string()) -> aws_config()).
new(AccessKeyID, SecretAccessKey) ->
    #aws_config{access_key_id=AccessKeyID,
                secret_access_key=SecretAccessKey}.

-spec(new/3 :: (string(), string(), string()) -> aws_config()).
new(AccessKeyID, SecretAccessKey, Host) ->
    #aws_config{access_key_id=AccessKeyID,
                secret_access_key=SecretAccessKey,
                ses_host=Host}.

-spec(configure/2 :: (string(), string()) -> ok).
configure(AccessKeyID, SecretAccessKey) ->
    put(aws_config, new(AccessKeyID, SecretAccessKey)),
    ok.

-spec(configure/3 :: (string(), string(), string()) -> ok).
configure(AccessKeyID, SecretAccessKey, Host) ->
    put(aws_config, new(AccessKeyID, SecretAccessKey, Host)),
    ok.

default_config() -> erlcloud_aws:default_config().

%%%------------------------------------------------------------------------------
%%% SendEmail
%%%------------------------------------------------------------------------------

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
%% Simple email send.
%%
%% `
%% {ok, _} =
%%     erlcloud_ses:send_email(<<"a@to.com">>, <<"Email Body">>, <<"Subject">>,
%%                             <<"b@from.com">>, []),
%% '
%%
%% All supported inputs.
%%
%% `
%%  {ok, _} =
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
%%                          {return_path, "return path"}]),
%% '
%% @end
%%------------------------------------------------------------------------------

send_email(Destination, Body, Subject, Source, Opts, Config) ->
    Params1 = encode_destination(Destination, []),
    Params2 = encode_body(Body, Params1),
    Params3 = encode_subject(Subject, Params2),
    Params4 = encode_source(Source, Params3),
    Params5 = encode_opts(Opts, Params4),
    ses_request(Config, "SendEmail", Params5).

%%%------------------------------------------------------------------------------
%%% Encoders
%%%------------------------------------------------------------------------------

encode_list(Prefix, List, Acc) ->
    encode_list(Prefix, List, 1, Acc).

encode_list(_, [], _, Acc) ->
    Acc;
encode_list(Prefix, [H | T], N, Acc) ->
    encode_list(Prefix, T, N + 1, [{Prefix ++ ".member." ++ integer_to_list(N), H} | Acc]).

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
encode_destination(ToAddress, Acc) when is_list(ToAddress) or is_binary(ToAddress) ->
    %% Single entry
    encode_destination_pairs([{to_addresses, [ToAddress]}], Acc).

encode_content_pairs(_, [], Acc) ->
    Acc;
encode_content_pairs(Prefix, [{charset, Charset} | T], Acc) ->
    encode_content_pairs(Prefix, T, [{Prefix ++ ".Charset", Charset} | Acc]);
encode_content_pairs(Prefix, [{data, Data} | T], Acc) ->
    encode_content_pairs(Prefix, T, [{Prefix ++ ".Data", Data} | Acc]).

encode_content(Prefix, [{_,_} | _] = Content, Acc) ->
    %% List of pairs
    encode_content_pairs(Prefix, Content, Acc);
encode_content(Prefix, Data, Acc) when is_list(Data) or is_binary(Data) ->
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
encode_body(Body, Acc) when is_list(Body) or is_binary(Body) ->
    %% Single entry
    encode_body_pairs([{text, Body}], Acc).

encode_subject(Subject, Acc) ->
    encode_content("Message.Subject", Subject, Acc).

encode_source(Source, Acc) when is_list(Source) or is_binary(Source) ->
    [{"Source", Source} | Acc].

encode_opts([], Acc) ->
    Acc;
encode_opts([{reply_to_addresses, List} | T], Acc) ->
    encode_opts(T, encode_list("ReplyToAddresses", List, Acc));
encode_opts([{return_path, ReturnPath} | T], Acc) ->
    encode_opts(T, [{"ReturnPath", ReturnPath} | Acc]).

%%%------------------------------------------------------------------------------
%%% Internal Functions
%%%------------------------------------------------------------------------------

ses_request(Config, Action, Params) ->
    case erlcloud_aws:update_config(Config) of
        {ok, Config1} ->
            ses_request_no_update(Config1, Action, Params);
        {error, Reason} ->
            {error, Reason}
    end.

ses_request_no_update(Config, Action, Params) ->
    Date = httpd_util:rfc1123_date(),
    Signature = base64:encode_to_string(
                  erlcloud_util:sha256_mac(Config#aws_config.secret_access_key, Date)),
    Auth = lists:flatten(
             ["AWS3-HTTPS AWSAccessKeyId=",
              Config#aws_config.access_key_id, 
              ",Algorithm=HmacSHA256,Signature=",
              Signature]),
             
    Headers = [{"Date", Date},
               {"X-Amzn-Authorization", Auth}],
    Headers2 = case Config#aws_config.security_token of
                   undefined ->
                       Headers;
                   Token ->
                       [{"x-amz-security-token", Token} | Headers]
               end,
    QParams = [{"Action", Action}, 
               {"Version", ?API_VERSION} | 
               Params],
    Query = erlcloud_http:make_query_string(QParams),

    erlcloud_aws:aws_request_form(
      post, "https", Config#aws_config.ses_host, 443, "/", Query, Headers2, Config).
