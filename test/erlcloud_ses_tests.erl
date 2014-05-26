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
     [fun sendemail_tests/1
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

%%%===================================================================
%%% Actual test specifiers
%%%===================================================================

sendemail_tests(_) ->
    [
     fun() ->
             erlcloud_ddb2:configure(string:copies("A", 20), string:copies("a", 40)),
             Want = "Action=SendEmail&Version=2010-12-01&Source=b%40from.com&Message.Subject.Data=Subject&Message.Body.Text.Data=Email%20Body&Destination.ToAddresses.member.1=a%40to.com",
             meck:expect(erlcloud_httpc, request, input_expect("response", Want)),
             {ok, <<"response">>} =
                 erlcloud_ses:send_email(<<"a@to.com">>, <<"Email Body">>, <<"Subject">>,
                                         <<"b@from.com">>, []),
             ok
     end,
     fun() ->
             erlcloud_ddb2:configure(string:copies("A", 20), string:copies("a", 40)),
             Want = "Action=SendEmail&Version=2010-12-01&ReturnPath=return%20path&ReplyToAddresses.member.2=g%40reply.com&ReplyToAddresses.member.1=f%40reply.com&Source=e%40from.com&Message.Subject.Data=subject%20data&Message.Subject.Charset=subject%20charset&Message.Body.Text.Data=text%20data&Message.Body.Text.Charset=text%20charset&Message.Body.Html.Data=html%20data&Message.Body.Html.Charset=html%20charset&Destination.ToAddresses.member.1=d%40to.com&Destination.CcAddresses.member.1=c%40cc.com&Destination.BccAddresses.member.2=b%40bcc.com&Destination.BccAddresses.member.1=a%40bcc.com",
             meck:expect(erlcloud_httpc, request, input_expect("response", Want)),
             {ok, <<"response">>} =
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
                                          {return_path, "return path"}]),
             ok
     end
    ].
