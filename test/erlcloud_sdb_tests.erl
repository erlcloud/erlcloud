-module(erlcloud_sdb_tests).

-ifdef(TEST).
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

setup() ->
    erlcloud_sdb:configure("fake", "fake-secret"),
    meck:new(erlcloud_httpc).

cleanup(_) ->
    meck:unload(erlcloud_httpc).

%% Helpers

expect_chain([Response | Chain]) ->
    meck:expect(erlcloud_httpc, request,
                fun(_, _, _, _, _, _) ->
                        expect_chain(Chain),
                        Response
                end);
expect_chain([]) ->
    ok.

parse_document(XML) ->
    element(1, xmerl_scan:string(XML)).

%% Fixtures

next_token() ->
    "rO0ABXNyACdjb20uYW1hem9uLnNkcy5RdWVyeVByb2Nlc3Nvci5Nb3JlVG9rZW7racXLnINNqwMA\nC0kAFGluaXRpYWxDb25qdW5jdEluZGV4WgAOaXNQYWdlQm91bmRhcnlKAAxsYXN0RW50aXR5SURa\nAApscnFFbmFibGVkSQAPcXVlcnlDb21wbGV4aXR5SgATcXVlcnlTdHJpbmdDaGVja3N1bUkACnVu\naW9uSW5kZXhaAA11c2VRdWVyeUluZGV4TAANY29uc2lzdGVudExTTnQAEkxqYXZhL2xhbmcvU3Ry\naW5nO0wAEmxhc3RBdHRyaWJ1dGVWYWx1ZXEAfgABTAAJc29ydE9yZGVydAAvTGNvbS9hbWF6b24v\nc2RzL1F1ZXJ5UHJvY2Vzc29yL1F1ZXJ5JFNvcnRPcmRlcjt4cAAAAAAB//////////8AAAAAAAAA\nAADl8izPAAAAAAB0ABZbMTM1NTM1MzAzNTk2NywyOTE5OTldcH5yAC1jb20uYW1hem9uLnNkcy5R\ndWVyeVByb2Nlc3Nvci5RdWVyeSRTb3J0T3JkZXIAAAAAAAAAABIAAHhyAA5qYXZhLmxhbmcuRW51\nbQAAAAAAAAAAEgAAeHB0AAlBU0NFTkRJTkd4".

single_result_response_body(Name) ->
    "<SelectResponse>
  <SelectResult>
    <Item>
      <Name>" ++ Name ++ "</Name>
      <Attribute><Name>Color</Name><Value>Black</Value></Attribute>
    </Item>
  </SelectResult>
  <ResponseMetadata>
    <RequestId>b1e8f1f7-42e9-494c-ad09-2674e557526d</RequestId>
    <BoxUsage>0.1</BoxUsage>
  </ResponseMetadata>
</SelectResponse>".

only_token_response_body() ->
    "<?xml version=\"1.0\"?><SelectResponse xmlns=\"http://sdb.amazonaws.com/doc/2009-04-15/\"><SelectResult><NextToken>rO0ABXNyACdjb20uYW1hem9uLnNkcy5RdWVyeVByb2Nlc3Nvci5Nb3JlVG9rZW7racXLnINNqwMA\nC0kAFGluaXRpYWxDb25qdW5jdEluZGV4WgAOaXNQYWdlQm91bmRhcnlKAAxsYXN0RW50aXR5SURa\nAApscnFFbmFibGVkSQAPcXVlcnlDb21wbGV4aXR5SgATcXVlcnlTdHJpbmdDaGVja3N1bUkACnVu\naW9uSW5kZXhaAA11c2VRdWVyeUluZGV4TAANY29uc2lzdGVudExTTnQAEkxqYXZhL2xhbmcvU3Ry\naW5nO0wAEmxhc3RBdHRyaWJ1dGVWYWx1ZXEAfgABTAAJc29ydE9yZGVydAAvTGNvbS9hbWF6b24v\nc2RzL1F1ZXJ5UHJvY2Vzc29yL1F1ZXJ5JFNvcnRPcmRlcjt4cAAAAAAB//////////8AAAAAAAAA\nAADl8izPAAAAAAB0ABZbMTM1NTM1MzAzNTk2NywyOTE5OTldcH5yAC1jb20uYW1hem9uLnNkcy5R\ndWVyeVByb2Nlc3Nvci5RdWVyeSRTb3J0T3JkZXIAAAAAAAAAABIAAHhyAA5qYXZhLmxhbmcuRW51\nbQAAAAAAAAAAEgAAeHB0AAlBU0NFTkRJTkd4</NextToken></SelectResult><ResponseMetadata><RequestId>f20b398c-9744-0f0c-5b8a-41c436e41987</RequestId><BoxUsage>0.0000137200</BoxUsage></ResponseMetadata></SelectResponse>".

single_result_and_token_response_body() ->
    "<?xml version=\"1.0\"?><SelectResponse xmlns=\"http://sdb.amazonaws.com/doc/2009-04-15/\"><SelectResult><Item><Name>item0</Name><Attribute><Name>Color</Name><Value>Black</Value></Attribute></Item><NextToken>rO0ABXNyACdjb20uYW1hem9uLnNkcy5RdWVyeVByb2Nlc3Nvci5Nb3JlVG9rZW7racXLnINNqwMA\nC0kAFGluaXRpYWxDb25qdW5jdEluZGV4WgAOaXNQYWdlQm91bmRhcnlKAAxsYXN0RW50aXR5SURa\nAApscnFFbmFibGVkSQAPcXVlcnlDb21wbGV4aXR5SgATcXVlcnlTdHJpbmdDaGVja3N1bUkACnVu\naW9uSW5kZXhaAA11c2VRdWVyeUluZGV4TAANY29uc2lzdGVudExTTnQAEkxqYXZhL2xhbmcvU3Ry\naW5nO0wAEmxhc3RBdHRyaWJ1dGVWYWx1ZXEAfgABTAAJc29ydE9yZGVydAAvTGNvbS9hbWF6b24v\nc2RzL1F1ZXJ5UHJvY2Vzc29yL1F1ZXJ5JFNvcnRPcmRlcjt4cAAAAAAB//////////8AAAAAAAAA\nAADl8izPAAAAAAB0ABZbMTM1NTM1MzAzNTk2NywyOTE5OTldcH5yAC1jb20uYW1hem9uLnNkcy5R\ndWVyeVByb2Nlc3Nvci5RdWVyeSRTb3J0T3JkZXIAAAAAAAAAABIAAHhyAA5qYXZhLmxhbmcuRW51\nbQAAAAAAAAAAEgAAeHB0AAlBU0NFTkRJTkd4</NextToken></SelectResult><ResponseMetadata><RequestId>f20b398c-9744-0f0c-5b8a-41c436e41987</RequestId><BoxUsage>0.0000137200</BoxUsage></ResponseMetadata></SelectResponse>".

single_result_response() ->
    single_result_response("item0").

single_result_response(Name) ->
    {ok, {{200, "OK"}, [], list_to_binary(single_result_response_body(Name))}}.

only_token_response() ->
    {ok, {{200, "OK"}, [], list_to_binary(only_token_response_body())}}.

single_result_and_token_response() ->
    {ok, {{200, "OK"}, [], list_to_binary(single_result_and_token_response_body())}}.

unavailable_response() ->
    {ok, {{503, "Unavailable"}, [], ""}}.

%% Tests - select
select_test_() ->
    {foreach, local,
     fun setup/0,
     fun cleanup/1,
     [{test, ?MODULE, select_single_response},
      {test, ?MODULE, select_next_token}
     ]}.

select_single_response() ->
    expect_chain([single_result_response()]),

    Result = erlcloud_sdb:select("select"),
    ?assertEqual(undefined, proplists:get_value(next_token, Result)).

select_next_token() ->
    expect_chain([only_token_response(),
                 single_result_response()]),

    Result = erlcloud_sdb:select("select"),
    ?assertEqual(next_token(), proplists:get_value(next_token, Result)).

%% Tests - select_all

select_all_test_() ->
    {foreach, local,
     fun setup/0,
     fun cleanup/1,
     [{test, ?MODULE, select_all_single_response},
      {test, ?MODULE, select_all_failure},
      {test, ?MODULE, select_all_503},
      {test, ?MODULE, select_all_next_token},
      {test, ?MODULE, select_all_next_and_failure},
      {test, ?MODULE, select_all_two_results},
      {test, ?MODULE, extract_token_test}
     ]}.

select_all_single_response() ->
    expect_chain([single_result_response()]),

    Result = erlcloud_sdb:select_all("select"),
    Items = proplists:get_value(items, Result),
    ?assertEqual(1, length(Items)).

select_all_failure() ->
    expect_chain([{error, {conn_failed,{error,ssl_not_started}}}]),

    {'EXIT', {Error, _Stack}} = (catch erlcloud_sdb:select_all("select")),
    ?assertEqual({aws_error,{socket_error,{conn_failed,{error,ssl_not_started}}}}, Error).

select_all_503() ->
    expect_chain([unavailable_response(),
                  single_result_response()]),

    Result = erlcloud_sdb:select_all("select"),
    Items = proplists:get_value(items, Result),
    ?assertEqual(1, length(Items)).

select_all_next_token() ->
    expect_chain([only_token_response(),
                  single_result_response()]),

    Result = erlcloud_sdb:select_all("select"),
    Items = proplists:get_value(items, Result),
    ?assertEqual(1, length(Items)).

select_all_next_and_failure() ->
    expect_chain([only_token_response(),
                  unavailable_response(),
                  single_result_response()]),

    Result = erlcloud_sdb:select_all("select"),
    Items = proplists:get_value(items, Result),
    ?assertEqual(1, length(Items)).

select_all_two_results() ->
    expect_chain([single_result_and_token_response(),
                  single_result_response("item1")]),

    Result = erlcloud_sdb:select_all("select"),
    Items = proplists:get_value(items, Result),
    ?assertEqual(["item0", "item1"], [proplists:get_value(name, Item) || Item <- Items]).

extract_token_test() ->
    ?assertEqual(next_token(), erlcloud_sdb:extract_token(parse_document(only_token_response_body()))),
    ?assertEqual(next_token(), erlcloud_sdb:extract_token(parse_document(single_result_and_token_response_body()))),
    ?assertEqual(done, erlcloud_sdb:extract_token(parse_document(single_result_response_body("item0")))).

-endif.
