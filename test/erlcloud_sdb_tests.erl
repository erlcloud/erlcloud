-module(erlcloud_sdb_tests).
-include_lib("eunit/include/eunit.hrl").

setup() ->
    erlcloud_sdb:configure("fake", "fake-secret"),
    meck:new(httpc, [unstick]).

cleanup() ->
    meck:unload(httpc).

%% Helpers

create_chain(Response) ->
    add_response_to_chain([], Response).

add_response_to_chain(Chain, Response) ->
    Chain ++ [Response].

expect_chain([Response | Chain]) ->
    meck:expect(httpc, request,
                fun(_, _, _, _) ->
                        expect_chain(Chain),
                        Response
                end);
expect_chain([]) ->
    ok.

parse_document(XML) ->
    element(1, xmerl_scan:string(XML)).

%% Fixtures
single_result_response() ->
    single_result_response("item0").

single_result_response(Name) ->
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

next_token() ->
    "rO0ABXNyACdjb20uYW1hem9uLnNkcy5RdWVyeVByb2Nlc3Nvci5Nb3JlVG9rZW7racXLnINNqwMA\nC0kAFGluaXRpYWxDb25qdW5jdEluZGV4WgAOaXNQYWdlQm91bmRhcnlKAAxsYXN0RW50aXR5SURa\nAApscnFFbmFibGVkSQAPcXVlcnlDb21wbGV4aXR5SgATcXVlcnlTdHJpbmdDaGVja3N1bUkACnVu\naW9uSW5kZXhaAA11c2VRdWVyeUluZGV4TAANY29uc2lzdGVudExTTnQAEkxqYXZhL2xhbmcvU3Ry\naW5nO0wAEmxhc3RBdHRyaWJ1dGVWYWx1ZXEAfgABTAAJc29ydE9yZGVydAAvTGNvbS9hbWF6b24v\nc2RzL1F1ZXJ5UHJvY2Vzc29yL1F1ZXJ5JFNvcnRPcmRlcjt4cAAAAAAB//////////8AAAAAAAAA\nAADl8izPAAAAAAB0ABZbMTM1NTM1MzAzNTk2NywyOTE5OTldcH5yAC1jb20uYW1hem9uLnNkcy5R\ndWVyeVByb2Nlc3Nvci5RdWVyeSRTb3J0T3JkZXIAAAAAAAAAABIAAHhyAA5qYXZhLmxhbmcuRW51\nbQAAAAAAAAAAEgAAeHB0AAlBU0NFTkRJTkd4".

only_token_response() ->
    "<?xml version=\"1.0\"?><SelectResponse xmlns=\"http://sdb.amazonaws.com/doc/2009-04-15/\"><SelectResult><NextToken>rO0ABXNyACdjb20uYW1hem9uLnNkcy5RdWVyeVByb2Nlc3Nvci5Nb3JlVG9rZW7racXLnINNqwMA\nC0kAFGluaXRpYWxDb25qdW5jdEluZGV4WgAOaXNQYWdlQm91bmRhcnlKAAxsYXN0RW50aXR5SURa\nAApscnFFbmFibGVkSQAPcXVlcnlDb21wbGV4aXR5SgATcXVlcnlTdHJpbmdDaGVja3N1bUkACnVu\naW9uSW5kZXhaAA11c2VRdWVyeUluZGV4TAANY29uc2lzdGVudExTTnQAEkxqYXZhL2xhbmcvU3Ry\naW5nO0wAEmxhc3RBdHRyaWJ1dGVWYWx1ZXEAfgABTAAJc29ydE9yZGVydAAvTGNvbS9hbWF6b24v\nc2RzL1F1ZXJ5UHJvY2Vzc29yL1F1ZXJ5JFNvcnRPcmRlcjt4cAAAAAAB//////////8AAAAAAAAA\nAADl8izPAAAAAAB0ABZbMTM1NTM1MzAzNTk2NywyOTE5OTldcH5yAC1jb20uYW1hem9uLnNkcy5R\ndWVyeVByb2Nlc3Nvci5RdWVyeSRTb3J0T3JkZXIAAAAAAAAAABIAAHhyAA5qYXZhLmxhbmcuRW51\nbQAAAAAAAAAAEgAAeHB0AAlBU0NFTkRJTkd4</NextToken></SelectResult><ResponseMetadata><RequestId>f20b398c-9744-0f0c-5b8a-41c436e41987</RequestId><BoxUsage>0.0000137200</BoxUsage></ResponseMetadata></SelectResponse>".

single_result_and_token_response() ->
    "<?xml version=\"1.0\"?><SelectResponse xmlns=\"http://sdb.amazonaws.com/doc/2009-04-15/\"><SelectResult><Item><Name>item0</Name><Attribute><Name>Color</Name><Value>Black</Value></Attribute></Item><NextToken>rO0ABXNyACdjb20uYW1hem9uLnNkcy5RdWVyeVByb2Nlc3Nvci5Nb3JlVG9rZW7racXLnINNqwMA\nC0kAFGluaXRpYWxDb25qdW5jdEluZGV4WgAOaXNQYWdlQm91bmRhcnlKAAxsYXN0RW50aXR5SURa\nAApscnFFbmFibGVkSQAPcXVlcnlDb21wbGV4aXR5SgATcXVlcnlTdHJpbmdDaGVja3N1bUkACnVu\naW9uSW5kZXhaAA11c2VRdWVyeUluZGV4TAANY29uc2lzdGVudExTTnQAEkxqYXZhL2xhbmcvU3Ry\naW5nO0wAEmxhc3RBdHRyaWJ1dGVWYWx1ZXEAfgABTAAJc29ydE9yZGVydAAvTGNvbS9hbWF6b24v\nc2RzL1F1ZXJ5UHJvY2Vzc29yL1F1ZXJ5JFNvcnRPcmRlcjt4cAAAAAAB//////////8AAAAAAAAA\nAADl8izPAAAAAAB0ABZbMTM1NTM1MzAzNTk2NywyOTE5OTldcH5yAC1jb20uYW1hem9uLnNkcy5R\ndWVyeVByb2Nlc3Nvci5RdWVyeSRTb3J0T3JkZXIAAAAAAAAAABIAAHhyAA5qYXZhLmxhbmcuRW51\nbQAAAAAAAAAAEgAAeHB0AAlBU0NFTkRJTkd4</NextToken></SelectResult><ResponseMetadata><RequestId>f20b398c-9744-0f0c-5b8a-41c436e41987</RequestId><BoxUsage>0.0000137200</BoxUsage></ResponseMetadata></SelectResponse>".

%% Tests - select
select_single_response_test() ->
    setup(),
    expect_chain(create_chain({ok, {{0, 200, ""}, [], single_result_response()}})),

    Result = erlcloud_sdb:select("select"),
    ?assertEqual(undefined, proplists:get_value(next_token, Result)),

    cleanup().

select_next_token_test() ->
    setup(),
    Chain = create_chain({ok, {{0, 200, ""}, [], only_token_response()}}),
    expect_chain(add_response_to_chain(Chain, {ok, "200", [], single_result_response()})),

    Result = erlcloud_sdb:select("select"),
    ?assertEqual(next_token(), proplists:get_value(next_token, Result)),

    cleanup().

%% Tests - select_all

select_all_single_response_test() ->
    setup(),
    expect_chain(create_chain({ok, {{0, 200, ""}, [], single_result_response()}})),

    Result = erlcloud_sdb:select_all("select"),
    Items = proplists:get_value(items, Result),
    ?assertEqual(1, length(Items)),

    cleanup().

select_all_failure_test() ->
    setup(),
    expect_chain(create_chain({error, {conn_failed,{error,ssl_not_started}}})),

    {'EXIT', {Error, _Stack}} = (catch erlcloud_sdb:select_all("select")),
    ?assertEqual({aws_error,{socket_error,{conn_failed,{error,ssl_not_started}}}}, Error),

    cleanup().

select_all_503_test() ->
    setup(),
    Chain = create_chain({ok, {{0, 503, "Unavailable"}, [], single_result_response()}}),
    expect_chain(add_response_to_chain(Chain, {ok, {{0, 200, ""}, [], single_result_response()}})),

    Result = erlcloud_sdb:select_all("select"),
    Items = proplists:get_value(items, Result),
    ?assertEqual(1, length(Items)),

    cleanup().

select_all_next_token_test() ->
    setup(),
    Chain = create_chain({ok, {{0, 200, ""}, [], only_token_response()}}),
    expect_chain(add_response_to_chain(Chain, {ok, {{0, 200, ""}, [], single_result_response()}})),

    Result = erlcloud_sdb:select_all("select"),
    Items = proplists:get_value(items, Result),
    ?assertEqual(1, length(Items)),

    cleanup().

select_all_next_and_failure_test() ->
    setup(),
    Chain = create_chain({ok, {{0, 200, ""}, [], only_token_response()}}),
    Chain2 = add_response_to_chain(Chain, {ok, {{0, 503, "Unavailable"}, [], single_result_response()}}),
    Chain3 = add_response_to_chain(Chain2, {ok, {{0, 200, ""}, [], single_result_response()}}),
    expect_chain(Chain3),

    Result = erlcloud_sdb:select_all("select"),
    Items = proplists:get_value(items, Result),
    ?assertEqual(1, length(Items)),

    cleanup().

select_all_two_results_test() ->
    setup(),
    Chain = create_chain({ok, {{0, 200, ""}, [], single_result_and_token_response()}}),
    Chain2 = add_response_to_chain(Chain, {ok, {{0, 200, ""}, [], single_result_response("item1")}}),
    expect_chain(Chain2),

    Result = erlcloud_sdb:select_all("select"),
    Items = proplists:get_value(items, Result),
    ?assertEqual(["item0", "item1"], [proplists:get_value(name, Item) || Item <- Items]),

    cleanup().

extract_token_test() ->
    ?assertEqual(next_token(), erlcloud_sdb:extract_token(parse_document(only_token_response()))),
    ?assertEqual(next_token(), erlcloud_sdb:extract_token(parse_document(single_result_and_token_response()))),
    ?assertEqual(done, erlcloud_sdb:extract_token(parse_document(single_result_response()))).
