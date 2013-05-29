-module(erlcloud_sdb_tests).
-include_lib("eunit/include/eunit.hrl").

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

setup() ->
    erlcloud_sdb:configure("fake", "fake-secret"),
    meck:new(httpc, [unstick]).

cleanup() ->
    meck:unload(httpc).

single_result_response() ->
    "<SelectResponse>
  <SelectResult>
    <Item>
      <Name>item0</Name>
      <Attribute><Name>Color</Name><Value>Black</Value></Attribute>
    </Item>
  </SelectResult>
  <ResponseMetadata>
    <RequestId>b1e8f1f7-42e9-494c-ad09-2674e557526d</RequestId>
    <BoxUsage>0.1</BoxUsage>
  </ResponseMetadata>
</SelectResponse>".

select_single_response_test() ->
    setup(),
    expect_chain(create_chain({ok, {{0, 200, ""}, [], single_result_response()}})),

    Result = erlcloud_sdb:select("select"),
    Items = proplists:get_value(items, Result),
    ?assertEqual(1, length(Items)),

    cleanup().

select_failure_test() ->
    setup(),
    expect_chain(create_chain({error, {conn_failed,{error,ssl_not_started}}})),

    {'EXIT', {Error, _Stack}} = (catch erlcloud_sdb:select("select")),
    ?assertEqual({aws_error,{socket_error,{conn_failed,{error,ssl_not_started}}}}, Error),

    cleanup().

select_503_test() ->
    setup(),
    Chain = create_chain({ok, {{0, 503, "Unavailable"}, [], single_result_response()}}),
    expect_chain(add_response_to_chain(Chain, {ok, {{0, 200, ""}, [], single_result_response()}})),

    Result = erlcloud_sdb:select("select"),
    Items = proplists:get_value(items, Result),
    ?assertEqual(1, length(Items)),

    cleanup().
