-module(erlcloud_sm_tests).
-include_lib("eunit/include/eunit.hrl").
-include("erlcloud.hrl").

%% Unit tests for sm.
%% These tests work by using meck to mock erlcloud_httpc. There are two classes of test: input and output.
%%
%% Input tests verify that different function args produce the desired JSON request.
%% An input test list provides a list of funs and the JSON that is expected to result.
%%
%% Output tests verify that the http response produces the correct return from the fun.
%% An output test lists provides a list of response bodies and the expected return.

%% The _sm_test macro provides line number annotation to a test, similar to _test, but doesn't wrap in a fun
-define(_sm_test(T), {?LINE, T}).
%% The _f macro is a terse way to wrap code in a fun. Similar to _test but doesn't annotate with a line number
-define(_f(F), fun() -> F end).

-export([validate_body/2]).

%%%===================================================================
%%% Common Test Values
%%%===================================================================

-define(SECRET_ID, <<"MyTestDatabaseSecret">>).
-define(SECRET_STRING, <<"{\"username\":\"david\",\"password\":\"SECRET-PASSWORD\"}">>).
-define(SECRET_BINARY, base64:encode(?SECRET_STRING)).
-define(CLIENT_REQUEST_TOKEN, <<"EXAMPLE2-90ab-cdef-fedc-ba987EXAMPLE">>).
-define(VERSION_ID, <<"EXAMPLE1-90ab-cdef-fedc-ba987SECRET1">>).
-define(VERSION_STAGE, <<"AWSPREVIOUS">>).

%%%===================================================================
%%% Test entry points
%%%===================================================================

operation_test_() ->
    {foreach,
        fun start/0,
        fun stop/1,
        [
            fun get_secret_value_input_tests/1,
            fun get_secret_value_output_tests/1,
            fun put_secret_value_input_tests/1,
            fun put_secret_value_output_tests/1
        ]}.

start() ->
    meck:new(erlcloud_httpc),
    ok.

stop(_) ->
    meck:unload(erlcloud_httpc).

%%%===================================================================
%%% Input test helpers
%%%===================================================================

sort_json([{_, _} | _] = Json) ->
    %% Value is an object
    SortedChildren = [{K, sort_json(V)} || {K, V} <- Json],
    lists:keysort(1, SortedChildren);
sort_json([_ | _] = Json) ->
    %% Value is an array
    [sort_json(I) || I <- Json];
sort_json(V) ->
    V.

%% verifies that the parameters in the body match the expected parameters
-spec validate_body(binary(), binary()) -> ok.
validate_body(Body, Expected) ->
    Want = sort_json(jsx:decode(Expected)),
    Actual = sort_json(jsx:decode(Body)),
    case Want =:= Actual of
        true ->
            ok;
        false ->
            ?debugFmt("~nEXPECTED~n~p~nACTUAL~n~p~n", [Want, Actual])
    end,
    ?assertEqual(Want, Actual).

%% returns the mock of the erlcloud_httpc function input tests expect to be called.
%% Validates the request body and responds with the provided response.
-spec input_expect(binary(), binary()) -> fun().
input_expect(Response, Expected) ->
    fun(_Url, post, _Headers, Body, _Timeout, _Config) ->
        validate_body(Body, Expected),
        {ok, {{200, "OK"}, [], Response}}
    end.

%% input_test converts an input_test specifier into an eunit test generator
-type input_test_spec() :: {pos_integer(), {fun(), binary()} | {string(), fun(), binary()}}.
-spec input_test(binary(), input_test_spec()) -> tuple().
input_test(Response, {Line, {Description, Fun, Expected}}) when
    is_list(Description) ->
    {Description,
        {Line,
            fun() ->
                meck:expect(erlcloud_httpc, request, input_expect(Response, Expected)),
                erlcloud_config:configure(string:copies("A", 20),
                    string:copies("a", 40), fun erlcloud_sm:new/2),
                Fun()
            end}}.

%% input_tests converts a list of input_test specifiers into an eunit test generator
-spec input_tests(binary(), [input_test_spec()]) -> [tuple()].
input_tests(Response, Tests) ->
    [input_test(Response, Test) || Test <- Tests].

%%%===================================================================
%%% Output test helpers
%%%===================================================================

%% returns the mock of the erlcloud_httpc function output tests expect to be called.
-spec output_expect(string()) -> fun().
output_expect(Response) ->
    fun(_Url, post, _Headers, _Body, _Timeout, _Config) ->
        {ok, {{200, "OK"}, [], Response}}
    end.

%% output_test converts an output_test specifier into an eunit test generator
-type output_test_spec() :: {pos_integer(), {binary(), term()} | {string(), binary(), term()}}.
-spec output_test(fun(), output_test_spec()) -> tuple().
output_test(Fun, {Line, {Description, Response, Result}}) ->
    {Description,
        {Line,
            fun() ->
                meck:expect(erlcloud_httpc, request, output_expect(Response)),
                erlcloud_config:configure(string:copies("A", 20),
                    string:copies("a", 40), fun erlcloud_sm:new/2),
                Actual = Fun(),
                case Result =:= Actual of
                    true -> ok;
                    false ->
                        ?debugFmt("~nEXPECTED~n~p~nACTUAL~n~p~n", [Result, Actual])
                end,
                ?assertEqual(Result, Actual)
            end}}.

%% output_tests converts a list of output_test specifiers into an eunit test generator
-spec output_tests(fun(), [output_test_spec()]) -> [term()].
output_tests(Fun, Tests) ->
    [output_test(Fun, Test) || Test <- Tests].

%%%===================================================================
%%% Tests
%%%===================================================================

get_secret_value_input_tests(_) ->
    Tests = [
        ?_sm_test(
            {"get_secret_value2id input test",
                ?_f(erlcloud_sm:get_secret_value(?SECRET_ID, [{version_id, ?VERSION_ID}])),
                jsx:encode([
                    {<<"SecretId">>, ?SECRET_ID},
                    {<<"VersionId">>, ?VERSION_ID}
                ])
        }),
        ?_sm_test(
            {"get_secret_value2stage input test",
                ?_f(erlcloud_sm:get_secret_value(?SECRET_ID, [{version_stage, ?VERSION_STAGE}])),
                jsx:encode([
                    {<<"SecretId">>, ?SECRET_ID},
                    {<<"VersionStage">>, ?VERSION_STAGE}
                ])
        })
    ],
    Response = <<>>,
    input_tests(Response, Tests).


-define(GET_SECRET_VALUE_RESP,[
    {<<"ARN">>,
        <<"arn:aws:secretsmanager:us-west-2:123456789012:secret:MyTestDatabaseSecret-a1b2c3">>},
    {<<"CreatedDate">>, 1.523477145713E9},
    {<<"Name">>, ?SECRET_ID},
    {<<"SecretString">>,
        <<"{\n \"username\":\"david\",\n \"password\":\"BnQw&XDWgaEeT9XGTT29\"\n}\n">>},
    {<<"VersionId">>, ?VERSION_ID},
    {<<"VersionStages">>, [?VERSION_STAGE]}
]).


get_secret_value_output_tests(_) ->
    Tests = [?_sm_test(
        {"get_secret_value output test",
            jsx:encode(?GET_SECRET_VALUE_RESP),
            {ok, ?GET_SECRET_VALUE_RESP}}
        )],

    output_tests(?_f(erlcloud_sm:get_secret_value(?SECRET_ID, [{version_id, ?VERSION_ID}])), Tests),
    output_tests(?_f(erlcloud_sm:get_secret_value(?SECRET_ID, [{version_stage, ?VERSION_STAGE}])), Tests).


put_secret_value_input_tests(_) ->
    Tests = [
        ?_sm_test(
            {"put_secret_string input test",
                ?_f(erlcloud_sm:put_secret_string(?SECRET_ID, ?CLIENT_REQUEST_TOKEN, ?SECRET_STRING)),
                jsx:encode([
                    {<<"ClientRequestToken">>,?CLIENT_REQUEST_TOKEN},
                    {<<"SecretId">>,?SECRET_ID},
                    {<<"SecretString">>,?SECRET_STRING}])
            }),
        ?_sm_test(
            {"put_secret_binary input test",
                ?_f(erlcloud_sm:put_secret_binary(?SECRET_ID, ?CLIENT_REQUEST_TOKEN, ?SECRET_STRING)),
                jsx:encode([
                    {<<"ClientRequestToken">>,?CLIENT_REQUEST_TOKEN},
                    {<<"SecretId">>,?SECRET_ID},
                    {<<"SecretBinary">>,?SECRET_BINARY}])
            })
    ],
    Response = <<>>,
    input_tests(Response, Tests).


-define(PUT_SECRET_VALUE_RESP,[
    {<<"ARN">>,
        <<"arn:aws:secretsmanager:us-west-2:123456789012:secret:MyTestDatabaseSecret-a1b2c3">>},
    {<<"Name">>, ?SECRET_ID},
    {<<"VersionId">>, ?VERSION_ID},
    {<<"VersionStages">>, [?VERSION_STAGE]}
]).


put_secret_value_output_tests(_) ->
    Tests = [?_sm_test(
        {"put_secret_string output test",
            jsx:encode(?PUT_SECRET_VALUE_RESP),
            {ok, ?PUT_SECRET_VALUE_RESP}}
    )],

    output_tests(?_f(erlcloud_sm:put_secret_string(?SECRET_ID, ?CLIENT_REQUEST_TOKEN, ?SECRET_STRING)), Tests),
    output_tests(?_f(erlcloud_sm:put_secret_binary(?SECRET_ID, ?CLIENT_REQUEST_TOKEN, ?SECRET_STRING)), Tests).
