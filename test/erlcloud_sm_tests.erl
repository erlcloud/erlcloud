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
-define(SECRET_ID2, <<"MyTestDatabaseSecret2">>).
-define(SECRET_STRING, <<"{\"username\":\"david\",\"password\":\"SECRET-PASSWORD\"}">>).
-define(SECRET_BINARY, base64:encode(?SECRET_STRING)).
-define(CLIENT_REQUEST_TOKEN, <<"EXAMPLE2-90ab-cdef-fedc-ba987EXAMPLE">>).
-define(MAX_RESULTS, 10).
-define(NEXT_TOKEN, <<"TOKEN">>).
-define(VERSION_ID, <<"EXAMPLE1-90ab-cdef-fedc-ba987SECRET1">>).
-define(VERSION_ID2, <<"EXAMPLE1-90ab-cdef-fedc-ba987SECRET2">>).
-define(VERSION_STAGE, <<"AWSPREVIOUS">>).

%%%===================================================================
%%% Test entry points
%%%===================================================================

operation_test_() ->
    {foreach,
        fun start/0,
        fun stop/1,
        [
            fun batch_get_secret_value_input_tests/1,
            fun batch_get_secret_value_output_tests/1,
            fun cancel_rotate_secret_value_input_tests/1,
            fun cancel_rotate_secret_value_output_tests/1,
            fun create_secret_value_input_tests/1,
            fun create_secret_value_output_tests/1,
            fun get_random_password_value_input_tests/1,
            fun get_random_password_value_output_tests/1,
            fun get_secret_value_input_tests/1,
            fun get_secret_value_output_tests/1,
            fun list_secrets_value_input_tests/1,
            fun list_secrets_value_output_tests/1,
            fun list_secret_version_ids_value_input_tests/1,
            fun list_secret_version_ids_value_output_tests/1,
            fun put_secret_value_input_tests/1,
            fun put_secret_value_output_tests/1,
            fun remove_regions_from_replication_value_input_tests/1,
            fun remove_regions_from_replication_value_output_tests/1,
            fun replicate_secret_to_regions_value_input_tests/1,
            fun replicate_secret_to_regions_value_output_tests/1,
            fun restore_secret_value_input_tests/1,
            fun restore_secret_value_output_tests/1,
            fun rotate_secret_value_input_tests/1,
            fun rotate_secret_value_output_tests/1,
            fun stop_replication_to_replica_value_input_tests/1,
            fun stop_replication_to_replica_value_output_tests/1,
            fun tag_resource_value_input_tests/1,
            fun tag_resource_value_output_tests/1,
            fun untag_resource_value_input_tests/1,
            fun untag_resource_value_output_tests/1,
            fun update_secret_value_input_tests/1,
            fun update_secret_value_output_tests/1,
            fun update_secret_version_stage_value_input_tests/1,
            fun update_secret_version_stage_value_output_tests/1,
            fun validate_resource_policy_value_input_tests/1,
            fun validate_resource_policy_value_output_tests/1
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
batch_get_secret_value_input_tests(_) ->
        Tests = [
            ?_sm_test(
                {"batch_get_secret_value2id input test",
                    ?_f(
                        erlcloud_sm:batch_get_secret_value(
                            {secret_id_list, [?SECRET_ID, ?SECRET_ID2]},
                            [{max_results, ?MAX_RESULTS}, {next_token, ?NEXT_TOKEN}]
                        )
                    ),
                    jsx:encode([
                        {<<"SecretIdList">>, [?SECRET_ID, ?SECRET_ID2]},
                        {<<"MaxResults">>, ?MAX_RESULTS},
                        {<<"NextToken">>, ?NEXT_TOKEN}
                    ])
            }),
            ?_sm_test(
                {"batch_get_secret_value2Filter input test",
                    ?_f(
                        erlcloud_sm:batch_get_secret_value(
                            {filters, [[{<<"Key">>,<<"name">>}, {<<"Values">>, [<<"value1">>, <<"value2">>]}]]},
                            [{max_results, ?MAX_RESULTS}, {next_token, ?NEXT_TOKEN}]
                        )
                    ),
                    jsx:encode([
                        {<<"Filters">>, [[{<<"Key">>,<<"name">>}, {<<"Values">>, [<<"value1">>, <<"value2">>]}]]},
                        {<<"MaxResults">>, ?MAX_RESULTS},
                        {<<"NextToken">>, ?NEXT_TOKEN}
                    ])
            })
        ],
        Response = <<>>,
        input_tests(Response, Tests).

-define(BATCH_GET_SECRET_VALUE_RESP,[
    {<<"Errors">>, [
        {<<"ErrorCode">>, <<"ResourceNotFoundException">>},
        {<<"ErrorMessage">>, <<"Secret with id 'NonExistentSecret' not found">>},
        {<<"SecretId">>, <<"NonExistentSecret">>}]},
    {<<"NextToken">>, ?NEXT_TOKEN},
    {<<"SecretValues">>, [
        [
            {<<"ARN">>,
                <<"arn:aws:secretsmanager:us-west-2:123456789012:secret:MyTestDatabaseSecret-a1b2c3">>},
            {<<"CreatedDate">>, 1.523477145713E9},
            {<<"Name">>, ?SECRET_ID},
            {<<"SecretString">>,
                <<"{\n \"username\":\"david\",\n \"password\":\"BnQw&XDWgaEeT9XGTT29\"\n}\n">>},
            {<<"VersionId">>, ?VERSION_ID},
            {<<"VersionStages">>, [?VERSION_STAGE]}
        ],
        [
            {<<"ARN">>,
                <<"arn:aws:secretsmanager:us-west-2:123456789012:secret:MyTestDatabaseSecret2-b2c3d4">>},
            {<<"CreatedDate">>, 1.523477145713E9},
            {<<"Name">>, ?SECRET_ID2},
            {<<"SecretString">>,
                <<"{\n \"username\":\"alice\",\n \"password\":\"XyZ1234567890\"\n}\n">>},
            {<<"VersionId">>, ?VERSION_ID2},
            {<<"VersionStages">>, [?VERSION_STAGE]}
        ]
    ]}
]).


batch_get_secret_value_output_tests(_) ->
    Tests = [?_sm_test(
        {"batch_get_secret_value output test",
            jsx:encode(?BATCH_GET_SECRET_VALUE_RESP),
            {ok, ?BATCH_GET_SECRET_VALUE_RESP}}
        )],

    output_tests(?_f(
        erlcloud_sm:batch_get_secret_value(
            {secret_id_list, [?SECRET_ID, ?SECRET_ID2]},
            [{max_results, ?MAX_RESULTS}, {next_token, ?NEXT_TOKEN}])
        ),
        Tests),
    output_tests(?_f(
        erlcloud_sm:batch_get_secret_value(
            {filters, [[{<<"Key">>,<<"name">>}, {<<"Values">>, [?SECRET_ID, ?SECRET_ID2]}]]},
            [{max_results, ?MAX_RESULTS}, {next_token, ?NEXT_TOKEN}])
        ),
        Tests).

cancel_rotate_secret_value_input_tests(_) ->
    Tests = [
        ?_sm_test(
            {"cancel_rotate_secret input test",
                ?_f(erlcloud_sm:cancel_rotate_secret(?SECRET_ID)),
                jsx:encode([
                    {<<"SecretId">>, ?SECRET_ID}
                ])
        })
    ],
    Response = <<>>,
    input_tests(Response, Tests).

-define(CANCEL_ROTATE_SECRET_RESP,[
    {<<"ARN">>,
        <<"arn:aws:secretsmanager:us-west-2:123456789012:secret:MyTestDatabaseSecret-a1b2c3">>},
    {<<"Name">>, ?SECRET_ID},
    {<<"VersionId">>, ?VERSION_ID}
]).

cancel_rotate_secret_value_output_tests(_) ->
    Tests = [?_sm_test(
        {"cancel_rotate_secret output test",
            jsx:encode(?CANCEL_ROTATE_SECRET_RESP),
            {ok, ?CANCEL_ROTATE_SECRET_RESP}}
        )],

    output_tests(?_f(erlcloud_sm:cancel_rotate_secret(?SECRET_ID)), Tests).


create_secret_value_input_tests(_) ->
    Tests = [
        ?_sm_test(
            {"create_secret_string input test",
                ?_f(erlcloud_sm:create_secret_string(?SECRET_ID, ?CLIENT_REQUEST_TOKEN, ?SECRET_STRING)),
                jsx:encode([
                    {<<"ClientRequestToken">>,?CLIENT_REQUEST_TOKEN},
                    {<<"Name">>,?SECRET_ID},
                    {<<"SecretString">>,?SECRET_STRING}])
            }),
        ?_sm_test(
            {"create_secret_binary input test",
                ?_f(erlcloud_sm:create_secret_binary(?SECRET_ID, ?CLIENT_REQUEST_TOKEN, ?SECRET_STRING)),
                jsx:encode([
                    {<<"ClientRequestToken">>,?CLIENT_REQUEST_TOKEN},
                    {<<"Name">>,?SECRET_ID},
                    {<<"SecretBinary">>,?SECRET_BINARY}])
            }),
        ?_sm_test(
            {"create_secret string input test",
                ?_f(erlcloud_sm:create_secret(?SECRET_ID, ?CLIENT_REQUEST_TOKEN, {secret_string, ?SECRET_STRING})),
                jsx:encode([
                    {<<"ClientRequestToken">>,?CLIENT_REQUEST_TOKEN},
                    {<<"Name">>,?SECRET_ID},
                    {<<"SecretString">>,?SECRET_STRING}])
            }),
        ?_sm_test(
            {"create_secret binary input test",
                ?_f(erlcloud_sm:create_secret(?SECRET_ID, ?CLIENT_REQUEST_TOKEN, {secret_binary, ?SECRET_STRING})),
                jsx:encode([
                    {<<"ClientRequestToken">>,?CLIENT_REQUEST_TOKEN},
                    {<<"Name">>,?SECRET_ID},
                    {<<"SecretBinary">>,?SECRET_BINARY}])
            }),
        ?_sm_test(
            {"create_secret binary with options input test",
                ?_f(erlcloud_sm:create_secret(
                    ?SECRET_ID,
                    ?CLIENT_REQUEST_TOKEN,
                    {secret_binary, ?SECRET_STRING},
                    [
                        {add_replica_regions, [
                            [
                                {<<"Region">>, <<"us-east-1">>},
                                {<<"KmsKeyId">>, <<"alias/aws/abcd1234-a123-456a-a12b-a123b4cd56ef">>}
                            ]
                        ]},
                        {description, <<"My test database secret">>},
                        {force_overwrite_replica_secret, true},
                        {kms_key_id, <<"alias/aws/abcd1234-a123-456a-a12b-a123b4cd56ef">>},
                        {tags, [
                            [
                                {<<"Key">>, <<"Environment">>},
                                {<<"Value">>, <<"Production">>}
                            ]
                        ]}
                    ]
                )),
                jsx:encode([
                    {<<"ClientRequestToken">>,?CLIENT_REQUEST_TOKEN},
                    {<<"Name">>,?SECRET_ID},
                    {<<"SecretBinary">>,?SECRET_BINARY},
                    {<<"AddReplicaRegions">>, [
                        [
                            {<<"Region">>, <<"us-east-1">>},
                            {<<"KmsKeyId">>, <<"alias/aws/abcd1234-a123-456a-a12b-a123b4cd56ef">>}
                        ]
                    ]},
                    {<<"Description">>, <<"My test database secret">>},
                    {<<"ForceOverwriteReplicaSecret">>, true},
                    {<<"KmsKeyId">>, <<"alias/aws/abcd1234-a123-456a-a12b-a123b4cd56ef">>},
                    {<<"Tags">>, [
                        [
                            {<<"Key">>, <<"Environment">>},
                            {<<"Value">>, <<"Production">>}
                        ]
                    ]}
                ])
            }),
        ?_sm_test(
            {"create_secret binary with options input test",
                ?_f(erlcloud_sm:create_secret(
                    ?SECRET_ID,
                    ?CLIENT_REQUEST_TOKEN,
                    {secret_binary, ?SECRET_STRING},
                    [
                        {add_replica_regions, [
                            [
                                {region, <<"us-east-1">>},
                                {kms_key_id, <<"alias/aws/abcd1234-a123-456a-a12b-a123b4cd56ef">>}
                            ],
                            [
                                {region, <<"us-west-2">>},
                                {kms_key_id, <<"alias/aws/abcd1234-a123-456a-a12b-a123b4cd56ef">>}
                            ]
                        ]},
                        {description, <<"My test database secret">>},
                        {force_overwrite_replica_secret, true},
                        {kms_key_id, <<"alias/aws/abcd1234-a123-456a-a12b-a123b4cd56ef">>},
                        {tags, [
                            [
                                {key, <<"Environment">>},
                                {value, <<"Production">>}
                            ]
                        ]}
                    ]
                )),
                jsx:encode([
                    {<<"ClientRequestToken">>,?CLIENT_REQUEST_TOKEN},
                    {<<"Name">>,?SECRET_ID},
                    {<<"SecretBinary">>,?SECRET_BINARY},
                    {<<"AddReplicaRegions">>, [
                        [
                            {<<"Region">>, <<"us-east-1">>},
                            {<<"KmsKeyId">>, <<"alias/aws/abcd1234-a123-456a-a12b-a123b4cd56ef">>}
                        ],
                        [
                            {<<"Region">>, <<"us-west-2">>},
                            {<<"KmsKeyId">>, <<"alias/aws/abcd1234-a123-456a-a12b-a123b4cd56ef">>}
                        ]
                    ]},
                    {<<"Description">>, <<"My test database secret">>},
                    {<<"ForceOverwriteReplicaSecret">>, true},
                    {<<"KmsKeyId">>, <<"alias/aws/abcd1234-a123-456a-a12b-a123b4cd56ef">>},
                    {<<"Tags">>, [
                        [
                            {<<"Key">>, <<"Environment">>},
                            {<<"Value">>, <<"Production">>}
                        ]
                    ]}
                ])
            })
    ],
    Response = <<>>,
    input_tests(Response, Tests).


-define(CREATE_SECRET_RESP,[
    {<<"ARN">>,
        <<"arn:aws:secretsmanager:us-west-2:123456789012:secret:MyTestDatabaseSecret-a1b2c3">>},
    {<<"Name">>, ?SECRET_ID},
    {<<"ReplicationStatus">>, [
        [
            {<<"KmsKeyId">>,
                <<"alias/aws/abcd1234-a123-456a-a12b-a123b4cd56ef">>},
            {<<"LastAccessedDate">>, 1.523477145813E9},
            {<<"Region">>, <<"us-east-1">>},
            {<<"Status">>, <<"InSync">>},
            {<<"StatusMessage">>, <<"Replication succeeded">>}
        ]
    ]},
    {<<"VersionId">>, ?VERSION_ID}
]).


create_secret_value_output_tests(_) ->
    Tests = [?_sm_test(
        {"create_secret output test",
            jsx:encode(?CREATE_SECRET_RESP),
            {ok, ?CREATE_SECRET_RESP}}
    )],

    output_tests(?_f(erlcloud_sm:create_secret_string(?SECRET_ID, ?CLIENT_REQUEST_TOKEN, ?SECRET_STRING)), Tests),
    output_tests(?_f(erlcloud_sm:create_secret_binary(?SECRET_ID, ?CLIENT_REQUEST_TOKEN, ?SECRET_STRING)), Tests),
    output_tests(?_f(erlcloud_sm:create_secret(?SECRET_ID, ?CLIENT_REQUEST_TOKEN, {secret_string, ?SECRET_STRING})), Tests),
    output_tests(?_f(erlcloud_sm:create_secret(?SECRET_ID, ?CLIENT_REQUEST_TOKEN, {secret_binary, ?SECRET_BINARY})), Tests).

get_random_password_value_input_tests(_) ->
    Tests = [
        ?_sm_test(
            {"get_random_password value input test",
                ?_f(erlcloud_sm:get_random_password()),
                jsx:encode([])
        }),
        ?_sm_test(
            {"get_random_password input test",
                ?_f(erlcloud_sm:get_random_password(
                    [
                        {<<"ExcludeCharacters">>, <<"!@#$%^&*()_+">>},
                        {<<"ExcludeLowercase">>, true},
                        {<<"ExcludeNumbers">>, true},
                        {<<"ExcludePunctuation">>, true},
                        {<<"ExcludeUppercase">>, true},
                        {<<"IncludeSpace">>, true},
                        {<<"PasswordLength">>, 16},
                        {<<"RequireEachIncludedType">>, true}
                    ])),
                jsx:encode(
                    [
                        {<<"ExcludeCharacters">>, <<"!@#$%^&*()_+">>},
                        {<<"ExcludeLowercase">>, true},
                        {<<"ExcludeNumbers">>, true},
                        {<<"ExcludePunctuation">>, true},
                        {<<"ExcludeUppercase">>, true},
                        {<<"IncludeSpace">>, true},
                        {<<"PasswordLength">>, 16},
                        {<<"RequireEachIncludedType">>, true}
                    ])
        })
    ],
    Response = <<>>,
    input_tests(Response, Tests).

-define(GET_RANDOM_PASSWORD_RESP,[
    {<<"RandomPassword">>, <<"BnQw&XDWgaEeT9XGTT29">>}
]).

get_random_password_value_output_tests(_) ->
    Tests = [?_sm_test(
        {"get_random_password value output test",
            jsx:encode(?GET_RANDOM_PASSWORD_RESP),
            {ok, ?GET_RANDOM_PASSWORD_RESP}}
        )],

    output_tests(?_f(erlcloud_sm:get_random_password()), Tests).

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

list_secrets_value_input_tests(_) ->
    Tests = [
        ?_sm_test(
            {"list_secrets value input test",
                ?_f(erlcloud_sm:list_secrets()),
                jsx:encode([])
        }),
        ?_sm_test(
            {"list_secrets input test",
                ?_f(erlcloud_sm:list_secrets(
                    [
                        {filters, [[{<<"Key">>,<<"name">>}, {<<"Values">>, [<<"value1">>, <<"value2">>]}]]},
                        {max_results, ?MAX_RESULTS},
                        {include_planned_deletion, true},
                        {next_token, ?NEXT_TOKEN},
                        {sort_order, <<"asc">>}
                    ])),
                jsx:encode(
                    [
                        {<<"Filters">>, [[{<<"Key">>,<<"name">>}, {<<"Values">>, [<<"value1">>, <<"value2">>]}]]},
                        {<<"MaxResults">>, ?MAX_RESULTS},
                        {<<"IncludePlannedDeletion">>, true},
                        {<<"NextToken">>, ?NEXT_TOKEN},
                        {<<"SortOrder">>, <<"asc">>}
                    ])
        })
    ],
    Response = <<>>,
    input_tests(Response, Tests).

-define(LIST_SECRETS_VALUE_RESP,[
    {<<"NextToken">>, ?NEXT_TOKEN},
    {<<"SecretList">>, [
        [
            {<<"ARN">>,
                <<"arn:aws:secretsmanager:us-west-2:123456789012:secret:MyTestDatabaseSecret-a1b2c3">>},
            {<<"CreatedDate">>, 1.523477145713E9},
            {<<"DeletedDate">>, 1.523477145813E9},
            {<<"Description">>, <<"My test database secret">>},
            {<<"KmsKeyId">>,
                <<"alias/aws/abcd1234-a123-456a-a12b-a123b4cd56ef">>},
            {<<"LastAccessedDate">>, 1.523477145913E9},
            {<<"LastChangedDate">>, 1.523477146013E9},
            {<<"LastRotatedDate">>, 1.523477146113E9},
            {<<"Name">>, ?SECRET_ID},
            {<<"NextRotationDate">>, 1.523477146213E9},
            {<<"OwningService">>, <<"secretsmanager">>},
            {<<"PrimaryRegion">>, <<"us-west-2">>},
            {<<"RotationEnabled">>, true},
            {<<"RotationLambdaARN">>,
                <<"arn:aws:lambda:us-west-2:123456789012:function:MySecretRotationFunction">>},
            {<<"RotationRules">>, [
                {<<"AutomaticallyAfterDays">>,90},
                {<<"Duration">>,<<"3h">>},
                {<<"ScheduleExpression">>,<<"rate(30 days)">>}
            ]},
            {<<"SecretVersionsToStages">>, [
                { ?VERSION_ID, [?VERSION_STAGE]},
                { ?VERSION_ID2, [<<"AWSCURRENT">>]}
            ]},
            {<<"Tags">>, [
                [
                    {<<"Key">>, <<"Environment">>},
                    {<<"Value">>, <<"Production">>}
                ]
            ]}
        ]
    ]}
]).

list_secrets_value_output_tests(_) ->
    Tests = [?_sm_test(
        {"list_secrets value output test",
            jsx:encode(?LIST_SECRETS_VALUE_RESP),
            {ok, ?LIST_SECRETS_VALUE_RESP}}
        )],

    output_tests(?_f(erlcloud_sm:list_secrets()), Tests).

list_secret_version_ids_value_input_tests(_) ->
    Tests = [
        ?_sm_test(
            {"list_secret_version_ids value input test",
                ?_f(erlcloud_sm:list_secret_version_ids(?SECRET_ID)),
                jsx:encode([{<<"SecretId">>, ?SECRET_ID}])
        }),
        ?_sm_test(
            {"list_secret_version_ids input test",
                ?_f(erlcloud_sm:list_secret_version_ids(
                    ?SECRET_ID,
                    [
                        {include_deprecated, true},
                        {max_results, ?MAX_RESULTS},
                        {next_token, ?NEXT_TOKEN}
                    ])),
                jsx:encode(
                    [
                        {<<"SecretId">>, ?SECRET_ID},
                        {<<"IncludeDeprecated">>, true},
                        {<<"MaxResults">>, ?MAX_RESULTS},
                        {<<"NextToken">>, ?NEXT_TOKEN}
                    ])
        })
    ],
    Response = <<>>,
    input_tests(Response, Tests).

-define(LIST_SECRET_VERSION_IDS_VALUE_RESP,[
    {<<"ARN">>,
        <<"arn:aws:secretsmanager:us-west-2:123456789012:secret:MyTestDatabaseSecret-a1b2c3">>},
    {<<"Name">>, ?SECRET_ID},
    {<<"NextToken">>, ?NEXT_TOKEN},
    {<<"Versions">>, [
        [
            {<<"CreatedDate">>, 1.523477145713E9},
            {<<"KmsKeyId">>,
                <<"alias/aws/abcd1234-a123-456a-a12b-a123b4cd56ef">>},
            {<<"LastAccessedDate">>, 1.523477145813E9},
            {<<"VersionId">>, ?VERSION_ID},
            {<<"VersionStages">>, [?VERSION_STAGE]}
        ]
    ]}
]).

list_secret_version_ids_value_output_tests(_) ->
    Tests = [?_sm_test(
        {"list_secret_version_ids value output test",
            jsx:encode(?LIST_SECRET_VERSION_IDS_VALUE_RESP),
            {ok, ?LIST_SECRET_VERSION_IDS_VALUE_RESP}}
        )],

    output_tests(?_f(erlcloud_sm:list_secret_version_ids(?SECRET_ID)), Tests).


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
            }),
        ?_sm_test(
            {"put_secret_value string input test",
                ?_f(erlcloud_sm:put_secret_value(?SECRET_ID, ?CLIENT_REQUEST_TOKEN, {secret_string, ?SECRET_STRING})),
                jsx:encode([
                    {<<"ClientRequestToken">>,?CLIENT_REQUEST_TOKEN},
                    {<<"SecretId">>,?SECRET_ID},
                    {<<"SecretString">>,?SECRET_STRING}])
            }),
        ?_sm_test(
            {"put_secret_value binary input test",
                ?_f(erlcloud_sm:put_secret_value(?SECRET_ID, ?CLIENT_REQUEST_TOKEN, {secret_binary, ?SECRET_STRING})),
                jsx:encode([
                    {<<"ClientRequestToken">>,?CLIENT_REQUEST_TOKEN},
                    {<<"SecretId">>,?SECRET_ID},
                    {<<"SecretBinary">>,?SECRET_BINARY}])
            }),
        ?_sm_test(
            {"put_secret_value binary input test",
                ?_f(erlcloud_sm:put_secret_value(
                    ?SECRET_ID,
                    ?CLIENT_REQUEST_TOKEN,
                    {secret_binary, ?SECRET_STRING},
                    [
                        {version_stages, [?VERSION_STAGE]},
                        {rotation_token, <<"ROTATION-TOKEN-EXAMPLE-1234">>}
                    ])),
                jsx:encode([
                    {<<"ClientRequestToken">>,?CLIENT_REQUEST_TOKEN},
                    {<<"SecretId">>,?SECRET_ID},
                    {<<"SecretBinary">>,?SECRET_BINARY},
                    {<<"VersionStages">>, [?VERSION_STAGE]},
                    {<<"RotationToken">>, <<"ROTATION-TOKEN-EXAMPLE-1234">>}
                ])
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
    output_tests(?_f(erlcloud_sm:put_secret_binary(?SECRET_ID, ?CLIENT_REQUEST_TOKEN, ?SECRET_STRING)), Tests),
    output_tests(?_f(erlcloud_sm:put_secret_value(?SECRET_ID, ?CLIENT_REQUEST_TOKEN, {secret_string, ?SECRET_STRING})), Tests),
    output_tests(?_f(erlcloud_sm:put_secret_value(?SECRET_ID, ?CLIENT_REQUEST_TOKEN, {secret_binary, ?SECRET_BINARY})), Tests).

remove_regions_from_replication_value_input_tests(_) ->
    Tests = [
        ?_sm_test(
            {"remove_regions_from_replication value input test",
                ?_f(erlcloud_sm:remove_regions_from_replication(?SECRET_ID, [<<"us-east-1">>])),
                jsx:encode([{<<"SecretId">>, ?SECRET_ID}, {<<"RemoveReplicaRegions">>, [<<"us-east-1">>]}])
        })
    ],
    Response = <<>>,
    input_tests(Response, Tests).

-define(REMOVE_REGIONS_FROM_REPLICATION_VALUE_RESP,[
    {<<"ARN">>,
        <<"arn:aws:secretsmanager:us-west-2:123456789012:secret:MyTestDatabaseSecret-a1b2c3">>},
    {<<"ReplicationStatus">>, [
        [
            {<<"KmsKeyId">>,
                <<"alias/aws/abcd1234-a123-456a-a12b-a123b4cd56ef">>},
            {<<"LastAccessedDate">>, 1.523477145813E9},
            {<<"Region">>, <<"us-west-2">>},
            {<<"Status">>, <<"InSync">>},
            {<<"StatusMessage">>, <<"Replication succeeded">>}
        ]
    ]}
]).

remove_regions_from_replication_value_output_tests(_) ->
    Tests = [?_sm_test(
        {"remove_regions_from_replication value output test",
            jsx:encode(?REMOVE_REGIONS_FROM_REPLICATION_VALUE_RESP),
            {ok, ?REMOVE_REGIONS_FROM_REPLICATION_VALUE_RESP}}
        )],

    output_tests(?_f(erlcloud_sm:remove_regions_from_replication(?SECRET_ID, [<<"us-east-1">>])), Tests).

replicate_secret_to_regions_value_input_tests(_) ->
    Tests = [
        ?_sm_test(
            {"replicate_secret_to_regions value input test",
                ?_f(erlcloud_sm:replicate_secret_to_regions(
                    ?SECRET_ID,
                    [[{<<"Region">>, <<"us-east-1">>}, {<<"KmsKeyId">>, <<"alias/aws/abcd1234-a123-456a-a12b-a123b4cd56ef">>}]]
                )),
                jsx:encode([
                    {<<"SecretId">>, ?SECRET_ID},
                    {<<"ReplicaRegions">>, [[{<<"Region">>, <<"us-east-1">>}, {<<"KmsKeyId">>, <<"alias/aws/abcd1234-a123-456a-a12b-a123b4cd56ef">>}]]}
                ])
        }),
        ?_sm_test(
            {"replicate_secret_to_regions value input test",
                ?_f(erlcloud_sm:replicate_secret_to_regions(
                    ?SECRET_ID,
                    [[{region, <<"us-east-1">>}, {kms_key_id, <<"alias/aws/abcd1234-a123-456a-a12b-a123b4cd56ef">>}]]
                )),
                jsx:encode([
                    {<<"SecretId">>, ?SECRET_ID},
                    {<<"ReplicaRegions">>, [[{<<"Region">>, <<"us-east-1">>}, {<<"KmsKeyId">>, <<"alias/aws/abcd1234-a123-456a-a12b-a123b4cd56ef">>}]]}
                ])
        }),
        ?_sm_test(
            {"replicate_secret_to_regions value input test",
                ?_f(erlcloud_sm:replicate_secret_to_regions(
                    ?SECRET_ID,
                    [[{<<"Region">>, <<"us-east-1">>}, {<<"KmsKeyId">>, <<"alias/aws/abcd1234-a123-456a-a12b-a123b4cd56ef">>}]],
                    [{force_overwrite_replica_secret, true}]
                )),
                jsx:encode([
                    {<<"SecretId">>, ?SECRET_ID},
                    {<<"ReplicaRegions">>, [[{<<"Region">>, <<"us-east-1">>}, {<<"KmsKeyId">>, <<"alias/aws/abcd1234-a123-456a-a12b-a123b4cd56ef">>}]]},
                    {<<"ForceOverwriteReplicaSecret">>, true}
                ])
        })
    ],
    Response = <<>>,
    input_tests(Response, Tests).

-define(REPLICATE_SECRET_TO_REGIONS_VALUE_RESP,[
    {<<"ARN">>,
        <<"arn:aws:secretsmanager:us-west-2:123456789012:secret:MyTestDatabaseSecret-a1b2c3">>},
    {<<"ReplicationStatus">>, [
        [
            {<<"KmsKeyId">>,
                <<"alias/aws/abcd1234-a123-456a-a12b-a123b4cd56ef">>},
            {<<"LastAccessedDate">>, 1.523477145813E9},
            {<<"Region">>, <<"us-west-2">>},
            {<<"Status">>, <<"InSync">>},
            {<<"StatusMessage">>, <<"Replication succeeded">>}
        ],
        [
            {<<"KmsKeyId">>,
                <<"alias/aws/abcd1234-a123-456a-a12b-a123b4cd56ef">>},
            {<<"LastAccessedDate">>, 1.523477145813E9},
            {<<"Region">>, <<"us-east-1">>},
            {<<"Status">>, <<"InSync">>},
            {<<"StatusMessage">>, <<"Replication succeeded">>}
        ]
    ]}
]).

replicate_secret_to_regions_value_output_tests(_) ->
    Tests = [?_sm_test(
        {"replicate_secret_to_regions value output test",
            jsx:encode(?REPLICATE_SECRET_TO_REGIONS_VALUE_RESP),
            {ok, ?REPLICATE_SECRET_TO_REGIONS_VALUE_RESP}}
        )],

    output_tests(?_f(erlcloud_sm:replicate_secret_to_regions(?SECRET_ID, [[{region, <<"us-east-1">>}, {kms_key_id, <<"alias/aws/abcd1234-a123-456a-a12b-a123b4cd56ef">>}]])), Tests).

restore_secret_value_input_tests(_) ->
    Tests = [
        ?_sm_test(
            {"restore_secret input test",
                ?_f(erlcloud_sm:restore_secret(?SECRET_ID)),
                jsx:encode([{<<"SecretId">>, ?SECRET_ID}])
        })
    ],
    Response = <<>>,
    input_tests(Response, Tests).

-define(RESTORE_SECRET_VALUE_RESP,[
    {<<"ARN">>,
        <<"arn:aws:secretsmanager:us-west-2:123456789012:secret:MyTestDatabaseSecret-a1b2c3">>},
    {<<"Name">>, ?SECRET_ID}]).

restore_secret_value_output_tests(_) ->
    Tests = [?_sm_test(
        {"restore_secret value output test",
            jsx:encode(?RESTORE_SECRET_VALUE_RESP),
            {ok, ?RESTORE_SECRET_VALUE_RESP}}
        )],

    output_tests(?_f(erlcloud_sm:restore_secret(?SECRET_ID)), Tests).

rotate_secret_value_input_tests(_) ->
    Tests = [
        ?_sm_test(
            {"rotate_secret input test",
                ?_f(erlcloud_sm:rotate_secret(?SECRET_ID, ?CLIENT_REQUEST_TOKEN)),
                jsx:encode([
                    {<<"ClientRequestToken">>,?CLIENT_REQUEST_TOKEN},
                    {<<"SecretId">>,?SECRET_ID}])
        }),
        ?_sm_test(
            {"rotate_secret with options input test",
                ?_f(erlcloud_sm:rotate_secret(
                    ?SECRET_ID,
                    ?CLIENT_REQUEST_TOKEN,
                    [
                        {rotation_lambda_arn, <<"arn:aws:lambda:us-west-2:123456789012:function:MySecretRotationFunction">>},
                        {rotate_immediately, false},
                        {rotation_rules, [
                            {automatically_after_days, 30},
                            {duration, <<"3h">>},
                            {schedule_expression, <<"rate(30 days)">>}
                        ]}
                    ]
                )),
                jsx:encode([
                    {<<"ClientRequestToken">>,?CLIENT_REQUEST_TOKEN},
                    {<<"SecretId">>,?SECRET_ID},
                    {<<"RotationLambdaARN">>, <<"arn:aws:lambda:us-west-2:123456789012:function:MySecretRotationFunction">>},
                    {<<"RotateImmediately">>, false},
                    {<<"RotationRules">>, [
                        {<<"AutomaticallyAfterDays">>,30},
                        {<<"Duration">>,<<"3h">>},
                        {<<"ScheduleExpression">>,<<"rate(30 days)">>}
                    ]}
                ])
        })
    ],
    Response = <<>>,
    input_tests(Response, Tests).

    -define(ROTATE_SECRET_VALUE_RESP,[
        {<<"ARN">>,
            <<"arn:aws:secretsmanager:us-west-2:123456789012:secret:MyTestDatabaseSecret-a1b2c3">>},
        {<<"Name">>, ?SECRET_ID},
        {<<"VersionId">>, ?VERSION_ID}
    ]).

    rotate_secret_value_output_tests(_) ->
        Tests = [?_sm_test(
            {"rotate_secret value output test",
                jsx:encode(?ROTATE_SECRET_VALUE_RESP),
                {ok, ?ROTATE_SECRET_VALUE_RESP}}
            )],

    output_tests(?_f(erlcloud_sm:rotate_secret(?SECRET_ID, ?CLIENT_REQUEST_TOKEN)), Tests),
    output_tests(?_f(erlcloud_sm:rotate_secret(
        ?SECRET_ID,
        ?CLIENT_REQUEST_TOKEN,
        [
            {rotation_lambda_arn, <<"arn:aws:lambda:us-west-2:123456789012:function:MySecretRotationFunction">>},
            {rotate_immediately, false},
            {rotation_rules, [
                {automatically_after_days, 30},
                {duration, <<"3h">>},
                {schedule_expression, <<"rate(30 days)">>}
            ]}
        ]
    )), Tests).

    stop_replication_to_replica_value_input_tests(_) ->
        Tests = [
            ?_sm_test(
                {"stop_replication_to_replica input test",
                    ?_f(erlcloud_sm:stop_replication_to_replica(?SECRET_ID)),
                    jsx:encode([{<<"SecretId">>, ?SECRET_ID}])
            })
        ],
        Response = <<>>,
        input_tests(Response, Tests).

    -define(STOP_REPLICATION_TO_REPLICA_VALUE_RESP,[
        [{<<"ARN">>, <<"arn:aws:secretsmanager:us-west-2:123456789012:secret:MyTestDatabaseSecret-a1b2c3">>}]
    ]).

    stop_replication_to_replica_value_output_tests(_) ->
        Tests = [?_sm_test(
            {"stop_replication_to_replica value output test",
                jsx:encode(?STOP_REPLICATION_TO_REPLICA_VALUE_RESP),
                {ok, ?STOP_REPLICATION_TO_REPLICA_VALUE_RESP}}
            )],
        output_tests(?_f(erlcloud_sm:stop_replication_to_replica(?SECRET_ID)), Tests).

    tag_resource_value_input_tests(_) ->
        Tests = [
            ?_sm_test(
                {"tag_resource input test",
                    ?_f(erlcloud_sm:tag_resource(?SECRET_ID, [
                        [ {<<"Key">>, <<"Environment">>}, {<<"Value">>, <<"Production">>} ]
                    ])),
                    jsx:encode([
                        {<<"SecretId">>, ?SECRET_ID},
                        {<<"Tags">>, [
                            [ {<<"Key">>, <<"Environment">>}, {<<"Value">>, <<"Production">>} ]
                        ]}
                    ])
            }),
            ?_sm_test(
                {"tag_resource input test",
                    ?_f(erlcloud_sm:tag_resource(?SECRET_ID, [
                        [ {key, <<"Environment">>}, {value, <<"Production">>} ]
                    ])),
                    jsx:encode([
                        {<<"SecretId">>, ?SECRET_ID},
                        {<<"Tags">>, [
                            [ {<<"Key">>, <<"Environment">>}, {<<"Value">>, <<"Production">>} ]
                        ]}
                    ])
            })
        ],
        Response = <<>>,
        input_tests(Response, Tests).

    tag_resource_value_output_tests(_) ->
        Tests = [?_sm_test(
            {"tag_resource value output test",
                <<>>,
                {ok, []}}
            )],
        output_tests(?_f(erlcloud_sm:tag_resource(?SECRET_ID, [
            [ {<<"Key">>, <<"Environment">>}, {<<"Value">>, <<"Production">>} ]
        ])), Tests),
        output_tests(?_f(erlcloud_sm:tag_resource(?SECRET_ID, [
            [ {key, <<"Environment">>}, {value, <<"Production">>} ]
        ])), Tests).

    untag_resource_value_input_tests(_) ->
        Tests = [
            ?_sm_test(
                {"untag_resource input test",
                    ?_f(erlcloud_sm:untag_resource(?SECRET_ID, ["Environment"])),
                    jsx:encode([{<<"SecretId">>, ?SECRET_ID}, {<<"TagKeys">>, ["Environment"]}])
                })
        ],
        Response = <<>>,
        input_tests(Response, Tests).

    untag_resource_value_output_tests(_) ->
        Tests = [?_sm_test(
            {"untag_resource value output test",
                <<>>,
                {ok, []}}
            )],
        output_tests(?_f(erlcloud_sm:untag_resource(?SECRET_ID, ["Environment"])), Tests).

    update_secret_value_input_tests(_) ->
        Tests = [
            ?_sm_test(
                {"update_secret input test",
                    ?_f(erlcloud_sm:update_secret(?SECRET_ID, ?CLIENT_REQUEST_TOKEN,
                        [
                            {secret_string, ?SECRET_STRING},
                            {description, <<"Updated secret description">>},
                            {kms_key_id, <<"alias/aws/abcd1234-a123-456a-a12b-a123b4cd56ef">>}
                        ])),
                    jsx:encode([
                        {<<"ClientRequestToken">>,?CLIENT_REQUEST_TOKEN},
                        {<<"SecretId">>,?SECRET_ID},
                        {<<"SecretString">>,?SECRET_STRING},
                        {<<"Description">>, <<"Updated secret description">>},
                        {<<"KmsKeyId">>, <<"alias/aws/abcd1234-a123-456a-a12b-a123b4cd56ef">>}
                    ])
            }),
            ?_sm_test(
                {"update_secret input test",
                    ?_f(erlcloud_sm:update_secret(?SECRET_ID, ?CLIENT_REQUEST_TOKEN,
                        [
                            {secret_binary, ?SECRET_STRING},
                            {description, <<"Updated secret description">>},
                            {kms_key_id, <<"alias/aws/abcd1234-a123-456a-a12b-a123b4cd56ef">>}
                        ])),
                    jsx:encode([
                        {<<"ClientRequestToken">>,?CLIENT_REQUEST_TOKEN},
                        {<<"SecretId">>,?SECRET_ID},
                        {<<"SecretBinary">>,?SECRET_BINARY},
                        {<<"Description">>, <<"Updated secret description">>},
                        {<<"KmsKeyId">>, <<"alias/aws/abcd1234-a123-456a-a12b-a123b4cd56ef">>}
                    ])
            })
        ],
        Response = <<>>,
        input_tests(Response, Tests).

    -define(UPDATE_SECRET_VALUE_RESP,[
        {<<"ARN">>,
            <<"arn:aws:secretsmanager:us-west-2:123456789012:secret:MyTestDatabaseSecret-a1b2c3">>},
        {<<"Name">>, ?SECRET_ID},
        {<<"VersionId">>, ?VERSION_ID}
    ]).

    update_secret_value_output_tests(_) ->
        Tests = [?_sm_test(
            {"update_secret value output test",
                jsx:encode(?UPDATE_SECRET_VALUE_RESP),
                {ok, ?UPDATE_SECRET_VALUE_RESP}}
            )],
        output_tests(?_f(erlcloud_sm:update_secret(?SECRET_ID, ?CLIENT_REQUEST_TOKEN,
            [
                {secret_string, ?SECRET_STRING},
                {description, <<"Updated secret description">>},
                {kms_key_id, <<"alias/aws/abcd1234-a123-456a-a12b-a123b4cd56ef">>}
            ])), Tests),
        output_tests(?_f(erlcloud_sm:update_secret(?SECRET_ID, ?CLIENT_REQUEST_TOKEN,
            [
                {secret_binary, ?SECRET_STRING},
                {description, <<"Updated secret description">>},
                {kms_key_id, <<"alias/aws/abcd1234-a123-456a-a12b-a123b4cd56ef">>}
            ])), Tests).

    update_secret_version_stage_value_input_tests(_) ->
        Tests = [
            ?_sm_test(
                {"update_secret_version_stage input test",
                    ?_f(erlcloud_sm:update_secret_version_stage(
                        ?SECRET_ID,
                        ?VERSION_STAGE
                    )),
                    jsx:encode([
                        {<<"SecretId">>, ?SECRET_ID},
                        {<<"VersionStage">>, ?VERSION_STAGE}
                    ])
            }),
            ?_sm_test(
                {"update_secret_version_stage input test",
                    ?_f(erlcloud_sm:update_secret_version_stage(
                        ?SECRET_ID,
                        ?VERSION_STAGE,
                        [{remove_from_version_id, ?VERSION_ID2}, {move_to_version_id, ?VERSION_ID}]
                    )),
                    jsx:encode([
                        {<<"SecretId">>, ?SECRET_ID},
                        {<<"VersionStage">>, ?VERSION_STAGE},
                        {<<"MoveToVersionId">>, ?VERSION_ID},
                        {<<"RemoveFromVersionId">>, ?VERSION_ID2}
                    ])
            })
        ],
        Response = <<>>,
        input_tests(Response, Tests).

    -define(UPDATE_SECRET_VERSION_STAGE_VALUE_RESP,[
        {<<"ARN">>,
            <<"arn:aws:secretsmanager:us-west-2:123456789012:secret:MyTestDatabaseSecret-a1b2c3">>},
        {<<"Name">>, ?SECRET_ID}
    ]).

    update_secret_version_stage_value_output_tests(_) ->
        Tests = [?_sm_test(
            {"update_secret_version_stage value output test",
                jsx:encode(?UPDATE_SECRET_VERSION_STAGE_VALUE_RESP),
                {ok, ?UPDATE_SECRET_VERSION_STAGE_VALUE_RESP}}
            )],
        output_tests(?_f(erlcloud_sm:update_secret_version_stage(
            ?SECRET_ID,
            ?VERSION_STAGE
        )), Tests),
        output_tests(?_f(erlcloud_sm:update_secret_version_stage(
            ?SECRET_ID,
            ?VERSION_STAGE,
            [{remove_from_version_id, ?VERSION_ID2}, {move_to_version_id, ?VERSION_ID}]
        )), Tests).

    validate_resource_policy_value_input_tests(_) ->
        Tests = [
            ?_sm_test(
                {"validate_resource_policy input test",
                    ?_f(erlcloud_sm:validate_resource_policy(<<"policy-document">>)),
                    jsx:encode([
                        {<<"ResourcePolicy">>, <<"policy-document">>}
                    ])
            }),
            ?_sm_test(
                {"validate_resource_policy input test",
                    ?_f(erlcloud_sm:validate_resource_policy(<<"policy-document">>, [{secret_id, ?SECRET_ID}])),
                    jsx:encode([
                        {<<"ResourcePolicy">>, <<"policy-document">>},
                        {<<"SecretId">>, ?SECRET_ID}
                    ])
            })
        ],
        Response = <<>>,
        input_tests(Response, Tests).

    -define(VALIDATE_RESOURCE_POLICY_VALUE_RESP,[
        {<<"PolicyValidationPassed">>, true},
        {<<"ValidationErrors">>, []}
    ]).

    validate_resource_policy_value_output_tests(_) ->
        Tests = [?_sm_test(
            {"validate_resource_policy value output test",
                jsx:encode(?VALIDATE_RESOURCE_POLICY_VALUE_RESP),
                {ok, ?VALIDATE_RESOURCE_POLICY_VALUE_RESP}}
            )],
        output_tests(?_f(erlcloud_sm:validate_resource_policy(<<"policy-document">>)), Tests),
        output_tests(?_f(erlcloud_sm:validate_resource_policy(<<"policy-document">>, [{secret_id, ?SECRET_ID}])), Tests).