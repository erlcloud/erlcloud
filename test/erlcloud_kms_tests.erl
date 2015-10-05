%% -*- mode: erlang;erlang-indent-level: 4;indent-tabs-mode: nil -*-
-module(erlcloud_kms_tests).
-include_lib("eunit/include/eunit.hrl").
-include("erlcloud.hrl").

%% Unit tests for kms.
%% These tests work by using meck to mock erlcloud_httpc. There are two classes of test: input and output.
%%
%% Input tests verify that different function args produce the desired JSON request.
%% An input test list provides a list of funs and the JSON that is expected to result.
%%
%% Output tests verify that the http response produces the correct return from the fun.
%% An output test lists provides a list of response bodies and the expected return.

%% The _kms_test macro provides line number annotation to a test, similar to _test, but doesn't wrap in a fun
-define(_kms_test(T), {?LINE, T}).
%% The _f macro is a terse way to wrap code in a fun. Similar to _test but doesn't annotate with a line number
-define(_f(F), fun() -> F end).

-export([validate_body/2]).

%%%===================================================================
%%% Common Test Values
%%%===================================================================
-define(AWS_ACCOUNT_ID, <<"012345678901">>).
-define(KEY_ID, <<"12345678-1234-1234-1234-?KEY_ARN">>).
-define(KEY_ARN, <<"arn:aws:kms:us-west-2:", ?AWS_ACCOUNT_ID/binary, ":key/", ?KEY_ID/binary>>).


%%%===================================================================
%%% Test entry points
%%%===================================================================
operation_test_() ->
    {foreach,
     fun start/0,
     fun stop/1,
     [fun create_alias_input_tests/1,
      fun create_alias_output_tests/1,
      fun create_grant_input_tests/1,
      fun create_grant_output_tests/1,
      fun create_key_input_tests/1,
      fun create_key_output_tests/1,
      fun decrypt_input_tests/1,
      fun decrypt_output_tests/1,
      fun delete_alias_input_tests/1,
      fun delete_alias_output_tests/1,
      fun describe_key_input_tests/1,
      fun describe_key_output_tests/1,
      fun disable_key_input_tests/1,
      fun disable_key_output_tests/1,
      fun disable_key_rotation_input_tests/1,
      fun disable_key_rotation_output_tests/1,
      fun enable_key_input_tests/1,
      fun enable_key_output_tests/1,
      fun enable_key_rotation_input_tests/1,
      fun enable_key_rotation_output_tests/1,
      fun encrypt_input_tests/1,
      fun encrypt_output_tests/1,
      fun generate_data_key_input_tests/1,
      fun generate_data_key_output_tests/1,
      fun generate_data_key_without_plaintext_input_tests/1,
      fun generate_data_key_without_plaintext_output_tests/1,
      fun generate_random_input_tests/1,
      fun generate_random_output_tests/1,
      fun get_key_policy_input_tests/1,
      fun get_key_policy_output_tests/1,
      fun get_key_rotation_status_input_tests/1,
      fun get_key_rotation_status_output_tests/1,
      fun list_aliases_input_tests/1,
      fun list_aliases_output_tests/1,
      fun list_grants_input_tests/1,
      fun list_grants_output_tests/1,
      fun list_key_policies_input_tests/1,
      fun list_key_policies_output_tests/1,
      fun list_keys_input_tests/1,
      fun list_keys_output_tests/1,
      fun put_key_policy_input_tests/1,
      fun put_key_policy_output_tests/1,
      fun re_encrypt_input_tests/1,
      fun re_encrypt_output_tests/1,
      fun retire_grant_input_tests/1,
      fun retire_grant_output_tests/1,
      fun revoke_grant_input_tests/1,
      fun revoke_grant_output_tests/1,
      fun update_alias_input_tests/1,
      fun update_alias_output_tests/1,
      fun update_key_description_input_tests/1,
      fun update_key_description_output_tests/1]}.


start() ->
    meck:new(erlcloud_httpc),
    ok.


stop(_) ->
    meck:unload(erlcloud_httpc).


%%%===================================================================
%%% Input test helpers
%%%===================================================================

-type expected_body() :: string().

sort_json([{_, _} | _] = Json) ->
    %% Value is an object
    SortedChildren = [{K, sort_json(V)} || {K,V} <- Json],
    lists:keysort(1, SortedChildren);
sort_json([_|_] = Json) ->
    %% Value is an array
    [sort_json(I) || I <- Json];
sort_json(V) ->
    V.

%% verifies that the parameters in the body match the expected parameters
-spec validate_body(binary(), expected_body()) -> ok.
validate_body(Body, Expected) ->
    Want = sort_json(jsx:decode(Expected)),
    Actual = sort_json(jsx:decode(Body)),
    case Want =:= Actual of
        true -> ok;
        false ->
            ?debugFmt("~nEXPECTED~n~p~nACTUAL~n~p~n", [Want, Actual])
    end,
    ?assertEqual(Want, Actual).


%% returns the mock of the erlcloud_httpc function input tests expect to be called.
%% Validates the request body and responds with the provided response.
-spec input_expect(string(), expected_body()) -> fun().
input_expect(Response, Expected) ->
    fun(_Url, post, _Headers, Body, _Timeout, _Config) ->
            validate_body(Body, Expected),
            {ok, {{200, "OK"}, [], Response}}
    end.


%% input_test converts an input_test specifier into an eunit test generator
-type input_test_spec() :: {pos_integer(), {fun(), expected_body()} | {string(), fun(), expected_body()}}.
-spec input_test(string(), input_test_spec()) -> tuple().
input_test(Response, {Line, {Description, Fun, Expected}})
  when is_list(Description) ->
    {Description,
     {Line,
      fun() ->
              meck:expect(erlcloud_httpc, request, input_expect(Response, Expected)),
              erlcloud_kms:configure(string:copies("A", 20), string:copies("a", 40)),
              Fun()
      end}}.


%% input_tests converts a list of input_test specifiers into an eunit test generator
-spec input_tests(string(), [input_test_spec()]) -> [tuple()].
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
-type output_test_spec() :: {pos_integer(), {string(), term()} | {string(), string(), term()}}.
-spec output_test(fun(), output_test_spec()) -> tuple().
output_test(Fun, {Line, {Description, Response, Result}}) ->
    {Description,
     {Line,
      fun() ->
              meck:expect(erlcloud_httpc, request, output_expect(Response)),
              erlcloud_kms:configure(string:copies("A", 20), string:copies("a", 40)),
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
%%% Actual test specifiers
%%%===================================================================


create_alias_input_tests(_) ->
    Tests = [?_kms_test(
             {"CreateAlias input test",
              ?_f(erlcloud_kms:create_alias(<<"alias/eunit_test_key">>,
                                            ?KEY_ID)),
              jsx:encode([{<<"AliasName">>, <<"alias/eunit_test_key">>},
                          {<<"TargetKeyId">>, ?KEY_ID}])
             }
             )],

    Response = <<>>,
    input_tests(Response, Tests).


create_alias_output_tests(_) ->
    Tests = [?_kms_test(
             {"CreateAlias example response",
              <<>>,
              {ok, []}}
             )],

    output_tests(?_f(erlcloud_kms:create_alias(<<"alias/eunit_test_key">>,
                                               ?KEY_ID)), Tests).

-define(CREATE_GRANT_RESP,
        [{<<"GrantId">>,
          <<"a1d15d430c883bd52e43e2a6257fab186d2c319f2e65eafff699d4f933d4ea80">>},
         {<<"GrantToken">>,
          <<"AQpAM2RhZTk1MGMyNTk2ZmZmMzEyYWVhOWViN2I1MWM4Mzc0MWFiYjc0ZDE1ODkyNGFlNTIzODZhMzgyZjBlNGY3NiKI">>}]).

create_grant_input_tests(_) ->
    Tests = [?_kms_test(
             {"CreateGrant input test",
              ?_f(erlcloud_kms:create_grant(<<"arn:aws:iam::", ?AWS_ACCOUNT_ID/binary, ":root">>,
                                            ?KEY_ID,
                                            [<<"Decrypt">>])),
              jsx:encode([{<<"GranteePrincipal">>, <<"arn:aws:iam::", ?AWS_ACCOUNT_ID/binary, ":root">>},
                          {<<"KeyId">>, ?KEY_ID},
                          {<<"Operations">>, [<<"Decrypt">>]}])
             }
             )],

    Response = jsx:encode(?CREATE_GRANT_RESP),
    input_tests(Response, Tests).


create_grant_output_tests(_) ->
    Tests = [?_kms_test(
             {"CreateGrant example response",
              jsx:encode(?CREATE_GRANT_RESP),
              {ok, ?CREATE_GRANT_RESP}}
             )],

    output_tests(?_f(erlcloud_kms:create_grant(<<"arn:aws:iam::", ?AWS_ACCOUNT_ID/binary, ":root">>,
                                            ?KEY_ID,
                                            [<<"Decrypt">>])), Tests).


-define(CREATE_KEY_RESP,
        [{<<"KeyMetadata">>,
          [{<<"AWSAccountId">>, ?AWS_ACCOUNT_ID},
           {<<"Arn">>,
            ?KEY_ARN},
           {<<"CreationDate">>, 1437488394.767},
           {<<"Description">>, <<>>},
           {<<"Enabled">>, true},
           {<<"KeyId">>, ?KEY_ID},
           {<<"KeyUsage">>, <<"ENCRYPT_DECRYPT">>}]}]).


create_key_input_tests(_) ->
    Tests = [?_kms_test(
             {"CreateKey input test",
              ?_f(erlcloud_kms:create_key()),
              <<"{}">>
             }
             )],

    Response = jsx:encode(?CREATE_KEY_RESP),
    input_tests(Response, Tests).


create_key_output_tests(_) ->
    Tests = [?_kms_test(
             {"CreateKey example response",
              jsx:encode(?CREATE_KEY_RESP),
              {ok, ?CREATE_KEY_RESP}}
             )],

    output_tests(?_f(erlcloud_kms:create_key()), Tests).


-define(CIPHERTEXT_BLOB,
        <<"CiAJvbtBARaBdslDOuU+YikmFNseUW5y1c0d8q78DTJs7xKVAQEBAgB4Cb27QQEWgXbJQzrlPmIpJhTbHlFuctXNHfKu/A0ybO",
          "8AAABsMGoGCSqGSIb3DQEHBqBdMFsCAQAwVgYJKoZIhvcNAQcBMB4GCWCGSAFlAwQBLjARBAwkgghcrmyiMibDmvYCARCAKSBG",
          "/sHl/AThNW0TRCOY2eh5eRcTEugadzOukS/TT5GqA82G4UJ4LKWi">>).


-define(DECRYPT_RESP,
        [{<<"KeyId">>,
          ?KEY_ARN},
         {<<"Plaintext">>,<<"VGVzdCBQbGFpbnRleHQ=">>}]).


decrypt_input_tests(_) ->
    Tests = [?_kms_test(
             {"Decrypt input test",
              ?_f(erlcloud_kms:decrypt(?CIPHERTEXT_BLOB)),
              jsx:encode([{<<"CiphertextBlob">>, ?CIPHERTEXT_BLOB}])
             }
             )],

    Response = jsx:encode(?DECRYPT_RESP),
    input_tests(Response, Tests).


decrypt_output_tests(_) ->
    Tests = [?_kms_test(
             {"Decrypt example response",
              jsx:encode(?DECRYPT_RESP),
              {ok, ?DECRYPT_RESP}}
             )],

    output_tests(?_f(erlcloud_kms:decrypt(?CIPHERTEXT_BLOB)), Tests).


-define(DELETE_ALIAS_RESP, <<>>).


delete_alias_input_tests(_) ->
    Tests = [?_kms_test(
             {"DeleteAlias input test",
              ?_f(erlcloud_kms:delete_alias(<<"alias/eunit_test_key">>)),
              jsx:encode([{<<"AliasName">>, <<"alias/eunit_test_key">>}])
             }
             )],

    Response = jsx:encode(?DELETE_ALIAS_RESP),
    input_tests(Response, Tests).


delete_alias_output_tests(_) ->
    Tests = [?_kms_test(
             {"DeleteAlias example response",
              jsx:encode(?DELETE_ALIAS_RESP),
              {ok, ?DELETE_ALIAS_RESP}}
             )],

    output_tests(?_f(erlcloud_kms:delete_alias(<<"alias/eunit_test_key">>)), Tests).


-define(DESCRIBE_KEY_RESP,
        [{<<"KeyMetadata">>,
          [{<<"AWSAccountId">>, ?AWS_ACCOUNT_ID},
           {<<"Arn">>,
            ?KEY_ARN},
           {<<"CreationDate">>, 1437417909.352},
           {<<"Description">>, <<"Unit Testing Key">>},
           {<<"Enabled">>, true},
           {<<"KeyId">>, ?KEY_ID},
           {<<"KeyUsage">>, <<"ENCRYPT_DECRYPT">>}]}]).


describe_key_input_tests(_) ->
    Tests = [?_kms_test(
             {"DescribeKey input test",
              ?_f(erlcloud_kms:describe_key(?KEY_ID)),
              jsx:encode([{<<"KeyId">>, ?KEY_ID}])
             }
             )],

    Response = jsx:encode(?DESCRIBE_KEY_RESP),
    input_tests(Response, Tests).


describe_key_output_tests(_) ->
    Tests = [?_kms_test(
             {"DescribeKey example response",
              jsx:encode(?DESCRIBE_KEY_RESP),
              {ok, ?DESCRIBE_KEY_RESP}}
             )],

    output_tests(?_f(erlcloud_kms:describe_key(?KEY_ID)), Tests).


disable_key_input_tests(_) ->
    Tests = [?_kms_test(
             {"DisableKey input test",
              ?_f(erlcloud_kms:disable_key(?KEY_ID)),
              jsx:encode([{<<"KeyId">>, ?KEY_ID}])
             }
             )],

    Response = <<>>,
    input_tests(Response, Tests).


disable_key_output_tests(_) ->
    Tests = [?_kms_test(
             {"DisableKey example response",
              <<>>,
              {ok, []}}
             )],

    output_tests(?_f(erlcloud_kms:disable_key(?KEY_ID)), Tests).


disable_key_rotation_input_tests(_) ->
    Tests = [?_kms_test(
             {"DisableKeyRotation input test",
              ?_f(erlcloud_kms:disable_key_rotation(?KEY_ID)),
              jsx:encode([{<<"KeyId">>, ?KEY_ID}])
             }
             )],

    Response = <<>>,
    input_tests(Response, Tests).


disable_key_rotation_output_tests(_) ->
    Tests = [?_kms_test(
             {"DisableKeyRotation example response",
              <<>>,
              {ok, []}}
             )],

    output_tests(?_f(erlcloud_kms:disable_key_rotation(?KEY_ID)), Tests).


enable_key_input_tests(_) ->
    Tests = [?_kms_test(
             {"EnableKey input test",
              ?_f(erlcloud_kms:enable_key(?KEY_ID)),
              jsx:encode([{<<"KeyId">>, ?KEY_ID}])
             }
             )],

    Response = <<>>,
    input_tests(Response, Tests).


enable_key_output_tests(_) ->
    Tests = [?_kms_test(
             {"EnableKey example response",
              <<>>,
              {ok, []}}
             )],

    output_tests(?_f(erlcloud_kms:enable_key(?KEY_ID)), Tests).


enable_key_rotation_input_tests(_) ->
    Tests = [?_kms_test(
             {"EnableKeyRotation input test",
              ?_f(erlcloud_kms:enable_key_rotation(?KEY_ID)),
              jsx:encode([{<<"KeyId">>, ?KEY_ID}])
             }
             )],

    Response = <<>>,
    input_tests(Response, Tests).


enable_key_rotation_output_tests(_) ->
    Tests = [?_kms_test(
             {"EnableKeyRotation example response",
              <<>>,
              {ok, []}}
             )],

    output_tests(?_f(erlcloud_kms:enable_key_rotation(?KEY_ID)), Tests).


-define(ENCRYPT_RESP,
        [{<<"CiphertextBlob">>,
          <<"CiAJvbtBARaBdslDOuU+YikmFNseUW5y1c0d8q78DTJs7xKTAQEBAgB4Cb27QQEWgXbJQzrlPmIpJhTbHlFuctXNHfKu/A0ybO8AAABqMGgGCSqGSIb3DQEHBqBbMFkCAQAwVAYJKoZIhvcNAQcBMB4GCWCGSAFlAwQBLjARBAwrNLJQSv7YOQWyFhwCARCAJ0tvYuqFp5hO7po+Elq/wTg9AMIYYWEUd4Czk12ml3BwQMZdtDl7ag==">>},
         {<<"KeyId">>,
          ?KEY_ARN}]).


encrypt_input_tests(_) ->
    Tests = [?_kms_test(
             {"Encrypt input test",
              ?_f(erlcloud_kms:encrypt(?KEY_ID, <<"Test Plaintext">>)),
              jsx:encode([{<<"KeyId">>, ?KEY_ID},
                          {<<"Plaintext">>, <<"Test Plaintext">>}])
             }
             )],

    Response = jsx:encode(?ENCRYPT_RESP),
    input_tests(Response, Tests).


encrypt_output_tests(_) ->
    Tests = [?_kms_test(
             {"Encrypt example response",
              jsx:encode(?ENCRYPT_RESP),
              {ok, ?ENCRYPT_RESP}}
             )],

    output_tests(?_f(erlcloud_kms:encrypt(?KEY_ID, <<"Test Plaintext">>)), Tests).


-define(GENERATE_DATA_KEY_RESP,
        [{<<"CiphertextBlob">>,
          <<"CiAJvbtBARaBdslDOuU+YikmFNseUW5y1c0d8q78DTJs7xKnAQEBAwB4Cb27QQEWgXbJQzrlPmIpJhTbHlFuctXNHfKu/A0ybO8AAAB+MHwGCSqGSIb3DQEHBqBvMG0CAQAwaAYJKoZIhvcNAQcBMB4GCWCGSAFlAwQBLjARBAzisekcxmVRv9EAJKQCARCAOwFrrL4+tlnIW+GE8iIe+TmfI7lsuRk7i4kGMbH/hCzWeqSu2MUUWWWQx2X8q7zYpxbYhNZNL9rghi+U">>},
         {<<"KeyId">>, ?KEY_ARN},
         {<<"Plaintext">>, <<"LWdAHKkJxoT3jid9OMnXov2h7kapWIk1gcxbpXocaWo=">>}]).


generate_data_key_input_tests(_) ->
    Tests = [?_kms_test(
             {"GenerateDataKey input test",
              ?_f(erlcloud_kms:generate_data_key(?KEY_ID, [{key_spec, <<"AES_256">>}])),
              jsx:encode([{<<"KeyId">>, ?KEY_ID},
                          {<<"KeySpec">>, <<"AES_256">>}])
             }
             )],

    Response = jsx:encode(?GENERATE_DATA_KEY_RESP),
    input_tests(Response, Tests).


generate_data_key_output_tests(_) ->
    Tests = [?_kms_test(
             {"GenerateDataKey example response",
              jsx:encode(?GENERATE_DATA_KEY_RESP),
              {ok, ?GENERATE_DATA_KEY_RESP}}
             )],

    output_tests(?_f(erlcloud_kms:generate_data_key(?KEY_ID, [{key_spec, <<"AES_256">>}])), Tests).


-define(GENERATE_DATA_KEY_WITHOUT_PLAINTEXT_RESP,
        [{<<"CiphertextBlob">>,
          <<"CiAJvbtBARaBdslDOuU+YikmFNseUW5y1c0d8q78DTJs7xKnAQEBAwB4Cb27QQEWgXbJQzrlPmIpJhTbHlFuctXNHfKu/A0ybO8AAAB+MHwGCSqGSIb3DQEHBqBvMG0CAQAwaAYJKoZIhvcNAQcBMB4GCWCGSAFlAwQBLjARBAzisekcxmVRv9EAJKQCARCAOwFrrL4+tlnIW+GE8iIe+TmfI7lsuRk7i4kGMbH/hCzWeqSu2MUUWWWQx2X8q7zYpxbYhNZNL9rghi+U">>},
         {<<"KeyId">>, ?KEY_ARN}]).


generate_data_key_without_plaintext_input_tests(_) ->
    Tests = [?_kms_test(
             {"GenerateDataKeyWithoutPlaintext input test",
              ?_f(erlcloud_kms:generate_data_key_without_plaintext(?KEY_ID, [{key_spec, <<"AES_256">>}])),
              jsx:encode([{<<"KeyId">>, ?KEY_ID},
                          {<<"KeySpec">>, <<"AES_256">>}])
             }
             )],

    Response = jsx:encode(?GENERATE_DATA_KEY_WITHOUT_PLAINTEXT_RESP),
    input_tests(Response, Tests).


generate_data_key_without_plaintext_output_tests(_) ->
    Tests = [?_kms_test(
             {"GenerateDataKeyWithoutPlaintext example response",
              jsx:encode(?GENERATE_DATA_KEY_WITHOUT_PLAINTEXT_RESP),
              {ok, ?GENERATE_DATA_KEY_WITHOUT_PLAINTEXT_RESP}}
             )],

    output_tests(?_f(erlcloud_kms:generate_data_key_without_plaintext(?KEY_ID, [{key_spec, <<"AES_256">>}])), Tests).


-define(GENERATE_RANDOM_RESP,
        [{<<"Plaintext">>,
          <<"Eaq/hnaIOuZHETTQgKYVOlG3G3FrXa/wW9Z7RN1XxZu2uk1JODBk8KJVyGp/gtjpwTyI+OwbkNlXCRzVunSwGwDZpJdWkpz9rrTZ",
            "HHI1rQuMoGz3pdUesKkBX+/zW/upXhido3ZnzwpyOJhp9kdzbX9h6Bj48lI0u7MWg5uPeX5gBWfGxcRLb4wG/6CNH8ubdlr5U3dQ",
            "PLlDBcZ78JJQtJL1vfaUc2fhwQmP6JnliY1fOUmDUmrs9AVKKNDkeHoo06SIg5oq7YeaOo29Hy+pPhiP/8cOk0bzBDeds+CQ7dKd",
            "BtPiCe2cgk+a5uuUO2oENVKQKyVr5TEnVSQtZxz2nw==">>}]).


generate_random_input_tests(_) ->
    Tests = [?_kms_test(
             {"GenerateRandom input test",
              ?_f(erlcloud_kms:generate_random(256)),
              jsx:encode([{<<"NumberOfBytes">>, 256}])
             }
             )],

    Response = jsx:encode(?GENERATE_RANDOM_RESP),
    input_tests(Response, Tests).


generate_random_output_tests(_) ->
    Tests = [?_kms_test(
             {"GenerateRandom example response",
              jsx:encode(?GENERATE_RANDOM_RESP),
              {ok, ?GENERATE_RANDOM_RESP}}
             )],

    output_tests(?_f(erlcloud_kms:generate_random(256)), Tests).


-define(GET_KEY_POLICY_RESP,
        [{<<"Policy">>,
          <<"{\"Version\":\"2012-10-17\",\"Id\":\"key-default-1\",\"Statement\":",
            "[{\"Sid\":\"Enable IAM User Permissions\",\"Effect\":\"Allow\",\"Principal\":",
            "{\"AWS\":\"arn:aws:iam::", ?AWS_ACCOUNT_ID/binary, ":root\"},",
            "\"Action\":\"kms:*\",\"Resource\":\"*\"}]\n}">>}]).


get_key_policy_input_tests(_) ->
    Tests = [?_kms_test(
             {"GetKeyPolicy input test",
              ?_f(erlcloud_kms:get_key_policy(?KEY_ID, <<"default">>)),
              jsx:encode([{<<"KeyId">>, ?KEY_ID},
                          {<<"PolicyName">>, <<"default">>}])
             }
             )],

    Response = jsx:encode(?GET_KEY_POLICY_RESP),
    input_tests(Response, Tests).


get_key_policy_output_tests(_) ->
    Tests = [?_kms_test(
             {"GetKeyPolicy example response",
              jsx:encode(?GET_KEY_POLICY_RESP),
              {ok, ?GET_KEY_POLICY_RESP}}
             )],

    output_tests(?_f(erlcloud_kms:get_key_policy(?KEY_ID, <<"default">>)), Tests).


-define(GET_KEY_ROTATION_STATUS_RESP,
        [{<<"KeyRotationEnabled">>,false}]).


get_key_rotation_status_input_tests(_) ->
    Tests = [?_kms_test(
             {"GetKeyRotationStatus input test",
              ?_f(erlcloud_kms:get_key_rotation_status(?KEY_ID)),
              jsx:encode([{<<"KeyId">>, ?KEY_ID}])
             }
             )],

    Response = jsx:encode(?GET_KEY_ROTATION_STATUS_RESP),
    input_tests(Response, Tests).


get_key_rotation_status_output_tests(_) ->
    Tests = [?_kms_test(
             {"GetKeyRotationStatus example response",
              jsx:encode(?GET_KEY_ROTATION_STATUS_RESP),
              {ok, ?GET_KEY_ROTATION_STATUS_RESP}}
             )],

    output_tests(?_f(erlcloud_kms:get_key_rotation_status(?KEY_ID)), Tests).


-define(LIST_ALIASES_RESP,
        [{<<"Aliases">>,
          [[{<<"AliasArn">>,<<"arn:aws:kms:us-west-2:", ?AWS_ACCOUNT_ID/binary, ":alias/unit_test_key">>},
            {<<"AliasName">>,<<"alias/unit_test_key">>}]]},
         {<<"Truncated">>,false}]).


list_aliases_input_tests(_) ->
    Tests = [?_kms_test(
             {"ListAliases input test",
              ?_f(erlcloud_kms:list_aliases()),
              <<"{}">>
             }
             )],

    Response = jsx:encode(?LIST_ALIASES_RESP),
    input_tests(Response, Tests).


list_aliases_output_tests(_) ->
    Tests = [?_kms_test(
             {"ListAliases example response",
              jsx:encode(?LIST_ALIASES_RESP),
              {ok, ?LIST_ALIASES_RESP}}
             )],

    output_tests(?_f(erlcloud_kms:list_aliases()), Tests).


-define(LIST_GRANTS_RESP,
        [{<<"Grants">>,
          [[{<<"GrantId">>,
             <<"a1d15d430c883bd52e43e2a6257fab186d2c319f2e65eafff699d4f933d4ea80">>},
            {<<"GranteePrincipal">>, <<"arn:aws:iam::", ?AWS_ACCOUNT_ID/binary, ":user/test_user">>},
            {<<"IssuingAccount">>, <<"arn:aws:iam::", ?AWS_ACCOUNT_ID/binary, ":root">>},
            {<<"Operations">>, [<<"Decrypt">>]}]]},
         {<<"Truncated">>, false}]).


list_grants_input_tests(_) ->
    Tests = [?_kms_test(
             {"Listgrants input test",
              ?_f(erlcloud_kms:list_grants(?KEY_ID)),
              jsx:encode([{<<"KeyId">>, ?KEY_ID}])
             }
             )],

    Response = jsx:encode(?LIST_GRANTS_RESP),
    input_tests(Response, Tests).


list_grants_output_tests(_) ->
    Tests = [?_kms_test(
             {"Listgrants example response",
              jsx:encode(?LIST_GRANTS_RESP),
              {ok, ?LIST_GRANTS_RESP}}
             )],

    output_tests(?_f(erlcloud_kms:list_grants(?KEY_ID)), Tests).


-define(LIST_KEY_POLICIES_RESP,
        [{<<"PolicyNames">>, [<<"default">>]}, {<<"Truncated">>, false}]).


list_key_policies_input_tests(_) ->
    Tests = [?_kms_test(
             {"ListKeyPolicies input test",
              ?_f(erlcloud_kms:list_key_policies(?KEY_ID)),
              jsx:encode([{<<"KeyId">>, ?KEY_ID}])
             }
             )],

    Response = jsx:encode(?LIST_KEY_POLICIES_RESP),
    input_tests(Response, Tests).


list_key_policies_output_tests(_) ->
    Tests = [?_kms_test(
             {"ListKeyPolicies example response",
              jsx:encode(?LIST_KEY_POLICIES_RESP),
              {ok, ?LIST_KEY_POLICIES_RESP}}
             )],

    output_tests(?_f(erlcloud_kms:list_key_policies(?KEY_ID)), Tests).


-define(LIST_KEYS_RESP,
        [{<<"Keys">>, [[{<<"KeyArn">>, ?KEY_ARN}, {<<"KeyId">>, ?KEY_ID}]]},
         {<<"Truncated">>,false}]).


list_keys_input_tests(_) ->
    Tests = [?_kms_test(
             {"ListKeys input test",
              ?_f(erlcloud_kms:list_keys()),
              <<"{}">>
             }
             )],

    Response = jsx:encode(?LIST_KEYS_RESP),
    input_tests(Response, Tests).


list_keys_output_tests(_) ->
    Tests = [?_kms_test(
             {"ListKeys example response",
              jsx:encode(?LIST_KEYS_RESP),
              {ok, ?LIST_KEYS_RESP}}
             )],

    output_tests(?_f(erlcloud_kms:list_keys()), Tests).


-define(KEY_POLICY,
        jsx:encode([{<<"Version">>, <<"2012-10-17">>},
                     {<<"Id">>, <<"key-default-1">>},
                     {<<"Statement">>,
                      [[{<<"Sid">>, <<"Enable IAM User Permissions">>},
                        {<<"Effect">>, <<"Allow">>},
                        {<<"Principal">>,
                         [{<<"AWS">>, <<"arn:aws:iam::", ?AWS_ACCOUNT_ID/binary, ":root">>}]},
                        {<<"Action">>, <<"kms:*">>},
                        {<<"Resource">>, <<"*">>}]]}])).


put_key_policy_input_tests(_) ->
    Tests = [?_kms_test(
             {"PutKeyPolicy input test",
              ?_f(erlcloud_kms:put_key_policy(?KEY_ID, ?KEY_POLICY, <<"default">>)),
              jsx:encode([{<<"KeyId">>, ?KEY_ID},
                          {<<"Policy">>, ?KEY_POLICY},
                          {<<"PolicyName">>, <<"default">>}])
             }
             )],

    Response = <<>>,
    input_tests(Response, Tests).


put_key_policy_output_tests(_) ->
    Tests = [?_kms_test(
             {"PutKeyPolicy example response",
              <<>>,
              {ok, []}}
             )],

    output_tests(?_f(erlcloud_kms:put_key_policy(?KEY_ID, ?KEY_POLICY, <<"default">>)), Tests).


-define(RE_ENCRYPT_RESP,
        [{<<"CiphertextBlob">>,
          ?CIPHERTEXT_BLOB},
         {<<"KeyId">>,
          ?KEY_ARN},
         {<<"SourceKeyId">>,
          ?KEY_ARN}]).


re_encrypt_input_tests(_) ->
    Tests = [?_kms_test(
             {"ReEncrypt input test",
              ?_f(erlcloud_kms:re_encrypt(?CIPHERTEXT_BLOB, ?KEY_ID)),
              jsx:encode([{<<"CiphertextBlob">>, ?CIPHERTEXT_BLOB},
                          {<<"DestinationKeyId">>, ?KEY_ID}])
             }
             )],

    Response = jsx:encode(?RE_ENCRYPT_RESP),
    input_tests(Response, Tests).


re_encrypt_output_tests(_) ->
    Tests = [?_kms_test(
             {"ReEncrypt example response",
              jsx:encode(?RE_ENCRYPT_RESP),
              {ok, ?RE_ENCRYPT_RESP}}
             )],

    output_tests(?_f(erlcloud_kms:re_encrypt(<<"Test Plaintext">>, ?KEY_ID)), Tests).


retire_grant_input_tests(_) ->
    Tests = [?_kms_test(
             {"RetireGrant input test",
              ?_f(erlcloud_kms:retire_grant(
                    [{key_id, ?KEY_ARN},
                     {grant_id, <<"a1d15d430c883bd52e43e2a6257fab186d2c319f2e65eafff699d4f933d4ea80">>}])),
              jsx:encode([{<<"KeyId">>, ?KEY_ARN},
                          {<<"GrantId">>, <<"a1d15d430c883bd52e43e2a6257fab186d2c319f2e65eafff699d4f933d4ea80">>}])
             }
             )],

    Response = <<>>,
    input_tests(Response, Tests).


retire_grant_output_tests(_) ->
    Tests = [?_kms_test(
             {"RetireGrant example response",
              <<>>,
              {ok, []}}
             )],

    output_tests(?_f(erlcloud_kms:retire_grant(
                       [{key_id, ?KEY_ARN},
                        {grant_id, <<"a1d15d430c883bd52e43e2a6257fab186d2c319f2e65eafff699d4f933d4ea80">>}])), Tests).


revoke_grant_input_tests(_) ->
    Tests = [?_kms_test(
             {"RevokeGrant input test",
              ?_f(erlcloud_kms:revoke_grant(<<"a1d15d430c883bd52e43e2a6257fab186d2c319f2e65eafff699d4f933d4ea80">>,
                                            ?KEY_ID)),
              jsx:encode([{<<"GrantId">>, <<"a1d15d430c883bd52e43e2a6257fab186d2c319f2e65eafff699d4f933d4ea80">>},
                          {<<"KeyId">>, ?KEY_ID}])
             }
             )],

    Response = <<>>,
    input_tests(Response, Tests).


revoke_grant_output_tests(_) ->
    Tests = [?_kms_test(
             {"RevokeGrant example response",
              <<>>,
              {ok, []}}
             )],

    output_tests(?_f(erlcloud_kms:revoke_grant(<<"a1d15d430c883bd52e43e2a6257fab186d2c319f2e65eafff699d4f933d4ea80">>,
                                               ?KEY_ID)), Tests).


update_alias_input_tests(_) ->
    Tests = [?_kms_test(
             {"UpdateAlias input test",
              ?_f(erlcloud_kms:update_alias(<<"alias/unit_test_update">>, ?KEY_ID)),
              jsx:encode([{<<"AliasName">>, <<"alias/unit_test_update">>},
                          {<<"TargetKeyId">>, ?KEY_ID}])
             }
             )],

    Response = <<>>,
    input_tests(Response, Tests).


update_alias_output_tests(_) ->
    Tests = [?_kms_test(
             {"UpdateAlias example response",
              <<>>,
              {ok, []}}
             )],

    output_tests(?_f(erlcloud_kms:update_alias(<<"alias/unit_test_update">>, ?KEY_ID)), Tests).


update_key_description_input_tests(_) ->
    Tests = [?_kms_test(
             {"UpdateAlias input test",
              ?_f(erlcloud_kms:update_key_description(?KEY_ID, <<"Updated key description">>)),
              jsx:encode([{<<"KeyId">>, ?KEY_ID},
                          {<<"Description">>, <<"Updated key description">>}])
             }
             )],

    Response = <<>>,
    input_tests(Response, Tests).


update_key_description_output_tests(_) ->
    Tests = [?_kms_test(
             {"UpdateAlias example response",
              <<>>,
              {ok, []}}
             )],

    output_tests(?_f(erlcloud_kms:update_key_description(?KEY_ID, <<"Updated key description">>)), Tests).
