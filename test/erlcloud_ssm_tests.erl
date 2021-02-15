%% -*- mode: erlang;erlang-indent-level: 4;indent-tabs-mode: nil -*-
-module(erlcloud_ssm_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("include/erlcloud_ssm.hrl").

%% Unit tests for erlcloud_ssm.
%% These tests work by using meck to mock erlcloud_httpc. There are two classes of test: input and output.
%%
%% Input tests verify that different function args produce the desired JSON request.
%% An input test list provides a list of funs and the JSON that is expected to result.
%%
%% Output tests verify that the http response produces the correct return from the fun.
%% An output test lists provides a list of response bodies and the expected return.

%% The _ssm_test macro provides line number annotation to a test, similar to _test, but doesn't wrap in a fun
-define(_ssm_test(T), {?LINE, T}).
%% The _f macro is a terse way to wrap code in a fun. Similar to _test but doesn't annotate with a line number
-define(_f(F), fun() -> F end).

-export([validate_body/2]).

%%%===================================================================
%%% Test entry points
%%%===================================================================
operation_test_() ->
    {foreach,
     fun start/0,
     fun stop/1,
     [
      fun get_parameter_input_tests/1,
      fun get_parameter_output_tests/1,
      fun get_parameters_input_tests/1,
      fun get_parameters_output_tests/1,
      fun get_parameters_by_path_input_tests/1,
      fun get_parameters_by_path_output_tests/1,
      fun put_parameter_input_tests/1,
      fun put_parameter_output_tests/1,
      fun delete_parameter_input_tests/1,
      fun delete_parameter_output_tests/1
     ]
    }.

start() ->
    meck:new(erlcloud_httpc),
    ok.


stop(_) ->
    meck:unload(erlcloud_httpc).

%%%===================================================================
%% Actual test specifiers
%%%===================================================================

%% GetParameter test based on the API examples:
%% https://docs.aws.amazon.com/systems-manager/latest/APIReference/API_GetParameter.html
get_parameter_input_tests(_) ->
    Tests = 
        [?_ssm_test(
            {"GetParameter example request",
             ?_f(erlcloud_ssm:get_parameter([{name, "/root/parameter_1"}])), "
{
    \"Name\": \"/root/parameter_1\"
}"
            })
        ],

    Response = "
{    
    \"Parameter\": [ 
      { 
         \"ARN\": \"arn:aws:ssm:us-west-2:000000000000:parameter/root/parameter_1\",
         \"DataType\": \"text\",
         \"LastModifiedDate\": 1612970194.057,
         \"Name\": \"/root/parameter_1\",
         \"Type\": \"String\",
         \"Value\": \"testvalue\",
         \"Version\": 1
      }
   ]
}",
    input_tests(Response, Tests).


get_parameter_output_tests(_) ->
    Tests =
        [?_ssm_test(
            {"GetParameter example response", "
{    
    \"Parameter\":
      { 
         \"ARN\": \"arn:aws:ssm:us-west-2:000000000000:parameter/root/parameter_1\",
         \"DataType\": \"text\",
         \"LastModifiedDate\": 1612970194.057,
         \"Name\": \"/root/parameter_1\",
         \"Type\": \"String\",
         \"Value\": \"testvalue\",
         \"Version\": 1
      }
}",
            {ok, #ssm_get_parameter{parameter = #ssm_parameter{arn = <<"arn:aws:ssm:us-west-2:000000000000:parameter/root/parameter_1">>,
                                                               data_type = <<"text">>, last_modified_date = 1612970194.057,
                                                               name = <<"/root/parameter_1">>, selector = undefined,
                                                               source_result = undefined, type = <<"String">>,
                                                               value = <<"testvalue">>, version = 1}}}
        })],
    output_tests(?_f(erlcloud_ssm:get_parameter([{name, "/root/parameter_1"}, {out, record}])), Tests).


%% GetParameters test based on the API examples:
%% https://docs.aws.amazon.com/systems-manager/latest/APIReference/API_GetParameters.html
get_parameters_input_tests(_) ->
    Tests = 
        [?_ssm_test(
            {"GetParameters example request",
             ?_f(erlcloud_ssm:get_parameters([{names, ["/root/parameter_1", "/root/secured_parameter", "some_invalid_param"]},
                                              {with_decryption, true}])), "
{
    \"Names\": [\"/root/parameter_1\", \"/root/secured_parameter\", \"some_invalid_param\"],
    \"WithDecryption\": true
}"
            })
        ],

    Response = "
{
   \"InvalidParameters\": [\"some_invalid_param\"],
   \"Parameters\": [ 
      { 
         \"ARN\": \"arn:aws:ssm:us-west-2:000000000000:parameter/root/parameter_1\",
         \"DataType\": \"text\",
         \"LastModifiedDate\": 1612970194.057,
         \"Name\": \"/root/parameter_1\",
         \"Type\": \"String\",
         \"Value\": \"testvalue\",
         \"Version\": 1
      },
      { 
         \"ARN\": \"arn:aws:ssm:us-west-2:000000000000:parameter/root/secured_parameter\",
         \"DataType\": \"text\",
         \"LastModifiedDate\": 1612970194.31,
         \"Name\": \"/root/secured_parameter\",
         \"Type\": \"SecureString\",
         \"Value\": \"MyP@ssw0rd\",
         \"Version\": 1
      }
   ]
}",
    input_tests(Response, Tests).


get_parameters_output_tests(_) ->
    Tests =
        [?_ssm_test(
            {"GetParameters example response", "
{
   \"InvalidParameters\": [\"some_invalid_param\"],
   \"Parameters\": [ 
      { 
         \"ARN\": \"arn:aws:ssm:us-west-2:000000000000:parameter/root/parameter_1\",
         \"DataType\": \"text\",
         \"LastModifiedDate\": 1612970194.057,
         \"Name\": \"/root/parameter_1\",
         \"Type\": \"String\",
         \"Value\": \"testvalue\",
         \"Version\": 1
      },
      { 
         \"ARN\": \"arn:aws:ssm:us-west-2:000000000000:parameter/root/secured_parameter\",
         \"DataType\": \"text\",
         \"LastModifiedDate\": 1612970194.31,
         \"Name\": \"/root/secured_parameter\",
         \"Type\": \"SecureString\",
         \"Value\": \"MyP@ssw0rd\",
         \"Version\": 1
      }
   ]
}",
            {ok, #ssm_get_parameters{invalid_parameters = [<<"some_invalid_param">>],
                                     parameters = [#ssm_parameter{arn = <<"arn:aws:ssm:us-west-2:000000000000:parameter/root/parameter_1">>,
                                                                  data_type = <<"text">>, last_modified_date = 1612970194.057,
                                                                  name = <<"/root/parameter_1">>, selector = undefined,
                                                                  source_result = undefined, type = <<"String">>,
                                                                  value = <<"testvalue">>, version = 1},
                                                   #ssm_parameter{arn = <<"arn:aws:ssm:us-west-2:000000000000:parameter/root/secured_parameter">>,
                                                                  data_type = <<"text">>, last_modified_date = 1612970194.31,
                                                                  name = <<"/root/secured_parameter">>, selector = undefined,
                                                                  source_result = undefined, type = <<"SecureString">>,
                                                                  value = <<"MyP@ssw0rd">>,
                                                                  version = 1}]}}
        })],
    output_tests(?_f(erlcloud_ssm:get_parameters([{names, ["/root/parameter_1", "/root/secured_parameter", "some_invalid_param"]},
                                                  {with_decryption, true},
                                                  {out, record}])), Tests).


%% GetParametersByPath test based on the API examples:
%% https://docs.aws.amazon.com/systems-manager/latest/APIReference/API_GetParametersByPath.html
get_parameters_by_path_input_tests(_) ->
    Tests = 
        [?_ssm_test(
            {"GetParametersByPath example request",
             ?_f(erlcloud_ssm:get_parameters_by_path([{max_results, 2},
                                                      {path, <<"/root">>},
                                                      {recursive, true},
                                                      {with_decryption, false},
                                                      {out, record}])), "
{
   \"MaxResults\": 2,
   \"Path\": \"/root\",
   \"Recursive\": true,
   \"WithDecryption\": false,
}"
            })
        ],

    Response = "
{
   \"NextToken\": \"AAEAAe2HRCL1+nBkC3kuOVp0r3fpOpPOH+/c+hH5VIdx7vntLivo/iGhhR8yllmtqsCdoNiwS4EQP+QrRDX+T1NT9vN7X2Fj+/Gb/++F089cGZc6/+/OAaqe/==\",
   \"Parameters\": [ 
      { 
         \"ARN\": \"arn:aws:ssm:us-west-2:000000000000:parameter/root/parameter_1\",
         \"DataType\": \"text\",
         \"LastModifiedDate\": 1612970194.057,
         \"Name\": \"/root/parameter_1\",
         \"Type\": \"String\",
         \"Value\": \"testvalue\",
         \"Version\": 1
      },
      { 
         \"ARN\": \"arn:aws:ssm:us-west-2:000000000000:parameter/root/secured_parameter\",
         \"DataType\": \"text\",
         \"LastModifiedDate\": 1612970194.31,
         \"Name\": \"/root/secured_parameter\",
         \"Type\": \"SecureString\",
         \"Value\": \"6EzK+k13aK/VKMYk77pyiErjGpaoEDA==\",
         \"Version\": 1
      }
   ]
}",
    input_tests(Response, Tests).


get_parameters_by_path_output_tests(_) ->
    Tests =
        [?_ssm_test(
            {"GetParametersByPath example response", "
{
   \"NextToken\": \"AAEAAe2HRCL1+nBkC3kuOVp0r3fpOpPOH+/c+hH5VIdx7vntLivo/iGhhR8yllmtqsCdoNiwS4EQP+QrRDX+T1NT9vN7X2Fj+/Gb/++F089cGZc6/+/OAaqe/==\",
   \"Parameters\": [ 
      { 
         \"ARN\": \"arn:aws:ssm:us-west-2:000000000000:parameter/root/parameter_1\",
         \"DataType\": \"text\",
         \"LastModifiedDate\": 1612970194.057,
         \"Name\": \"/root/parameter_1\",
         \"Type\": \"String\",
         \"Value\": \"testvalue\",
         \"Version\": 1
      },
      { 
         \"ARN\": \"arn:aws:ssm:us-west-2:000000000000:parameter/root/secured_parameter\",
         \"DataType\": \"text\",
         \"LastModifiedDate\": 1612970194.31,
         \"Name\": \"/root/secured_parameter\",
         \"Type\": \"SecureString\",
         \"Value\": \"6EzK+k13aK/VKMYk77pyiErjGpaoEDA==\",
         \"Version\": 1
      }
   ]
}",
            {ok, #ssm_get_parameters_by_path{next_token = <<"AAEAAe2HRCL1+nBkC3kuOVp0r3fpOpPOH+/c+hH5VIdx7vntLivo/iGhhR8yllmtqsCdoNiwS4EQP+QrRDX+T1NT9vN7X2Fj+/Gb/++F089cGZc6/+/OAaqe/==">>,
                                             parameters = [#ssm_parameter{arn = <<"arn:aws:ssm:us-west-2:000000000000:parameter/root/parameter_1">>,
                                                                          data_type = <<"text">>, last_modified_date = 1612970194.057,
                                                                          name = <<"/root/parameter_1">>, selector = undefined,
                                                                          source_result = undefined, type = <<"String">>,
                                                                          value = <<"testvalue">>, version = 1},
                                                           #ssm_parameter{arn = <<"arn:aws:ssm:us-west-2:000000000000:parameter/root/secured_parameter">>,
                                                                          data_type = <<"text">>, last_modified_date = 1612970194.31,
                                                                          name = <<"/root/secured_parameter">>, selector = undefined,
                                                                          source_result = undefined, type = <<"SecureString">>,
                                                                          value = <<"6EzK+k13aK/VKMYk77pyiErjGpaoEDA==">>,
                                                                          version = 1}]}}
        })],
    output_tests(?_f(erlcloud_ssm:get_parameters_by_path([{max_results, 2},
                                                          {path, <<"/root">>},
                                                          {recursive, true},
                                                          {with_decryption, false},
                                                          {out, record}])), Tests).


%% PutParameter test based on the API examples:
%% https://docs.aws.amazon.com/systems-manager/latest/APIReference/API_PutParameter.html
put_parameter_input_tests(_) ->
    Tests = 
        [?_ssm_test(
            {"PutParameter example request",
             ?_f(erlcloud_ssm:put_parameter([{name, <<"/root/parameter_1">>}, {value, <<"testvalue">>}, {type, <<"String">>}])), "
{
    \"Name\": \"/root/parameter_1\",
    \"Type\": \"String\",
    \"Value\": \"testvalue\"
}"
            })
        ],

    Response = "
{
   \"Tier\": \"Standard\",
   \"Version\": 1
}",
    input_tests(Response, Tests).


put_parameter_output_tests(_) ->
    Tests =
        [?_ssm_test(
            {"PutParameter example response", "
{
   \"Tier\": \"Standard\",
   \"Version\": 1
}",
            {ok, #ssm_put_parameter{tier = <<"Standard">>,
                                    version = 1}}
        })],
    output_tests(?_f(erlcloud_ssm:put_parameter([{name, <<"/root/parameter_1">>}, {value, <<"testvalue">>}, {type, <<"String">>}, {out, record}])), Tests).


%% DeleteParameter test based on the API examples:
%% https://docs.aws.amazon.com/systems-manager/latest/APIReference/API_DeleteParameter.html
delete_parameter_input_tests(_) ->
    Tests = 
        [?_ssm_test(
            {"DeleteParameter example request",
             ?_f(erlcloud_ssm:delete_parameter([{name, <<"/root/parameter_1">>}])), "
{
    \"Name\": \"/root/parameter_1\"
}"
            })
        ],
    Response = "{}",
    input_tests(Response, Tests).


delete_parameter_output_tests(_) ->
    Tests =
        [?_ssm_test(
            {"DeleteParameter example response", "{}",
            ok
        })],
    output_tests(?_f(erlcloud_ssm:delete_parameter([{name, <<"/root/parameter_1">>}])), Tests).

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
    Want = sort_json(jsx:decode(list_to_binary(Expected), [{return_maps, false}])),
    Actual = sort_json(jsx:decode(Body, [{return_maps, false}])),
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
            {ok, {{200, "OK"}, [], list_to_binary(Response)}}
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
              erlcloud_ssm:configure(string:copies("A", 20), string:copies("a", 40)),
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
-spec output_expect(binary()) -> fun().
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
              meck:expect(erlcloud_httpc, request, output_expect(list_to_binary(Response))),
              erlcloud_ssm:configure(string:copies("A", 20), string:copies("a", 40)),
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
