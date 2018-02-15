-module(erlcloud_app).

-behaviour(application).

-export([start/2, stop/1]).

-include("erlcloud.hrl").
-define(ENV_PREFIX, "ERLCLOUD_").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.


-spec start(application:start_type(), term()) -> {ok, pid()} .
start(_StartType, _StartArgs) ->
    load_environment(),
    erlcloud_sup:start_link().

-spec(stop(State :: term()) -> term()).
stop(_State) ->
    ok.


-spec load_environment() -> ok.
-ifdef(ERLANG_OTP_VERSION_20).
%% load_environment takes all environment variables that begin with ERLCLOUD_
%% and turns them into Erlang environment variables after down-casing the key
load_environment() ->
    Env = os:getenv(),
    Environment = parse_environment(Env, []),
    lists:foreach(fun({Key, Val}) -> ok = application:set_env(?APP, Key, Val) end, Environment).

-spec parse_environment([string()], [{atom(), string()}]) -> [{atom(), string()}].
parse_environment([], Acc) ->
    %% This is mostly to make testing easier
    lists:sort(Acc);
parse_environment([EnvVar|Rest], Acc) ->
    case string:prefix(EnvVar, ?ENV_PREFIX) of
        nomatch ->
            parse_environment(Rest, Acc);
        EnvVarStripped ->
            parse_environment(Rest, [do_parse_one_stripped_environment(EnvVarStripped)|Acc])
    end.

-spec do_parse_one_stripped_environment(string()) -> {atom(), string()}.
do_parse_one_stripped_environment(EnvVarStripped) ->
    %% Split the Key=Val representation
    [EnvKey0, EnvVal0] = string:split(EnvVarStripped, "="),
    EnvKey1 = lists:flatten(string:lowercase(EnvKey0)),
    EnvVal1 = lists:flatten(EnvVal0),
    %% Eventually we should either define all possible environment variables
    %% that can be loaded in erlcloud.app.src, or have a whitelist, so we can use list_to_existing_atom
    EnvKeyAsAtom = erlang:list_to_atom(EnvKey1),
    {EnvKeyAsAtom, EnvVal1}.


-ifdef(TEST).
load_env_test() ->
    TestEnvironment = ["ERLCLOUD_FOO_BAR=SomeTestVal", "RANDOMVARIABLE=Cat", "CORRUPTEDEnvironmentVariable",
        "ERLCLOUD_BAR_BAZ=two=equals"],
    ?assertEqual([{bar_baz, "two=equals"}, {foo_bar, "SomeTestVal"}], parse_environment(TestEnvironment, [])).

-endif.
-else.
load_environment() ->
    ok.
-endif.

