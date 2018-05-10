%% -*- mode: erlang;erlang-indent-level: 4;indent-tabs-mode: nil -*-
-module(erlcloud_emr).

-include("erlcloud_aws.hrl").

%% Library initialization
-export([configure/2, configure/3, configure/4, configure/5,
         new/2, new/3, new/4, new/5]).

%% ElasticMapReduce API
-export([add_job_flow_steps/2, add_job_flow_steps/3, add_job_flow_steps/4]).
-export([describe_step/2, describe_step/3, describe_step/4]).
-export([run_job_flow/1, run_job_flow/2, run_job_flow/3]).
-export([terminate_job_flows/1, terminate_job_flows/2, terminate_job_flows/3]).
-export([set_termination_protection/2, set_termination_protection/3,
         set_termination_protection/4]).

-export_type([emr_opts/0, emr_return/0]).

%%------------------------------------------------------------------------------
%%  Library initialization
%%------------------------------------------------------------------------------

-spec new(string(), string()) -> aws_config().
new(AccessKeyID, SecretAccessKey) ->
    #aws_config{access_key_id=AccessKeyID,
                secret_access_key=SecretAccessKey}.

-spec new(string(), string(), string()) -> aws_config().
new(AccessKeyID, SecretAccessKey, Host) ->
    #aws_config{access_key_id=AccessKeyID,
                secret_access_key=SecretAccessKey,
                emr_host=Host}.

-spec new(string(), string(), string(), non_neg_integer()) -> aws_config().
new(AccessKeyID, SecretAccessKey, Host, Port) ->
    #aws_config{access_key_id=AccessKeyID,
                secret_access_key=SecretAccessKey,
                emr_host=Host,
                emr_port=Port}.

-spec new(string(), string(), string(), non_neg_integer(), string()) -> aws_config().
new(AccessKeyID, SecretAccessKey, Host, Port, Scheme) ->
    #aws_config{access_key_id=AccessKeyID,
                secret_access_key=SecretAccessKey,
                emr_host=Host,
                emr_port=Port,
                emr_scheme=Scheme}.

-spec configure(string(), string()) -> ok.
configure(AccessKeyID, SecretAccessKey) ->
    erlcloud_config:configure(AccessKeyID, SecretAccessKey, fun new/2).

-spec configure(string(), string(), string()) -> ok.
configure(AccessKeyID, SecretAccessKey, Host) ->
    erlcloud_config:configure(AccessKeyID, SecretAccessKey, Host, fun new/3).

-spec configure(string(), string(), string(), non_neg_integer()) -> ok.
configure(AccessKeyID, SecretAccessKey, Host, Port) ->
    erlcloud_config:configure(AccessKeyID, SecretAccessKey, Host, Port, fun new/4).

-spec configure(string(), string(), string(), non_neg_integer(), string()) -> ok.
configure(AccessKeyID, SecretAccessKey, Host, Port, Scheme) ->
    erlcloud_config:configure(AccessKeyID, SecretAccessKey, Host, Port, Scheme, fun new/5).

%%------------------------------------------------------------------------------
%%  ElasticMapReduce API
%%------------------------------------------------------------------------------

-type emr_opts() :: [{out, json | raw}].
-type emr_return() :: {ok, jsx:json_term() | binary()} |
                      {error, {aws_error, jsx:json_term()}} |
                      {error, {socket_error, any()}} |
                      {error, tuple()}.


%% --- AddJobFlowSteps ---
%% http://docs.aws.amazon.com/ElasticMapReduce/latest/API/API_AddJobFlowSteps.html
-spec add_job_flow_steps(binary(), jsx:json_term()) -> emr_return().
add_job_flow_steps(JobFlowId, Steps) ->
    add_job_flow_steps(JobFlowId, Steps, [], erlcloud_aws:default_config()).

-spec add_job_flow_steps(binary(), jsx:json_term(), aws_config() | emr_opts()) -> emr_return().
add_job_flow_steps(JobFlowId, Steps, Config = #aws_config{}) ->
    add_job_flow_steps(JobFlowId, Steps, [], Config);
add_job_flow_steps(JobFlowId, Steps, Opts) when is_list(Opts) ->
    add_job_flow_steps(JobFlowId, Steps, Opts, erlcloud_aws:default_config()).

-spec add_job_flow_steps(binary(), jsx:json_term(), emr_opts(), aws_config()) -> emr_return().
add_job_flow_steps(JobFlowId, Steps, Opts, Config) ->
    emr_request("ElasticMapReduce.AddJobFlowSteps",
                [{'JobFlowId', JobFlowId}, {'Steps', Steps}], Opts, Config).


%% --- DescribeStep ---
%% http://docs.aws.amazon.com/ElasticMapReduce/latest/API/API_DescribeStep.html
-spec describe_step(binary(), binary()) -> emr_return().
describe_step(ClusterId, StepId) ->
    describe_step(ClusterId, StepId, [], erlcloud_aws:default_config()).

-spec describe_step(binary(), binary(), aws_config() | emr_opts()) -> emr_return().
describe_step(ClusterId, StepId, Config = #aws_config{}) ->
    describe_step(ClusterId, StepId, [], Config);
describe_step(ClusterId, StepId, Opts) when is_list(Opts) ->
    describe_step(ClusterId, StepId, Opts, erlcloud_aws:default_config()).

-spec describe_step(binary(), binary(), emr_opts(), aws_config()) -> emr_return().
describe_step(ClusterId, StepId, Opts, Config) ->
    emr_request("ElasticMapReduce.DescribeStep",
                [{'ClusterId', ClusterId}, {'StepId', StepId}], Opts, Config).


%% --- RunJobFlow ---
%% http://docs.aws.amazon.com/ElasticMapReduce/latest/API/API_RunJobFlow.html
-spec run_job_flow(jsx:json_term()) -> emr_return().
run_job_flow(Params) ->
    run_job_flow(Params, [], erlcloud_aws:default_config()).

-spec run_job_flow(jsx:json_term(), aws_config() | emr_opts()) -> emr_return().
run_job_flow(Params, Config = #aws_config{}) ->
    run_job_flow(Params, [], Config);
run_job_flow(Params, Opts) when is_list(Opts) ->
    run_job_flow(Params, Opts, erlcloud_aws:default_config()).

-spec run_job_flow(jsx:json_term(), emr_opts(), aws_config()) -> emr_return().
run_job_flow(Params, Opts, Config) ->
    emr_request("ElasticMapReduce.RunJobFlow", Params, Opts, Config).


%% --- TerminateJobFlows ---
%% http://docs.aws.amazon.com/ElasticMapReduce/latest/API/API_TerminateJobFlows.html
-spec terminate_job_flows([binary()]) -> emr_return().
terminate_job_flows(JobFlowIds) ->
    terminate_job_flows(JobFlowIds, [], erlcloud_aws:default_config()).

-spec terminate_job_flows([binary()], aws_config() | emr_opts()) -> emr_return().
terminate_job_flows(JobFlowIds, Config = #aws_config{}) ->
    terminate_job_flows(JobFlowIds, [], Config);
terminate_job_flows(JobFlowIds, Opts) when is_list(Opts) ->
    terminate_job_flows(JobFlowIds, Opts, erlcloud_aws:default_config()).

-spec terminate_job_flows([binary()], emr_opts(), aws_config()) -> emr_return().
terminate_job_flows(JobFlowIds, Opts, Config) ->
    emr_request("ElasticMapReduce.TerminateJobFlows",
                [{'JobFlowIds', JobFlowIds}], Opts, Config).


%% --- SetTerminationProtection ---
%% http://docs.aws.amazon.com/ElasticMapReduce/latest/API/API_SetTerminationProtection.html
-spec set_termination_protection([binary()], boolean()) -> emr_return().
set_termination_protection(JobFlowIds, TerminationProtected) ->
    set_termination_protection(JobFlowIds, TerminationProtected,
                               [], erlcloud_aws:default_config()).

-spec set_termination_protection([binary()], boolean(),
                                 aws_config() | emr_opts()) -> emr_return().
set_termination_protection(JobFlowIds, TerminationProtected,
                           Config = #aws_config{}) ->
    set_termination_protection(JobFlowIds, TerminationProtected,
                               [], Config);
set_termination_protection(JobFlowIds, TerminationProtected,
                           Opts) when is_list(Opts) ->
    set_termination_protection(JobFlowIds, TerminationProtected,
                               Opts, erlcloud_aws:default_config()).

-spec set_termination_protection([binary()], boolean(),
                                 emr_opts(), aws_config()) -> emr_return().
set_termination_protection(JobFlowIds, TerminationProtected, Opts, Config) ->
    emr_request("ElasticMapReduce.SetTerminationProtection",
                [{'JobFlowIds', JobFlowIds},
                 {'TerminationProtected', TerminationProtected}], Opts, Config).


%%------------------------------------------------------------------------------
%%  Internal functions
%%------------------------------------------------------------------------------

emr_request(Action, Json, Opts, Cfg) ->
    {Scheme, Host, Port, Service} = emr_endpoint(Cfg),
    request(Action, Json, Scheme, Host, Port, Service, Opts, Cfg).

emr_endpoint(Cfg) ->
    %% Scheme, Host, Port, Service
    {Cfg#aws_config.emr_scheme,
     Cfg#aws_config.emr_host,
     Cfg#aws_config.emr_port,
     "elasticmapreduce"}.

request(Action, Json, Scheme, Host, Port, Service, Opts, Cfg0) ->
    case erlcloud_aws:update_config(Cfg0) of
        {ok, Cfg} -> request_no_update(Action, Json, Scheme, Host, Port,
                                          Service, Opts, Cfg);
        Err -> Err
    end.

request_no_update(Action, Json, Scheme, Host, Port, Service, Opts, Cfg) ->
    ReqBody = jsx:encode(if Json == [] -> [{}];
                                  true -> Json end),
    H1 = [{"host", Host}],
    H2 = [{"content-type", "application/x-amz-json-1.1"},
          {"x-amz-target", Action}],
    Region = erlcloud_aws:aws_region_from_host(Host),
    Headers = erlcloud_aws:sign_v4_headers(Cfg, H1, ReqBody, Region, Service) ++ H2,
    case erlcloud_aws:aws_request_form_raw(post, Scheme, Host, Port,
                                           "/", ReqBody, Headers, Cfg) of
        {ok, Body} -> case proplists:get_value(out, Opts, json) of
                          raw -> {ok, Body};
                          _   -> case Body of
                                     <<>> -> {ok, <<>>};
                                     _    -> {ok, jsx:decode(Body)}
                                 end
                      end;
        {error, {http_error, _Code, _StatusLine, ErrBody}} ->
            {error, {aws_error, jsx:decode(ErrBody)}};
        {error, {socket_error, Reason}} ->
            {error, {socket_error, Reason}}
    end.
