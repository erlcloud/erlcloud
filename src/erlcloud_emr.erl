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
    put(aws_config, new(AccessKeyID, SecretAccessKey)),
    ok.

-spec configure(string(), string(), string()) -> ok.
configure(AccessKeyID, SecretAccessKey, Host) ->
    put(aws_config, new(AccessKeyID, SecretAccessKey, Host)),
    ok.

-spec configure(string(), string(), string(), non_neg_integer()) -> ok.
configure(AccessKeyID, SecretAccessKey, Host, Port) ->
    put(aws_config, new(AccessKeyID, SecretAccessKey, Host, Port)),
    ok.

-spec configure(string(), string(), string(), non_neg_integer(), string()) -> ok.
configure(AccessKeyID, SecretAccessKey, Host, Port, Scheme) ->
    put(aws_config, new(AccessKeyID, SecretAccessKey, Host, Port, Scheme)),
    ok.


%%------------------------------------------------------------------------------
%%  ElasticMapReduce API
%%------------------------------------------------------------------------------

-type emr_opts() :: [{out, json | raw}].
-type emr_return() :: {ok, jsx:json_term() | binary()} |
                      {error, {aws_error, jsx:json_term()}} |
                      {error, {socket_error, any()}} |
                      {error, tuple()}.


%% --- AddJobFlowSteps ---
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
    Body = jsx:encode(if Json == [] -> [{}];
                         true -> Json end),
    H1 = [{"host", Host}],
    H2 = [{"content-type", "application/x-amz-json-1.1"},
          {"x-amz-target", Action}],
    Region = erlcloud_aws:aws_region_from_host(Host),
    Headers = erlcloud_aws:sign_v4_headers(Cfg, H1, Body, Region, Service) ++ H2,
    case erlcloud_aws:aws_request_form_raw(post, Scheme, Host, Port,
                                           "/", Body, Headers, Cfg) of
        {ok, RespBody} -> case proplists:get_value(out, Opts, json) of
                              raw -> {ok, RespBody};
                              _ -> {ok, jsx:decode(RespBody)}
                          end;
        {error, {http_error, _Code, _StatusLine, ErrBody}} ->
            {error, {aws_error, jsx:decode(ErrBody)}};
        {error, {socket_error, Reason}} ->
            {error, {socket_error, Reason}};
        {error, Unexpected} ->
            {error, Unexpected}
    end.
