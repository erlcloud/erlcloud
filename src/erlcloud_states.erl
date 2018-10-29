-module(erlcloud_states).
-author('nikolay.kovalev@alertlogic.com').

-include("erlcloud.hrl").
-include("erlcloud_aws.hrl").

%%% Library initialization.
-export([configure/2, configure/3, configure/4, configure/5,
         new/2, new/3, new/4, new/5]).

-export([create_activity/2, create_activity/1,
         create_state_machine/4, create_state_machine/3,
         delete_activity/2, delete_activity/1,
         delete_state_machine/2, delete_state_machine/1,
         describe_activity/2, describe_activity/1,
         describe_execution/2, describe_execution/1,
         describe_state_machine/2, describe_state_machine/1,
         describe_state_machine_for_execution/2, describe_state_machine_for_execution/1,
         get_activity_task/3, get_activity_task/2, get_activity_task/1,
         get_execution_history/3, get_execution_history/2, get_execution_history/1,
         list_activities/2, list_activities/1, list_activities/0,
         list_executions/3, list_executions/2, list_executions/1,
         list_state_machines/2, list_state_machines/1, list_state_machines/0,
         send_task_failure/3, send_task_failure/2, send_task_failure/1,
         send_task_heartbeat/2, send_task_heartbeat/1,
         send_task_success/3, send_task_success/2,
         start_execution/3, start_execution/2, start_execution/1,
         stop_execution/3, stop_execution/2, stop_execution/1,
         update_state_machine/3, update_state_machine/2, update_state_machine/1]).

%%------------------------------------------------------------------------------
%% Library initialization.
%%------------------------------------------------------------------------------
-spec new(string(), string()) -> aws_config().
new(AccessKeyID, SecretAccessKey) ->
    #aws_config{access_key_id     = AccessKeyID,
                secret_access_key = SecretAccessKey}.

-spec new(string(), string(), string()) -> aws_config().
new(AccessKeyID, SecretAccessKey, Host) ->
    #aws_config{access_key_id     = AccessKeyID,
                secret_access_key = SecretAccessKey,
                states_host       = Host}.

-spec new(string(), string(), string(), non_neg_integer()) -> aws_config().
new(AccessKeyID, SecretAccessKey, Host, Port) ->
    #aws_config{access_key_id     = AccessKeyID,
                secret_access_key = SecretAccessKey,
                states_host       = Host,
                states_port       = Port}.

-spec new(string(), string(), string(), non_neg_integer(), string()) ->
    aws_config().
new(AccessKeyID, SecretAccessKey, Host, Port, Scheme) ->
    #aws_config{access_key_id     = AccessKeyID,
                secret_access_key = SecretAccessKey,
                states_host       = Host,
                states_port       = Port,
                states_scheme     = Scheme}.

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

default_config() -> erlcloud_aws:default_config().

%%------------------------------------------------------------------------------
%% CreateActivity
%%------------------------------------------------------------------------------

%%------------------------------------------------------------------------------
%% @doc
%% Create Activity API:
%% [https://docs.aws.amazon.com/step-functions/latest/apireference/API_CreateActivity.html]
%%
%% ===Example===
%%
%%------------------------------------------------------------------------------
-spec create_activity(ActivityName  :: binary()) -> {ok, map()} | {error, any()}.
create_activity(ActivityName) ->
    create_activity(ActivityName, default_config()).

-spec create_activity(ActivityName  :: binary(),
                      Config        :: aws_config()) -> {ok, map()} | {error, any()}.
create_activity(ActivityName, Config) when is_binary(ActivityName) ->
    Req = #{<<"name">> => ActivityName},
    step_request(Config, post, "CreateActivity", Req).

%%------------------------------------------------------------------------------
%% CreateStateMachine
%%------------------------------------------------------------------------------

%%------------------------------------------------------------------------------
%% @doc
%% Create State Machine API:
%% [https://docs.aws.amazon.com/step-functions/latest/apireference/API_CreateStateMachine.html]
%%
%% ===Example===
%%
%%------------------------------------------------------------------------------
-spec create_state_machine(Definition       :: map(),
                           StateMachineName :: binary(),
                           RoleArn          :: binary()) ->
    {ok, map()} | {error, any()}.
create_state_machine(Definition, StateMachineName, RoleArn) ->
    create_state_machine(Definition, StateMachineName, RoleArn, default_config()).

-spec create_state_machine(Definition       :: map(),
                           StateMachineName :: binary(),
                           RoleArn          :: binary(),
                           Config           :: aws_config()) ->
    {ok, map()} | {error, any()}.
create_state_machine(Definition, StateMachineName, RoleArn, Config)
        when is_binary(StateMachineName), is_binary(RoleArn), is_map(Definition) ->
    Req = #{
        <<"definition">>    => jsx:encode(Definition),
        <<"name">>          => StateMachineName,
        <<"roleArn">>       => RoleArn
    },
    step_request(Config, post, "CreateStateMachine", Req).

%%------------------------------------------------------------------------------
%% DeleteActivity
%%------------------------------------------------------------------------------

%%------------------------------------------------------------------------------
%% @doc
%% Delete Activity API:
%% [https://docs.aws.amazon.com/step-functions/latest/apireference/API_DeleteActivity.html]
%%
%% ===Example===
%%
%%------------------------------------------------------------------------------
-spec delete_activity(ActivityArn  :: binary()) -> ok | {error, any()}.
delete_activity(ActivityArn) ->
    delete_activity(ActivityArn, default_config()).

-spec delete_activity(ActivityArn  :: binary(),
                      Config       :: aws_config()) -> ok | {error, any()}.
delete_activity(ActivityArn, Config) when is_binary(ActivityArn) ->
    Req = #{<<"activityArn">> => ActivityArn},
    step_request(Config, post, "DeleteActivity", Req).

%%------------------------------------------------------------------------------
%% DeleteStateMachine
%%------------------------------------------------------------------------------

%%------------------------------------------------------------------------------
%% @doc
%% Delete State Machine API:
%% [https://docs.aws.amazon.com/step-functions/latest/apireference/API_DeleteStateMachine.html]
%%
%% ===Example===
%%
%%------------------------------------------------------------------------------
-spec delete_state_machine(StateMachineArn :: binary()) -> ok | {error, any()}.
delete_state_machine(StateMachineArn) ->
    delete_state_machine(StateMachineArn, default_config()).

-spec delete_state_machine(StateMachineArn  :: binary(),
                           Config           :: aws_config()) -> ok | {error, any()}.
delete_state_machine(StateMachineArn, Config) when is_binary(StateMachineArn) ->
    Req = #{<<"stateMachineArn">> => StateMachineArn},
    step_request(Config, post, "DeleteStateMachine", Req).


%%------------------------------------------------------------------------------
%% DescribeActivity
%%------------------------------------------------------------------------------

%%------------------------------------------------------------------------------
%% @doc
%% Describe Activity API:
%% [https://docs.aws.amazon.com/step-functions/latest/apireference/API_DescribeActivity.html]
%%
%% ===Example===
%%
%%------------------------------------------------------------------------------
-spec describe_activity(ActivityArn :: binary()) -> {ok, map()} | {error, any()}.
describe_activity(ActivityArn) ->
    describe_activity(ActivityArn, default_config()).

-spec describe_activity(ActivityArn   :: binary(),
                        Config        :: aws_config()) -> {ok, map()} | {error, any()}.
describe_activity(ActivityArn, Config) ->
    Req = #{<<"activityArn">> => ActivityArn},
    step_request(Config, post, "DescribeActivity", Req).

%%------------------------------------------------------------------------------
%% DescribeExecution
%%------------------------------------------------------------------------------

%%------------------------------------------------------------------------------
%% @doc
%% Describe Execution API:
%% [https://docs.aws.amazon.com/step-functions/latest/apireference/API_DescribeExecution.html]
%%
%% ===Example===
%%
%%------------------------------------------------------------------------------
-spec describe_execution(ExecutionArn :: binary()) ->
    {ok, map()} | {error, any()}.
describe_execution(ExecutionArn) ->
    describe_execution(ExecutionArn, default_config()).

-spec describe_execution(ExecutionArn   :: binary(),
                         Config         :: aws_config()) ->
    {ok, map()} | {error, any()}.
describe_execution(ExecutionArn, Config) ->
    Req = #{<<"executionArn">> => ExecutionArn},
    step_request(Config, post, "DescribeExecution", Req).

%%------------------------------------------------------------------------------
%% DescribeStateMachine
%%------------------------------------------------------------------------------

%%------------------------------------------------------------------------------
%% @doc
%% Describe State Machine API:
%% [https://docs.aws.amazon.com/step-functions/latest/apireference/API_DescribeStateMachine.html]
%%
%% ===Example===
%%
%%------------------------------------------------------------------------------
-spec describe_state_machine(StateMachineArn :: binary()) ->
    {ok, map()} | {error, any()}.
describe_state_machine(StateMachineArn) ->
    describe_state_machine(StateMachineArn, default_config()).

-spec describe_state_machine(StateMachineArn  :: binary(),
                             Config           :: aws_config()) ->
    {ok, map()} | {error, any()}.
describe_state_machine(StateMachineArn, Config) when is_binary(StateMachineArn) ->
    Req = #{<<"stateMachineArn">> => StateMachineArn},
    step_request(Config, post, "DescribeStateMachine", Req).

%%------------------------------------------------------------------------------
%% DescribeStateMachineForExecution
%%------------------------------------------------------------------------------

%%------------------------------------------------------------------------------
%% @doc
%% Describe State Machine For Execution API:
%% [https://docs.aws.amazon.com/step-functions/latest/apireference/API_DescribeStateMachineForExecution.html]
%%
%% ===Example===
%%
%%------------------------------------------------------------------------------
-spec describe_state_machine_for_execution(ExecutionArn :: binary()) ->
    {ok, map()} | {error, any()}.
describe_state_machine_for_execution(ExecutionArn) ->
    describe_state_machine_for_execution(ExecutionArn, default_config()).

-spec describe_state_machine_for_execution(ExecutionArn     :: binary(),
                                           Config           :: aws_config()) ->
    {ok, map()} | {error, any()}.
describe_state_machine_for_execution(ExecutionArn, Config)
        when is_binary(ExecutionArn) ->
    Req = #{<<"executionArn">> => ExecutionArn},
    step_request(Config, post, "DescribeStateMachineForExecution", Req).

%%------------------------------------------------------------------------------
%% GetActivityTask
%%------------------------------------------------------------------------------

%%------------------------------------------------------------------------------
%% @doc
%% Get a task for particular activity API:
%% [https://docs.aws.amazon.com/step-functions/latest/apireference/API_GetActivityTask.html]
%%  Options map may be provided with optional parameters by key:
%%  <<"workerName">>
%% ===Example===
%%
%%------------------------------------------------------------------------------
-spec get_activity_task(ActivityArn :: binary()) -> {ok, map()} | {error, any()}.
get_activity_task(ActivityArn) ->
    get_activity_task(ActivityArn, #{}, default_config()).

-spec get_activity_task(ActivityArn :: binary(),
                        Options     :: map()) -> {ok, map()} | {error, any()}.
get_activity_task(ActivityArn, Options) ->
    get_activity_task(ActivityArn, Options, default_config()).

-spec get_activity_task(ActivityArn     :: binary(),
                        Options         :: map(),
                        Config          :: aws_config()) ->
    {ok, map()} | {error, any()}.
get_activity_task(ActivityArn, Options, Config)
        when is_binary(ActivityArn), is_map(Options) ->
    OptReq = maps:with([<<"workerName">>], Options),
    Req = #{<<"activityArn">> => ActivityArn},
    step_request(Config, post, "GetActivityTask", maps:merge(OptReq, Req)).

%%------------------------------------------------------------------------------
%% GetExecutionHistory
%%------------------------------------------------------------------------------

%%------------------------------------------------------------------------------
%% @doc
%% Get Execution History API:
%% [https://docs.aws.amazon.com/step-functions/latest/apireference/API_GetExecutionHistory.html]
%%  Options map may be provided with optional parameters by key:
%%  <<"maxResults">>
%%  <<"nextToken">>
%%  <<"reverseOrder">>
%% ===Example===
%%
%%------------------------------------------------------------------------------
-spec get_execution_history(ExecutionArn :: binary()) ->
    {ok, map()} | {error, any()}.
get_execution_history(ExecutionArn) ->
    get_execution_history(ExecutionArn, #{}, default_config()).

-spec get_execution_history(ExecutionArn :: binary(),
                            Options      :: map()) ->
    {ok, map()} | {error, any()}.
get_execution_history(ExecutionArn, Options) ->
    get_execution_history(ExecutionArn, Options, default_config()).

-spec get_execution_history(ExecutionArn   :: binary(),
                            Options        :: map(),
                            Config         :: aws_config()) ->
    {ok, map()} | {error, any()}.
get_execution_history(ExecutionArn, Options, Config)
        when is_binary(ExecutionArn), is_map(Options) ->
    ReservedKeys = [<<"maxResults">>, <<"nextToken">>, <<"reverseOrder">>],
    OptReq = maps:with(ReservedKeys, Options),
    Req = #{<<"executionArn">> => ExecutionArn},
    step_request(Config, post, "GetExecutionHistory", maps:merge(OptReq, Req)).

%%------------------------------------------------------------------------------
%% ListActivities
%%------------------------------------------------------------------------------

%%------------------------------------------------------------------------------
%% @doc
%% List Activities API:
%% [https://docs.aws.amazon.com/step-functions/latest/apireference/API_ListActivities.html]
%%  Options map may be provided with optional parameters by key:
%%  <<"maxResults">>
%%  <<"nextToken">>
%% ===Example===
%%
%%------------------------------------------------------------------------------
-spec list_activities() -> {ok, map()} | {error, any()}.
list_activities() ->
    list_activities(#{}, default_config()).

-spec list_activities(Options :: map()) -> {ok, map()} | {error, any()}.
list_activities(Options) ->
    list_activities(Options, default_config()).

-spec list_activities(Options       :: map(),
                      Config        :: aws_config()) -> {ok, map()} | {error, any()}.
list_activities(Options, Config) when is_map(Options) ->
    ReservedKeys = [<<"maxResults">>, <<"nextToken">>],
    OptReq = maps:with(ReservedKeys, Options),
    step_request(Config, post, "ListActivities", OptReq).

%%------------------------------------------------------------------------------
%% ListExecutions
%%------------------------------------------------------------------------------

%%------------------------------------------------------------------------------
%% @doc
%% List Executions API:
%% [https://docs.aws.amazon.com/step-functions/latest/apireference/API_ListExecutions.html]
%%  Options map may be provided with optional parameters by key:
%%  <<"maxResults">>
%%  <<"nextToken">>
%%  <<"statusFilter">>
%% ===Example===
%%
%%------------------------------------------------------------------------------
-spec list_executions(StateMachineArn :: binary()) -> {ok, map()} | {error, any()}.
list_executions(StateMachineArn) ->
    list_executions(StateMachineArn, #{}, default_config()).

-spec list_executions(StateMachineArn :: binary(),
                      Options         :: map()) -> {ok, map()} | {error, any()}.
list_executions(StateMachineArn, Options) ->
    list_executions(StateMachineArn, Options, default_config()).

-spec list_executions(StateMachineArn   :: binary(),
                      Options           :: map(),
                      Config            :: aws_config()) -> {ok, map()} | {error, any()}.
list_executions(StateMachineArn, Options, Config)
        when is_binary(StateMachineArn), is_map(Options) ->
    ReservedKeys = [<<"maxResults">>, <<"nextToken">>, <<"statusFilter">>],
    OptReq = maps:with(ReservedKeys, Options),
    Req = #{<<"stateMachineArn">> => StateMachineArn},
    step_request(Config, post, "ListExecutions", maps:merge(OptReq, Req)).

%%------------------------------------------------------------------------------
%% ListStateMachines
%%------------------------------------------------------------------------------

%%------------------------------------------------------------------------------
%% @doc
%% List State Machines API:
%% [https://docs.aws.amazon.com/step-functions/latest/apireference/API_ListStateMachines.html]
%%  Options map may be provided with optional parameters by key:
%%  <<"maxResults">>
%%  <<"nextToken">>
%% ===Example===
%%
%%------------------------------------------------------------------------------
-spec list_state_machines() -> {ok, map()} | {error, any()}.
list_state_machines() ->
    list_state_machines(#{}, default_config()).

-spec list_state_machines(Options :: map()) -> {ok, map()} | {error, any()}.
list_state_machines(Options) ->
    list_state_machines(Options, default_config()).

-spec list_state_machines(Options       :: map(),
                          Config        :: aws_config()) -> {ok, map()} | {error, any()}.
list_state_machines(Options, Config) when is_map(Options) ->
    ReservedKeys = [<<"maxResults">>, <<"nextToken">>],
    OptReq = maps:with(ReservedKeys, Options),
    step_request(Config, post, "ListStateMachines", OptReq).

%%------------------------------------------------------------------------------
%% SendTaskFailure
%%------------------------------------------------------------------------------

%%------------------------------------------------------------------------------
%% @doc
%% Sends Task Failure API:
%% [https://docs.aws.amazon.com/step-functions/latest/apireference/API_SendTaskFailure.html]
%%  Options map may be provided with optional parameters by key:
%%  <<"cause">>
%%  <<"error">>
%% ===Example===
%%
%%------------------------------------------------------------------------------
-spec send_task_failure(TaskToken :: binary()) -> ok | {error, any()}.
send_task_failure(TaskToken) ->
    send_task_failure(TaskToken, #{}, default_config()).

-spec send_task_failure(TaskToken :: binary(),
                        Options   :: map()) -> ok | {error, any()}.
send_task_failure(TaskToken, Options) ->
    send_task_failure(TaskToken, Options, default_config()).

-spec send_task_failure(TaskToken   :: binary(),
                        Options     :: map(),
                        Config      :: aws_config()) -> ok | {error, any()}.
send_task_failure(TaskToken, Options, Config)
        when is_binary(TaskToken), is_map(Options) ->
    ReservedKeys = [<<"cause">>, <<"error">>],
    OptReq = maps:with(ReservedKeys, Options),
    Req = #{<<"taskToken">> => TaskToken},
    step_request(Config, post, "SendTaskFailure", maps:merge(OptReq, Req)).

%%------------------------------------------------------------------------------
%% SendTaskHeartbeat
%%------------------------------------------------------------------------------

%%------------------------------------------------------------------------------
%% @doc
%% Sends Task Heartbeat API:
%% [https://docs.aws.amazon.com/step-functions/latest/apireference/API_SendTaskHeartbeat.html]
%% ===Example===
%%
%%------------------------------------------------------------------------------
-spec send_task_heartbeat(TaskToken :: binary()) -> ok | {error, any()}.
send_task_heartbeat(TaskToken) ->
    send_task_heartbeat(TaskToken, default_config()).

-spec send_task_heartbeat(TaskToken :: binary(),
                          Config    :: aws_config()) -> ok | {error, any()}.
send_task_heartbeat(TaskToken, Config) when is_binary(TaskToken) ->
    Req = #{<<"taskToken">> => TaskToken},
    step_request(Config, post, "SendTaskHeartbeat", Req).

%%------------------------------------------------------------------------------
%% SendTaskSuccess
%%------------------------------------------------------------------------------

%%------------------------------------------------------------------------------
%% @doc
%% Sends Task Success API:
%% [https://docs.aws.amazon.com/step-functions/latest/apireference/API_SendTaskSuccess.html]
%% ===Example===
%%
%%------------------------------------------------------------------------------
-spec send_task_success(Output :: map(), TaskToken :: binary()) -> ok | {error, any()}.
send_task_success(Output, TaskToken) ->
    send_task_success(Output, TaskToken, default_config()).

-spec send_task_success(Output      :: map(),
                        TaskToken   :: binary(),
                        Config      :: aws_config()) -> ok | {error, any()}.
send_task_success(Output, TaskToken, Config) ->
    Req = #{
        <<"taskToken">>     => TaskToken,
        <<"output">>        => jsx:encode(Output)
    },
    step_request(Config, post, "SendTaskSuccess", Req).

%%------------------------------------------------------------------------------
%% StartExecution
%%------------------------------------------------------------------------------

%%------------------------------------------------------------------------------
%% @doc
%% Starts Execution API:
%% [https://docs.aws.amazon.com/step-functions/latest/apireference/API_StartExecution.html]
%%  Options map may be provided with optional parameters by key:
%%  <<"name">>
%%  <<"input">> key may be specified but MUST be presented in request
%% ===Example===
%%
%%------------------------------------------------------------------------------
-spec start_execution(StateMachineArn :: binary()) -> {ok, map()} | {error, any()}.
start_execution(StateMachineArn) ->
    start_execution(StateMachineArn, #{}, default_config()).

-spec start_execution(StateMachineArn :: binary(),
                      Options         :: map()) -> {ok, map()} | {error, any()}.
start_execution(StateMachineArn, Options) ->
    start_execution(StateMachineArn, Options, default_config()).

-spec start_execution(StateMachineArn   :: binary(),
                      Options           :: list(),
                      Config            :: aws_config()) ->
    {ok, map()} | {error, any()}.
start_execution(StateMachineArn, Options, Config)
        when is_binary(StateMachineArn), is_map(Options) ->
    InputValue = jsx:encode(maps:get(<<"input">>, Options, #{})),
    OptReq = maps:merge(#{<<"input">> => InputValue}, maps:with([<<"name">>], Options)),
    Req = #{<<"stateMachineArn">> => StateMachineArn},
    step_request(Config, post, "StartExecution", maps:merge(OptReq, Req)).

%%------------------------------------------------------------------------------
%% StopExecution
%%------------------------------------------------------------------------------

%%------------------------------------------------------------------------------
%% @doc
%% Stop Execution API:
%% [https://docs.aws.amazon.com/step-functions/latest/apireference/API_StopExecution.html]
%%  Options map may be provided with optional parameters by key:
%%  <<"cause">>
%%  <<"error">>
%% ===Example===
%%
%%------------------------------------------------------------------------------
-spec stop_execution(ExecutionArn :: binary()) -> {ok, map()} | {error, any()}.
stop_execution(ExecutionArn) ->
    stop_execution(ExecutionArn, #{}, default_config()).

-spec stop_execution(ExecutionArn :: binary(),
                     Options      :: map()) -> {ok, map()} | {error, any()}.
stop_execution(ExecutionArn, Options) ->
    stop_execution(ExecutionArn, Options, default_config()).

-spec stop_execution(ExecutionArn   :: binary(),
                     Options           :: list(),
                     Config            :: aws_config()) ->
    {ok, map()} | {error, any()}.
stop_execution(ExecutionArn, Options, Config)
        when is_binary(ExecutionArn), is_map(Options) ->
    ReservedKeys = [<<"cause">>, <<"error">>],
    OptReq = maps:with(ReservedKeys, Options),
    Req = #{<<"executionArn">> => ExecutionArn},
    step_request(Config, post, "StopExecution", maps:merge(OptReq, Req)).

%%------------------------------------------------------------------------------
%% UpdateStateMachine
%%------------------------------------------------------------------------------

%%------------------------------------------------------------------------------
%% @doc
%% Update State Machine API:
%% [https://docs.aws.amazon.com/step-functions/latest/apireference/API_UpdateStateMachine.html]
%%  Options map may be provided with optional parameters by key:
%%  <<"definition">>
%%  <<"roleArn">>
%% ===Example===
%%
%%------------------------------------------------------------------------------
-spec update_state_machine(StateMachineArn  :: binary()) ->
    {ok, map()} | {error, any()}.
update_state_machine(StateMachineArn) ->
    update_state_machine(StateMachineArn, #{}, default_config()).

-spec update_state_machine(StateMachineArn  :: binary(),
                           Options          :: map()) ->
    {ok, map()} | {error, any()}.
update_state_machine(StateMachineArn, Options) ->
    update_state_machine(StateMachineArn, Options, default_config()).

-spec update_state_machine(StateMachineArn  :: binary(),
                           Options          :: map(),
                           Config           :: aws_config()) ->
    {ok, map()} | {error, any()}.
update_state_machine(StateMachineArn, Options, Config)
        when is_binary(StateMachineArn), is_map(Options) ->
    ReservedKeys = [<<"definition">>, <<"error">>],
    OptReq = maps:with(ReservedKeys, Options),
    OptReqJson = case maps:get(<<"definition">>, OptReq, undefined) of
        undefined ->
            OptReq;
        Definition when is_map(Definition) ->
            maps:put(<<"definition">>, jsx:encode(Definition), OptReq)
    end,
    Req = #{<<"stateMachineArn">> => StateMachineArn},
    step_request(Config, post, "UpdateStateMachine", maps:merge(OptReqJson, Req)).

%%------------------------------------------------------------------------------
%% Utility Functions
%%------------------------------------------------------------------------------
step_request(Config, Method, OpName, Request) ->
    case erlcloud_aws:update_config(Config) of
        {ok, Config} ->
            Body       = jsx:encode(Request),
            Headers    = get_headers(Config, "AWSStepFunctions." ++ OpName, Body),
            AwsRequest = #aws_request{service         = states,
                                      uri             = get_url(Config),
                                      method          = Method,
                                      request_headers = Headers,
                                      request_body    = Body},
            request(Config, AwsRequest);
        {error, Reason} ->
            {error, Reason}
    end.

request(Config, Request) ->
    Result = erlcloud_retry:request(Config, Request, fun handle_result/1),
    case erlcloud_aws:request_to_return(Result) of
        {ok, {_, <<>>}} ->
            ok;
        {ok, {_, <<"{}">>}} ->
            ok;
        {ok, {_, RespBody}} ->
            {ok, jsx:decode(RespBody, [return_maps])};
        {error, _} = Error ->
            Error
    end.

handle_result(#aws_request{response_type = ok} = Request) ->
    Request;
handle_result(#aws_request{response_type    = error,
                           error_type       = aws,
                           response_status  = Status} = Request)
  when Status >= 500 ->
    Request#aws_request{should_retry = true};
handle_result(#aws_request{response_type = error,
                           error_type    = aws} = Request) ->
    Request#aws_request{should_retry = false}.

get_headers(#aws_config{states_host = Host} = Config, Operation, Body) ->
    Headers = [{"host",         Host},
               {"x-amz-target", Operation},
               {"content-type", "application/x-amz-json-1.0"}],
    Region = erlcloud_aws:aws_region_from_host(Host),
    erlcloud_aws:sign_v4_headers(Config, Headers, Body, Region, "states").

get_url(#aws_config{states_scheme = Scheme,
                    states_host   = Host,
                    states_port   = Port}) ->
    Scheme ++ Host ++ ":" ++ integer_to_list(Port).
