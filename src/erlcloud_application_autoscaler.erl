-module(erlcloud_application_autoscaler).

-include("erlcloud_aws.hrl").
-include("erlcloud_as.hrl").
-include("erlcloud_xmerl.hrl").

%% ------------------------------------------------------------------
%% AWS Application Autoscaling Function Exports
%% ------------------------------------------------------------------

-export([delete_scaling_policy/5]).

-export([delete_scheduled_action/2]).
-export([delete_scheduled_action/5]).

-export([deregister_scalable_target/2]).
-export([deregister_scalable_target/4]).

-export([describe_scalable_targets/2]).

-export([describe_scaling_activities/2]).

-export([describe_scaling_policies/2]).

-export([describe_scheduled_actions/2]).

-export([put_scaling_policy/2]).
-export([put_scaling_policy/5]).
-export([put_scaling_policy/7]).

-export([put_scheduled_action/2]).
-export([put_scheduled_action/5]).
-export([put_scheduled_action/6]).
-export([put_scheduled_action/7]).
-export([put_scheduled_action/8]).

-export([register_scalable_target/2]).
-export([register_scalable_target/4]).
-export([register_scalable_target/5]).
-export([register_scalable_target/7]).

%% ------------------------------------------------------------------
%% AWS Application Autoscaling Library Initialization Function Exports
%% ------------------------------------------------------------------

-export([configure/2, configure/3, configure/4, configure/5,
         new/2, new/3, new/4, new/5]).
-export([default_config/0]).

%% ------------------------------------------------------------------
%% Macro Definitions
%% ------------------------------------------------------------------

-define(NUM_ATTEMPTS, 10).
-define(DEFAULT_MAX_RECORDS, 20).

%% ------------------------------------------------------------------
%% Type Definitions
%% ------------------------------------------------------------------

-type response_attribute() :: binary().
-type response_key() :: atom().
-type response() :: [{response_key(), response_attribute()} | proplist:proplist()].

-type aws_aas_request_body() :: [proplist:proplist()].

-spec extract_alarm(J :: binary()) -> response().
extract_alarm(J) ->
    erlcloud_json:decode([
        {alarm_name, <<"AlarmName">>, optional_string},
        {alarm_arn, <<"AlarmARN">>, optional_string}
    ], J).

-spec extract_step_adjustments(J :: binary) -> response().
extract_step_adjustments(J) ->
    erlcloud_json:decode([
        {metric_interval_lower_bound, <<"MetricIntervalLowerBound">>, optional_integer},
        {metric_interval_upper_bound, <<"MetricIntervalupperBound">>, optional_integer},
        {scaling_adjustment, <<"ScalingAdjustment">>, optional_integer}
    ], J).

-spec extract_step_scaling_policy(J :: binary) -> response().
extract_step_scaling_policy(J) ->
    Scaling = erlcloud_json:decode([
        {adjustment_type, <<"AdjustmentType">>, optional_string},
        {cooldown, <<"Cooldown">>, optional_integer},
        {metric_aggregation_type, <<"MetricAggregationType">>, optional_string},
        {min_adjustment_magnitude, <<"MinAdjustmentMagnitude">>, optional_integer}
    ], J),
    case proplists:get_value(<<"StepAdjustments">>, J, undefined) of
        undefined ->
            Scaling;
        Steps ->
            [{step_adjustments,  [ extract_step_adjustments(Step) || Step <- Steps]} | Scaling]
    end.

-spec extract_dimensions(J :: binary) -> response().
extract_dimensions(J) ->
    erlcloud_json:decode([
        {name, <<"Name">>, optional_string},
        {value, <<"Value">>, optional_string}
    ], J).

-spec extract_predefined_metric_specifications(J :: binary) -> response().
extract_predefined_metric_specifications(J) ->
    erlcloud_json:decode([
        {predefined_metric_type, <<"PredefinedMetricType">>, optional_string},
        {resource_label, <<"ResourceLabel">>, optional_string}
    ], J).

-spec extract_customized_metric_specification(J :: binary) -> response().
extract_customized_metric_specification(J) ->
    CustomizedMetricSpecification = erlcloud_json:decode([
        {metric_name, <<"MetricName">>, optional_string},
        {namespace, <<"Namespace">>, optional_string},
        {statistic, <<"Statistic">>, optional_string},
        {unit, <<"Unit">>, optional_string}
    ], J),
    MaybeHasDimension = case proplists:get_value(<<"Dimensions">>, J, undefined) of
        undefined ->
            [];
        Dimensions ->
            [{dimension, [extract_dimensions(Dim) || Dim <- Dimensions ]}]
    end,
    CustomizedMetricSpecification ++ MaybeHasDimension.

-spec extract_target_tracking_policy(J :: binary) -> response().
extract_target_tracking_policy(J) ->
    Target = erlcloud_json:decode([
        {disable_scale_in, <<"DisableScaleIn">>, optional_boolean},
        {scale_in_cooldown, <<"ScaleInCooldown">>, optional_integer},
        {scale_out_cooldown, <<"ScaleOutCooldown">>, optional_integer},
        {target_value, <<"TargetValue">>, optional_integer}
    ], J),
    MaybeHasCustomizedMetrics = case proplists:get_value(<<"CustomizedMetricsSpecification">>, J, undefined) of
        undefined ->
            [];
        CustomMetrics ->
            [{customized_metrics_specification, extract_customized_metric_specification(CustomMetrics)}]
    end,
    MaybeHasPredefinedMetrics = case proplists:get_value(<<"PredefinedMetricSpecification">>, J, undefined) of
        undefined ->
            [];
        PredefinedMetrics ->
            [{predefined_metric_specification, extract_predefined_metric_specifications(PredefinedMetrics)}]
    end,
    Target ++ MaybeHasCustomizedMetrics ++ MaybeHasPredefinedMetrics.

-spec extract_scaling_policies(J :: binary()) -> response().
extract_scaling_policies(J) ->
    Policy = erlcloud_json:decode([
        {creation_time, <<"CreationTime">>, optional_integer},
        {policy_arn, <<"PolicyARN">>, optional_string},
        {policy_name, <<"PolicyName">>, optional_string},
        {policy_type, <<"PolicyType">>, optional_string},
        {resource_id, <<"ResourceId">>, optional_string},
        {scalable_dimension, <<"ScalableDimension">>, optional_string},
        {service_namespace, <<"ServiceNamespace">>, optional_string}
    ], J),
    MaybeHasAlarms = case proplists:get_value(<<"Alarms">>, J, undefined) of
        undefined ->
            [];
        AlarmJSON ->
            [{alarms, [extract_alarm(Alarm) || Alarm <- AlarmJSON]}]
    end,
    MaybeHasStepScaling = case proplists:get_value(<<"StepScalingPolicyConfiguration">>, J, undefined) of
        undefined ->
            [];
        StepScaling ->
            [{step_scaling_policy_configuration, extract_step_scaling_policy(StepScaling)}]
    end,
    MaybeHasTargetTracking = case proplists:get_value(<<"TargetTrackingScalingPolicyConfiguration">>, J, undefined) of
        undefined ->
            [];
        TargetTracking ->
            [{target_tracking_scaling_policy_configuration, extract_target_tracking_policy(TargetTracking)}]
    end,
    Policy ++ MaybeHasAlarms ++ MaybeHasStepScaling ++ MaybeHasTargetTracking.


-spec extract_scalable_targets(J :: binary()) -> response().
extract_scalable_targets(J) ->
    erlcloud_json:decode([
        {creation_time, <<"CreationTime">>, optional_integer},
        {max_capacity, <<"MaxCapacity">>, optional_integer},
        {min_capacity, <<"MinCapacity">>, optional_integer},
        {resource_id, <<"ResourceId">>, optional_string},
        {role_arn, <<"RoleARN">>, optional_string},
        {scalable_dimension, <<"ScalableDimension">>, optional_string},
        {service_namespace, <<"ServiceNamespace">>, optional_string}
    ], J).

-spec extract_scaling_activities(J :: binary) -> response().
extract_scaling_activities(J) ->
    erlcloud_json:decode([
        {activity_id, <<"ActivityId">>, optional_string},
        {cause, <<"Cause">>, optional_string},
        {description, <<"Description">>, optional_string},
        {resource_id, <<"ResourceId">>, optional_string},
        {scalable_dimension, <<"ScalableDimension">>, optional_string},
        {service_namespace, <<"ServiceNamespace">>, optional_string},
        {start_time, <<"StartTime">>, optional_integer},
        {status_code, <<"StatusCode">>, optional_string},
        {status_message, <<"StatusMessage">>, optional_string}
    ], J).

-spec extract_scheduled_action(J :: binary) -> response().
extract_scheduled_action(J) ->
    Res = erlcloud_json:decode([
        {creation_time, <<"CreationTime">>, optional_integer},
        {resource_id, <<"ResourceId">>, optional_string},
        {scalable_dimension, <<"ScalableDimension">>, optional_string},
        {service_namespace, <<"ServiceNamespace">>, optional_string},
        {scheduled_action_arn, <<"ScheduledActionARN">>, optional_string},
        {scheduled_action_name, <<"ScheduledActionName">>, optional_string},
        {start_time, <<"StartTime">>, optional_integer},
        {end_time, <<"EndTime">>, optional_integer},
        {schedule, <<"Schedule">>, optional_string}
    ], J),
    Action = proplists:get_value(<<"ScalableTargetAction">>, J),
    PropAction = erlcloud_json:decode([
        {max_capacity, <<"MaxCapacity">>, optional_integer},
        {min_capacity, <<"MinCapacity">>, optional_integer}
    ], Action),
    [{scalable_target_action, PropAction} | Res].


%%%------------------------------------------------------------------------------
%%% Library initialization.
%%%------------------------------------------------------------------------------

-spec new(string(), string()) -> aws_config().
new(AccessKeyID, SecretAccessKey) ->
    #aws_config{access_key_id=AccessKeyID,
                secret_access_key=SecretAccessKey}.

-spec new(string(), string(), string()) -> aws_config().
new(AccessKeyID, SecretAccessKey, Host) ->
    #aws_config{access_key_id=AccessKeyID,
                secret_access_key=SecretAccessKey,
                application_autoscaling_host=Host}.

-spec new(string(), string(), string(), non_neg_integer()) -> aws_config().
new(AccessKeyID, SecretAccessKey, Host, Port) ->
    #aws_config{access_key_id=AccessKeyID,
                secret_access_key=SecretAccessKey,
                application_autoscaling_host=Host,
                application_autoscaling_port=Port}.

-spec new(string(), string(), string(), non_neg_integer(), string()) -> aws_config().
new(AccessKeyID, SecretAccessKey, Host, Port, Scheme) ->
    #aws_config{access_key_id=AccessKeyID,
                secret_access_key=SecretAccessKey,
                application_autoscaling_host=Host,
                application_autoscaling_port=Port,
                application_autoscaling_scheme=Scheme}.

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

default_config() -> erlcloud_aws:default_config().

%%%------------------------------------------------------------------------------
%%% AWS Application Autoscaling functions.
%%%
%%% API Documentation on AWS: https://docs.aws.amazon.com/autoscaling/application/APIReference/Welcome.html
%%%------------------------------------------------------------------------------

%%------------------------------------------------------------------------------
%% @doc.
%% DeleteScalingPolicy.
%%
%% https://docs.aws.amazon.com/autoscaling/application/APIReference/API_DeleteScalingPolicy.html
%%------------------------------------------------------------------------------

-spec delete_scaling_policy(Configuration :: aws_config(),
                            PolicyName :: binary(),
                            ResourceId :: binary(),
                            ScalableDimension :: binary(),
                            ServiceNamespace :: binary()
                        ) -> term().
delete_scaling_policy(Configuration, PolicyName, ResourceId, ScalableDimension, ServiceNamespace) ->
    BodyProps = [{<<"PolicyName">>, PolicyName},
                 {<<"ResourceId">>, ResourceId},
                 {<<"ScalableDimension">>, ScalableDimension},
                 {<<"ServiceNamespace">>, ServiceNamespace}],
    delete_scaling_policy(Configuration, BodyProps).

-spec delete_scaling_policy(Configuration :: aws_config(),
                            BodyConfiguration :: aws_aas_request_body()
                        ) -> term().
delete_scaling_policy(Configuration, BodyConfiguration) ->
    request_with_action(Configuration, BodyConfiguration, "AnyScaleFrontendService.DescribeScalingPolicies").

%%------------------------------------------------------------------------------
%% @doc.
%% DeleteScheduledAction.
%%
%% https://docs.aws.amazon.com/autoscaling/application/APIReference/API_DeleteScheduledAction.html
%%------------------------------------------------------------------------------

-spec delete_scheduled_action(
                            Configuration :: erlcloud_aws:aws_config(),
                            ResourceId :: binary(),
                            ScalableDimension :: binary(),
                            ScheduledActionName :: binary(),
                            ServiceNamespace :: binary()
                        ) -> response().
delete_scheduled_action(Configuration, ResourceId, ScalableDimension, ScheduledActionName, ServiceNamespace) ->
    BodyProps = [{<<"ScheduledActionName">>, ScheduledActionName},
                 {<<"ResourceId">>, ResourceId},
                 {<<"ScalableDimension">>, ScalableDimension},
                 {<<"ServiceNamespace">>, ServiceNamespace}],
    delete_scheduled_action(Configuration, BodyProps).


-spec delete_scheduled_action(Configuration :: aws_config(),
                              BodyConfiguration :: aws_aas_request_body()
                            ) -> response().
delete_scheduled_action(Configuration, BodyConfiguration) ->
    request_with_action(Configuration, BodyConfiguration, "AnyScaleFrontendService.DeleteScheduledAction").

%%------------------------------------------------------------------------------
%% @doc.
%% DeregisterScalableTarget.
%%
%% https://docs.aws.amazon.com/autoscaling/application/APIReference/API_DeregisterScalableTarget.html
%%------------------------------------------------------------------------------

-spec deregister_scalable_target(
                            Configuration :: erlcloud_aws:aws_config(),
                            ResourceId :: binary(),
                            ScalableDimension :: binary(),
                            ServiceNamespace :: binary()
                            ) -> response().
deregister_scalable_target(Configuration, ResourceId, ScalableDimension, ServiceNamespace) ->
    BodyProps = [{<<"ResourceId">>, ResourceId},
                 {<<"ScalableDimension">>, ScalableDimension},
                 {<<"ServiceNamespace">>, ServiceNamespace}],
    deregister_scalable_target(Configuration, BodyProps).

-spec deregister_scalable_target(
                            Configuration :: erlcloud_aws:aws_config(),
                            BodyConfiguration :: aws_aas_request_body()
                            ) -> response().
deregister_scalable_target(Configuration, BodyConfiguration) ->
    request_with_action(Configuration, BodyConfiguration, "AnyScaleFrontendService.DeregisterScalableTarget").

%%------------------------------------------------------------------------------
%% @doc.
%% DescribeScalableTargets.
%%
%% https://docs.aws.amazon.com/autoscaling/application/APIReference/API_DescribeScalableTargets.html
%%------------------------------------------------------------------------------

-spec describe_scalable_targets(
                            erlcloud_aws:aws_config(),
                            aws_aas_request_body() |  binary()
                            ) -> response().
describe_scalable_targets(Configuration, ServiceNamespace) when is_binary(ServiceNamespace)->
    describe_scalable_targets(Configuration, [{<<"ServiceNamespace">>, ServiceNamespace}]);
describe_scalable_targets(Configuration, BodyConfiguration) ->
    case request_with_action(Configuration, BodyConfiguration, "AnyScaleFrontendService.DescribeScalableTargets") of
        {ok, Result} ->
            ScalableTargets = proplists:get_value(<<"ScalableTargets">>, Result),
            PropRes = [extract_scalable_targets(E) || E <- ScalableTargets],
            NextToken = proplists:get_value(<<"NextToken">>, Result, undefined),
            MaybeHasNext = case NextToken of
                undefined -> [];
                Token -> [{next_token, Token}]
            end,
            {ok, PropRes ++ MaybeHasNext};
        {error, Error} ->
            {error, Error}
    end.

%%------------------------------------------------------------------------------
%% @doc.
%% DescribeScalingActivities.
%%
%% https://docs.aws.amazon.com/autoscaling/application/APIReference/API_DescribeScalingActivities.html
%%------------------------------------------------------------------------------

-spec describe_scaling_activities(
                            erlcloud_aws:aws_config(),
                            aws_aas_request_body() |  binary()
                            ) -> response().
describe_scaling_activities(Configuration, ServiceNamespace) when is_binary(ServiceNamespace) ->
    describe_scaling_activities(Configuration, [{<<"ServiceNamespace">>, ServiceNamespace}]);
describe_scaling_activities(Configuration, BodyConfiguration) ->
    case request_with_action(Configuration, BodyConfiguration, "AnyScaleFrontendService.DescribeScalingActivities") of
        {ok, Result} ->
            {ok, Result} = request_with_action(Configuration, BodyConfiguration, "AnyScaleFrontendService.DescribeScalingActivities"),
            ScalingActivities = proplists:get_value(<<"ScalingActivities">>, Result),
            PropRes = [extract_scaling_activities(E) || E <- ScalingActivities],
            NextToken = proplists:get_value(<<"NextToken">>, Result, undefined),
            MaybeHasNext = case NextToken of
                undefined -> [];
                Token -> [{next_token, Token}]
            end,
            {ok, PropRes ++ MaybeHasNext};
        {error, Error} ->
            {error, Error}
    end.

%%------------------------------------------------------------------------------
%% @doc.
%% DescribeScalingPolicies.
%%
%% https://docs.aws.amazon.com/autoscaling/application/APIReference/API_DescribeScalingPolicies.html
%%------------------------------------------------------------------------------

-spec describe_scaling_policies(
                            erlcloud_aws:aws_config(),
                            aws_aas_request_body() |  binary()
                            ) -> response().
describe_scaling_policies(Configuration, ServiceNamespace) when is_binary(ServiceNamespace) ->
    describe_scaling_policies(Configuration, [{<<"ServiceNamespace">>, ServiceNamespace}]);
describe_scaling_policies(Configuration, BodyConfiguration) ->
    case request_with_action(Configuration, BodyConfiguration, "AnyScaleFrontendService.DescribeScalingPolicies") of
        {ok, Result} ->
            ScalingPolicies = proplists:get_value(<<"ScalingPolicies">>, Result),
            PropRes = [extract_scaling_policies(Extracted) || Extracted <- ScalingPolicies],
            NextToken = proplists:get_value(<<"NextToken">>, Result, undefined),
            MaybeHasNext = case NextToken of
                undefined -> [];
                Token -> [{next_token, Token}]
            end,
            {ok, PropRes ++ MaybeHasNext};
        {error, Error} ->
            {error, Error}
    end.


%%------------------------------------------------------------------------------
%% @doc.
%% DescribeScheduledActions.
%%
%% https://docs.aws.amazon.com/autoscaling/application/APIReference/API_DescribeScheduledActions.html
%%------------------------------------------------------------------------------

-spec describe_scheduled_actions(
                            erlcloud_aws:aws_config(),
                            aws_aas_request_body() |  binary()
                            ) -> response().
describe_scheduled_actions(Configuration, ServiceNamespace) when is_binary(ServiceNamespace) ->
    describe_scheduled_actions(Configuration, [{<<"ServiceNamespace">>, ServiceNamespace}]);
describe_scheduled_actions(Configuration, BodyConfiguration) ->
    case request_with_action(Configuration, BodyConfiguration, "AnyScaleFrontendService.DescribeScheduledActions") of
        {ok, Result} ->
            ScheduledActions = proplists:get_value(<<"ScheduledActions">>, Result),
            PropRes = [extract_scheduled_action(Extracted) || Extracted <- ScheduledActions],
            NextToken = proplists:get_value(<<"NextToken">>, Result, undefined),
            MaybeHasNext = case NextToken of
                undefined -> [];
                Token -> [{next_token, Token}]
            end,
            {ok, PropRes ++ MaybeHasNext};
        {error, Error} ->
            {error, Error}

    end.

%%------------------------------------------------------------------------------
%% @doc.
%% PutScalingPolicy.
%%
%% https://docs.aws.amazon.com/autoscaling/application/APIReference/API_PutScalingPolicy.html
%%------------------------------------------------------------------------------

-spec put_scaling_policy(
                            Configuration :: aws_config(),
                            PolicyName :: binary(),
                            ResourceId :: binary(),
                            ScalableDimension :: binary(),
                            ServiceNamespace :: binary()
                        ) -> response().
put_scaling_policy(Configuration, PolicyName, ResourceId, ScalableDimension, ServiceNamespace) ->
    BodyProps = [{<<"PolicyName">>, PolicyName},
                 {<<"ResourceId">>, ResourceId},
                 {<<"ScalableDimension">>, ScalableDimension},
                 {<<"ServiceNamespace">>, ServiceNamespace}],
    put_scaling_policy(Configuration, BodyProps).

-spec put_scaling_policy(
                            Configuration :: aws_config(),
                            PolicyName :: binary(),
                            ResourceId :: binary(),
                            ScalableDimension :: binary(),
                            ServiceNamespace :: binary(),
                            PolicyType :: binary(),
                            Policy :: [proplist:proplist()]
                        ) -> response().
put_scaling_policy(Configuration, PolicyName, ResourceId, ScalableDimension, ServiceNamespace, PolicyType, Policy) ->
    BodyProps = [{<<"PolicyName">>, PolicyName},
                 {<<"ResourceId">>, ResourceId},
                 {<<"ScalableDimension">>, ScalableDimension},
                 {<<"ServiceNamespace">>, ServiceNamespace},
                 {<<"PolicyType">>, PolicyType}],
    case PolicyType of
        <<"StepScaling">> ->
            put_scaling_policy(Configuration, [{<<"StepScalingPolicyConfiguration">>, Policy} | BodyProps]);
        <<"TargetTrackingScaling">> ->
            put_scaling_policy(Configuration, [{<<"TargetTrackingScalingPolicyConfiguration">>, Policy} | BodyProps])
    end.

-spec put_scaling_policy(
                            Configuration :: erlcloud_aws:aws_config(),
                            BodyConfiguration :: aws_aas_request_body()
                            ) -> response().
put_scaling_policy(Configuration, BodyConfiguration) ->
    case request_with_action(Configuration, BodyConfiguration, "AnyScaleFrontendService.PutScalingPolicy") of
        {ok, Result} ->
            Alarms = proplists:get_value(<<"Alarms">>, Result),
            PropAlarms = [extract_alarm(E) || E <- Alarms],
            {ok, [{policy_arn, proplists:get_value(<<"PolicyARN">>, Result)} | PropAlarms]};
        {error, Error} ->
            {error, Error}
    end.

%%------------------------------------------------------------------------------
%% @doc.
%% PutScheduledAction.
%%
%% https://docs.aws.amazon.com/autoscaling/application/APIReference/API_PutScheduledAction.html
%%------------------------------------------------------------------------------

-spec put_scheduled_action(
                            Configuration :: aws_config(),
                            ResourceId :: binary(),
                            ScalableDimension :: binary(),
                            ServiceNamespace :: binary(),
                            ScheduledActionName :: binary()
                        ) -> response().
put_scheduled_action(Configuration, ResourceId, ScalableDimension, ServiceNamespace, ScheduledActionName) ->
    BodyProps = [{<<"ScheduledActionName">>, ScheduledActionName},
                 {<<"ResourceId">>, ResourceId},
                 {<<"ScalableDimension">>, ScalableDimension},
                 {<<"ServiceNamespace">>, ServiceNamespace}],
    put_scheduled_action(Configuration, BodyProps).

-spec put_scheduled_action(
                            Configuration :: aws_config(),
                            ResourceId :: binary(),
                            ScalableDimension :: binary(),
                            ServiceNamespace :: binary(),
                            ScheduledActionName :: binary(),
                            Schedule :: binary()
                        ) -> response().
put_scheduled_action(Configuration, ResourceId, ScalableDimension, ServiceNamespace, ScheduledActionName, Schedule) ->
    BodyProps = [{<<"ScheduledActionName">>, ScheduledActionName},
                 {<<"ResourceId">>, ResourceId},
                 {<<"ScalableDimension">>, ScalableDimension},
                 {<<"ServiceNamespace">>, ServiceNamespace},
                 {<<"Schedule">>, Schedule}],
    put_scheduled_action(Configuration, BodyProps).

-spec put_scheduled_action(
                            Configuration :: aws_config(),
                            ResourceId :: binary(),
                            ScalableDimension :: binary(),
                            ServiceNamespace :: binary(),
                            ScheduledActionName :: binary(),
                            Schedule :: binary(),
                            StartTime :: pos_integer()
                        ) -> response().
put_scheduled_action(Configuration, ResourceId, ScalableDimension, ServiceNamespace, ScheduledActionName, Schedule, StartTime) ->
    BodyProps = [{<<"ScheduledActionName">>, ScheduledActionName},
                 {<<"ResourceId">>, ResourceId},
                 {<<"ScalableDimension">>, ScalableDimension},
                 {<<"ServiceNamespace">>, ServiceNamespace},
                 {<<"Schedule">>, Schedule},
                 {<<"StartTime">>, StartTime}],
    put_scheduled_action(Configuration, BodyProps).

-spec put_scheduled_action(
                            Configuration :: aws_config(),
                            ResourceId :: binary(),
                            ScalableDimension :: binary(),
                            ServiceNamespace :: binary(),
                            ScheduledActionName :: binary(),
                            Schedule :: binary(),
                            StartTime :: pos_integer(),
                            EndTime :: pos_integer()
                        ) -> response().
put_scheduled_action(Configuration, ResourceId, ScalableDimension, ServiceNamespace, ScheduledActionName, Schedule, StartTime, EndTime) ->
    BodyProps = [{<<"ScheduledActionName">>, ScheduledActionName},
                 {<<"ResourceId">>, ResourceId},
                 {<<"ScalableDimension">>, ScalableDimension},
                 {<<"ServiceNamespace">>, ServiceNamespace},
                 {<<"Schedule">>, Schedule},
                 {<<"StartTime">>, StartTime},
                 {<<"EndTime">>, EndTime}],
    put_scheduled_action(Configuration, BodyProps).

-spec put_scheduled_action(Configuration :: aws_config(),
                              BodyConfiguration :: aws_aas_request_body()
                            ) -> response().
put_scheduled_action(Configuration, BodyConfiguration) ->
    request_with_action(Configuration, BodyConfiguration, "AnyScaleFrontendService.PutScheduledAction").

%%------------------------------------------------------------------------------
%% @doc.
%% RegisterScalableTarget.
%%
%% https://docs.aws.amazon.com/autoscaling/application/APIReference/API_RegisterScalableTarget.html
%%------------------------------------------------------------------------------

-spec register_scalable_target(Configuration :: aws_config(),
                               ResourceId :: binary(),
                               ScalableDimension :: binary(),
                               ServiceNamespace :: binary()) -> response().
register_scalable_target(Configuration, ResourceId, ScalableDimension, ServiceNamespace) ->
    BodyProps = [{<<"ResourceId">>, ResourceId},
                 {<<"ScalableDimension">>, ScalableDimension},
                 {<<"ServiceNamespace">>, ServiceNamespace}],
    register_scalable_target(Configuration, BodyProps).

-spec register_scalable_target(Configuration :: aws_config(),
                               ResourceId :: binary(),
                               ScalableDimension :: binary(),
                               ServiceNamespace :: binary(),
                               ResourceARN :: binary()) -> response().
register_scalable_target(Configuration, ResourceId, ScalableDimension, ServiceNamespace, ResourceARN) ->
    BodyProps = [{<<"ResourceId">>, ResourceId},
                 {<<"ScalableDimension">>, ScalableDimension},
                 {<<"ServiceNamespace">>, ServiceNamespace},
                 {<<"ResourceARN">>, ResourceARN}],
    register_scalable_target(Configuration, BodyProps).

-spec register_scalable_target(Configuration :: aws_config(),
                               ResourceId :: binary(),
                               ScalableDimension :: binary(),
                               ServiceNamespace :: binary(),
                               ResourceARN :: binary(),
                               MinCapacity :: integer() | undefined,
                               MaxCapacity :: integer() | undefined) -> response().
register_scalable_target(Configuration, ResourceId, ScalableDimension, ServiceNamespace, ResourceARN, MinCapacity, MaxCapacity) ->
    BodyProps = [{<<"ResourceId">>, ResourceId},
                 {<<"ScalableDimension">>, ScalableDimension},
                 {<<"ServiceNamespace">>, ServiceNamespace},
                 {<<"ResourceARN">>, ResourceARN}],
    MaybeBodyWithMax = case MaxCapacity of
        undefined -> [];
        Max -> [{<<"MaxCapacity">>, Max}]
    end,
    MaybeBodyWithMin = case MinCapacity of
        undefined -> [];
        Min -> [{<<"MinCapacity">>, Min}]
    end,
    register_scalable_target(Configuration, BodyProps ++ MaybeBodyWithMax ++ MaybeBodyWithMin).

-spec register_scalable_target(
    Configuration :: erlcloud_aws:aws_config(),
    BodyConfiguration :: aws_aas_request_body()
) -> response().
register_scalable_target(Configuration, BodyConfiguration) ->
    request_with_action(Configuration, BodyConfiguration, "AnyScaleFrontendService.RegisterScalableTarget").


%%%------------------------------------------------------------------------------
%%% Internal functions.
%%%------------------------------------------------------------------------------

aas_result_fun(#aws_request{response_type = ok} = Request) ->
    Request;
aas_result_fun(#aws_request{response_type = error,
                           error_type = aws,
                           response_status = Status} = Request) when
%% Retry conflicting operations 409,Conflict and 500s
%% including 503, SlowDown, Reduce your request rate.
      Status =:= 409; Status >= 500 ->
    Request#aws_request{should_retry = true};
aas_result_fun(#aws_request{response_type = error, error_type = aws} = Request) ->
    Request#aws_request{should_retry = false};
aas_result_fun(Request) ->
    Request#aws_request{should_retry = false}.


request_with_action(Configuration, BodyConfiguration, Action) ->
    Body = jsx:encode(BodyConfiguration),
    case erlcloud_aws:update_config(Configuration) of
        {ok, Config} ->
            HeadersPrev = headers(Config, Action, Body),
            Headers = [{"content-type", "application/x-amz-json-1.1"} | HeadersPrev],
            Request = prepare_record(Config, post, Headers, Body),
            Response = erlcloud_retry:request(Config, Request, fun aas_result_fun/1),
            {ok, jsx:decode(Response#aws_request.response_body)};
        {error, Reason} ->
            {error, Reason}
    end.

prepare_record(Config, Method, Headers, Body) ->
    %%% URI: awsConfig.Scheme + awsConfig.Host + [awsConfig.Port]
    %%% URI: https://autoscaling.us-west-2.amazonaws.com/

    RequestURI = lists:flatten([
        Config#aws_config.application_autoscaling_scheme,
        Config#aws_config.application_autoscaling_host,
        port_spec(Config)
    ]),

    #aws_request{service = application_autoscaling,
                           method = Method,
                           request_headers = Headers,
                           request_body = Body,
                           uri = RequestURI}.

port_spec(#aws_config{application_autoscaling_port=80}) ->
    "";
port_spec(#aws_config{application_autoscaling_port=Port}) ->
    [":", erlang:integer_to_list(Port)].


headers(Config, Operation, Body) ->
    Headers = [{"host", Config#aws_config.application_autoscaling_host},
               {"x-amz-target", Operation}],
    erlcloud_aws:sign_v4_headers(Config, Headers, Body, erlcloud_aws:aws_region_from_host(Config#aws_config.application_autoscaling_host), "application-autoscaling").
