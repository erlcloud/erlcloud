%% @author Carl Isom III <carl.isom@gmail.com>
%% @doc
%% An Erlang interface to Marketplace Metering Service.
%%
%% [http://docs.aws.amazon.com/marketplacemetering/latest/APIReference/API_MeterUsage.html]
%%
%% [http://docs.aws.amazon.com/AWSJavaScriptSDK/latest/AWS/MarketplaceMetering.html]
%%
%% Method names match Marketplace Metering Service operations converted to
%% lower_case_with_underscores.
%%
%% Required parameters are passed as function arguments. In addition
%% all methods take an options proplist argument which can be used to
%% pass optional parameters.
%%
%% Output is in the form of `{ok, Value}' or `{error, Reason}'.
%%
%% As to the error retries, we use `erlcloud_retry:default_retry/1'
%% as the default retry strategy, this behaviour can be changed through
%% `aws_config()'.
%%
%% @end

-module(erlcloud_mms).
-author('cisom@gmail.com').

-include("erlcloud.hrl").
-include("erlcloud_aws.hrl").

%%% Library initialization.
-export([configure/2, configure/3, new/2, new/3]).

%%% Marketplace Metering API
-export([batch_meter_usage/2, batch_meter_usage/3,
         meter_usage/5, meter_usage/6,
         resolve_customer/1, resolve_customer/2
        ]).

-export_type(
   [
   ]).

%%%------------------------------------------------------------------------------
%%% Library initialization.
%%%------------------------------------------------------------------------------

-spec new(string(), string()) -> aws_config().
new(AccessKeyID, SecretAccessKey) ->
    #aws_config{access_key_id=AccessKeyID,
                secret_access_key=SecretAccessKey,
                retry = fun erlcloud_retry:default_retry/1}.

-spec new(string(), string(), string()) -> aws_config().
new(AccessKeyID, SecretAccessKey, Host) ->
    #aws_config{access_key_id=AccessKeyID,
                secret_access_key=SecretAccessKey,
                mms_host=Host,
                retry = fun erlcloud_retry:default_retry/1}.

-spec configure(string(), string()) -> ok.
configure(AccessKeyID, SecretAccessKey) ->
    put(aws_config, new(AccessKeyID, SecretAccessKey)),
    ok.

-spec configure(string(), string(), string()) -> ok.
configure(AccessKeyID, SecretAccessKey, Host) ->
    put(aws_config, new(AccessKeyID, SecretAccessKey, Host)),
    ok.

default_config() -> erlcloud_aws:default_config().

%%%------------------------------------------------------------------------------
%%% Shared Types
%%%------------------------------------------------------------------------------

-type json_term() :: jsx:json_term().

%%%------------------------------------------------------------------------------
%%% @doc
%%% Batch Meter Usage
%%%
%%% HTTP Documentation is still missing. Please see Java Script SDK Documentation
%%% [http://docs.aws.amazon.com/AWSJavaScriptSDK/latest/AWS/MarketplaceMetering.html#batchMeterUsage-property]
%%%------------------------------------------------------------------------------

-spec batch_meter_usage(ProductCode :: binary(),
                        UsageRecords :: json_term()) -> json_return().
batch_meter_usage(ProductCode, UsageRecords) ->
    batch_meter_usage(ProductCode, UsageRecords, default_config()).

-spec batch_meter_usage(ProductCode :: binary(),
                        UsageRecords :: json_term(),
                        aws_config()) -> json_return().
batch_meter_usage(ProductCode, UsageRecords, Config) ->
    Json = [{<<"ProductCode">>, ProductCode}, {<<"UsageRecords">>, UsageRecords}],

    mms_request(Config,
            "AWSMPMeteringService.BatchMeterUsage",
            Json).
%%%------------------------------------------------------------------------------
%%% Meter Usage
%%%
%%% http://docs.aws.amazon.com/marketplacemetering/latest/APIReference/API_MeterUsage.html
%%%------------------------------------------------------------------------------

-spec meter_usage(ProductCode :: binary(),
                  Timestamp :: number(),
                  UsageDimension  :: binary(),
                  UsageQuantity :: number(),
                  DryRun  :: boolean()) -> json_return().
meter_usage(ProductCode, Timestamp, UsageDimension, UsageQuantity, DryRun) ->
    meter_usage(ProductCode, Timestamp, UsageDimension, UsageQuantity, DryRun, default_config()).

-spec meter_usage(ProductCode :: binary(),
                  Timestamp :: number(),
                  UsageDimension  :: binary(),
                  UsageQuantity :: number(),
                  DryRun  :: boolean(),
                  aws_config()) -> json_return().
meter_usage(ProductCode, Timestamp, UsageDimension, UsageQuantity, DryRun, Config) ->
    Json = [{<<"DryRun">>, DryRun},
            {<<"ProductCode">>, ProductCode},
            {<<"Timestamp">>, Timestamp},
            {<<"UsageDimension">>, UsageDimension},
            {<<"UsageQuantity">>, UsageQuantity}],

    mms_request(Config,
            "AWSMPMeteringService.MeterUsage",
            Json).

%%%------------------------------------------------------------------------------
%%% Resolve Customer
%%%
%%% HTTP Documentation is still missing. Please see Java Script SDK Documentation
%%% [http://docs.aws.amazon.com/AWSJavaScriptSDK/latest/AWS/MarketplaceMetering.html#resolveCustomer-property]
%%%------------------------------------------------------------------------------

-spec resolve_customer(RegistrationToken :: binary()) -> json_return().
resolve_customer(RegistrationToken) ->
    resolve_customer(RegistrationToken, default_config()).

-spec resolve_customer(RegistrationToken :: binary(),
                       aws_config()) -> json_return().
resolve_customer(RegistrationToken, Config) ->
    Json = [{<<"RegistrationToken">>, RegistrationToken}],

    mms_request(Config,
            "AWSMPMeteringService.ResolveCustomer",
            Json).

%%%------------------------------------------------------------------------------
%%% Request
%%%------------------------------------------------------------------------------

-type operation() :: string().
-type json_return() :: {ok, json_term()} | {error, term()}.

-spec mms_request(aws_config(), operation(), json_term()) -> json_return().
mms_request(Config, Operation, Json) ->
    case erlcloud_aws:update_config(Config) of
        {ok, Config1} ->
            mms_request_no_update(Config1, Operation, Json);
        {error, Reason} ->
            {error, Reason}
    end.

-spec mms_request_no_update(aws_config(), operation(), json_term()) -> json_return().
mms_request_no_update(#aws_config{mms_scheme = Scheme, mms_host = Host, mms_port = Port} = Config, Operation, Json) ->
    Body = jsx:encode(Json),
    Headers = headers(Config, Operation, Body),
    case erlcloud_aws:aws_request_form_raw(post, Scheme, Host, Port, "/", Body, Headers, Config) of
        {ok, Response} ->
            {ok, jsx:decode(Response)};
        {error, Reason} ->
            {error, Reason}
    end.

-spec headers(aws_config(), string(), binary()) -> [{string(), string()}].
headers(Config, Operation, Body) ->
    Headers = [{"host", Config#aws_config.mms_host},
               {"x-amz-target", Operation},
               {"content-type", "application/x-amz-json-1.1"}],
    Region = erlcloud_aws:aws_region_from_host(Config#aws_config.mms_host),
    erlcloud_aws:sign_v4_headers(Config, Headers, Body, Region, "aws-marketplace").
