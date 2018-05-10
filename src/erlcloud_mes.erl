%% @author Carl Isom III <carl.isom@gmail.com>
%% @doc
%% An Erlang interface to Marketplace Entitlement Service.
%%
%% TODO: Verify URLs
%% [http://docs.aws.amazon.com/marketplaceentitlement/latest/APIReference/API_GetEntitement.html]
%%
%% [http://docs.aws.amazon.com/AWSJavaScriptSDK/latest/AWS/MarketplaceEntitlement.html]
%%
%% Method names match Marketplace Entitlement Service operations converted to
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

-module(erlcloud_mes).
-author('cisom@gmail.com').

-include("erlcloud.hrl").
-include("erlcloud_aws.hrl").

%%% Library initialization.
-export([configure/2, configure/3, new/2, new/3]).

%%% Marketplace Entitlement API
-export([get_entitlement/1, get_entitlement/2, get_entitlement/3, get_entitlement/4
        ]).

-export_type(
    [filter_value/0,
     filter_value_list/0,
     entitlment_filter/0,
     entitlment_filter_list/0
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
                mes_host=Host,
                retry = fun erlcloud_retry:default_retry/1}.

-spec configure(string(), string()) -> ok.
configure(AccessKeyID, SecretAccessKey) ->
    erlcloud_config:configure(AccessKeyID, SecretAccessKey, fun new/2).

-spec configure(string(), string(), string()) -> ok.
configure(AccessKeyID, SecretAccessKey, Host) ->
    erlcloud_config:configure(AccessKeyID, SecretAccessKey, Host, fun new/3).

default_config() -> erlcloud_aws:default_config().

%%%------------------------------------------------------------------------------
%%% Shared Types
%%%------------------------------------------------------------------------------

-type filter_value() :: binary().
-type filter_value_list() :: [filter_value()].

-type filter_type() :: customer_identifier | dimension.

-type entitlment_filter() :: {filter_type(), filter_value_list()}.
-type entitlment_filter_list() :: [entitlment_filter()].

-type json_term() :: jsx:json_term().

-type option_type() :: max_results | next_token.
-type options() :: [{option_type(), any()}].

%%%------------------------------------------------------------------------------
%%% Helper Functions
%%%------------------------------------------------------------------------------

-spec dynamize_filter_type(filter_type()) -> binary().
dynamize_filter_type(customer_identifier) ->
    <<"CUSTOMER_IDENTIFIER">>;
dynamize_filter_type(dimension) ->
    <<"DIMENSION">>.

-spec dynamize_filters(entitlment_filter_list()) -> [{binary(), filter_value_list()}].
dynamize_filters([]) ->
    [];
dynamize_filters(EntitlmentFilters) ->
    [{<<"Filter">>, [{dynamize_filter_type(FT), FVL} || {FT, FVL} <- EntitlmentFilters]}].


-spec dynamize_option_type(option_type()) -> binary().
dynamize_option_type(max_results) ->
    <<"MaxResults">>;
dynamize_option_type(next_token) ->
    <<"NextToken">>.

-spec dynamize_options(options()) -> [{binary(), any()}].
dynamize_options(Options) ->
    [{dynamize_option_type(Option), V} || {Option, V} <- Options].

%%%------------------------------------------------------------------------------
%%% @doc
%%% Get Entitlements
%%%
%%% [http://docs.aws.amazon.com/marketplaceentitlement/latest/APIReference/API_GetEntitlements.html]
%%%------------------------------------------------------------------------------

-spec get_entitlement(ProductCode :: binary()) -> json_return().
get_entitlement(ProductCode) ->
    get_entitlement(ProductCode, []).

-spec get_entitlement(ProductCode :: binary(),
                      EntitlementFilters :: entitlment_filter_list()) -> json_return().
get_entitlement(ProductCode, EntitlementFilters) ->
    get_entitlement(ProductCode, EntitlementFilters, []).

-spec get_entitlement(ProductCode :: binary(),
                      EntitlementFilters :: entitlment_filter_list(),
                      Options :: options()) -> json_return().
get_entitlement(ProductCode, EntitlementFilters, Options) ->
    get_entitlement(ProductCode, EntitlementFilters, Options, default_config()).

-spec get_entitlement(ProductCode :: binary(),
                      EntitlementFilters :: entitlment_filter_list(),
                      Options :: options(),
                      aws_config()) -> json_return().
get_entitlement(ProductCode, EntitlementFilters, Options, Config) ->
    DynamizedFilters = dynamize_filters(EntitlementFilters),
    DynamizedOptions = dynamize_options(Options),
    Json = [{<<"ProductCode">>, ProductCode} | DynamizedFilters ++ DynamizedOptions],

    mes_request(Config,
                "AWSMPEntitlementService.GetEntitlements",
                Json).


%%%------------------------------------------------------------------------------
%%% Request
%%%------------------------------------------------------------------------------

-type operation() :: string().
-type json_return() :: {ok, json_term()} | {error, term()}.

-spec mes_request(aws_config(), operation(), json_term()) -> json_return().
mes_request(Config, Operation, Json) ->
    case erlcloud_aws:update_config(Config) of
        {ok, Config1} ->
            mes_request_no_update(Config1, Operation, Json);
        {error, Reason} ->
            {error, Reason}
    end.

-spec mes_request_no_update(aws_config(), operation(), json_term()) -> json_return().
mes_request_no_update(#aws_config{mes_scheme = Scheme, mes_host = Host, mes_port = Port} = Config, Operation, Json) ->
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
    Headers = [{"host", Config#aws_config.mes_host},
               {"x-amz-target", Operation},
               {"content-type", "application/x-amz-json-1.1"}],
    Region = erlcloud_aws:aws_region_from_host(Config#aws_config.mes_host),
    erlcloud_aws:sign_v4_headers(Config, Headers, Body, Region, "aws-marketplace").
