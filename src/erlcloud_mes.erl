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
-include("erlcloud_mes.hrl").

%%% Library initialization.
-export([configure/2, configure/3, new/2, new/3]).

%%% Marketplace Entitlement API
-export([get_entitlement/1, get_entitlement/2]).

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
    put(aws_config, new(AccessKeyID, SecretAccessKey)),
    ok.

-spec configure(string(), string(), string()) -> ok.
configure(AccessKeyID, SecretAccessKey, Host) ->
    put(aws_config, new(AccessKeyID, SecretAccessKey, Host)),
    ok.

%%%------------------------------------------------------------------------------
%%% @doc
%%% Get Entitlements
%%%
%%% [http://docs.aws.amazon.com/marketplaceentitlement/latest/APIReference/API_GetEntitlements.html]
%%%------------------------------------------------------------------------------

-spec get_entitlement(Request :: entitlement_request()) -> json_return().
get_entitlement(#entitlement_request{} = Request) ->
    get_entitlement(Request, default_config()).


-spec get_entitlement(EntitlementRequest :: entitlement_request(), aws_config()) -> json_return().
get_entitlement(#entitlement_request{} = Request, #aws_config{} = Config) ->
    DynamizedFilters = dynamize_filters(Request#entitlement_request.filter),
    Params = filter_undefined([{<<"MaxResults">>, Request#entitlement_request.max_results},
                               {<<"NextToken">>, Request#entitlement_request.next_token},
                               {<<"ProductCode">>, Request#entitlement_request.product_code}
                              ]) ++ DynamizedFilters,
    mes_request(Config,
                "AWSMPEntitlementService.GetEntitlements",
                Params).

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


%%%------------------------------------------------------------------------------
%%% Helper Functions
%%%------------------------------------------------------------------------------

default_config() -> erlcloud_aws:default_config().

-spec dynamize_filter_type(filter_type()) -> binary().
dynamize_filter_type(customer_identifier) ->
    <<"CUSTOMER_IDENTIFIER">>;
dynamize_filter_type(dimension) ->
    <<"DIMENSION">>.

-spec dynamize_filters(entitlement_filter_list()) -> [{binary(), filter_value_list()}].
dynamize_filters([]) ->
    [];
dynamize_filters(entitlementFilters) ->
    [{<<"Filter">>, [{dynamize_filter_type(FT), FVL} || {FT, FVL} <- entitlementFilters]}].


filter_undefined(Params) ->
    filter_undefined(Params, []).


filter_undefined([], Acc) ->
    Acc;
filter_undefined([{_, undefined} | T], Acc) ->
    filter_undefined(T, Acc);
filter_undefined([H | T], Acc) ->
    filter_undefined(T, [H | Acc]).
