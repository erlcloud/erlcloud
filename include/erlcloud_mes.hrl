-type filter_value() :: binary().
-type filter_value_list() :: [filter_value()].

-type filter_type() :: customer_identifier | dimension.

-type entitlement_filter() :: {filter_type(), filter_value_list()}.
-type entitlement_filter_list() :: [entitlement_filter()].

-type json_term() :: jsx:json_term().

% AWS Marketplace Entitlement Service - API version 2017/01/11
-record(entitlement_request, {
         filter :: entitlement_filter_list(),
         product_code :: binary(),
         max_results :: integer() | undefined,
         next_token :: binary() | undefined
        }).

-type entitlement_request() :: #entitlement_request{}.
