-ifndef(erlcloud_ssm_hrl).
-define(erlcloud_ssm_hrl, 0).

-include("erlcloud.hrl").

%%%------------------------------------------------------------------------------
%%
%% Common data types
%%
%%%------------------------------------------------------------------------------

-record(ssm_parameter, {
    arn :: undefined | binary(),
    data_type :: undefined | binary(),
    last_modified_date :: undefined | float(),
    name :: undefined | binary(),
    selector :: undefined | binary(),
    source_result :: undefined | binary(),
    type :: undefined | binary(),
    value :: undefined | binary(),
    version :: undefined | non_neg_integer()
}).

-record(ssm_get_parameter, {
    parameter :: undefined | #ssm_parameter{}
}).

-record(ssm_get_parameters, {
    invalid_parameters :: undefined | list(binary()),
    parameters :: undefined | [#ssm_parameter{}]
}).

-record(ssm_get_parameters_by_path, {
    next_token :: undefined | binary(),
    parameters :: undefined | [#ssm_parameter{}]
}).

-record(ssm_put_parameter, {
    tier :: undefined | binary(),
    version :: undefined | non_neg_integer()
}).

-endif.
