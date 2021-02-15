%% @doc
%% An Erlang interface to AWS Systems Manager (SSM).
%%
%% Output is in the form of `{ok, Value}' or `{error, Reason}'. The
%% format of `Value' is controlled by the `out' option, which defaults
%% to `json'. The possible values are:
%%
%% * `json' - The output from Systems Manager as processed by `jsx:decode'
%% with no further manipulation.
%%
%% * `record' - A record containing all the information from the
%% Systems Manager.
%%
%% * `map' - Same output of `json` opt but in a map formatting.
%%
%% Systems Manager errors are returned in the form `{error, {ErrorCode, Message}}' 
%% where `ErrorCode' and 'Message' are both binary
%% strings. 

%% See the unit tests for additional usage examples beyond what are
%% provided for each function.
%%
%% @end

-module(erlcloud_ssm).

-include("erlcloud_aws.hrl").
-include("erlcloud_ssm.hrl").

%%% Library initialization.
-export([configure/2, configure/3, configure/4, new/2, new/3, new/4]).

-define(API_VERSION, "20150408").
-define(OUTPUT_CHOICES, [json, record, map]).

-export([
    get_parameter/1, get_parameter/2,
    get_parameters/1, get_parameters/2,
    get_parameters_by_path/1, get_parameters_by_path/2,
    put_parameter/1, put_parameter/2,
    delete_parameter/1, delete_parameter/2
]).

-export_type([
    get_parameter_opt/0, get_parameter_opts/0,
    get_parameters_by_path_opt/0, get_parameters_by_path_opts/0,
    get_parameters_opt/0, get_parameters_opts/0,
    put_parameter_opt/0, put_parameter_opts/0,
    delete_parameter_opt/0, delete_parameter_opts/0
 ]).

%%%------------------------------------------------------------------------------
%% Library initialization.
%%%------------------------------------------------------------------------------

-spec new(string(), string()) -> aws_config().

new(AccessKeyID, SecretAccessKey) ->
    #aws_config{
       access_key_id=AccessKeyID,
       secret_access_key=SecretAccessKey
      }.

-spec new(string(), string(), string()) -> aws_config().

new(AccessKeyID, SecretAccessKey, Host) ->
    #aws_config{
       access_key_id=AccessKeyID,
       secret_access_key=SecretAccessKey,
       ssm_host=Host
      }.

-spec new(string(), string(), string(), non_neg_integer()) -> aws_config().

new(AccessKeyID, SecretAccessKey, Host, Port) ->
    #aws_config{
       access_key_id=AccessKeyID,
       secret_access_key=SecretAccessKey,
       ssm_host=Host,
       ssm_port=Port
      }.

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

default_config() ->
    erlcloud_aws:default_config().


%%%------------------------------------------------------------------------------
%% Shared types
%%%------------------------------------------------------------------------------

-type string_param() :: binary() | string().

-type json_pair() :: {binary() | atom(), jsx:json_term()}.
-type json_return() :: {ok, jsx:json_term()} | {error, term()}.
-type ssm_return(Record) :: {ok, jsx:json_term() | Record } | {error, term()}.
-type decode_fun() :: fun((jsx:json_term(), decode_opts()) -> tuple()).

%%%------------------------------------------------------------------------------
%% Shared Options
%%%------------------------------------------------------------------------------
-type out_type() :: json | record | map.
-type out_opt() :: {out, out_type()}.
-type property() :: proplists:property().

-type aws_opts() :: [json_pair()].
-type ssm_opts() :: [out_opt()].
-type opts() :: {aws_opts(), ssm_opts()}.

-spec verify_ssm_opt(atom(), term()) -> ok.
verify_ssm_opt(out, Value) ->
    case lists:member(Value, ?OUTPUT_CHOICES) of
        true ->
            ok;
        false ->
            error({erlcloud_ssm, {invalid_opt, {out, Value}}})
    end;
verify_ssm_opt(Name, Value) ->
    error({erlcloud_ssm, {invalid_opt, {Name, Value}}}).

-type opt_table_entry() :: {atom(), binary(), fun((_) -> jsx:json_term())}.
-type opt_table() :: [opt_table_entry()].
-spec opt_folder(opt_table(), property(), opts()) -> opts().
opt_folder(_, {_, undefined}, Opts) ->
    %% ignore options set to undefined
    Opts;
opt_folder(Table, {Name, Value}, {AwsOpts, EcsOpts}) ->
    case lists:keyfind(Name, 1, Table) of
        {Name, Key, ValueFun} ->
            {[{Key, ValueFun(Value)} | AwsOpts], EcsOpts};
        false ->
            verify_ssm_opt(Name, Value),
            {AwsOpts, [{Name, Value} | EcsOpts]}
    end.

-spec opts(opt_table(), proplist()) -> opts().
opts(Table, Opts) when is_list(Opts) ->
    %% remove duplicate options
    Opts1 = lists:ukeysort(1, proplists:unfold(Opts)),
    lists:foldl(fun(Opt, A) -> opt_folder(Table, Opt, A) end, {[], []}, Opts1);
opts(_, _) ->
    error({erlcloud_ssm, opts_not_list}).

%%%------------------------------------------------------------------------------
%% Shared Decoders
%%%------------------------------------------------------------------------------

-type decode_opt() :: {typed, boolean()}.
-type decode_opts() :: [decode_opt()].
-type record_desc() :: {tuple(), field_table()}.

-spec id(X) -> X.
id(X) -> X.

-spec id(X, decode_opts()) -> X.
id(X, _) -> X.

-type field_table() :: [{binary(), pos_integer(), 
                         fun((jsx:json_term(), decode_opts()) -> term())}].

-spec decode_folder(field_table(), json_pair(), decode_opts(), tuple()) -> tuple().
decode_folder(Table, {Key, Value}, Opts, A) ->
    case lists:keyfind(Key, 1, Table) of
        {Key, Index, ValueFun} ->
            setelement(Index, A, ValueFun(Value, Opts));
        false ->
            A
    end.


-spec decode_record(record_desc(), jsx:json_term(), decode_opts()) -> tuple().
decode_record({Record, _}, [{}], _) ->
    %% jsx returns [{}] for empty objects
    Record;
decode_record({Record, Table}, Json, Opts) ->
    lists:foldl(fun(Pair, A) -> decode_folder(Table, Pair, Opts, A) end, Record, Json).

%%%------------------------------------------------------------------------------
%% Output
%%%------------------------------------------------------------------------------
-spec out(json_return(), decode_fun(), ssm_opts())
         -> {ok, jsx:json_term() | tuple()} |
            {simple, term()} |
            {error, term()}.
out({error, Reason}, _, _) ->
    {error, Reason};
out({ok, Json}, Decode, Opts) ->
    case proplists:get_value(out, Opts, json) of
        json ->
            {ok, Json};
        record ->
            {ok, Decode(Json, [])};
        map ->
            {ok, erlcloud_util:proplists_to_map(Json)}
    end.

%%%------------------------------------------------------------------------------
%% Shared Records
%%%------------------------------------------------------------------------------

-spec parameter_record() -> record_desc().
parameter_record() ->
    {#ssm_parameter{},
        [
            {<<"ARN">>, #ssm_parameter.arn, fun id/2},
            {<<"DataType">>, #ssm_parameter.data_type, fun id/2},
            {<<"LastModifiedDate">>, #ssm_parameter.last_modified_date, fun id/2},
            {<<"Name">>, #ssm_parameter.name, fun id/2},
            {<<"Selector">>, #ssm_parameter.selector, fun id/2},
            {<<"SourceResult">>, #ssm_parameter.source_result, fun id/2},
            {<<"Type">>, #ssm_parameter.type, fun id/2},
            {<<"Value">>, #ssm_parameter.value, fun id/2},
            {<<"Version">>, #ssm_parameter.version, fun id/2}
        ]
    }.

-spec get_parameter_record() -> record_desc().
get_parameter_record() ->
    {#ssm_get_parameter{},
        [
            {<<"Parameter">>, #ssm_get_parameter.parameter, fun decode_parameter/2}
        ]
    }.

-spec get_parameters_record() -> record_desc().
get_parameters_record() ->
    {#ssm_get_parameters{},
        [
            {<<"InvalidParameters">>, #ssm_get_parameters.invalid_parameters, fun id/2},
            {<<"Parameters">>, #ssm_get_parameters.parameters, fun decode_parameters/2}
        ]
    }.

-spec get_parameters_by_path_record() -> record_desc().
get_parameters_by_path_record() ->
    {#ssm_get_parameters_by_path{},
        [
            {<<"NextToken">>, #ssm_get_parameters_by_path.next_token, fun id/2},
            {<<"Parameters">>, #ssm_get_parameters_by_path.parameters, fun decode_parameters/2}
        ]
    }.

-spec put_parameter_record() -> record_desc().
put_parameter_record() ->
    {#ssm_put_parameter{},
        [
            {<<"Tier">>, #ssm_put_parameter.tier, fun id/2},
            {<<"Version">>, #ssm_put_parameter.version, fun id/2}
        ]
    }.

decode_parameter(V, Opts) ->
    decode_record(parameter_record(), V, Opts).

decode_parameters(V, Opts) ->
    [decode_record(parameter_record(), I, Opts) || I <- V].

decode_get_parameter(V, Opts) ->
    decode_record(get_parameter_record(), V, Opts).

decode_get_parameters(V, Opts) ->
    decode_record(get_parameters_record(), V, Opts).

decode_get_parameters_by_path(V, Opts) ->
    decode_record(get_parameters_by_path_record(), V, Opts).

decode_put_parameter(V, Opts) ->
    decode_record(put_parameter_record(), V, Opts).

%%%------------------------------------------------------------------------------
%% AWS Systems Manager API Functions
%%%------------------------------------------------------------------------------

%%%------------------------------------------------------------------------------
%% GetParameter
%%%------------------------------------------------------------------------------
-type get_parameter_opt() :: {name, string_param()} | {with_decryption, boolean()} |
                             out_opt().
-type get_parameter_opts() :: [get_parameter_opt()].

-spec get_parameter_opts() -> opt_table().
get_parameter_opts() ->
    [
        {name, <<"Name">>, fun encode_json_value/1},
        {with_decryption, <<"WithDecryption">>, fun encode_json_value/1}
    ].

-spec get_parameter(Opts :: get_parameter_opts()) -> ssm_return(#ssm_get_parameter{}).
get_parameter(Opts) ->
    get_parameter(Opts, default_config()).

%%%------------------------------------------------------------------------------
%% @doc 
%% SSM API
%% [https://docs.aws.amazon.com/systems-manager/latest/APIReference/API_GetParameter.html]
%%
%% ===Example===
%%
%% Get information about a parameter by using the parameter name.
%%
%% `
%% {ok, Parameter} = erlcloud_ssm:get_parameter([{name, "some_parameter"}, {out, json}])
%% '
%% @end
%%%------------------------------------------------------------------------------
-spec get_parameter(Opts :: get_parameter_opts(), Config :: aws_config()) -> ssm_return(#ssm_get_parameter{}).
get_parameter(Opts, #aws_config{} = Config) ->
    {AwsOpts, SSMOpts} = opts(get_parameter_opts(), Opts),
    Return = ssm_request(
                Config,
                "GetParameter",
                AwsOpts),
    out(Return, fun(Json, UOpts) ->
            decode_get_parameter(Json, UOpts)
        end, 
        SSMOpts).

%%%------------------------------------------------------------------------------
%% GetParameters
%%%------------------------------------------------------------------------------
-type get_parameters_opt() :: {names, [string_param()]} | {with_decryption, boolean()} |
                             out_opt().
-type get_parameters_opts() :: [get_parameters_opt()].

-spec get_parameters_opts() -> opt_table().
get_parameters_opts() ->
    [
        {names, <<"Names">>, fun encode_json_value/1},
        {with_decryption, <<"WithDecryption">>, fun encode_json_value/1}
    ].

-spec get_parameters(Opts :: get_parameters_opts()) -> ssm_return(#ssm_get_parameters{}).
get_parameters(Opts) ->
    get_parameters(Opts, default_config()).

%%%------------------------------------------------------------------------------
%% @doc 
%% SSM API
%% [https://docs.aws.amazon.com/systems-manager/latest/APIReference/API_GetParameters.html]
%%
%% ===Example===
%%
%% Get information about parameters by using the parameters' names.
%%
%% `
%% {ok, Parameter} = erlcloud_ssm:get_parameters([{names, ["some_parameter_1", "some_parameter_2"]}])
%% '
%% @end
%%%------------------------------------------------------------------------------
-spec get_parameters(Opts :: get_parameters_opts(), Config :: aws_config()) -> ssm_return(#ssm_get_parameters{}).
get_parameters(Opts, #aws_config{} = Config) ->
    {AwsOpts, SSMOpts} = opts(get_parameters_opts(), Opts),
    Return = ssm_request(
                Config,
                "GetParameters",
                AwsOpts),
    out(Return, fun(Json, UOpts) ->
            decode_get_parameters(Json, UOpts)
        end, 
        SSMOpts).

%%%------------------------------------------------------------------------------
%% GetParametersByPath
%%%------------------------------------------------------------------------------
-type get_parameters_by_path_filter_opt() :: {key, string_param()} | {option, string_param()} |
                                             {values, [string_param()]}.
-type get_parameters_by_path_filter_opts() :: [get_parameters_by_path_filter_opt()].                                  
-type get_parameters_by_path_opt() :: {max_results, non_neg_integer()} | {next_token, string_param()} |
                                      {parameter_filters, get_parameters_by_path_filter_opts()} | {path, string_param()} |
                                      {recursive, boolean()} | {with_decryption, boolean()} |
                                      out_opt().
-type get_parameters_by_path_opts() :: [get_parameters_by_path_opt()].

-spec get_parameters_by_path_opts() -> opt_table().
get_parameters_by_path_opts() ->
    [
        {max_results, <<"MaxResults">>, fun id/1},
        {next_token, <<"NextToken">>, fun encode_json_value/1},
        {parameter_filters, <<"ParameterFilters">>, fun encode_json_parameter_filters_value/1},
        {path, <<"Path">>, fun encode_json_value/1},
        {recursive, <<"Recursive">>, fun encode_json_value/1},
        {with_decryption, <<"WithDecryption">>, fun encode_json_value/1}
    ].

-spec get_parameters_by_path(Opts :: get_parameters_by_path_opts()) -> ssm_return(#ssm_get_parameters_by_path{}).
get_parameters_by_path(Opts) ->
    get_parameters_by_path(Opts, default_config()).

%%%------------------------------------------------------------------------------
%% @doc 
%% SSM API
%% [https://docs.aws.amazon.com/systems-manager/latest/APIReference/API_GetParametersByPath.html]
%%
%% ===Example===
%%
%% Retrieve information about one or more parameters in a specific hierarchy.
%%
%% `
%% {ok, Parameters} = erlcloud_ssm:get_parameters_by_path([{path, "/desired/path"}])
%% '
%% @end
%%%------------------------------------------------------------------------------
-spec get_parameters_by_path(Opts :: get_parameters_by_path_opts(), Config :: aws_config()) -> ssm_return(#ssm_get_parameters_by_path{}).
get_parameters_by_path(Opts, #aws_config{} = Config) ->
    {AwsOpts, SSMOpts} = opts(get_parameters_by_path_opts(), Opts),
    Return = ssm_request(
                Config,
                "GetParametersByPath",
                AwsOpts),
    out(Return, fun(Json, UOpts) ->
            decode_get_parameters_by_path(Json, UOpts)
        end, 
        SSMOpts).

%%%------------------------------------------------------------------------------
%% PutParameter
%%%------------------------------------------------------------------------------
-type tag_parameter_opt() :: {key, string_param()} | {value, string_param()}.
-type tag_parameter_opts() :: [tag_parameter_opt()].

-type put_parameter_opt() :: {allowed_pattern, string_param()} | {data_type, string_param()} |
                             {description, string_param()} | {key_id, string_param()} |
                             {name, string_param()} | {overwrite, boolean()} |
                             {policies, string_param()} | {tags, [tag_parameter_opts()]} |
                             {tier, string_param()} | {type, string_param()} |
                             {value, string_param()} | out_opt().
-type put_parameter_opts() :: [put_parameter_opt()].

-spec put_parameter_opts() -> opt_table().
put_parameter_opts() ->
    [
        {allowed_pattern, <<"AllowedPattern">>, fun encode_json_value/1},
        {data_type, <<"DataType">>, fun encode_json_value/1},
        {description, <<"Description">>, fun encode_json_value/1},
        {key_id, <<"KeyId">>, fun encode_json_value/1},
        {name, <<"Name">>, fun encode_json_value/1},
        {overwrite, <<"Overwrite">>, fun encode_json_value/1},
        {policies, <<"Policies">>, fun encode_json_value/1},
        {tags, <<"Tags">>, fun encode_json_tags_value/1},
        {tier, <<"Tier">>, fun encode_json_value/1},
        {type, <<"Type">>, fun encode_json_value/1},
        {value, <<"Value">>, fun encode_json_value/1}
    ].

-spec put_parameter(Opts :: put_parameter_opts()) -> ssm_return(#ssm_put_parameter{}).
put_parameter(Opts) ->
    put_parameter(Opts, default_config()).

%%%------------------------------------------------------------------------------
%% @doc 
%% SSM API
%% [https://docs.aws.amazon.com/systems-manager/latest/APIReference/API_PutParameter.html]
%%
%% ===Example===
%%
%% Add a parameter to the system.
%%
%% `
%% {ok, Parameter} = erlcloud_ssm:put_parameter([{name, <<"password">>}, {value, <<"myP@ssw0rd">>}, {type, <<"String">>}])
%% '
%% @end
%%%------------------------------------------------------------------------------
-spec put_parameter(Opts :: put_parameter_opts(), Config :: aws_config()) -> ssm_return(#ssm_put_parameter{}).
put_parameter(Opts, #aws_config{} = Config) ->
    {AwsOpts, SSMOpts} = opts(put_parameter_opts(), Opts),
    Return = ssm_request(
                Config,
                "PutParameter",
                AwsOpts),
    out(Return, fun(Json, UOpts) ->
            decode_put_parameter(Json, UOpts)
        end, 
        SSMOpts).

%%%------------------------------------------------------------------------------
%% DeleteParameter
%%%------------------------------------------------------------------------------
-type delete_parameter_opt() :: {name, string_param()} | out_opt().
-type delete_parameter_opts() :: [delete_parameter_opt()].

-spec delete_parameter_opts() -> opt_table().
delete_parameter_opts() ->
    [
        {name, <<"Name">>, fun encode_json_value/1}
    ].

-spec delete_parameter(Opts :: delete_parameter_opts()) -> ok | {error, term()}.
delete_parameter(Opts) ->
    delete_parameter(Opts, default_config()).

%%%------------------------------------------------------------------------------
%% @doc 
%% SSM API
%% [https://docs.aws.amazon.com/systems-manager/latest/APIReference/API_DeleteParameter.html]
%%
%% ===Example===
%%
%% Delete a parameter from the system.
%%
%% `
%% ok = erlcloud_ssm:delete_parameter([{name, <<"password">>}])
%% '
%% @end
%%%------------------------------------------------------------------------------
-spec delete_parameter(Opts :: delete_parameter_opts(), Config :: aws_config()) -> ok | {error, term()}.
delete_parameter(Opts, #aws_config{} = Config) ->
    {AwsOpts, _} = opts(delete_parameter_opts(), Opts),
    case ssm_request(Config, "DeleteParameter", AwsOpts) of
        {ok, _} -> ok;
        {error, _} = Error -> Error
    end.

%%%------------------------------------------------------------------------------
%% Internal Functions
%%%------------------------------------------------------------------------------
ssm_request(Config, Operation, Body) ->
    case erlcloud_aws:update_config(Config) of
        {ok, Config1} ->
            ssm_request_impl(Config1, Operation, Body);
        {error, Reason} ->
            {error, Reason}
    end.

ssm_request_impl(Config, Operation, Body) ->
    Payload = case Body of
               [] -> <<"{}">>;
               _ -> jsx:encode(lists:flatten(Body))
           end,
    Headers = headers(Config, Operation, Payload),
    Request = #aws_request{service = ssm,
                           uri = uri(Config),
                           method = post,
                           request_headers = Headers,
                           request_body = Payload},
    case erlcloud_aws:request_to_return(erlcloud_retry:request(Config, Request, fun ssm_result_fun/1)) of
        {ok, {_RespHeaders, <<>>}} -> {ok, []};
        {ok, {_RespHeaders, RespBody}} -> {ok, jsx:decode(RespBody, [{return_maps, false}])};
        {error, _} = Error -> Error
    end.

-spec ssm_result_fun(Request :: aws_request()) -> aws_request().
ssm_result_fun(#aws_request{response_type = ok} = Request) ->
    Request;
ssm_result_fun(#aws_request{response_type = error,
                            error_type = aws,
                            response_status = Status} = Request) when Status >= 500 ->
    Request#aws_request{should_retry = true};
ssm_result_fun(#aws_request{response_type = error, error_type = aws} = Request) ->
    Request#aws_request{should_retry = false}.

headers(Config, Operation, Body) ->
    Headers = [{"host", Config#aws_config.ssm_host},
               {"x-amz-target", lists:append(["AmazonSSM.", Operation])},
               {"content-type", "application/x-amz-json-1.1"}],
    Region = erlcloud_aws:aws_region_from_host(Config#aws_config.ssm_host),
    erlcloud_aws:sign_v4_headers(Config, Headers, Body, Region, "ssm").

uri(#aws_config{ssm_scheme = Scheme, ssm_host = Host} = Config) ->
    lists:flatten([Scheme, Host, port_spec(Config)]).

port_spec(#aws_config{ssm_port=80}) ->
    "";
port_spec(#aws_config{ssm_port=Port}) ->
    [":", erlang:integer_to_list(Port)].

encode_json_value(undefined) -> undefined;
encode_json_value(true) -> true;
encode_json_value(false) -> false;
encode_json_value(L) when is_list(L), is_list(hd(L)) -> [encode_json_value(V) || V <- L];
encode_json_value(L) when is_list(L), is_binary(hd(L)) -> [encode_json_value(V) || V <- L];
encode_json_value(L) when is_list(L) -> list_to_binary(L);
encode_json_value(B) when is_binary(B) -> B;
encode_json_value(A) when is_atom(A) -> atom_to_binary(A, latin1).

encode_json_tags_value(Tags) ->
    encode_json_tags_value(Tags, []).

encode_json_tags_value([], Acc) ->
    Acc;
encode_json_tags_value([Tag|Tags], Acc) ->
    encode_json_tags_value(Tags, [encode_json_tag_value(Tag)|Acc]).

encode_json_tag_value(Tag) ->
    encode_json_tag_value(Tag, []).

encode_json_tag_value([], Acc) ->
    Acc;
encode_json_tag_value([{key, Key}|Tags], Acc) ->
    encode_json_tag_value(Tags, [{<<"Key">>, encode_json_value(Key)}|Acc]);
encode_json_tag_value([{value, Key}|Tags], Acc) ->
    encode_json_tag_value(Tags, [{<<"Value">>, encode_json_value(Key)}|Acc]).

encode_json_parameter_filters_value(ParameterFilters) ->
    encode_json_parameter_filters_value(ParameterFilters, []).

encode_json_parameter_filters_value([], Acc) ->
    Acc;
encode_json_parameter_filters_value([{key, Key}|Filters], Acc) ->
    encode_json_parameter_filters_value(Filters, [{<<"Key">>, encode_json_value(Key)}|Acc]);
encode_json_parameter_filters_value([{option, Option}|Filters], Acc) ->
    encode_json_parameter_filters_value(Filters, [{<<"Option">>, encode_json_value(Option)}|Acc]);
encode_json_parameter_filters_value([{values, Values}|Filters], Acc) ->
    encode_json_parameter_filters_value(Filters, [{<<"Values">>, encode_json_value(Values)}|Acc]).
