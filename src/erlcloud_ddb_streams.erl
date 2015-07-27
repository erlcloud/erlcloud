%% -*- mode: erlang;erlang-indent-level: 4;indent-tabs-mode: nil -*-

%% @author Smiler Lee <smilerlee@live.com>
%% @doc
%% An Erlang interface to Amazon's DynamoDB Streams.
%%
%% [http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Streams.html]
%%
%% Method names match DynamoDB Streams operations converted to
%% lower_case_with_underscores.
%%
%% Required parameters are passed as function arguments. In addition
%% all methods take an options proplist argument which can be used to
%% pass optional parameters. See function documentation for examples.
%%
%% @end

-module(erlcloud_ddb_streams).

-include("erlcloud.hrl").
-include("erlcloud_aws.hrl").
-include("erlcloud_ddb_streams.hrl").

%%% Library initialization.
-export([configure/2, configure/3, new/2, new/3]).

%%% DynamoDB Streams API
-export([describe_stream/1, describe_stream/2, describe_stream/3,
         get_records/1, get_records/2, get_records/3,
         get_shard_iterator/3, get_shard_iterator/4, get_shard_iterator/5,
         list_streams/0, list_streams/1, list_streams/2
        ]).

-export_type(
   [sequence_number/0,
    shard_id/0,
    shard_iterator/0,
    shard_iterator_type/0,
    stream_arn/0,
    stream_label/0,
    table_name/0
   ]).

%%%------------------------------------------------------------------------------
%%% Library initialization.
%%%------------------------------------------------------------------------------

-spec(new/2 :: (string(), string()) -> aws_config()).
new(AccessKeyID, SecretAccessKey) ->
    #aws_config{access_key_id=AccessKeyID,
                secret_access_key=SecretAccessKey}.

-spec(new/3 :: (string(), string(), string()) -> aws_config()).
new(AccessKeyID, SecretAccessKey, Host) ->
    #aws_config{access_key_id=AccessKeyID,
                secret_access_key=SecretAccessKey,
                ddb_streams_host=Host}.

-spec(configure/2 :: (string(), string()) -> ok).
configure(AccessKeyID, SecretAccessKey) ->
    put(aws_config, new(AccessKeyID, SecretAccessKey)),
    ok.

-spec(configure/3 :: (string(), string(), string()) -> ok).
configure(AccessKeyID, SecretAccessKey, Host) ->
    put(aws_config, new(AccessKeyID, SecretAccessKey, Host)),
    ok.

default_config() -> erlcloud_aws:default_config().

%%%------------------------------------------------------------------------------
%%% Shared Types
%%%------------------------------------------------------------------------------

-type table_name() :: binary().

-type stream_arn() :: binary().
-type stream_label() :: binary().

-type shard_id() :: binary().
-type shard_iterator() :: binary().
-type shard_iterator_type() :: trim_horizon | latest | at_sequence_number | after_sequence_number.
-type sequence_number() :: binary().

-type json_pair() :: {binary(), json_term()}.
-type json_term() :: jsx:json_term().

-type ok_return(T) :: {ok, T} | {error, term()}.

%%%------------------------------------------------------------------------------
%%% Shared Dynamizers
%%%------------------------------------------------------------------------------

-spec dynamize_shard_iterator_type(shard_iterator_type()) -> binary().
dynamize_shard_iterator_type(trim_horizon) -> <<"TRIM_HORIZON">>;
dynamize_shard_iterator_type(latest) -> <<"LATEST">>;
dynamize_shard_iterator_type(at_sequence_number) -> <<"AT_SEQUENCE_NUMBER">>;
dynamize_shard_iterator_type(after_sequence_number) -> <<"AFTER_SEQUENCE_NUMBER">>.

%%%------------------------------------------------------------------------------
%%% Shared Undynamizers
%%%------------------------------------------------------------------------------

-type undynamize_opt() :: {typed, boolean()}.
-type undynamize_opts() :: [undynamize_opt()].

-type field_table() :: [{binary(), pos_integer(),
                         fun((json_term(), undynamize_opts()) -> term())}].

-spec undynamize_folder(field_table(), json_pair(), undynamize_opts(), tuple()) -> tuple().
undynamize_folder(Table, {Key, Value}, Opts, A) ->
    case lists:keyfind(Key, 1, Table) of
        {Key, Index, ValueFun} ->
            setelement(Index, A, ValueFun(Value, Opts));
        false ->
            A
    end.

-type record_desc() :: {tuple(), field_table()}.

-spec undynamize_record(record_desc(), json_term(), undynamize_opts()) -> tuple().
undynamize_record({Record, _}, [{}], _) ->
    %% jsx returns [{}] for empty objects
    Record;
undynamize_record({Record, Table}, Json, Opts) ->
    lists:foldl(fun(Pair, A) -> undynamize_folder(Table, Pair, Opts, A) end, Record, Json).

%%%------------------------------------------------------------------------------
%%% Shared Options
%%%------------------------------------------------------------------------------

-spec id(X) -> X.
id(X) -> X.

-type out_type() :: json | record | typed_record | simple.
-type out_opt() :: {out, out_type()}.
-type boolean_opt(Name) :: Name | {Name, boolean()}.
-type property() :: proplists:property().

-type aws_opts() :: [json_pair()].
-type ddb_opts() :: [out_opt()].
-type opts() :: {aws_opts(), ddb_opts()}.

-spec verify_ddb_opt(atom(), term()) -> ok.
verify_ddb_opt(out, Value) ->
    case lists:member(Value, [json, record, typed_record, simple]) of
        true ->
            ok;
        false ->
            error({erlcloud_ddb, {invalid_opt, {out, Value}}})
    end;
verify_ddb_opt(Name, Value) ->
    error({erlcloud_ddb, {invalid_opt, {Name, Value}}}).

-type opt_table_entry() :: {atom(), binary(), fun((_) -> json_term())}.
-type opt_table() :: [opt_table_entry()].
-spec opt_folder(opt_table(), property(), opts()) -> opts().
opt_folder(_, {_, undefined}, Opts) ->
    %% ignore options set to undefined
    Opts;
opt_folder(Table, {Name, Value}, {AwsOpts, DdbOpts}) ->
    case lists:keyfind(Name, 1, Table) of
        {Name, Key, ValueFun} ->
            {[{Key, ValueFun(Value)} | AwsOpts], DdbOpts};
        false ->
            verify_ddb_opt(Name, Value),
            {AwsOpts, [{Name, Value} | DdbOpts]}
    end.

-spec opts(opt_table(), proplist()) -> opts().
opts(Table, Opts) when is_list(Opts) ->
    %% remove duplicate options
    Opts1 = lists:ukeysort(1, proplists:unfold(Opts)),
    lists:foldl(fun(Opt, A) -> opt_folder(Table, Opt, A) end, {[], []}, Opts1);
opts(_, _) ->
    error({erlcloud_ddb, opts_not_list}).

%%%------------------------------------------------------------------------------
%%% Output
%%%------------------------------------------------------------------------------

-type ddb_return(Record, Simple) :: {ok, json_term() | Record | Simple} | {error, term()}.
-type undynamize_fun() :: fun((json_term(), undynamize_opts()) -> tuple()).

-spec out(erlcloud_ddb_streams_impl:json_return(), undynamize_fun(), ddb_opts()) 
         -> {ok, json_term() | tuple()} |
            {simple, term()} |
            {error, term()}.
out({error, Reason}, _, _) ->
    {error, Reason};
out({ok, Json}, Undynamize, Opts) ->
    case proplists:get_value(out, Opts, json) of %% TODO use `simple` as default
        json ->
            {ok, Json};
        record ->
            {ok, Undynamize(Json, [])};
        typed_record ->
            {ok, Undynamize(Json, [{typed, true}])};
        simple ->
            {simple, Undynamize(Json, [])}
    end.

%% Returns specified field of tuple for simple return
-spec out(erlcloud_ddb_streams_impl:json_return(), undynamize_fun(), ddb_opts(), pos_integer()) 
         -> ok_return(term()).
out(Result, Undynamize, Opts, Index) ->
    out(Result, Undynamize, Opts, Index, {error, no_return}).

-spec out(erlcloud_ddb_streams_impl:json_return(), undynamize_fun(), ddb_opts(), pos_integer(), ok_return(term())) 
         -> ok_return(term()).
out(Result, Undynamize, Opts, Index, Default) ->
    case out(Result, Undynamize, Opts) of
        {simple, Record} ->
            case element(Index, Record) of
                undefined ->
                    Default;
                Element ->
                    {ok, Element}
            end;
        Else ->
            Else
    end.

%%%------------------------------------------------------------------------------
%%% Shared Records
%%%------------------------------------------------------------------------------

%%%------------------------------------------------------------------------------
%%% DescribeStream
%%%------------------------------------------------------------------------------

-type describe_stream_opt() :: {exclusive_start_shard_id, shard_id()} |
                               {limit, 1..100} |
                               out_opt().
-type describe_stream_opts() :: [describe_stream_opt()].

-spec describe_stream_opts() -> opt_table().
describe_stream_opts() ->
    [{exclusive_start_shard_id, <<"ExclusiveStartShardId">>, fun id/1},
     {limit, <<"Limit">>, fun id/1}].

-spec describe_stream_record() -> record_desc().    
describe_stream_record() ->
    {#ddb_streams_describe_stream{},
     [
     ]}.

-type describe_stream_return() :: ddb_return(#ddb_streams_describe_stream{}, #ddb_streams_describe_stream{}).

-spec describe_stream(stream_arn()) -> describe_stream_return().
describe_stream(StreamArn) ->
    describe_stream(StreamArn, [], default_config()).

-spec describe_stream(stream_arn(), describe_stream_opts()) -> describe_stream_return().
describe_stream(StreamArn, Opts) ->
    describe_stream(StreamArn, Opts, default_config()).

%%------------------------------------------------------------------------------
%% @doc 
%% DynamoDB Streams API:
%% [http://docs.aws.amazon.com/dynamodbstreams/latest/APIReference/API_DescribeStream.html]
%%
%% @end
%%------------------------------------------------------------------------------
-spec describe_stream(stream_arn(), describe_stream_opts(), aws_config()) -> describe_stream_return().
describe_stream(StreamArn, Opts, Config) ->
    {AwsOpts, DdbOpts} = opts(describe_stream_opts(), Opts),
    Return = erlcloud_ddb_streams_impl:request(
               Config,
               "DynamoDBStreams_20120810.DescribeStream",
               [{<<"StreamArn">>, StreamArn}]
               ++ AwsOpts),
    case out(Return, 
             fun(Json, UOpts) -> undynamize_record(describe_stream_record(), Json, UOpts) end, 
             DdbOpts) of
        {ok, _} = Out -> Out;
        {error, _} = Out -> Out
    end.

%%%------------------------------------------------------------------------------
%%% GetRecords
%%%------------------------------------------------------------------------------

-type get_records_opt() :: {limit, 1..1000} |
                           out_opt().
-type get_records_opts() :: [get_records_opt()].

-spec get_records_opts() -> opt_table().
get_records_opts() ->
    [{limit, <<"Limit">>, fun id/1}].

-spec get_records_record() -> record_desc().
get_records_record() ->
    {#ddb_streams_get_records{},
     [
     ]}.

-type get_records_return() :: ddb_return(#ddb_streams_get_records{}, #ddb_streams_get_records{}).

-spec get_records(shard_iterator()) -> get_records_return().
get_records(ShardIterator) ->
    get_records(ShardIterator, [], default_config()).

-spec get_records(shard_iterator(), get_records_opts()) -> get_records_return().
get_records(ShardIterator, Opts) ->
    get_records(ShardIterator, Opts, default_config()).

%%------------------------------------------------------------------------------
%% @doc
%% DynamoDB Streams API:
%% [http://docs.aws.amazon.com/dynamodbstreams/latest/APIReference/API_GetRecords.html]
%%
%% @end
%%------------------------------------------------------------------------------
-spec get_records(shard_iterator(), get_records_opts(), aws_config()) -> get_records_return().
get_records(ShardIterator, Opts, Config) ->
    {AwsOpts, DdbOpts} = opts(get_records_opts(), Opts),
    Return = erlcloud_ddb_streams_impl:request(
               Config,
               "DynamoDBStreams_20120810.GetRecords",
               [{<<"ShardIterator">>, ShardIterator}]
               ++ AwsOpts),
    case out(Return,
             fun(Json, UOpts) -> undynamize_record(get_records_record(), Json, UOpts) end,
             DdbOpts) of
        {ok, _} = Out -> Out;
        {error, _} = Out -> Out
    end.

%%%------------------------------------------------------------------------------
%%% GetShardIterator
%%%------------------------------------------------------------------------------

-type get_shard_iterator_opt() :: {sequence_number, sequence_number()} |
                                  out_opt().
-type get_shard_iterator_opts() :: [get_shard_iterator_opt()].

-spec get_shard_iterator_opts() -> opt_table().
get_shard_iterator_opts() ->
    [{sequence_number, <<"SequenceNumber">>, fun id/1}].

-spec get_shard_iterator_record() -> record_desc().
get_shard_iterator_record() ->
    {#ddb_streams_get_shard_iterator{},
     [
     ]}.

-type get_shard_iterator_return() :: ddb_return(#ddb_streams_get_shard_iterator{}, #ddb_streams_get_shard_iterator{}).

-spec get_shard_iterator(stream_arn(), shard_id(), shard_iterator_type()) -> get_shard_iterator_return().
get_shard_iterator(StreamArn, ShardId, ShardIteratorType) ->
    get_shard_iterator(StreamArn, ShardId, ShardIteratorType, [], default_config()).

-spec get_shard_iterator(stream_arn(), shard_id(), shard_iterator_type(), get_shard_iterator_opts())
                        -> get_shard_iterator_return().
get_shard_iterator(StreamArn, ShardId, ShardIteratorType, Opts) ->
    get_shard_iterator(StreamArn, ShardId, ShardIteratorType, Opts, default_config()).

%%------------------------------------------------------------------------------
%% @doc
%% DynamoDB API:
%% [http://docs.aws.amazon.com/dynamodbstreams/latest/APIReference/API_GetShardIterator.html]
%%
%% @end
%%------------------------------------------------------------------------------
-spec get_shard_iterator(stream_arn(), shard_id(), shard_iterator_type(), get_shard_iterator_opts(),
                         aws_config())
                        -> get_shard_iterator_return().
get_shard_iterator(StreamArn, ShardId, ShardIteratorType, Opts, Config) ->
    {AwsOpts, DdbOpts} = opts(get_shard_iterator_opts(), Opts),
    Return = erlcloud_ddb_streams_impl:request(
               Config,
               "DynamoDBStreams_20120810.GetShardIterator",
               [{<<"StreamArn">>, StreamArn},
                {<<"ShardId">>, ShardId},
                {<<"ShardIteratorType">>, dynamize_shard_iterator_type(ShardIteratorType)}]
               ++ AwsOpts),
    case out(Return,
             fun(Json, UOpts) -> undynamize_record(get_shard_iterator_record(), Json, UOpts) end,
             DdbOpts) of
        {ok, _} = Out -> Out;
        {error, _} = Out -> Out
    end.

%%%------------------------------------------------------------------------------
%%% ListStreams
%%%------------------------------------------------------------------------------

-type list_streams_opt() :: {exclusive_start_stream_arn, shard_id()} |
                            {limit, 1..100} |
                            {table_name, table_name()} |
                            out_opt().
-type list_streams_opts() :: [list_streams_opt()].

-spec list_streams_opts() -> opt_table().
list_streams_opts() ->
    [{exclusive_start_stream_arn, <<"ExclusiveStartStreamArn">>, fun id/1},
     {limit, <<"Limit">>, fun id/1},
     {table_name, <<"TableName">>, fun id/1}].

-spec list_streams_record() -> record_desc().
list_streams_record() ->
    {#ddb_streams_list_streams{},
     [
     ]}.

-type list_streams_return() :: ddb_return(#ddb_streams_list_streams{}, #ddb_streams_list_streams{}).

-spec list_streams() -> list_streams_return().
list_streams() ->
    list_streams([], default_config()).

-spec list_streams(list_streams_opts()) -> list_streams_return().
list_streams(Opts) ->
    list_streams(Opts, default_config()).

%%------------------------------------------------------------------------------
%% @doc
%% DynamoDB API:
%% [http://docs.aws.amazon.com/dynamodbstreams/latest/APIReference/API_ListStreams.html]
%%
%% @end
%%------------------------------------------------------------------------------
-spec list_streams(list_streams_opts(), aws_config()) -> list_streams_return().
list_streams(Opts, Config) ->
    {AwsOpts, DdbOpts} = opts(list_streams_opts(), Opts),
    Return = erlcloud_ddb_streams_impl:request(
               Config,
               "DynamoDBStreams_20120810.ListStreams",
               AwsOpts),
    case out(Return,
             fun(Json, UOpts) -> undynamize_record(list_streams_record(), Json, UOpts) end,
             DdbOpts) of
        {ok, _} = Out -> Out;
        {error, _} = Out -> Out
    end.
