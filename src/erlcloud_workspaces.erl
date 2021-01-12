%% @doc
%% An Erlang interface to AWS Workspaces.
%%
%% Output is in the form of `{ok, Value}' or `{error, Reason}'. The
%% format of `Value' is controlled by the `out' option, which defaults
%% to `json'. The possible values are:
%%
%% * `json' - The output from Workspaces as processed by `jsx:decode'
%% but with no further manipulation.
%%
%% * `record' - A record containing all the information from the
%% Workspaces response except field types.
%%
%% Workspaces errors are returned in the form `{error, {ErrorCode, Message}}' 
%% where `ErrorCode' and 'Message' are both binary
%% strings. 

%% See the unit tests for additional usage examples beyond what are
%% provided for each function.
%%
%% @end

-module(erlcloud_workspaces).

-include("erlcloud_aws.hrl").
-include("erlcloud_workspaces.hrl").

%%% Library initialization.
-export([configure/2, configure/3, configure/4, new/2, new/3, new/4]).

-define(API_VERSION, "20150408").
-define(OUTPUT_CHOICES, [json, record, map]).

-export([
    describe_tags/1, describe_tags/2,
    describe_workspaces/0, describe_workspaces/1, describe_workspaces/2,
    describe_workspace_directories/0, describe_workspace_directories/1, describe_workspace_directories/2
]).

-export_type([
    describe_workspaces_opt/0,
    describe_workspaces_opts/0,
    describe_workspace_directories_opt/0,
    describe_workspace_directories_opts/0
 ]).


%%%------------------------------------------------------------------------------
%%% Library initialization.
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
       workspaces_host=Host
      }.

-spec new(string(), string(), string(), non_neg_integer()) -> aws_config().

new(AccessKeyID, SecretAccessKey, Host, Port) ->
    #aws_config{
       access_key_id=AccessKeyID,
       secret_access_key=SecretAccessKey,
       workspaces_host=Host,
       workspaces_port=Port
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
%%% Shared types
%%%------------------------------------------------------------------------------

-type string_param() :: binary() | string().

-type json_pair() :: {binary() | atom(), jsx:json_term()}.
-type json_return() :: {ok, jsx:json_term()} | {error, term()}.
-type workspaces_return(Record) :: {ok, jsx:json_term() | Record } | {error, term()}.
-type decode_fun() :: fun((jsx:json_term(), decode_opts()) -> tuple()).

%%%------------------------------------------------------------------------------
%%% Shared Options
%%%------------------------------------------------------------------------------
-type out_type() :: json | record.
-type out_opt() :: {out, out_type()}.
-type property() :: proplists:property().

-type aws_opts() :: [json_pair()].
-type workspaces_opts() :: [out_opt()].
-type opts() :: {aws_opts(), workspaces_opts()}.

-spec verify_workspaces_opt(atom(), term()) -> ok.
verify_workspaces_opt(out, Value) ->
    case lists:member(Value, ?OUTPUT_CHOICES) of
        true ->
            ok;
        false ->
            error({erlcloud_workspaces, {invalid_opt, {out, Value}}})
    end;
verify_workspaces_opt(Name, Value) ->
    error({erlcloud_workspaces, {invalid_opt, {Name, Value}}}).

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
            verify_workspaces_opt(Name, Value),
            {AwsOpts, [{Name, Value} | EcsOpts]}
    end.

-spec opts(opt_table(), proplist()) -> opts().
opts(Table, Opts) when is_list(Opts) ->
    %% remove duplicate options
    Opts1 = lists:ukeysort(1, proplists:unfold(Opts)),
    lists:foldl(fun(Opt, A) -> opt_folder(Table, Opt, A) end, {[], []}, Opts1);
opts(_, _) ->
    error({erlcloud_workspaces, opts_not_list}).

%%%------------------------------------------------------------------------------
%%% Shared Decoders
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
%%% Output
%%%------------------------------------------------------------------------------
-spec out(json_return(), decode_fun(), workspaces_opts())
         -> {ok, jsx:json_term() | tuple()} |
            {simple, term()} |
            {error, term()}.
out({error, Reason}, _, _) ->
    {error, Reason};
out({ok, Json}, Decode, Opts) ->
    case proplists:get_value(out, Opts, record) of
        json ->
            {ok, Json};
        record ->
            {ok, Decode(Json, [])};
        map ->
            {ok, erlcloud_util:proplists_to_map(Json)}
    end.

%%%------------------------------------------------------------------------------
%%% Shared Records
%%%------------------------------------------------------------------------------

-spec workspace_record() -> record_desc().
workspace_record() ->
    {#workspace{},
        [
            {<<"BundleId">>, #workspace.bundle_id, fun id/2},
            {<<"ComputerName">>, #workspace.computer_name, fun id/2},
            {<<"DirectoryId">>, #workspace.directory_id, fun id/2},
            {<<"ErrorCode">>, #workspace.error_code, fun id/2},
            {<<"ErrorMessage">>, #workspace.error_message, fun id/2},
            {<<"IpAddress">>, #workspace.ip_address, fun id/2},
            {<<"ModificationStates">>, #workspace.modification_states, fun decode_modification_state_list/2},
            {<<"RootVolumeEncryptionEnabled">>, #workspace.root_volume_encryption_enabled, fun id/2},
            {<<"State">>, #workspace.state, fun id/2},
            {<<"SubnetId">>, #workspace.subnet_id, fun id/2},
            {<<"UserName">>, #workspace.user_name, fun id/2},
            {<<"UserVolumeEncryptionEnabled">>, #workspace.user_volume_encryption_enabled, fun id/2},
            {<<"VolumeEncryptionKey">>, #workspace.volume_encryption_key, fun id/2},
            {<<"WorkspaceId">>, #workspace.workspace_id, fun id/2},
            {<<"WorkspaceProperties">>, #workspace.workspace_properties, fun decode_workspace_properties/2}
        ]
    }.

-spec modification_state_record() -> record_desc().
modification_state_record() ->
    {#workspace_modification_state{},
        [
            {<<"Resource">>, #workspace_modification_state.resource, fun id/2},
            {<<"State">>, #workspace_modification_state.state, fun id/2}
        ]
    }.

-spec workspace_properties_record() -> record_desc().
workspace_properties_record() ->
    {#workspace_properties{},
        [
            {<<"ComputeTypeName">>, #workspace_properties.computer_type_name, fun id/2},
            {<<"RootVolumeSizeGib">>, #workspace_properties.root_volume_size_gib, fun id/2},
            {<<"RunningMode">>, #workspace_properties.running_mode, fun id/2},
            {<<"RunningModeAutoStopTimeoutInMinutes">>, #workspace_properties.running_mode_auto_stop_timeout_in_minutes, fun id/2},
            {<<"UserVolumeSizeGib">>, #workspace_properties.user_volume_size_gib, fun id/2}
        ]
    }.

-spec tag_record() -> record_desc().
tag_record() ->
    {#workspaces_tag{},
        [
            {<<"Key">>, #workspaces_tag.key, fun id/2},
            {<<"Value">>, #workspaces_tag.value, fun id/2}
        ]
    }.

-spec workspace_directory_record() -> record_desc().
workspace_directory_record() ->
    {#workspace_directory{},
        [
            {<<"Alias">>, #workspace_directory.alias, fun id/2},
            {<<"CustomerUserName">>, #workspace_directory.customer_user_name, fun id/2},
            {<<"DirectoryId">>, #workspace_directory.directory_id, fun id/2},
            {<<"DirectoryName">>, #workspace_directory.directory_name, fun id/2},
            {<<"DirectoryType">>, #workspace_directory.directory_type, fun id/2},
            {<<"DnsIpAddresses">>, #workspace_directory.dns_ip_address, fun id/2},
            {<<"IamRoleId">>, #workspace_directory.iam_role_id, fun id/2},
            {<<"ipGroupIds">>, #workspace_directory.ip_group_ids, fun id/2},
            {<<"RegistrationCode">>, #workspace_directory.registration_code, fun id/2},
            {<<"SelfservicePermissions">>, #workspace_directory.selfservice_permissions, fun decode_selfservice_permissions/2},
            {<<"State">>, #workspace_directory.state, fun id/2},
            {<<"SubnetIds">>, #workspace_directory.subnet_ids, fun id/2},
            {<<"Tenancy">>, #workspace_directory.tenancy, fun id/2},
            {<<"WorkspaceAccessProperties">>, #workspace_directory.workspace_access_properties, fun decode_workspace_access_properties/2},
            {<<"WorkspaceCreationProperties">>, #workspace_directory.workspace_creation_properties, fun decode_workspace_creation_properties/2},
            {<<"WorkspaceSecurityGroupId">>, #workspace_directory.workspace_security_group_id, fun id/2}
        ]
    }.

-spec workspaces_selfservice_permissions_record() -> record_desc().
workspaces_selfservice_permissions_record() ->
    {#workspaces_selfservice_permissions{},
        [
            {<<"ChangeComputeType">>, #workspaces_selfservice_permissions.change_compute_type, fun id/2},
            {<<"IncreaseVolumeSize">>, #workspaces_selfservice_permissions.increase_volume_size, fun id/2},
            {<<"RebuildWorkspace">>, #workspaces_selfservice_permissions.rebuild_workspace, fun id/2},
            {<<"RestartWorkspace">>, #workspaces_selfservice_permissions.restart_workspace, fun id/2},
            {<<"SwitchRunningMode">>, #workspaces_selfservice_permissions.switch_running_mode, fun id/2}
        ]
    }.

-spec workspace_access_properties_record() -> record_desc().
workspace_access_properties_record() ->
    {#workspace_access_properties{},
        [
            {<<"DeviceTypeAndroid">>, #workspace_access_properties.device_type_android, fun id/2},
            {<<"DeviceTypeChromeOs">>, #workspace_access_properties.device_type_chrome_os, fun id/2},
            {<<"DeviceTypeIos">>, #workspace_access_properties.device_type_ios, fun id/2},
            {<<"DeviceTypeOsx">>, #workspace_access_properties.device_type_osx, fun id/2},
            {<<"DeviceTypeWeb">>, #workspace_access_properties.device_type_web, fun id/2},
            {<<"DeviceTypeWindows">>, #workspace_access_properties.device_type_windows, fun id/2},
            {<<"DeviceTypeZeroClient">>, #workspace_access_properties.device_type_zero_client, fun id/2}
        ]
    }.

-spec workspace_creation_properties_record() -> record_desc().
workspace_creation_properties_record() ->
    {#workspace_creation_properties{},
        [
            {<<"CustomSecurityGroupId">>, #workspace_creation_properties.custom_security_group_id, fun id/2},
            {<<"DefaultOu">>, #workspace_creation_properties.default_ou, fun id/2},
            {<<"EnableInternetAccess">>, #workspace_creation_properties.enable_internet_access, fun id/2},
            {<<"EnableMaintenanceMode">>, #workspace_creation_properties.enable_maintenance_mode, fun id/2},
            {<<"EnableWorkDocs">>, #workspace_creation_properties.enable_work_docs, fun id/2},
            {<<"UserEnabledAsLocalAdministrator">>, #workspace_creation_properties.user_enabled_as_local_administrator, fun id/2}
        ]
    }.

decode_tags_list(V, Opts) ->
    [decode_record(tag_record(), I, Opts) || I <- V].

decode_workspace_properties(V, Opts) ->
    decode_record(workspace_properties_record(), V, Opts).

decode_modification_state_list(V, Opts) ->
    [decode_record(modification_state_record(), I, Opts) || I <- V].

decode_workspaces_list(V, Opts) ->
    [decode_record(workspace_record(), I, Opts) || I <- V].

decode_workspace_directories_list(V, Opts) ->
    [decode_record(workspace_directory_record(), I, Opts) || I <- V].

decode_selfservice_permissions(V, Opts) ->
    decode_record(workspaces_selfservice_permissions_record(), V, Opts).

decode_workspace_access_properties(V, Opts) ->
    decode_record(workspace_access_properties_record(), V, Opts).

decode_workspace_creation_properties(V, Opts) ->
    decode_record(workspace_creation_properties_record(), V, Opts).

%%%------------------------------------------------------------------------------
%%% AWS Workspaces API Functions
%%%------------------------------------------------------------------------------

%%%------------------------------------------------------------------------------
%% DescribeTags
%%%------------------------------------------------------------------------------
-type describe_tags_opt() :: {resource_id, string_param()} |
                             out_opt().
-type describe_tags_opts() :: [describe_tags_opt()].

-spec describe_tags_opts() -> opt_table().
describe_tags_opts() ->
    [
        {resource_id, <<"ResourceId">>, fun encode_json_value/1}
    ].

-spec describe_tags(Opts :: describe_tags_opts()) -> workspaces_return([#workspaces_tag{}]).
describe_tags(Opts) ->
    describe_tags(Opts, default_config()).

%%%------------------------------------------------------------------------------
%% @doc 
%% Workspaces API
%% [https://docs.aws.amazon.com/workspaces/latest/api/API_DescribeTags.html]
%%
%% ===Example===
%%
%% Describe tags for workspace id "ws-c8wvb67py"
%%
%% `
%% {ok, Tags} = erlcloud_workspaces:describe_tags([{resource_id, "ws-c8wvb67py"}, {out, json}])
%% '
%% @end
%%%------------------------------------------------------------------------------
-spec describe_tags(Opts :: describe_tags_opts(), Config :: aws_config()) -> workspaces_return([#workspaces_tag{}]).
describe_tags(Opts, #aws_config{} = Config) ->
    {AwsOpts, WorkspacesOpts} = opts(describe_tags_opts(), Opts),
    Return = workspaces_request(
                Config,
                "DescribeTags",
                AwsOpts),
    out(Return, fun(Json, UOpts) ->
            TagsList = proplists:get_value(<<"TagList">>, Json),
            decode_tags_list(TagsList, UOpts)
        end, 
        WorkspacesOpts).

%%%------------------------------------------------------------------------------
%% DescribeWorkspaces
%%%------------------------------------------------------------------------------
-type describe_workspaces_opt() :: {bundle_id, string_param()} |
                                   {directory_id, string_param()} |
                                   {limit, pos_integer()} |
                                   {next_token, string_param()} |
                                   {user_name, string_param()} |
                                   {workspace_ids, [string_param()]} | 
                                 out_opt().
-type describe_workspaces_opts() :: [describe_workspaces_opt()].

-spec describe_workspaces_opts() -> opt_table().
describe_workspaces_opts() ->
    [
        {bundle_id, <<"BundleId">>, fun encode_json_value/1},
        {directory_id, <<"DirectoryId">>, fun encode_json_value/1},
        {limit, <<"Limit">>, fun id/1},
        {next_token, <<"NextToken">>, fun encode_json_value/1},
        {user_name, <<"UserName">>, fun encode_json_value/1},
        {workspace_ids, <<"WorkspaceIds">>, fun encode_json_value/1}
    ].

-spec describe_workspaces_record() -> record_desc().
describe_workspaces_record() ->
    {#describe_workspaces{},
     [{<<"NextToken">>, #describe_workspaces.next_token, fun id/2},
      {<<"Workspaces">>, #describe_workspaces.workspaces, fun decode_workspaces_list/2}
     ]}.

-spec describe_workspaces() -> workspaces_return(#describe_workspaces{}).
describe_workspaces() ->
    describe_workspaces([], default_config()).

-spec describe_workspaces(describe_workspaces_opts() | aws_config()) -> workspaces_return(#describe_workspaces{}).
describe_workspaces(#aws_config{} = Config) ->
    describe_workspaces([], Config);
describe_workspaces(Opts) ->
    describe_workspaces(Opts, default_config()).

%%%------------------------------------------------------------------------------
%% @doc 
%% Workspaces API
%% [https://docs.aws.amazon.com/workspaces/latest/api/API_DescribeWorkspaces.html]
%%
%% ===Example===
%%
%% Describe workspaces in Directory "TestDirectory"
%%
%% `
%% {ok, Clusters} = erlcloud_workspaces:describe_workspaces([{directory_id, "TestDirectory"}, {out, json}])
%% '
%% @end
%%%------------------------------------------------------------------------------
-spec describe_workspaces(Opts :: describe_workspaces_opts(), Config :: aws_config()) -> workspaces_return(#describe_workspaces{}).
describe_workspaces(Opts, #aws_config{} = Config) ->
    {AwsOpts, WorkspacesOpts} = opts(describe_workspaces_opts(), Opts),
    Return = workspaces_request(
                Config,
                "DescribeWorkspaces",
                AwsOpts),
    out(Return, fun(Json, UOpts) -> decode_record(describe_workspaces_record(), Json, UOpts) end, 
        WorkspacesOpts).

%%%------------------------------------------------------------------------------
%% DescribeWorkspaceDirectories
%%%------------------------------------------------------------------------------
-type describe_workspace_directories_opt() :: {directory_ids, [string_param()]} |
                                   {limit, pos_integer()} |
                                   {next_token, string_param()} |
                                 out_opt().
-type describe_workspace_directories_opts() :: [describe_workspace_directories_opt()].

-spec describe_workspace_directories_opts() -> opt_table().
describe_workspace_directories_opts() ->
    [
        {directory_ids, <<"DirectoryIds">>, fun encode_json_value/1},
        {limit, <<"Limit">>, fun id/1},
        {next_token, <<"NextToken">>, fun encode_json_value/1}
    ].

-spec describe_workspace_directories_record() -> record_desc().
describe_workspace_directories_record() ->
    {#describe_workspace_directories{},
     [{<<"NextToken">>, #describe_workspace_directories.next_token, fun id/2},
      {<<"Directories">>, #describe_workspace_directories.workspace_directories, fun decode_workspace_directories_list/2}
     ]}.

-spec describe_workspace_directories() -> workspaces_return(#describe_workspace_directories{}).
describe_workspace_directories() ->
    describe_workspace_directories([], default_config()).

-spec describe_workspace_directories(describe_workspace_directories_opts() | aws_config())
  -> workspaces_return(#describe_workspace_directories{}).
describe_workspace_directories(#aws_config{} = Config) ->
    describe_workspace_directories([], Config);
describe_workspace_directories(Opts) ->
    describe_workspace_directories(Opts, default_config()).

%%%------------------------------------------------------------------------------
%% @doc 
%% Workspaces API
%% [https://docs.aws.amazon.com/workspaces/latest/api/API_DescribeWorkspaceDirectories.html]
%%
%% ===Example===
%%
%% Describe workspaces directory "TestDirectory"
%%
%% `
%% {ok, Clusters} = erlcloud_workspaces:describe_workspace_directories([{directory_ids, ["TestDirectory"]}, {out, json}])
%% '
%% @end
%%%------------------------------------------------------------------------------
-spec describe_workspace_directories(
        Opts :: describe_workspace_directories_opts(),
        Config :: aws_config())
  -> workspaces_return(#describe_workspace_directories{}).
describe_workspace_directories(Opts, #aws_config{} = Config) ->
    {AwsOpts, WorkspacesOpts} = opts(describe_workspace_directories_opts(), Opts),
    Return = workspaces_request(
                Config,
                "DescribeWorkspaceDirectories",
                AwsOpts),
    out(Return, fun(Json, UOpts) -> decode_record(describe_workspace_directories_record(), Json, UOpts) end, 
        WorkspacesOpts).

%%%------------------------------------------------------------------------------
%%% Internal Functions
%%%------------------------------------------------------------------------------
workspaces_request(Config, Operation, Body) ->
    case erlcloud_aws:update_config(Config) of
        {ok, Config1} ->
            workspaces_request_no_update(Config1, Operation, Body);
        {error, Reason} ->
            {error, Reason}
    end.

workspaces_request_no_update(Config, Operation, Body) ->
    Payload = case Body of
               [] -> <<"{}">>;
               _ -> jsx:encode(lists:flatten(Body))
           end,
    Headers = headers(Config, Operation, Payload),
    Request = #aws_request{service = workspaces,
                           uri = uri(Config),
                           method = post,
                           request_headers = Headers,
                           request_body = Payload},
    case erlcloud_aws:request_to_return(erlcloud_retry:request(Config, Request, fun workspaces_result_fun/1)) of
        {ok, {_RespHeaders, <<>>}} -> {ok, []};
        {ok, {_RespHeaders, RespBody}} -> {ok, jsx:decode(RespBody, [{return_maps, false}])};
        {error, _} = Error-> Error
    end.

-spec workspaces_result_fun(Request :: aws_request()) -> aws_request().
workspaces_result_fun(#aws_request{response_type = ok} = Request) ->
    Request;
workspaces_result_fun(#aws_request{response_type = error,
                                  error_type = aws,
                                  response_status = Status} = Request) when Status >= 500 ->
    Request#aws_request{should_retry = true};
workspaces_result_fun(#aws_request{response_type = error, error_type = aws} = Request) ->
    Request#aws_request{should_retry = false}.

headers(Config, Operation, Body) ->
    Headers = [{"host", Config#aws_config.workspaces_host},
               {"x-amz-target", lists:append(["WorkspacesService.", Operation])},
               {"content-type", "application/x-amz-json-1.1"}],
    Region = erlcloud_aws:aws_region_from_host(Config#aws_config.workspaces_host),
    erlcloud_aws:sign_v4_headers(Config, Headers, Body, Region, "workspaces").

uri(#aws_config{workspaces_scheme = Scheme, workspaces_host = Host} = Config) ->
    lists:flatten([Scheme, Host, port_spec(Config)]).

port_spec(#aws_config{workspaces_port=80}) ->
    "";
port_spec(#aws_config{workspaces_port=Port}) ->
    [":", erlang:integer_to_list(Port)].


encode_json_value(undefined) -> undefined;
encode_json_value(true) -> true;
encode_json_value(false) -> false;
encode_json_value(L) when is_list(L), is_list(hd(L)) -> [encode_json_value(V) || V <- L];
encode_json_value(L) when is_list(L), is_binary(hd(L)) -> [encode_json_value(V) || V <- L];
encode_json_value(L) when is_list(L) -> list_to_binary(L);
encode_json_value(B) when is_binary(B) -> B;
encode_json_value(A) when is_atom(A) -> atom_to_binary(A, latin1).

