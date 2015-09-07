-module(erlcloud_s3_multipart_server).

-behaviour(gen_server).

%% API functions
-export([start_link/0, put_object/3, put_object/4, finish/2]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(upload, {id, bucket, key, descriptor, from}).
-record(state, {uploadsInProgress=maps:new()}).

-define(DEFAULT_TIMEOUT, infinity).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec put_object(string(), string(), string()) -> ok | {error, any()}.
put_object(FilePath, Bucket, Key) ->
   gen_server:call(?MODULE, {upload, {FilePath, Bucket, Key}}, ?DEFAULT_TIMEOUT).

-spec put_object(string(), string(), string(), integer()) -> ok | {error, any()}.
put_object(FilePath, Bucket, Key, Timeout) ->
   gen_server:call(?MODULE, {upload, {FilePath, Bucket, Key}}, Timeout).

finish(UploadId, Reply) ->
    gen_server:cast(?MODULE, {uploaded, UploadId, Reply}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    {ok, #state{}}.

handle_call({upload, {FilePath, Bucket, Key}}, From, State) ->
    {ok, UploadProps} = erlcloud_s3:start_multipart(Bucket, Key),
    UploadId = proplists:get_value(uploadId, UploadProps),

    {ok, WorkerPid} = erlcloud_s3_multipart_worker_sup:start_upload(FilePath, UploadId, Bucket, Key),
    MonitorRef = erlang:monitor(process, WorkerPid),

    NewState = State#state{uploadsInProgress=maps:put(MonitorRef, #upload{id=UploadId, bucket=Bucket, key=Key, descriptor=FilePath, from=From}, State#state.uploadsInProgress)},

    {noreply, NewState}.

handle_cast({uploaded, UploadId, Reply}, State) ->
    UploadsThatJustFinished =    maps:filter(fun(_MonitorRef, Upload) -> Upload#upload.id =:= UploadId end, State#state.uploadsInProgress),
    UploadsWithoutJustFinished = maps:filter(fun(_MonitorRef, Upload) -> Upload#upload.id =/= UploadId end, State#state.uploadsInProgress),

    lists:foreach(fun({MonitorRef, Upload}) -> 
        erlang:demonitor(MonitorRef),
        gen_server:reply(Upload#upload.from, Reply)
    end, maps:to_list(UploadsThatJustFinished)),

    NewState = State#state{uploadsInProgress=UploadsWithoutJustFinished},

    {noreply, NewState}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------


handle_info({'DOWN', MonitorRef, process, _Object, Reason}, State) ->
    case maps:is_key(MonitorRef, State#state.uploadsInProgress) of
        false -> {noreply, State};
        true -> 
            Upload = maps:get(MonitorRef, State#state.uploadsInProgress),
            erlcloud_s3:abort_multipart(Upload#upload.bucket, Upload#upload.key, Upload#upload.id),
            gen_server:reply(Upload#upload.from, {error, Reason}),

            NewState = State#state{uploadsInProgress=maps:remove(MonitorRef, State#state.uploadsInProgress)},

            {noreply, NewState}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
