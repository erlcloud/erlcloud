-module(erlcloud_s3_multipart_worker).

-behaviour(gen_server).

%% API functions
-export([start_link/4, calculate_filesize_parts/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {file_path, bucket, key, uploadid, file_descriptor, parts_completed, etags, number_of_parts}).
-define(SPLIT_BY_BYTES, (10*1024*1024)).

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
start_link(FilePath, UploadId, Bucket, Key) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [FilePath, UploadId, Bucket, Key], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([FilePath, UploadId, Bucket, Key]) ->

    NumberOfParts=calculate_filesize_parts(FilePath),

    {ok, FileDescriptor} = file:open(FilePath, [read, {encoding, utf8}]),

    gen_server:cast(self(), upload_next_piece),

    {ok, #state{file_path=FilePath,
                file_descriptor=FileDescriptor, 
                bucket=Bucket, 
                key=Key, 
                uploadid=UploadId, 
                parts_completed=0, 
                etags=[],
                number_of_parts=NumberOfParts}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(upload_next_piece, State) when State#state.parts_completed+1 =< State#state.number_of_parts ->

    CurrentPartNumber = State#state.parts_completed+1,

    UploadId = State#state.uploadid,
    FileDescriptor = State#state.file_descriptor,
    ReadResult = file:read(FileDescriptor, ?SPLIT_BY_BYTES),

    case ReadResult of 
        {ok, Data} -> 
            
            UploadResult = erlcloud_s3:upload_part(State#state.bucket, State#state.key, UploadId, CurrentPartNumber, Data),

            case UploadResult of
                {ok, UploadResultProps} -> 

                    ETag = proplists:get_value(etag, UploadResultProps),
                    ETagWithoutQuotes = string:sub_string(ETag, 2, length(ETag)-1),

                    gen_server:cast(self(), upload_next_piece),
                    NewState = State#state{parts_completed=CurrentPartNumber, etags=[{CurrentPartNumber, ETagWithoutQuotes}|State#state.etags]},
                    {noreply, NewState};

                SomethingElse ->

                    Error = {error, {uploading, SomethingElse}},
                    abort(State, Error),

                    {stop, Error, State}
            end;

        SomethingElse -> 
            
            Error = {error, {reading, SomethingElse}},
            abort(State, Error),

            {stop, Error, State}
    end;

handle_cast(upload_next_piece, State) when State#state.parts_completed+1 > State#state.number_of_parts ->

    FileDescriptor = State#state.file_descriptor,

    ETags = lists:reverse(State#state.etags),
    erlcloud_s3:complete_multipart(State#state.bucket, State#state.key, State#state.uploadid, ETags),
    file:close(FileDescriptor),
    erlcloud_s3_multipart_server:finish(State#state.uploadid, ok),

    {stop, normal, State}.

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
handle_info(_Info, State) ->
    {noreply, State}.

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

abort(State, Reason) ->
    erlcloud_s3:abort_multipart(State#state.bucket, State#state.key, State#state.uploadid),
    file:close(State#state.file_descriptor),
    erlcloud_s3_multipart_server:finish(State#state.uploadid, Reason).

calculate_filesize_parts(FilePath) ->
    {ok, FileInfo} = file:read_file_info(FilePath),
    FileSize = element(2, FileInfo),

    case FileSize of 
        undefined -> {stop, undefinedsize};
        Size -> 

            %% Poor man's ceil()
            NumberOfPartsWithoutRemainder = Size div ?SPLIT_BY_BYTES,
            NumberOfPartsRemainder = Size rem ?SPLIT_BY_BYTES,
            NumberOfPartsFactor = case NumberOfPartsRemainder of
                0 -> 0;
                _ -> 1
            end,

            NumberOfPartsWithoutRemainder+NumberOfPartsFactor

    end.
