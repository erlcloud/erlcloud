-module(erlcloud_s3_multipart).

-export([put_object/3, put_object/4]).

-define(SPLIT_BY_BYTES, (10*1024*1024)).

put_object(FilePath, Bucket, Key) ->
    Config = erlcloud_aws:default_config(),
    put_object(FilePath, Bucket, Key, Config).

put_object(FilePath, Bucket, Key, Config) ->
    NumberOfParts = calculate_filesize_parts(FilePath),

    {ok, FileDescriptor} = file:open(FilePath, [read, raw]),
    {ok, UploadProps} = erlcloud_s3:start_multipart(Bucket, Key, [], [], Config),
    UploadId = proplists:get_value(uploadId, UploadProps),

    upload(Bucket, Key, Config, UploadId, FileDescriptor, NumberOfParts, 1, []).

upload(Bucket, Key, Config, UploadId, FileDescriptor, NumberOfParts, PartNumber, ETags) when PartNumber =< NumberOfParts -> 

    ReadResult = file:read(FileDescriptor, ?SPLIT_BY_BYTES),

    case ReadResult of 
        {ok, Data} -> 

            UploadResult = erlcloud_s3:upload_part(Bucket, Key, UploadId, PartNumber, Data, [], Config),

            case UploadResult of
                {ok, UploadResultProps} ->

                    ETag = proplists:get_value(etag, UploadResultProps),
                    ETagWithoutQuotes = string:sub_string(ETag, 2, length(ETag)-1),

                    upload(Bucket, Key, Config, UploadId, FileDescriptor, NumberOfParts, PartNumber+1, [{PartNumber, ETagWithoutQuotes} | ETags]);

                SomethingElse ->

                    erlcloud_s3:abort_multipart(Bucket, Key, UploadId, [], [], Config),
                    file:close(FileDescriptor),
                    {error, {uploading, SomethingElse}}
            end;

        SomethingElse -> 
            
            erlcloud_s3:abort_multipart(Bucket, Key, UploadId, [], [], Config),
            file:close(FileDescriptor),
            {error, {reading, SomethingElse}}
    end;

upload(Bucket, Key, Config, UploadId, FileDescriptor, NumberOfParts, PartNumber, ETags) when PartNumber =:= NumberOfParts+1 -> 

    ETagsInAscendingOrder = lists:reverse(ETags),
    erlcloud_s3:complete_multipart(Bucket, Key, UploadId, ETagsInAscendingOrder, [], Config),
    file:close(FileDescriptor),
    {ok, UploadId}.

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
