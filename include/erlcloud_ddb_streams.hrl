-record(ddb_streams_error,
        {attempt :: pos_integer(),
         error_type :: ddb | http | httpc,
         should_retry :: boolean(),
         reason :: term(),
         request_headers :: [{string(), string()}],
         request_body :: jsx:json_text(),
         response_status :: pos_integer(),
         response_status_line :: string(),
         response_headers :: [{string(), string()}],
         response_body :: binary()
        }).

-record(ddb_streams_stream,
        {stream_arn :: erlcloud_ddb_streams:stream_arn(),
         stream_label :: erlcloud_ddb_streams:stream_label(),
         table_name :: erlcloud_ddb_streams:table_name()
        }).

-record(ddb_streams_describe_stream,
        {
        }).
-record(ddb_streams_get_records,
        {
        }).
-record(ddb_streams_get_shard_iterator,
        {
        }).
-record(ddb_streams_list_streams,
        {last_evaluated_stream_arn :: erlcloud_ddb_streams:stream_arn(),
         streams :: [#ddb_streams_stream{}]
        }).
