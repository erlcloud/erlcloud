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
-record(ddb_streams_shard,
        {parent_shard_id :: erlcloud_ddb_streams:shard_id(),
         sequence_number_range :: erlcloud_ddb_streams:sequence_number_range(),
         shard_id :: erlcloud_ddb_streams:shard_id()
        }).
-record(ddb_streams_stream_description,
        {creation_request_date_time :: number(),
         key_schema :: erlcloud_ddb_streams:key_schema(),
         last_evaluated_shard_id :: erlcloud_ddb_streams:shard_id(),
         shards :: [#ddb_streams_shard{}],
         stream_arn :: erlcloud_ddb_streams:stream_arn(),
         stream_label :: erlcloud_ddb_streams:stream_label(),
         stream_status :: erlcloud_ddb_streams:stream_status(),
         stream_view_type :: erlcloud_ddb_streams:stream_view_type(),
         table_name :: erlcloud_ddb_streams:table_name()
        }).

-record(ddb_streams_describe_stream,
        {stream_description :: #ddb_streams_stream_description{}
        }).
-record(ddb_streams_get_records,
        {
        }).
-record(ddb_streams_get_shard_iterator,
        {shard_iterator :: erlcloud_ddb_streams:shard_iterator()
        }).
-record(ddb_streams_list_streams,
        {last_evaluated_stream_arn :: erlcloud_ddb_streams:stream_arn(),
         streams :: [#ddb_streams_stream{}]
        }).
