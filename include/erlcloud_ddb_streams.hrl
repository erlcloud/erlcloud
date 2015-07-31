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
-record(ddb_streams_stream_record,
        {keys :: erlcloud_ddb_streams:key(),
         new_image :: erlcloud_ddb_streams:item(),
         old_image :: erlcloud_ddb_streams:item(),
         sequence_number :: erlcloud_ddb_streams:sequence_number(),
         size_bytes :: pos_integer(),
         stream_view_type :: erlcloud_ddb_streams:stream_view_type()}).
-record(ddb_streams_record,
        {aws_region :: erlcloud_ddb_streams:aws_region(),
         dynamodb :: #ddb_streams_stream_record{},
         event_id :: erlcloud_ddb_streams:event_id(),
         event_name :: erlcloud_ddb_streams:event_name(),
         event_source :: erlcloud_ddb_streams:event_source(),
         event_version :: erlcloud_ddb_streams:event_version()
        }).

-record(ddb_streams_describe_stream,
        {stream_description :: #ddb_streams_stream_description{}
        }).
-record(ddb_streams_get_records,
        {next_shard_iterator :: erlcloud_ddb_streams:shard_iterator(),
         records :: [#ddb_streams_record{}]
        }).
-record(ddb_streams_get_shard_iterator,
        {shard_iterator :: erlcloud_ddb_streams:shard_iterator()
        }).
-record(ddb_streams_list_streams,
        {last_evaluated_stream_arn :: erlcloud_ddb_streams:stream_arn(),
         streams :: [#ddb_streams_stream{}]
        }).
