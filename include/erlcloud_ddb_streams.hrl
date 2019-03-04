-ifndef(erlcloud_ddb_streams_hrl).
-define(erlcloud_ddb_streams_hrl, 0).

-record(ddb_streams_stream,
        {stream_arn :: undefined | erlcloud_ddb_streams:stream_arn(),
         stream_label :: undefined | erlcloud_ddb_streams:stream_label(),
         table_name :: undefined | erlcloud_ddb_streams:table_name()
        }).
-record(ddb_streams_shard,
        {parent_shard_id :: undefined | erlcloud_ddb_streams:shard_id(),
         sequence_number_range :: undefined | erlcloud_ddb_streams:sequence_number_range(),
         shard_id :: undefined | erlcloud_ddb_streams:shard_id()
        }).
-record(ddb_streams_stream_description,
        {creation_request_date_time :: undefined | number(),
         key_schema :: undefined | erlcloud_ddb_streams:key_schema(),
         last_evaluated_shard_id :: undefined | erlcloud_ddb_streams:shard_id(),
         shards :: undefined | [#ddb_streams_shard{}],
         stream_arn :: undefined | erlcloud_ddb_streams:stream_arn(),
         stream_label :: undefined | erlcloud_ddb_streams:stream_label(),
         stream_status :: undefined | erlcloud_ddb_streams:stream_status(),
         stream_view_type :: undefined | erlcloud_ddb_streams:stream_view_type(),
         table_name :: undefined | erlcloud_ddb_streams:table_name()
        }).
-record(ddb_streams_stream_record,
        {approximate_creation_date_time :: undefined | number(),
         keys :: undefined | erlcloud_ddb_streams:key(),
         new_image :: undefined | erlcloud_ddb_streams:item(),
         old_image :: undefined | erlcloud_ddb_streams:item(),
         sequence_number :: undefined | erlcloud_ddb_streams:sequence_number(),
         size_bytes :: undefined | pos_integer(),
         stream_view_type :: undefined | erlcloud_ddb_streams:stream_view_type()}).
-record(ddb_streams_record,
        {aws_region :: undefined | erlcloud_ddb_streams:aws_region(),
         dynamodb :: undefined | #ddb_streams_stream_record{},
         event_id :: undefined | erlcloud_ddb_streams:event_id(),
         event_name :: undefined | erlcloud_ddb_streams:event_name(),
         event_source :: undefined | erlcloud_ddb_streams:event_source(),
         event_version :: undefined | erlcloud_ddb_streams:event_version()
        }).

-record(ddb_streams_describe_stream,
        {stream_description :: undefined | #ddb_streams_stream_description{}
        }).
-record(ddb_streams_get_records,
        {next_shard_iterator :: undefined | erlcloud_ddb_streams:shard_iterator(),
         records :: undefined | [#ddb_streams_record{}]
        }).
-record(ddb_streams_get_shard_iterator,
        {shard_iterator :: undefined | erlcloud_ddb_streams:shard_iterator()
        }).
-record(ddb_streams_list_streams,
        {last_evaluated_stream_arn :: undefined | erlcloud_ddb_streams:stream_arn(),
         streams :: undefined | [#ddb_streams_stream{}]
        }).

-endif.