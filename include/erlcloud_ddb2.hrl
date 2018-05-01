-ifndef(erlcloud_ddb2_hrl).
-define(erlcloud_ddb2_hrl, 0).

-record(ddb2_error,
        {attempt :: pos_integer(),
         error_type :: undefined | ddb | http | httpc,
         should_retry :: undefined | boolean(),
         reason :: term(),
         request_headers :: [{string(), string()}],
         request_body :: jsx:json_text(),
         response_status :: undefined | pos_integer(),
         response_status_line :: undefined | string(),
         response_headers :: undefined | [{string(), string()}],
         response_body :: undefined | binary()
        }).

-type date_time() :: number().
-type global_table_status() :: creating | active | deleting | updating.
-type table_status() :: creating | updating | deleting | active.
-type index_status() :: creating | updating | deleting | active.

-record(ddb2_replica_description,
        {region_name :: undefined | binary()}).
-record(ddb2_global_table_description,
        {creation_date_time :: undefined | number(),
         global_table_arn :: undefined | binary(),
         global_table_name :: undefined | erlcloud_ddb2:table_name(),
         global_table_status :: undefined | global_table_status(),
         replication_group :: undefined | [#ddb2_replica_description{}]
        }).
-record(ddb2_replica,
        {region_name :: undefined | binary()}).
-record(ddb2_global_table,
        {global_table_name :: undefined | erlcloud_ddb2:table_name(),
         replication_group :: undefined | [#ddb2_replica{}]
        }).
-record(ddb2_provisioned_throughput_description,
        {last_decrease_date_time :: undefined | date_time(),
         last_increase_date_time :: undefined | date_time(),
         number_of_decreases_today :: undefined | integer(),
         read_capacity_units :: undefined | pos_integer(),
         write_capacity_units :: undefined | pos_integer()
        }).
-record(ddb2_global_secondary_index_description,
        {backfilling :: undefined | boolean(),
         index_arn :: undefined | binary(),
         index_name :: undefined | erlcloud_ddb2:index_name(),
         index_size_bytes :: undefined | integer(),
         index_status :: undefined | index_status(),
         item_count :: undefined | integer(),
         key_schema :: undefined | erlcloud_ddb2:key_schema(),
         projection :: undefined | erlcloud_ddb2:projection(),
         provisioned_throughput :: undefined | #ddb2_provisioned_throughput_description{}
        }).
-record(ddb2_local_secondary_index_description,
        {index_arn :: undefined | binary(),
         index_name :: undefined | erlcloud_ddb2:index_name(),
         index_size_bytes :: undefined | integer(),
         item_count :: undefined | integer(),
         key_schema :: undefined | erlcloud_ddb2:key_schema(),
         projection :: undefined | erlcloud_ddb2:projection()
        }).
-record(ddb2_table_description,
        {attribute_definitions :: undefined | erlcloud_ddb2:attr_defs(),
         creation_date_time :: undefined | number(),
         global_secondary_indexes :: undefined | [#ddb2_global_secondary_index_description{}],
         item_count :: undefined | integer(),
         key_schema :: undefined | erlcloud_ddb2:key_schema(),
         latest_stream_arn :: undefined | binary(),
         latest_stream_label :: undefined | binary(),
         local_secondary_indexes :: undefined | [#ddb2_local_secondary_index_description{}],
         provisioned_throughput :: undefined | #ddb2_provisioned_throughput_description{},
         sse_description :: undefined | erlcloud_ddb2:sse_description(),
         stream_specification :: undefined | erlcloud_ddb2:stream_specification(),
         table_arn :: undefined | binary(),
         table_name :: undefined | binary(),
         table_size_bytes :: undefined | integer(),
         table_status :: undefined | table_status()
        }).
-record(ddb2_consumed_capacity,
        {capacity_units :: undefined | number(),
         global_secondary_indexes :: undefined | [{erlcloud_ddb2:index_name(), number()}],
         local_secondary_indexes :: undefined | [{erlcloud_ddb2:index_name(), number()}],
         table :: undefined | number(),
         table_name :: undefined | erlcloud_ddb2:table_name()
        }).
-record(ddb2_item_collection_metrics,
        {item_collection_key :: erlcloud_ddb2:out_attr_value(),
         size_estimate_range_gb :: undefined | {number(), number()}
        }).

-record(ddb2_batch_get_item_response,
        {table :: erlcloud_ddb2:table_name(),
         items :: [erlcloud_ddb2:out_item()]
        }).
-record(ddb2_batch_get_item,
        {consumed_capacity :: undefined | [#ddb2_consumed_capacity{}],
         responses :: undefined | [#ddb2_batch_get_item_response{}],
         unprocessed_keys = [] :: [erlcloud_ddb2:batch_get_item_request_item()]
        }).

-record(ddb2_batch_write_item,
        {consumed_capacity :: undefined | [#ddb2_consumed_capacity{}],
         item_collection_metrics :: undefined | [{erlcloud_ddb2:table_name(), [#ddb2_item_collection_metrics{}]}],
         unprocessed_items = [] :: [erlcloud_ddb2:batch_write_item_request_item()]
        }).

-record(ddb2_create_global_table,
        {global_table_description :: undefined | #ddb2_global_table_description{}
        }).

-record(ddb2_create_table,
        {table_description :: undefined | #ddb2_table_description{}
        }).

-record(ddb2_delete_item,
        {attributes :: undefined | erlcloud_ddb2:out_item(),
         consumed_capacity :: undefined | #ddb2_consumed_capacity{},
         item_collection_metrics :: undefined | #ddb2_item_collection_metrics{}
        }).

-record(ddb2_delete_table,
        {table_description :: undefined | #ddb2_table_description{}
        }).

-record(ddb2_describe_global_table,
        {global_table_description :: undefined | #ddb2_global_table_description{}
        }).

-record(ddb2_describe_limits,
        {account_max_read_capacity_units :: undefined | pos_integer(),
         account_max_write_capacity_units :: undefined | pos_integer(),
         table_max_read_capacity_units :: undefined | pos_integer(),
         table_max_write_capacity_units :: undefined | pos_integer()
        }).

-record(ddb2_describe_table,
        {table :: undefined | #ddb2_table_description{}
        }).

-record(ddb2_time_to_live_description,
        {attribute_name :: undefined | erlcloud_ddb2:attr_name(),
         time_to_live_status :: undefined | erlcloud_ddb2:time_to_live_status()
        }).

-record(ddb2_describe_time_to_live,
        {time_to_live_description :: undefined | #ddb2_time_to_live_description{}
        }).

-record(ddb2_get_item,
        {item :: undefined | erlcloud_ddb2:out_item(),
         consumed_capacity :: undefined | #ddb2_consumed_capacity{}
        }).

-record(ddb2_list_global_tables,
        {global_tables :: undefined | [#ddb2_global_table{}],
         last_evaluated_global_table_name :: undefined | erlcloud_ddb2:table_name()
        }).

-record(ddb2_list_tables,
        {table_names :: undefined | [erlcloud_ddb2:table_name()],
         last_evaluated_table_name :: undefined | erlcloud_ddb2:table_name()
        }).

-record(ddb2_list_tags_of_resource,
        {next_token :: undefined | binary(),
         tags :: undefined | [erlcloud_ddb2:tag()]}).

-record(ddb2_put_item,
        {attributes :: undefined | erlcloud_ddb2:out_item(),
         consumed_capacity :: undefined | #ddb2_consumed_capacity{},
         item_collection_metrics :: undefined | #ddb2_item_collection_metrics{}
        }).

-record(ddb2_q, 
        {consumed_capacity :: undefined | #ddb2_consumed_capacity{},
         count :: undefined | non_neg_integer(),
         items :: undefined | [erlcloud_ddb2:out_item()],
         last_evaluated_key :: undefined | erlcloud_ddb2:key(),
         scanned_count :: undefined | non_neg_integer()
        }).

-record(ddb2_scan, 
        {consumed_capacity :: undefined | #ddb2_consumed_capacity{},
         count :: undefined | non_neg_integer(),
         items :: undefined | [erlcloud_ddb2:out_item()],
         last_evaluated_key :: undefined | erlcloud_ddb2:key(),
         scanned_count :: undefined | non_neg_integer()
        }).

-record(ddb2_update_item,
        {attributes :: undefined | erlcloud_ddb2:out_item(),
         consumed_capacity :: undefined | #ddb2_consumed_capacity{},
         item_collection_metrics :: undefined | #ddb2_item_collection_metrics{}
        }).

-record(ddb2_update_global_table,
        {global_table_description :: undefined | #ddb2_global_table_description{}
        }).

-record(ddb2_update_table,
        {table_description :: undefined | #ddb2_table_description{}
        }).

-record(ddb2_time_to_live_specification, 
        {attribute_name :: undefined | erlcloud_ddb2:attr_name(), 
         enabled :: undefined | boolean()
        }).

-record(ddb2_update_time_to_live,
        {time_to_live_specification :: undefined | #ddb2_time_to_live_specification{}
        }).

-endif.