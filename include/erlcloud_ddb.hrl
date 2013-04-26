-type date_time() :: number().
-type table_status() :: creating | updating | deleting | active.

-record(ddb_local_secondary_index_description,
        {index_name :: erlcloud_ddb:index_name(),
         index_size_bytes :: integer(),
         item_count :: integer(),
         key_schema :: erlcloud_ddb:key_schema(),
         projection :: erlcloud_ddb:projection()
        }).
-record(ddb_provisioned_throughput_description,
        {last_decrease_date_time :: date_time(),
         last_increase_date_time :: date_time(),
         number_of_decreases_today :: integer(),
         read_capacity_units :: pos_integer(),
         write_capacity_units :: pos_integer()
        }).
-record(ddb_table_description,
        {attribute_definitions :: erlcloud_ddb:attr_defs(),
         creation_date_time :: number(),
         item_count :: integer(),
         key_schema :: erlcloud_ddb:key_schema(),
         local_secondary_indexes :: [#ddb_local_secondary_index_description{}],
         provisioned_throughput :: #ddb_provisioned_throughput_description{},
         table_name :: binary(),
         table_size_bytes :: integer(),
         table_status :: table_status()
        }).
-record(ddb_consumed_capacity,
        {capacity_units :: number(),
         table_name :: erlcloud_ddb:table_name()
        }).
-record(ddb_item_collection_metrics,
        {item_collection_key :: erlcloud_ddb:out_attr_value(),
         size_estimate_range_gb :: {number(), number()}
        }).

-record(ddb_batch_get_item_response,
        {table :: erlcloud_ddb:table_name(),
         items :: [erlcloud_ddb:out_item()]
        }).
-record(ddb_batch_get_item,
        {consumed_capacity :: [#ddb_consumed_capacity{}],
         responses :: [#ddb_batch_get_item_response{}],
         unprocessed_keys :: [erlcloud_ddb:batch_get_item_request_item()]
        }).

-record(ddb_batch_write_item_response,
        {table :: erlcloud_ddb:table_name(),
         consumed_capacity_units :: number()
        }).
-record(ddb_batch_write_item,
        {consumed_capacity :: [#ddb_consumed_capacity{}],
         item_collection_metrics :: [{erlcloud_ddb:table_name(), [#ddb_item_collection_metrics{}]}],
         unprocessed_items :: [erlcloud_ddb:batch_write_item_request_item()]
        }).

-record(ddb_create_table,
        {table_description :: #ddb_table_description{}
        }).

-record(ddb_delete_item,
        {attributes :: erlcloud_ddb:out_item(),
         consumed_capacity :: #ddb_consumed_capacity{},
         item_collection_metrics :: #ddb_item_collection_metrics{}
        }).

-record(ddb_delete_table,
        {table_description :: #ddb_table_description{}
        }).

-record(ddb_describe_table,
        {table :: #ddb_table_description{}
        }).

-record(ddb_get_item,
        {item :: erlcloud_ddb:out_item(),
         consumed_capacity :: #ddb_consumed_capacity{}
        }).

-record(ddb_list_tables,
        {table_names :: [erlcloud_ddb:table_name()],
         last_evaluated_table_name :: erlcloud_ddb:table_name()
        }).

-record(ddb_put_item,
        {attributes :: erlcloud_ddb:out_item(),
         consumed_capacity :: #ddb_consumed_capacity{},
         item_collection_metrics :: #ddb_item_collection_metrics{}
        }).

-record(ddb_q, 
        {consumed_capacity :: #ddb_consumed_capacity{},
         count :: non_neg_integer(),
         items :: [erlcloud_ddb:out_item()],
         last_evaluated_key :: erlcloud_ddb:hash_range_key()
        }).

-record(ddb_scan, 
        {consumed_capacity :: #ddb_consumed_capacity{},
         count :: non_neg_integer(),
         items :: [erlcloud_ddb:out_item()],
         last_evaluated_key :: erlcloud_ddb:hash_range_key(),
         scanned_count :: non_neg_integer()
        }).

-record(ddb_update_item,
        {attributes :: erlcloud_ddb:out_item(),
         consumed_capacity :: #ddb_consumed_capacity{},
         item_collection_metrics :: #ddb_item_collection_metrics{}
        }).

-record(ddb_update_table,
        {table_description :: #ddb_table_description{}
        }).

