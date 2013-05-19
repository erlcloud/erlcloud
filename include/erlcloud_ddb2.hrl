-type date_time() :: number().
-type table_status() :: creating | updating | deleting | active.

-record(ddb2_local_secondary_index_description,
        {index_name :: erlcloud_ddb2:index_name(),
         index_size_bytes :: integer(),
         item_count :: integer(),
         key_schema :: erlcloud_ddb2:key_schema(),
         projection :: erlcloud_ddb2:projection()
        }).
-record(ddb2_provisioned_throughput_description,
        {last_decrease_date_time :: date_time(),
         last_increase_date_time :: date_time(),
         number_of_decreases_today :: integer(),
         read_capacity_units :: pos_integer(),
         write_capacity_units :: pos_integer()
        }).
-record(ddb2_table_description,
        {attribute_definitions :: erlcloud_ddb2:attr_defs(),
         creation_date_time :: number(),
         item_count :: integer(),
         key_schema :: erlcloud_ddb2:key_schema(),
         local_secondary_indexes :: [#ddb2_local_secondary_index_description{}],
         provisioned_throughput :: #ddb2_provisioned_throughput_description{},
         table_name :: binary(),
         table_size_bytes :: integer(),
         table_status :: table_status()
        }).
-record(ddb2_consumed_capacity,
        {capacity_units :: number(),
         table_name :: erlcloud_ddb2:table_name()
        }).
-record(ddb2_item_collection_metrics,
        {item_collection_key :: erlcloud_ddb2:out_attr_value(),
         size_estimate_range_gb :: {number(), number()}
        }).

-record(ddb2_batch_get_item_response,
        {table :: erlcloud_ddb2:table_name(),
         items :: [erlcloud_ddb2:out_item()]
        }).
-record(ddb2_batch_get_item,
        {consumed_capacity :: [#ddb2_consumed_capacity{}],
         responses :: [#ddb2_batch_get_item_response{}],
         unprocessed_keys = [] :: [erlcloud_ddb2:batch_get_item_request_item()]
        }).

-record(ddb2_batch_write_item_response,
        {table :: erlcloud_ddb2:table_name(),
         consumed_capacity_units :: number()
        }).
-record(ddb2_batch_write_item,
        {consumed_capacity :: [#ddb2_consumed_capacity{}],
         item_collection_metrics :: [{erlcloud_ddb2:table_name(), [#ddb2_item_collection_metrics{}]}],
         unprocessed_items = [] :: [erlcloud_ddb2:batch_write_item_request_item()]
        }).

-record(ddb2_create_table,
        {table_description :: #ddb2_table_description{}
        }).

-record(ddb2_delete_item,
        {attributes :: erlcloud_ddb2:out_item(),
         consumed_capacity :: #ddb2_consumed_capacity{},
         item_collection_metrics :: #ddb2_item_collection_metrics{}
        }).

-record(ddb2_delete_table,
        {table_description :: #ddb2_table_description{}
        }).

-record(ddb2_describe_table,
        {table :: #ddb2_table_description{}
        }).

-record(ddb2_get_item,
        {item :: erlcloud_ddb2:out_item(),
         consumed_capacity :: #ddb2_consumed_capacity{}
        }).

-record(ddb2_list_tables,
        {table_names :: [erlcloud_ddb2:table_name()],
         last_evaluated_table_name :: erlcloud_ddb2:table_name()
        }).

-record(ddb2_put_item,
        {attributes :: erlcloud_ddb2:out_item(),
         consumed_capacity :: #ddb2_consumed_capacity{},
         item_collection_metrics :: #ddb2_item_collection_metrics{}
        }).

-record(ddb2_q, 
        {consumed_capacity :: #ddb2_consumed_capacity{},
         count :: non_neg_integer(),
         items :: [erlcloud_ddb2:out_item()],
         last_evaluated_key :: erlcloud_ddb2:key()
        }).

-record(ddb2_scan, 
        {consumed_capacity :: #ddb2_consumed_capacity{},
         count :: non_neg_integer(),
         items :: [erlcloud_ddb2:out_item()],
         last_evaluated_key :: erlcloud_ddb2:key(),
         scanned_count :: non_neg_integer()
        }).

-record(ddb2_update_item,
        {attributes :: erlcloud_ddb2:out_item(),
         consumed_capacity :: #ddb2_consumed_capacity{},
         item_collection_metrics :: #ddb2_item_collection_metrics{}
        }).

-record(ddb2_update_table,
        {table_description :: #ddb2_table_description{}
        }).

