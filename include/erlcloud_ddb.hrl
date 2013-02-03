-record(ddb_provisioned_throughput,
        {read_capacity_units :: pos_integer(),
         write_capacity_units :: pos_integer(),
         last_decrease_date_time :: number(),
         last_increase_date_time :: number()
        }).
-record(ddb_table_description,
        {creation_date_time :: number(),
         key_schema :: erlcloud_ddb:key_schema(),
         provisioned_throughput :: #ddb_provisioned_throughput{},
         name :: binary(),
         status :: binary()
        }).
-record(ddb_table,
        {creation_date_time :: number(),
         item_count :: non_neg_integer(),
         key_schema :: erlcloud_ddb:key_schema(),
         provisioned_throughput :: #ddb_provisioned_throughput{},
         name :: binary(),
         size_bytes :: non_neg_integer(),
         status :: binary()
        }).
-record(ddb_batch_get_item_response,
        {table :: erlcloud_ddb:table_name(),
         items :: [erlcloud_ddb:out_item()],
         consumed_capacity_units :: number()
        }).
-record(ddb_batch_get_item,
        {responses :: [#ddb_batch_get_item_response{}],
         unprocessed_keys :: [erlcloud_ddb:batch_get_item_request_item()]
        }).
-record(ddb_batch_write_item_response,
        {table :: erlcloud_ddb:table_name(),
         consumed_capacity_units :: number()
        }).
-record(ddb_batch_write_item,
        {responses :: [#ddb_batch_write_item_response{}],
         unprocessed_items :: [erlcloud_ddb:batch_write_item_request_item()]
        }).
-record(ddb_q, 
        {count :: non_neg_integer(),
         items :: [erlcloud_ddb:out_item()],
         last_evaluated_key :: erlcloud_ddb:hash_range_key(),
         consumed_capacity_units :: number()
        }).
