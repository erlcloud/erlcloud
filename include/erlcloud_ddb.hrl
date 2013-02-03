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
