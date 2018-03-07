-ifndef(erlcloud_ddb).
-define(erlcloud_ddb, 0).

-record(ddb_provisioned_throughput,
        {read_capacity_units :: undefined | pos_integer(),
         write_capacity_units :: undefined | pos_integer(),
         last_decrease_date_time :: undefined | number(),
         last_increase_date_time :: undefined | number()
        }).
-record(ddb_table_description,
        {creation_date_time :: undefined | number(),
         key_schema :: undefined | erlcloud_ddb:key_schema(),
         provisioned_throughput :: undefined | #ddb_provisioned_throughput{},
         table_name :: undefined | binary(),
         table_status :: undefined | binary()
        }).

-record(ddb_batch_get_item_response,
        {table :: erlcloud_ddb:table_name(),
         items :: undefined | [erlcloud_ddb:out_item()],
         consumed_capacity_units :: undefined | number()
        }).
-record(ddb_batch_get_item,
        {responses :: undefined | [#ddb_batch_get_item_response{}],
         unprocessed_keys :: undefined | [erlcloud_ddb:batch_get_item_request_item()]
        }).

-record(ddb_batch_write_item_response,
        {table :: erlcloud_ddb:table_name(),
         consumed_capacity_units :: undefined | number()
        }).
-record(ddb_batch_write_item,
        {responses :: undefined | [#ddb_batch_write_item_response{}],
         unprocessed_items :: undefined | [erlcloud_ddb:batch_write_item_request_item()]
        }).

-record(ddb_create_table,
        {table_description :: undefined | #ddb_table_description{}
        }).

-record(ddb_delete_item,
        {attributes :: undefined | erlcloud_ddb:out_item(),
         consumed_capacity_units :: undefined | number()
        }).

-record(ddb_delete_table,
        {table_description :: undefined | #ddb_table_description{}
        }).

-record(ddb_table,
        {creation_date_time :: undefined | number(),
         item_count :: undefined | non_neg_integer(),
         key_schema :: undefined | erlcloud_ddb:key_schema(),
         provisioned_throughput :: undefined | #ddb_provisioned_throughput{},
         table_name :: undefined | binary(),
         table_size_bytes :: undefined | non_neg_integer(),
         table_status :: undefined | binary()
        }).
-record(ddb_describe_table,
        {table :: undefined | #ddb_table{}
        }).

-record(ddb_get_item,
        {item :: undefined | erlcloud_ddb:out_item(),
         consumed_capacity_units :: undefined | number()
        }).

-record(ddb_list_tables,
        {table_names :: undefined | [erlcloud_ddb:table_name()],
         last_evaluated_table_name :: undefined | erlcloud_ddb:table_name()
        }).

-record(ddb_put_item,
        {attributes :: undefined | erlcloud_ddb:out_item(),
         consumed_capacity_units :: undefined | number()
        }).

-record(ddb_q, 
        {count :: undefined | non_neg_integer(),
         items :: undefined | [erlcloud_ddb:out_item()],
         last_evaluated_key :: undefined | erlcloud_ddb:hash_range_key(),
         consumed_capacity_units :: undefined | number()
        }).

-record(ddb_scan, 
        {items :: undefined | [erlcloud_ddb:out_item()],
         count :: undefined | non_neg_integer(),
         scanned_count :: undefined | non_neg_integer(),
         last_evaluated_key :: undefined | erlcloud_ddb:hash_range_key(),
         consumed_capacity_units :: undefined | number()
        }).

-record(ddb_update_item,
        {attributes :: undefined | erlcloud_ddb:out_item(),
         consumed_capacity_units :: undefined | number()
        }).

-record(ddb_update_table,
        {table_description :: undefined | #ddb_table_description{}
        }).

-endif.