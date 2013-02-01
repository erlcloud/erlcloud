-record(ddb_q, 
        {count :: non_neg_integer(),
         items :: [erlcloud_ddb:out_item()],
         last_evaluated_key :: erlcloud_ddb:hash_range_key(),
         consumed_capacity_units :: number()}).
