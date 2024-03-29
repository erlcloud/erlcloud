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

-record(ddb2_request,
       {headers :: ddb2_req_headers(),
        body :: jsx:json_text(),
        json :: jsx:json_term()
       }).

-type ddb2_req_headers() :: [{string(), string()}].
-type date_time() :: number().
-type global_table_status() :: creating | active | deleting | updating.
-type replica_status() :: creating | creation_failed | updating | deleting | active.
-type table_status() :: creating | updating | deleting | active.
-type backup_status() :: creating | deleted | available.
-type index_status() :: creating | updating | deleting | active.
-type continuous_backups_status() :: enabled | disabled.
-type point_in_time_recovery_status() :: enabling | enabled | disabled.

-record(ddb2_auto_scaling_target_tracking_scaling_policy_configuration_description,
        {target_value :: undefined | number(),
         disable_scale_in :: undefined | boolean(),
         scale_in_cooldown :: undefined | number(),
         scale_out_cooldown :: undefined | number()}).

-record(ddb2_auto_scaling_policy_description,
        {policy_name :: undefined | binary(),
         target_tracking_scaling_policy_configuration :: undefined | #ddb2_auto_scaling_target_tracking_scaling_policy_configuration_description{}}).

-record(ddb2_auto_scaling_settings_description,
        {auto_scaling_disabled :: undefined | boolean(),
         auto_scaling_role_arn :: undefined | binary(),
         maximum_units :: undefined | pos_integer(),
         minimum_units :: undefined | pos_integer(),
         scaling_policies :: undefined | [#ddb2_auto_scaling_policy_description{}]}).

-record(ddb2_provisioned_throughput_override,
        {read_capacity_units :: undefined | pos_integer()}).

-record(ddb2_replica_global_secondary_index_description,
        {index_name :: undefined | binary(),
         provisioned_throughput_override :: undefined | #ddb2_provisioned_throughput_override{}}).

-record(ddb2_replica_global_secondary_index_auto_scaling_description,
        {index_name :: undefined | binary(),
         index_status :: undefined | index_status(),
         provisioned_read_capacity_auto_scaling_settings :: undefined | #ddb2_auto_scaling_settings_description{},
         provisioned_write_capacity_auto_scaling_settings :: undefined | #ddb2_auto_scaling_settings_description{}}).

-record(ddb2_replica_auto_scaling_description,
        {global_secondary_indexes :: undefined | [#ddb2_replica_global_secondary_index_auto_scaling_description{}],
         region_name :: undefined | binary(),
         replica_provisioned_read_capacity_auto_scaling_settings :: undefined | #ddb2_auto_scaling_settings_description{},
         replica_provisioned_write_capacity_auto_scaling_settings :: undefined | #ddb2_auto_scaling_settings_description{},
         replica_status :: undefined | replica_status()}).

-record(ddb2_replica_description,
        {global_secondary_indexes :: undefined | [#ddb2_replica_global_secondary_index_description{}],
         kms_master_key_id :: undefined | binary(),
         provisioned_throughput_override :: undefined | #ddb2_provisioned_throughput_override{},
         region_name :: undefined | binary(),
         replica_status :: undefined | replica_status(),
         replica_status_description :: undefined | binary(),
         replica_status_percent_progress :: undefined | binary()
        }).

-record(ddb2_replica_global_secondary_index_settings_description,
        {index_name :: undefined | binary(),
         index_status :: undefined | index_status(),
         provisioned_read_capacity_auto_scaling_settings :: undefined | #ddb2_auto_scaling_settings_description{},
         provisioned_read_capacity_units :: undefined | pos_integer(),
         provisioned_write_capacity_auto_scaling_settings :: undefined | #ddb2_auto_scaling_settings_description{},
         provisioned_write_capacity_units :: undefined | pos_integer()}).

-record(ddb2_billing_mode_summary,
        {billing_mode :: undefined | erlcloud_ddb2:billing_mode(),
        last_update_to_pay_per_request_date_time :: undefined | date_time()}).

-record(ddb2_replica_settings_description,
        {region_name :: undefined | binary(),
         replica_billing_mode_summary :: undefined | #ddb2_billing_mode_summary{},
         replica_global_secondary_index_settings :: undefined | [#ddb2_replica_global_secondary_index_settings_description{}],
         replica_provisioned_read_capacity_auto_scaling_settings :: undefined | #ddb2_auto_scaling_settings_description{},
         replica_provisioned_read_capacity_units :: undefined | number(),
         replica_provisioned_write_capacity_auto_scaling_settings :: undefined | #ddb2_auto_scaling_settings_description{},
         replica_provisioned_write_capacity_units :: undefined | number(),
         replica_status :: undefined | replica_status()}).

-record(ddb2_global_table_description,
        {creation_date_time :: undefined | number(),
         global_table_arn :: undefined | binary(),
         global_table_name :: undefined | erlcloud_ddb2:table_name(),
         global_table_status :: undefined | global_table_status(),
         replication_group :: undefined | [#ddb2_replica_description{}]
        }).

-record(ddb2_replica,
        {region_name :: undefined | binary()
        }).

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

-record(ddb2_restore_summary,
        {restore_date_time ::undefined | date_time(),
         restore_in_progress :: undefined | boolean(),
         source_backup_arn :: undefined | binary(),
         source_table_arn :: undefined | binary()
        }).

-record(ddb2_table_description,
        {attribute_definitions :: undefined | erlcloud_ddb2:attr_defs(),
         billing_mode_summary :: undefined | #ddb2_billing_mode_summary{},
         creation_date_time :: undefined | number(),
         deletion_protection_enabled :: undefined | boolean(),
         global_secondary_indexes :: undefined | [#ddb2_global_secondary_index_description{}],
         global_table_version :: undefined | binary(),
         item_count :: undefined | integer(),
         key_schema :: undefined | erlcloud_ddb2:key_schema(),
         latest_stream_arn :: undefined | binary(),
         latest_stream_label :: undefined | binary(),
         local_secondary_indexes :: undefined | [#ddb2_local_secondary_index_description{}],
         provisioned_throughput :: undefined | #ddb2_provisioned_throughput_description{},
         replicas :: undefined | [#ddb2_replica_description{}],
         restore_summary :: undefined | #ddb2_restore_summary{},
         sse_description :: undefined | erlcloud_ddb2:sse_description(),
         stream_specification :: undefined | erlcloud_ddb2:stream_specification(),
         table_arn :: undefined | binary(),
         table_name :: undefined | binary(),
         table_size_bytes :: undefined | integer(),
         table_status :: undefined | table_status()
        }).

-record(ddb2_table_auto_scaling_description,
        {replicas :: undefined | [#ddb2_replica_auto_scaling_description{}],
         table_name :: undefined | binary(),
         table_status :: undefined | table_status()}).

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

-record(ddb2_describe_global_table_settings,
        {global_table_name :: undefined | binary(),
         replica_settings :: undefined | [#ddb2_replica_settings_description{}]}).

-record(ddb2_describe_limits,
        {account_max_read_capacity_units :: undefined | pos_integer(),
         account_max_write_capacity_units :: undefined | pos_integer(),
         table_max_read_capacity_units :: undefined | pos_integer(),
         table_max_write_capacity_units :: undefined | pos_integer()
        }).

-record(ddb2_describe_table,
        {table :: undefined | #ddb2_table_description{}
        }).

-record(ddb2_describe_table_replica_auto_scaling,
        {table_auto_scaling_description :: undefined | #ddb2_table_auto_scaling_description{}}).

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

-record(ddb2_update_global_table_settings,
        {global_table_name :: undefined | binary(),
         replica_settings :: undefined | [#ddb2_replica_settings_description{}]}).

-record(ddb2_update_table,
        {table_description :: undefined | #ddb2_table_description{}
        }).

-record(ddb2_update_table_replica_auto_scaling,
        {table_auto_scaling_description :: undefined | #ddb2_table_auto_scaling_description{}}).

-record(ddb2_time_to_live_specification, 
        {attribute_name :: undefined | erlcloud_ddb2:attr_name(),
         enabled :: undefined | boolean()
        }).

-record(ddb2_update_time_to_live,
        {time_to_live_specification :: undefined | #ddb2_time_to_live_specification{}
        }).

-record(ddb2_backup_summary,
        {backup_arn:: undefined | binary(),
         backup_creation_date_time :: undefined | date_time(),
         backup_name:: undefined | binary(),
         backup_size_bytes :: undefined | pos_integer(),
         backup_status:: undefined | backup_status(),
         table_arn:: undefined | binary(),
         table_id:: undefined | binary(),
         table_name:: undefined | binary()
        }).

-record(ddb2_list_backups,
        {backup_summaries :: undefined | [#ddb2_backup_summary{}],
         last_evaluated_backup_arn :: undefined | erlcloud_ddb2:table_name()
        }).

-record(ddb2_backup_details,
        {backup_arn:: undefined | binary(),
         backup_creation_date_time :: undefined | date_time(),
         backup_name:: undefined | binary(),
         backup_size_Bytes :: undefined | pos_integer(),
         backup_status:: undefined | backup_status()
        }).

-record(ddb2_create_backup,
        {backup_details :: undefined | #ddb2_backup_details{}
        }).

-record(ddb2_provisioned_throughput,
        {read_capacity_units :: undefined | pos_integer(),
         write_capacity_units :: undefined | pos_integer()
        }).

-record(ddb2_source_table_details,
        {item_count :: undefined | integer(),
         key_schema :: undefined | erlcloud_ddb2:key_schema(),
         provisioned_throughput :: undefined | #ddb2_provisioned_throughput{},
         table_arn :: undefined | binary(),
         table_creation_date_time :: undefined | number(),
         table_id :: undefined | binary(),
         table_name :: undefined | binary(),
         table_size_bytes :: undefined | integer()
        }).

-record(ddb2_global_secondary_index_info,
        {index_name :: undefined | erlcloud_ddb2:index_name(),
         key_schema :: undefined | erlcloud_ddb2:key_schema(),
         projection :: undefined | erlcloud_ddb2:projection(),
         provisioned_throughput :: undefined | #ddb2_provisioned_throughput{}
        }).

-record(ddb2_local_secondary_index_info,
        {index_name :: undefined | erlcloud_ddb2:index_name(),
         key_schema :: undefined | erlcloud_ddb2:key_schema(),
         projection :: undefined | erlcloud_ddb2:projection()
        }).

-record(ddb2_stream_description,
        {stream_enabled :: undefined | boolean(),
         stream_view_type :: undefined | erlcloud_ddb2:stream_view_type()
        }).

-record(ddb2_source_table_feature_details,
        {global_secondary_indexes :: undefined | [#ddb2_global_secondary_index_info{}],
         local_secondary_indexes  :: undefined | [#ddb2_local_secondary_index_info{}],
         sse_description :: undefined | erlcloud_ddb2:sse_description(),
         stream_description :: undefined | #ddb2_stream_description{} | {boolean(), erlcloud_ddb2:stream_view_type()},
         time_to_live_description :: undefined | #ddb2_time_to_live_description{}
        }).

-record(ddb2_transact_write_items,
        {consumed_capacity :: undefined | [#ddb2_consumed_capacity{}],
         item_collection_metrics :: undefined | [{erlcloud_ddb2:table_name(), [#ddb2_item_collection_metrics{}]}],
         % AWS documentation infer that it should be possible to get old return
         % values upon condition failure, but I have been unable to do so.
         % Still, let's put a field so we can return {ok, []} on success.
         attributes :: undefined
        }).

-record(ddb2_item_response,
        {item :: erlcloud_ddb2:out_item()
        }).

-record(ddb2_transact_get_items,
        {consumed_capacity :: undefined | [#ddb2_consumed_capacity{}],
         responses :: undefined | [#ddb2_item_response{}]
        }).

-record(ddb2_backup_description,
        {backup_details :: undefined | #ddb2_backup_details{},
         source_table_details :: undefined | #ddb2_source_table_details{},
         source_table_feature_details :: undefined | #ddb2_source_table_feature_details{}
        }).

-record(ddb2_delete_backup,
        {backup_description :: undefined | #ddb2_backup_description{}
        }).

-record(ddb2_describe_backup,
        {backup_description :: undefined | #ddb2_backup_description{}
        }).

-record(ddb2_point_in_time_recovery_description,
        {earliest_restorable_date_time :: undefined | number(),
         latest_restorable_date_time :: undefined | number(),
         point_in_time_recovery_status :: undefined | point_in_time_recovery_status()
        }).

-record(ddb2_continuous_backups_description,
        {continuous_backups_status :: undefined | continuous_backups_status(),
         point_in_time_recovery_description :: undefined | #ddb2_point_in_time_recovery_description{}
        }).

-record(ddb2_describe_continuous_backups,
        {continuous_backups_description :: undefined | #ddb2_continuous_backups_description{}
        }).

-record(ddb2_restore_table_from_backup,
        {table_description :: undefined | #ddb2_table_description{}
        }).

-record(ddb2_restore_table_to_point_in_time,
       {table_description :: undefined | #ddb2_table_description{}
       }).
-endif.
