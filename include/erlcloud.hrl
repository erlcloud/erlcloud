-ifndef(erlcloud_hrl).
-define(erlcloud_hrl, 0).

-type proplist() :: proplists:proplist().
-type datetime() :: {{pos_integer(), 1..12, 1..31}, {0..23, 0..59, 0..60}}.
-type paging_token() :: binary() | undefined.

-type success_result_paged(ObjectType) :: {ok, [ObjectType]}.
-type error_result() :: {error, Reason :: term()}.
-type result_paged(ObjectType) :: success_result_paged(ObjectType) | error_result().
-type result(ObjectType) :: {ok, [ObjectType]} | error_result().

-endif.