-module(erlcloud_mturk).

%% Library initialization.
-export([configure/2, configure/3, new/2, new/3]).

-export([decode_xml/1]).

%% Mechanical Turk API Functions
-export([
         approve_assignment/2, approve_assignment/3,
         assign_qualification/2, assign_qualification/3,
         assign_qualification/4, assign_qualification/5,
         block_worker/2, block_worker/3,
         change_hit_type_of_hit/2, change_hit_type_of_hit/3,
         create_hit/5, create_hit/6, create_hit/1, create_hit/2,
         create_qualification_type/1, create_qualification_type/2,
         disable_hit/1, disable_hit/2,
         dispose_hit/1, dispose_hit/2,
         dispose_qualification_type/1, dispose_qualification_type/2,
         extend_hit/3, extend_hit/4,
         force_expire_hit/1, force_expire_hit/2,
         get_account_balance/0, get_account_balance/1,
         get_assignments_for_hit/1, get_assignments_for_hit/2, get_assignments_for_hit/3,
         get_bonus_payments_for_hit/2, get_bonus_payments_for_hit/3,
         get_bonus_payments_for_assignment/2, get_bonus_payments_for_assignment/3,
         get_file_upload_url/2, get_file_upload_url/3,
         get_hit/1, get_hit/2,
         get_hits_for_qualification_type/1, get_hits_for_qualification_type/2,
         get_hits_for_qualification_type/3,
         get_qualification_requests/0, get_qualification_requests/1,
         get_qualification_requests/2,
         get_qualification_score/2, get_qualification_score/3,
         get_qualification_type/1, get_qualification_type/2,
         get_qualifications_for_qualification_type/1,
         get_qualifications_for_qualification_type/2,
         get_qualifications_for_qualification_type/3,
         get_requester_statistic/2, get_requester_statistic/3,
         get_requester_statistic/4,
         get_reviewable_hits/0, get_reviewable_hits/1, get_reviewable_hits/2,
         grant_bonus/4, grant_bonus/5,
         grant_qualification/1, grant_qualification/2, grant_qualification/3,
         notify_workers/3, notify_workers/4,
         register_hit_type/1, register_hit_type/2,
         reject_assignment/1, reject_assignment/2, reject_assignment/3,
         reject_qualification_request/1, reject_qualification_request/2,
         reject_qualification_request/3,
         revoke_qualification/2, revoke_qualification/3, revoke_qualification/4,
         search_hits/0, search_hits/1, search_hits/2,
         search_qualification_types/0, search_qualification_types/1,
         search_qualification_types/2,
         send_test_event_notification/2, send_test_event_notification/3,
         set_hit_as_reviewing/1, set_hit_as_reviewing/2, set_hit_as_reviewing/3,
         set_hit_type_notification/2, set_hit_type_notification/3,
         set_hit_type_notification/4,
         unblock_worker/1, unblock_worker/2,
         update_qualification_score/3, update_qualification_score/4,
         update_qualification_type/1, update_qualification_type/2
        ]).

-include("erlcloud.hrl").
-include("erlcloud_aws.hrl").
-include("erlcloud_mturk.hrl").
-include_lib("xmerl/include/xmerl.hrl").

-define(API_VERSION, "2008-08-02").
-define(API_SERVICE, "AWSMechanicalTurkRequester").
-define(DEFAULT_NOTIFICATION_VERSION, "2006-05-05").
-define(XMLNS_QUESTIONFORM, "http://mechanicalturk.amazonaws.com/AWSMechanicalTurkDataSchemas/2005-10-01/QuestionForm.xsd").
-define(XMLNS_EXTERNALQUESTION, "http://mechanicalturk.amazonaws.com/AWSMechanicalTurkDataSchemas/2006-07-14/ExternalQuestion.xsd").
-define(XMLNS_ANSWERKEY, "http://mechanicalturk.amazonaws.com/AWSMechanicalTurkDataSchemas/2005-10-01/AnswerKey.xsd").

-spec new(string(), string()) -> aws_config().
new(AccessKeyId, SecretAccessKey) ->
    #aws_config{access_key_id=AccessKeyId,
                secret_access_key=SecretAccessKey}.

-spec new(string(), string(), string()) -> aws_config().
new(AccessKeyId, SecretAccessKey, Host) ->
    #aws_config{access_key_id=AccessKeyId,
                secret_access_key=SecretAccessKey,
                mturk_host=Host}.

-spec configure(string(), string()) -> ok.
configure(AccessKeyId, SecretAccessKey) ->
    put(aws_config, new(AccessKeyId, SecretAccessKey)),
    ok.

-spec configure(string(), string(), string()) -> ok.
configure(AccessKeyId, SecretAccessKey, Host) ->
    put(aws_config, new(AccessKeyId, SecretAccessKey, Host)),
    ok.

default_config() -> erlcloud_aws:default_config().

-spec approve_assignment(string(), string() | none) -> ok | no_return().
approve_assignment(AssignmentId, RequesterFeedback) ->
    approve_assignment(AssignmentId, RequesterFeedback, default_config()).

-spec approve_assignment(string(), string() | none, aws_config()) -> ok | no_return().
approve_assignment(AssignmentId, RequesterFeedback, Config)
  when is_list(AssignmentId),
       is_list(RequesterFeedback) orelse RequesterFeedback =:= none ->
    mturk_simple_request(Config, "ApproveAssignment",
                         [{"AssignmentId", AssignmentId}, {"RequesterFeedback", RequesterFeedback}]).

-spec assign_qualification(string(), string()) -> ok | no_return().
assign_qualification(QualificationTypeId, WorkerId) ->
    assign_qualification(QualificationTypeId, WorkerId, default_config()).

-spec assign_qualification(string(), string(), integer() | aws_config()) -> ok | no_return().
assign_qualification(QualificationTypeId, WorkerId, Config)
  when is_record(Config, aws_config) ->
    assign_qualification(QualificationTypeId, WorkerId, 1, Config);
assign_qualification(QualificationTypeId, WorkerId, IntegerValue) ->
    assign_qualification(QualificationTypeId, WorkerId, IntegerValue, false).


-spec assign_qualification(string(), string(), integer(), boolean() | aws_config()) -> ok | no_return().
assign_qualification(QualificationTypeId, WorkerId, IntegerValue, Config)
  when is_record(Config, aws_config) ->
    assign_qualification(QualificationTypeId, WorkerId, IntegerValue, false, Config);
assign_qualification(QualificationTypeId, WorkerId, IntegerValue, SendNotification) ->
    assign_qualification(QualificationTypeId, WorkerId, IntegerValue,
                         SendNotification, default_config()).

-spec assign_qualification(string(), string(), integer(), boolean(), aws_config()) -> ok | no_return().
assign_qualification(QualificationTypeId, WorkerId, IntegerValue, SendNotification,
                     Config)
  when is_list(QualificationTypeId), is_list(WorkerId),
       is_integer(IntegerValue), is_boolean(SendNotification) ->
    mturk_simple_request(Config, "AssignQualification",
                         [{"QualificationTypeId", QualificationTypeId},
                          {"WorkerId", WorkerId},
                          {"IntegerValue", IntegerValue},
                          {"SendNotification", SendNotification}]).

-spec block_worker(string(), string()) -> ok | no_return().
block_worker(WorkerId, Reason) -> block_worker(WorkerId, Reason, default_config()).

-spec block_worker(string(), string(), aws_config()) -> ok | no_return().
block_worker(WorkerId, Reason, Config)
  when is_list(WorkerId), is_list(Reason) ->
    mturk_simple_request(Config, "BlockWorker",
                         [{"WorkerId", WorkerId}, {"Reason", Reason}]).

-spec change_hit_type_of_hit(string(), string()) -> ok | no_return().
change_hit_type_of_hit(HITId, HITTypeId) ->
    change_hit_type_of_hit(HITId, HITTypeId, default_config()).

-spec change_hit_type_of_hit(string(), string(), aws_config()) -> ok | no_return().
change_hit_type_of_hit(HITId, HITTypeId, Config)
  when is_list(HITId), is_list(HITTypeId) ->
    mturk_simple_request(Config, "ChangeHITTypeOfHIT",
                         [{"HITId", HITId}, {"HITTypeId", HITTypeId}]).

-spec create_hit(string(), mturk_question(), 30..3153600,
                       1..1000000000, string() | none) -> proplist() | no_return().
create_hit(HITTypeId, Question, LifetimeInSeconds, MaxAssignments,
           RequesterAnnotation) ->
    create_hit(HITTypeId, Question, LifetimeInSeconds, MaxAssignments,
               RequesterAnnotation, default_config()).

-spec create_hit(string(), mturk_question(), 30..3153600,
                       1..1000000000, string() | none, aws_config()) -> proplist() | no_return().
create_hit(HITTypeId, Question, LifetimeInSeconds, MaxAssignments,
           RequesterAnnotation, Config)
  when is_list(HITTypeId),
       is_integer(LifetimeInSeconds),
       LifetimeInSeconds >= 30, LifetimeInSeconds =< 31536000,
       is_integer(MaxAssignments),
       MaxAssignments >= 1, MaxAssignments =< 1000000000,
       is_list(RequesterAnnotation) orelse RequesterAnnotation =:= none ->
    QuestionXML = xml_to_string(encode_xml(Question)),
    Params = [
              {"HITTypeId", HITTypeId},
              {"Question", QuestionXML},
              {"LifetimeInSeconds", LifetimeInSeconds},
              {"MaxAssignments", MaxAssignments},
              {"RequesterAnnotation", RequesterAnnotation}
             ],
    Doc = mturk_xml_request(Config, "CreateHIT", Params),
    erlcloud_xml:decode(
      [
       {hit_id, "HITId", text},
       {hit_type_id, "HITTypeId", text}
      ],
      Doc
     ).

-spec create_hit(#mturk_hit{}) -> proplist() | no_return().
create_hit(HIT) ->
    create_hit(HIT, default_config()).

-spec create_hit(#mturk_hit{}, aws_config()) -> proplist() | no_return().
create_hit(HIT, Config) ->
    QuestionXML = xml_to_string(encode_xml(HIT#mturk_hit.question)),
    Params = [
              {"Title", HIT#mturk_hit.title},
              {"Description", HIT#mturk_hit.description},
              {"AssignmentDurationInSeconds", HIT#mturk_hit.assignment_duration_in_seconds},
              {"LifetimeInSeconds", HIT#mturk_hit.lifetime_in_seconds},
              {"Keywords", string:join(HIT#mturk_hit.keywords, ",")},
              {"MaxAssignments", HIT#mturk_hit.max_assignments},
              {"AutoApprovalDelayInSeconds", HIT#mturk_hit.auto_approval_delay_in_seconds},
              {"RequesterAnnotation", HIT#mturk_hit.requester_annotation},
              {"Question", QuestionXML}] ++
        erlcloud_aws:param_list([encode_money(HIT#mturk_hit.reward)], "Reward") ++
        erlcloud_aws:param_list([encode_qualification_requirement(QR) || QR <- HIT#mturk_hit.qualification_requirements],
                                "QualificationRequirement"),
    Doc = mturk_xml_request(Config, "CreateHIT", Params),
    erlcloud_xml:decode(
      [
       {hit_id, "HITId", text},
       {hit_type_id, "HITTypeId", text}
      ],
      Doc
     ).

encode_qualification_requirement(QR) ->
    [
     {"QualificationTypeId", QR#mturk_qualification_requirement.qualification_type_id},
     {"Comparator", encode_comparator(QR#mturk_qualification_requirement.comparator)},
     {"IntegerValue", QR#mturk_qualification_requirement.integer_value},
     {"LocaleValue", encode_locale_value(QR#mturk_qualification_requirement.locale_value)},
     {"RequiredToPreview", QR#mturk_qualification_requirement.required_to_preview}
    ].

encode_comparator('<') -> "LessThan";
encode_comparator('=<') -> "LessThanOrEqualTo";
encode_comparator('>') -> "GreaterThan";
encode_comparator('>=') -> "GreaterThanOrEqualTo";
encode_comparator('==') -> "EqualTo";
encode_comparator('/=') -> "NotEqualTo";
encode_comparator(exists) -> "Exists".

decode_comparator("LessThan") -> '<';
decode_comparator("LessThanOrEqualTo") -> '=<';
decode_comparator("GreaterThan") -> '>';
decode_comparator("GreaterThanOrEqualTo") -> '>=';
decode_comparator("EqualTo") -> '==';
decode_comparator("NotEqualTo") -> '/=';
decode_comparator("Exists") -> exists.

encode_locale_value(undefined) -> [];
encode_locale_value(#mturk_locale{country_code=Country}) ->
    [{"Country", Country}].

-spec create_qualification_type(#mturk_qualification_type{}) -> proplist() | no_return().
create_qualification_type(QType) ->
    create_qualification_type(QType, default_config()).

-spec create_qualification_type(#mturk_qualification_type{}, aws_config()) -> proplist() | no_return().
create_qualification_type(QType, Config)
  when is_record(QType, mturk_qualification_type) ->
    Doc = mturk_xml_request(Config, "CreateQualificationType",
                            qualification_type_params(QType)),
    hd(extract_qualification_types([Doc])).

qualification_type_params(QType) ->
    #mturk_qualification_type{name=Name, description=Description, keywords=Keywords,
                              retry_delay_in_seconds=RTD, qualification_type_status=InitialStatus,
                              test=Test, answer_key=AnswerKey, test_duration_in_seconds=TD,
                              auto_granted=AutoGranted, auto_granted_value=AutoGrantedValue} = QType,
    [
     {"Name", Name},
     {"Description", Description},
     {"Keywords", string:join(Keywords, ",")},
     {"RetryDelayInSeconds", RTD},
     {"QualificationTypeStatus", encode_qualification_type_status(InitialStatus)},
     {"Test", xml_to_string(encode_xml(Test))},
     {"AnswerKey", xml_to_string(encode_xml(AnswerKey))},
     {"TestDurationInSeconds", TD},
     {"AutoGranted", AutoGranted},
     {"AutoGrantedValue", case AutoGranted of true -> AutoGrantedValue; false -> undefined end}
    ].

-spec disable_hit(string()) -> ok | no_return().
disable_hit(HITId) -> disable_hit(HITId, default_config()).

-spec disable_hit(string(), aws_config()) -> ok | no_return().
disable_hit(HITId, Config)
  when is_list(HITId) ->
    mturk_simple_request(Config, "DisableHIT", [{"HITId", HITId}]).

-spec dispose_hit(string()) -> ok | no_return().
dispose_hit(HITId) -> dispose_hit(HITId, default_config()).

-spec dispose_hit(string(), aws_config()) -> ok | no_return().
dispose_hit(HITId, Config)
  when is_list(HITId) ->
    mturk_simple_request(Config, "DisposeHIT", [{"HITId", HITId}]).

-spec dispose_qualification_type(string()) -> ok | no_return().
dispose_qualification_type(QualificationTypeId) ->
    dispose_qualification_type(QualificationTypeId, default_config()).

-spec dispose_qualification_type(string(), aws_config()) -> ok | no_return().
dispose_qualification_type(QualificationTypeId, Config)
  when is_list(QualificationTypeId) ->
    mturk_simple_request(Config, "DisposeQualificationType",
                         [{"QualificationTypeId", QualificationTypeId}]).

-spec extend_hit(string(), 1..1000000000 | none, 3600..31536000 | none) -> ok | no_return().
extend_hit(HITId, MaxAssignmentsIncrement, ExpirationIncrementInSeconds) ->
    extend_hit(HITId, MaxAssignmentsIncrement, ExpirationIncrementInSeconds,
               default_config()).

-spec extend_hit(string(), 1..1000000000 | none, 3600..31536000 | none, aws_config()) -> ok | no_return().
extend_hit(HITId, MaxAssignmentsIncrement, ExpirationIncrementInSeconds, Config)
  when is_list(HITId),
       (MaxAssignmentsIncrement >= 1 andalso MaxAssignmentsIncrement =< 1000000000) orelse MaxAssignmentsIncrement =:= none,
       (ExpirationIncrementInSeconds >= 3600 andalso ExpirationIncrementInSeconds =< 31536000) orelse ExpirationIncrementInSeconds =:= none,
       MaxAssignmentsIncrement =/= none orelse ExpirationIncrementInSeconds =/= none ->
    mturk_simple_request(Config, "ExtendHIT",
                         [{"HITId", HITId},
                          {"MaxAssignmentsIncrement", MaxAssignmentsIncrement},
                          {"ExpirationIncrementInSeconds", ExpirationIncrementInSeconds}]).

-spec force_expire_hit(string()) -> ok | no_return().
force_expire_hit(HITId) -> force_expire_hit(HITId, default_config()).

-spec force_expire_hit(string(), aws_config()) -> ok | no_return().
force_expire_hit(HITId, Config)
  when is_list(HITId) ->
    mturk_simple_request(Config, "ForceExpireHIT", [{"HITId", HITId}]).

-spec get_account_balance() -> proplist() | no_return().
get_account_balance() ->
    get_account_balance(default_config()).

-spec get_account_balance(aws_config()) -> proplist() | no_return().
get_account_balance(Config) ->
    Doc = mturk_xml_request(Config, "GetAccountBalance", []),
    erlcloud_xml:decode(
      [
       {available_balance, "AvailableBalance", {single, fun extract_money/1}},
       {on_hold_balance, "OnHoldBalance", {single, fun extract_money/1}}
      ],
      Doc
     ).

-spec get_assignments_for_hit(string()) -> proplist() | no_return().
get_assignments_for_hit(HITId) ->
    get_assignments_for_hit(HITId, []).

-spec get_assignments_for_hit(string(), proplist() | aws_config()) -> proplist() | no_return().
get_assignments_for_hit(HITId, Config)
  when is_record(Config, aws_config) ->
    get_assignments_for_hit(HITId, [], Config);
get_assignments_for_hit(HITId, Options) ->
    get_assignments_for_hit(HITId, Options, default_config()).

-spec get_assignments_for_hit(string(), proplist(), aws_config()) -> proplist() | no_return().
get_assignments_for_hit(HITId, Options, Config)
  when is_list(HITId), is_list(Options) ->
    Params = [
              {"HITId", HITId},
              {"AssignmentStatus",
               case proplists:get_value(assignment_status, Options) of
                   undefined -> undefined;
                   submitted -> "Submitted";
                   approved -> "Approved";
                   rejected -> "Rejected"
               end
              },
              {"SortProperty",
               case proplists:get_value(sort_property, Options) of
                   undefined -> undefined;
                   accept_time -> "AcceptTime";
                   submit_time -> "SubmitTime";
                   assignment_status -> "AssignmentStatus"
               end
              },
              {"SortDirection",
               case proplists:get_value(sort_direction, Options) of
                   undefined -> undefined;
                   ascending -> "Ascending";
                   descending -> "Descending"
               end
              },
              {"PageSize", proplists:get_value(page_size, Options)},
              {"PageNumber", proplists:get_value(page_number, Options)}
             ],
    Doc = mturk_xml_request(Config, "GetAssignmentsForHIT", Params),
    erlcloud_xml:decode(
      [
       {num_results, "NumResults", integer},
       {page_number, "PageNumber", integer},
       {total_num_results, "TotalNumResults", integer},
       {assignments, "Assignment", fun extract_assignments/1}
      ],
      Doc
     ).

extract_assignments(Assignments) ->
    [extract_assignment(Assignment) || Assignment <- Assignments].

extract_assignment(Assignment) ->
    erlcloud_xml:decode(
      [
       {assignment_id, "AssignmentId", text},
       {worker_id, "WorkerId", text},
       {hit_id, "HITId", text},
       {assignment_status, "AssignmentStatus", text},
       {auto_approval_time, "AutoApprovalTime", time},
       {accept_time, "AcceptTime", time},
       {submit_time, "SubmitTime", time},
       {approval_time, "ApprovalTime", time},
       {answers, "Answer", {single, fun decode_xml/1}}
      ],
      Assignment
     ).

-spec get_bonus_payments_for_hit(string(), proplist()) -> proplist() | no_return().
get_bonus_payments_for_hit(HITId, Options) ->
    get_bonus_payments_for_hit(HITId, Options, default_config()).

-spec get_bonus_payments_for_hit(string(), proplist(), aws_config()) -> proplist() | no_return().
get_bonus_payments_for_hit(HITId, Options, Config)
  when is_list(HITId), is_list(Options) ->
    Params = [
              {"HITId", HITId},
              {"PageSize", proplists:get_value(page_size, Options)},
              {"PageNumber", proplists:get_value(page_number, Options)}
             ],
    Doc = mturk_xml_request(Config, "GetBonusPayments", Params),
    extract_bonus_payments(Doc).

-spec get_bonus_payments_for_assignment(string(), proplist()) -> proplist() | no_return().
get_bonus_payments_for_assignment(AssignmentId, Options) ->
    get_bonus_payments_for_assignment(AssignmentId, Options, default_config()).

-spec get_bonus_payments_for_assignment(string(), proplist(), aws_config()) -> proplist() | no_return().
get_bonus_payments_for_assignment(AssignmentId, Options, Config)
  when is_list(AssignmentId), is_list(Options) ->
    Params = [
              {"AssignmentId", AssignmentId},
              {"PageSize", proplists:get_value(page_size, Options)},
              {"PageNumber", proplists:get_value(page_number, Options)}
             ],
    Doc = mturk_xml_request(Config, "GetBonusPayments", Params),
    extract_bonus_payments(Doc).

extract_bonus_payments(Payments) when is_list(Payments) ->
    [extract_bonus_payment(Payment) || Payment <- Payments];
extract_bonus_payments(Doc) ->
    erlcloud_xml:decode(
      [
       {num_results, "NumResults", integer},
       {page_number, "PageNumber", integer},
       {total_num_results, "TotalNumResults", integer},
       {bonus_payments, "BonusPayment", fun extract_bonus_payments/1}
      ],
      Doc
     ).

extract_bonus_payment(Payment) ->
    erlcloud_xml:decode(
      [
       {worker_id, "WorkerId", text},
       {bonus_amount, "BonusAmount", {single, fun extract_money/1}},
       {assignment_id, "AssignmentId", text},
       {reason, "Reason", text},
       {grant_time, "GrantTime", time}
      ],
      Payment
     ).

-spec get_file_upload_url(string(), string()) -> string() | no_return().
get_file_upload_url(AssignmentId, QuestionIdentifier) ->
    get_file_upload_url(AssignmentId, QuestionIdentifier, default_config()).

-spec get_file_upload_url(string(), string(), aws_config()) -> string() | no_return().
get_file_upload_url(AssignmentId, QuestionIdentifier, Config)
  when is_record(Config, aws_config) ->
    Params = [
              {"AssignmentId", AssignmentId},
              {"QuestionIdentifier", QuestionIdentifier}
             ],
    Doc = mturk_xml_request(Config, "GetFileUploadURL", Params),
    erlcloud_xml:get_text("FileUploadURL", Doc).

-spec get_hit(string()) -> #mturk_hit{} | no_return().
get_hit(HITId) -> get_hit(HITId, default_config()).

-spec get_hit(string(), aws_config()) -> #mturk_hit{} | no_return().
get_hit(HITId, Config)
  when is_list(HITId) ->
    Doc = mturk_xml_request(Config, "GetHIT", [{"HITId", HITId}]),
    hd(extract_hits([Doc])).

-spec get_hits_for_qualification_type(string()) -> proplist() | no_return().
get_hits_for_qualification_type(QualificationTypeId) ->
    get_hits_for_qualification_type(QualificationTypeId, []).

-spec get_hits_for_qualification_type(string(), proplist() | aws_config()) -> proplist() | no_return().
get_hits_for_qualification_type(QualificationTypeId, Config)
  when is_record(Config, aws_config) ->
    get_hits_for_qualification_type(QualificationTypeId, [], Config);
get_hits_for_qualification_type(QualificationTypeId, Options) ->
    get_hits_for_qualification_type(QualificationTypeId, Options, default_config()).

-spec get_hits_for_qualification_type(string(), proplist(), aws_config()) -> proplist() | no_return().
get_hits_for_qualification_type(QualificationTypeId, Options, Config)
  when is_list(Options) ->
    Params = [
              {"QualificationTypeId", QualificationTypeId},
              {"PageSize", proplists:get_value(page_size, Options)},
              {"PageNumber", proplists:get_value(page_number, Options)}
             ],
    Doc = mturk_xml_request(Config, "GetHITsForQualificationType", Params),
    erlcloud_xml:decode(
      [
       {num_results, "NumResults", integer},
       {page_number, "PageNumber", integer},
       {total_num_results, "TotalNumResults", integer},
       {hits, "HIT", fun extract_hits/1}
      ],
      Doc
     ).

-spec get_reviewable_hits() -> proplist() | no_return().
get_reviewable_hits() ->
    get_reviewable_hits([]).

-spec get_reviewable_hits(proplist() | aws_config()) -> proplist() | no_return().
get_reviewable_hits(Config)
  when is_record(Config, aws_config) ->
    get_reviewable_hits([], Config);
get_reviewable_hits(Options) ->
    get_reviewable_hits(Options, default_config()).

-spec get_reviewable_hits(proplist(), aws_config()) -> proplist() | no_return().
get_reviewable_hits(Options, Config)
  when is_list(Options) ->
    Params = [
              {"HITTypeId", proplists:get_value(hit_type_id, Options)},
              {"Status",
               case proplists:get_value(status, Options) of
                   undefined -> undefined;
                   reviewable -> "Reviewable";
                   reviewing -> "Reviewing"
               end
              },
              {"SortProperty",
               case proplists:get_value(sort_property, Options) of
                   undefined -> undefined;
                   title -> "Title";
                   reward -> "Reward";
                   expiration -> "Expiration";
                   creation_time -> "CreationTime";
                   enumeration -> "Enumeration"
               end
              },
              {"SortDirection",
               case proplists:get_value(sort_direction, Options) of
                   undefined -> undefined;
                   ascending -> "Ascending";
                   descending -> "Descending"
               end
              },
              {"PageSize", proplists:get_value(page_size, Options)},
              {"PageNumber", proplists:get_value(page_number, Options)}
             ],
    Doc = mturk_xml_request(Config, "GetReviewableHITs", Params),
    erlcloud_xml:decode(
      [
       {num_results, "NumResults", integer},
       {page_number, "PageNumber", integer},
       {total_num_results, "TotalNumResults", integer},
       {hit_ids, "HIT/HITId", list}
      ],
      Doc
     ).

-spec get_qualification_score(string(), string()) -> proplist() | no_return().
get_qualification_score(QualificationTypeId, SubjectId) ->
    get_qualification_score(QualificationTypeId, SubjectId, default_config()).

-spec get_qualification_score(string(), string(), aws_config()) -> proplist() | no_return().
get_qualification_score(QualificationTypeId, SubjectId, Config)
  when is_list(QualificationTypeId), is_list(SubjectId) ->
    Doc = mturk_xml_request(Config, "GetQualificationScore",
                            [
                             {"QualificationTypeId", QualificationTypeId},
                             {"SubjectId", SubjectId}
                            ]
                           ),
    erlcloud_xml:decode(
      [
       {qualification_type_id, "QualificationTypeId", text},
       {subject_id, "SubjectId", optional_text},
       {grant_time, "GrantTime", time},
       {integer_value, "IntegerValue", integer}
      ],
      Doc
     ).

-spec get_qualification_type(string()) -> #mturk_qualification_type{} | no_return().
get_qualification_type(QualificationTypeId) ->
    get_qualification_type(QualificationTypeId, default_config()).

-spec get_qualification_type(string(), aws_config()) -> #mturk_qualification_type{} | no_return().
get_qualification_type(QualificationTypeId, Config)
  when is_record(Config, aws_config) ->
    Doc = mturk_xml_request(Config, "GetQualificationType",
                            [{"QualificationTypeId", QualificationTypeId}]),
    extract_qualification_type(Doc).

extract_qualification_types(Nodes) ->
    [extract_qualification_type(Node) || Node <- Nodes].

extract_qualification_type(Node) ->
    erlcloud_xml:decode(
      [
       {#mturk_qualification_type.qualification_type_id,
        "QualificationTypeId", text},
       {#mturk_qualification_type.creation_time,
        "CreationTime", time},
       {#mturk_qualification_type.name, "Name", text},
       {#mturk_qualification_type.description, "Description", text},
       {#mturk_qualification_type.keywords, "Keywords", {value, fun decode_keywords/1}},
       {#mturk_qualification_type.qualification_type_status, "QualificationTypeStatus",
        {value, fun decode_qualification_type_status/1}},
       {#mturk_qualification_type.retry_delay_in_seconds, "RetryDelayInSeconds", integer},
       {#mturk_qualification_type.is_requestable, "IsRequestable", optional_boolean},
       {#mturk_qualification_type.test, "Test", {single, fun decode_xml/1}},
       {#mturk_qualification_type.answer_key, "AnswerKey", {single, fun decode_xml/1}},
       {#mturk_qualification_type.test_duration_in_seconds, "TestDurationInSeconds", integer}
      ],
      Node,
      #mturk_qualification_type{}
     ).

decode_keywords(String) ->
    [string:strip(Keyword) || Keyword <- string:tokens(String, ",")].

-spec get_qualifications_for_qualification_type(string()) -> [proplist()] | no_return().
get_qualifications_for_qualification_type(QualificationTypeId) ->
    get_qualifications_for_qualification_type(QualificationTypeId, default_config()).

-spec get_qualifications_for_qualification_type(string(), proplist() | aws_config()) -> [proplist()] | no_return().
get_qualifications_for_qualification_type(QualificationTypeId, Config)
  when is_record(Config, aws_config) ->
    get_qualifications_for_qualification_type(QualificationTypeId, [], Config);
get_qualifications_for_qualification_type(QualificationTypeId, Options) ->
    get_qualifications_for_qualification_type(QualificationTypeId, Options, default_config()).

-spec get_qualifications_for_qualification_type(string(), proplist(), aws_config()) -> [proplist()] | no_return().
get_qualifications_for_qualification_type(QualificationTypeId, Options, Config)
  when is_list(QualificationTypeId), is_list(Options) ->
    Params = [
              {"QualificationTypeId", QualificationTypeId},
              {"Status",
               case proplists:get_value(status, Options) of
                   undefined -> undefined;
                   granted -> "Granted";
                   revoked -> "Revoked"
               end
              },
              {"PageSize", proplists:get_value(page_size, Options)},
              {"PageNumber", proplists:get_value(page_number, Options)}
             ],
    Doc = mturk_xml_request(Config, "GetQualificationsForQualificationType", Params),
    [erlcloud_xml:decode(
       [
        {qualification_type_id, "QualificationTypeId", text},
        {subject_id, "SubjectId", optional_text},
        {grant_time, "GrantTime", time},
        {integer_value, "IntegerValue", integer}
       ],
       Item
      ) || Item <- xmerl_xpath:string("Qualification", Doc)].

-spec get_qualification_requests() -> proplist() | no_return().
get_qualification_requests() ->
    get_qualification_requests([]).

-spec get_qualification_requests(proplist() | aws_config()) -> proplist() | no_return().
get_qualification_requests(Config)
  when is_record(Config, aws_config) ->
    get_qualification_requests([], Config);
get_qualification_requests(Options) ->
    get_qualification_requests(Options, default_config()).

-spec get_qualification_requests(proplist(), aws_config()) -> proplist() | no_return().
get_qualification_requests(Options, Config)
  when is_list(Options) ->
    Params = [
              {"QualificationTypeId", proplists:get_value(qualification_type_id, Options)},
              {"SortProperty",
               case proplists:get_value(sort_property, Options) of
                   undefined -> undefined;
                   qualification_type_id -> "QualificationTypeId";
                   submit_time -> "SubmitTime"
               end
              },
              {"SortDirection",
               case proplists:get_value(sort_direction, Options) of
                   undefined -> undefined;
                   ascending -> "Ascending";
                   descending -> "Descending"
               end
              },
              {"PageSize", proplists:get_value(page_size, Options)},
              {"PageNumber", proplists:get_value(page_number, Options)}
             ],
    Doc = mturk_xml_request(Config, "GetQualificationRequests", Params),
    erlcloud_xml:decode(
      [
       {num_results, "NumResults", integer},
       {page_number, "PageNumber", integer},
       {total_num_results, "TotalNumResults", integer},
       {qualification_requests, "QualificationRequest", fun extract_qualification_requests/1}
      ],
      Doc
     ).

extract_qualification_requests(Requests) ->
    [extract_qualification_request(Request) || Request <- Requests].

extract_qualification_request(Request) ->
    erlcloud_xml:decode(
      [
       {qualification_request_id, "QualificationRequestId", text},
       {qualification_type_id, "QualificationTypeId", text},
       {subject_id, "SubjectId", text},
       {test, "Test", {single, fun decode_xml/1}},
       {answer, "Answer", {single, fun decode_xml/1}},
       {submit_time, "SubmitTime", time}
      ],
      Request
     ).

-spec get_requester_statistic(string(), one_day | seven_days | thirty_days | life_to_date) -> [{datetime(), float()}] | no_return().
get_requester_statistic(Statistic, TimePeriod) ->
    get_requester_statistic(Statistic, TimePeriod, default_config()).

-spec get_requester_statistic(string(), one_day | seven_days | thirty_days | life_to_date, pos_integer() | aws_config()) -> [{datetime(), float()}] | no_return().
get_requester_statistic(Statistic, TimePeriod, Config)
  when is_record(Config, aws_config) ->
    get_requester_statistic(Statistic, TimePeriod, 1, Config);
get_requester_statistic(Statistic, TimePeriod, Count) ->
    get_requester_statistic(Statistic, TimePeriod, Count, default_config()).

-spec get_requester_statistic(string(), one_day | seven_days | thirty_days | life_to_date, pos_integer(), aws_config()) -> [{datetime(), float()}] | no_return().
get_requester_statistic(Statistic, TimePeriod, Count, Config)
  when is_list(Statistic),
       TimePeriod =:= one_day orelse TimePeriod =:= seven_days orelse
       TimePeriod =:= thirty_days orelse TimePeriod =:= life_to_date,
       is_integer(Count), Count >= 1 ->
    Doc = mturk_xml_request(Config, "GetRequesterStatistic",
                            [
                             {"Statistic", Statistic},
                             {"TimePeriod",
                              case TimePeriod of
                                  one_day -> "OneDay";
                                  seven_days -> "SevenDays";
                                  thirty_days -> "ThirtyDays";
                                  life_to_date -> "LifeToDate"
                              end
                             }
                            ]
                           ),
    [{erlcloud_xml:get_time("Date", DP),
      case erlcloud_xml:get_text("DoubleValue", DP) of
          [] -> erlcloud_xml:get_integer("LongValue", DP);
          DVal -> try list_to_float(DVal) catch error:badarg -> list_to_integer(DVal) + 0.0 end
      end} ||
        DP <- xmerl_xpath:string("DataPoint", Doc)].

-spec grant_bonus(string(), string(), #mturk_money{}, string()) -> ok | no_return().
grant_bonus(WorkerId, AssignmentId, BonusAmount, Reason) ->
    grant_bonus(WorkerId, AssignmentId, BonusAmount, Reason, default_config()).

-spec grant_bonus(string(), string(), #mturk_money{}, string(), aws_config()) -> ok | no_return().
grant_bonus(WorkerId, AssignmentId, BonusAmount, Reason, Config) ->
    mturk_simple_request(Config, "GrantBonus",
                         [
                          {"WorkerId", WorkerId},
                          {"AssignmentId", AssignmentId},
                          {"Reason", Reason}|
                          erlcloud_aws:param_list([encode_money(BonusAmount)], "BonusAmount")
                         ]
                        ).

-spec grant_qualification(string()) -> ok | no_return().
grant_qualification(QualificationRequestId) ->
    grant_qualification(QualificationRequestId, none).

-spec grant_qualification(string(), integer() | none | aws_config()) -> ok | no_return().
grant_qualification(QualificationRequestId, Config)
  when is_record(Config, aws_config) ->
    grant_qualification(QualificationRequestId, none, Config);
grant_qualification(QualificationRequestId, Value) ->
    grant_qualification(QualificationRequestId, Value, default_config()).

-spec grant_qualification(string(), integer() | none, aws_config()) -> ok | no_return().
grant_qualification(QualificationRequestId, Value, Config)
  when is_list(QualificationRequestId),
       is_integer(Value) orelse Value =:= none ->
    mturk_simple_request(Config, "GrantQualification",
                         [
                          {"QualificationRequestId", QualificationRequestId},
                          {"IntegerValue", Value}
                         ]
                        ).

extract_hits(HITs) ->
    [extract_hit(HIT) || HIT <- HITs].

extract_hit(HIT) ->
    erlcloud_xml:decode(
      [
       {#mturk_hit.hit_id, "HITId", text},
       {#mturk_hit.hit_type_id, "HITTypeId", text},
       {#mturk_hit.creation_time, "CreationTime", time},
       {#mturk_hit.title, "Title", text},
       {#mturk_hit.description, "Description", text},
       {#mturk_hit.keywords, "Keywords", {value, fun decode_keywords/1}},
       {#mturk_hit.requester_annotation, "RequesterAnnotation", text},
       {#mturk_hit.qualification_requirements, "QualificationRequirement", fun extract_qualification_requirements/1},
       {#mturk_hit.question, "Question", {single, fun decode_xml/1}},
       {#mturk_hit.hit_status, "HITStatus", text},
       {#mturk_hit.max_assignments, "MaxAssignments", integer},
       {#mturk_hit.reward, "Reward", {single, fun extract_money/1}},
       {#mturk_hit.auto_approval_delay_in_seconds, "AutoApprovalDelayInSeconds", integer},
       {#mturk_hit.expiration, "Expiration", time},
       {#mturk_hit.assignment_duration_in_seconds, "AssignmentDurationInSeconds", integer},
       {#mturk_hit.number_of_assignments_pending, "NumberOfAssignmentsPending", optional_integer},
       {#mturk_hit.number_of_assignments_available, "NumberOfAssignmentsAvailable", optional_integer},
       {#mturk_hit.number_of_assignments_completed, "NumberOfAssignmentsCompleted", optional_integer},
       {#mturk_hit.number_of_similar_hits, "NumberOfSimilarHITs", optional_integer},
       {#mturk_hit.hit_review_status, "HITReviewStatus", optional_text}
      ],
      HIT,
      #mturk_hit{}
     ).

decode_xml(List) when is_list(List) ->
    [decode_xml(Element) || Element <- List, is_record(Element, xmlElement)];
decode_xml(#xmlElement{name='Answer', parents=[{'QuestionFormAnswers', _}|_]} = Element) ->
    erlcloud_xml:decode(
      [
       {#mturk_answer.question_identifier, "QuestionIdentifier", text},
       {#mturk_answer.free_text, "FreeText", optional_text},
       {#mturk_answer.selection_identifier, "SelectionIdentifier", optional_text},
       {#mturk_answer.uploaded_file_size_in_bytes, "UploadedFileSizeInBytes", optional_integer},
       {#mturk_answer.uploaded_file_key, "UploadedFileKey", optional_text}
      ],
      Element,
      #mturk_answer{}
     );
decode_xml(#xmlElement{name='Answer'} = Element) ->
    AnswerXML = element(1,xmerl_scan:string(erlcloud_xml:get_text(Element))),
    decode_xml(AnswerXML);
decode_xml(#xmlElement{name='QuestionFormAnswers', content=Content}) ->
    decode_xml(Content);
decode_xml(#xmlElement{name='AnswerKey', parents=[]} = Element) ->
    erlcloud_xml:decode(
      [
       {#mturk_answer_key.answers, "Question", fun decode_xml/1},
       {#mturk_answer_key.qualification_value_mapping, "QualificationValueMapping", {single, fun decode_xml/1}}
      ],
      Element,
      #mturk_answer_key{}
     );
decode_xml(#xmlElement{name='QualificationValueMapping', content=Content}) ->
    hd(decode_xml(Content));
decode_xml(#xmlElement{name='PercentageMapping'} = Element) ->
    erlcloud_xml:decode(
      [
       {#mturk_percentage_mapping.maximum_summed_score, "MaximumSummedScore", integer}
      ],
      Element,
      #mturk_percentage_mapping{}
     );
decode_xml(#xmlElement{name='ScaleMapping'} = Element) ->
    erlcloud_xml:decode(
      [
       {#mturk_scale_mapping.summed_score_multiplier, "SummedScoreMultiplier", integer}
      ],
      Element,
      #mturk_percentage_mapping{}
     );
decode_xml(#xmlElement{name='Test'} = Element) ->
    XML = element(1,xmerl_scan:string(erlcloud_xml:get_text(Element))),
    decode_xml(XML);
decode_xml(#xmlElement{name='AnswerKey'} = Element) ->
    XML = element(1,xmerl_scan:string(erlcloud_xml:get_text(Element))),
    decode_xml(XML);
decode_xml(#xmlElement{name='AnswerOption'} = Element) ->
    erlcloud_xml:decode(
      [
       {#mturk_answer_option.selection_identifier, "SelectionIdentifier", text},
       {#mturk_answer_option.answer_score, "AnswerScore", integer}
      ],
      Element,
      #mturk_answer_option{}
     );
decode_xml(#xmlElement{name='Question', parents=[{'AnswerKey', _}|_]} = Element) ->
    erlcloud_xml:decode(
      [
       {#mturk_test_answer.question_identifier, "QuestionIdentifier", text},
       {#mturk_test_answer.answer_options, "AnswerOption", fun decode_xml/1},
       {#mturk_test_answer.default_score, "DefaultScore", optional_integer}
      ],
      Element,
      #mturk_test_answer{}
     );
decode_xml(#xmlElement{name='ExternalQuestion'} = Element) ->
    erlcloud_xml:decode(
      [
       {#mturk_external_question.external_url, "ExternalURL", text},
       {#mturk_external_question.frame_height, "FrameHeight", integer}
      ],
      Element,
      #mturk_external_question{}
     );
decode_xml(#xmlElement{name='QuestionForm', content=Content}) ->
    #mturk_question_form{content=decode_xml(Content)};
decode_xml(#xmlElement{name='Question', parents=[{'QuestionForm', _}|_]} = Element) ->
    erlcloud_xml:decode(
      [
       {#mturk_question.question_identifier, "QuestionIdentifier", text},
       {#mturk_question.display_name, "DisplayName", text},
       {#mturk_question.is_required, "IsRequired", boolean},
       {#mturk_question.question_content, "QuestionContent", {single, fun decode_xml/1}},
       {#mturk_question.answer_specification, "AnswerSpecification", {single, fun decode_xml/1}}
      ],
      Element,
      #mturk_question{}
     );
decode_xml(#xmlElement{name='Question'} = Element) ->
    XML = element(1,xmerl_scan:string(erlcloud_xml:get_text(Element))),
    decode_xml(XML);
decode_xml(#xmlElement{name='QuestionContent', content=Content}) ->
    decode_xml(Content);
decode_xml(#xmlElement{name='AnswerSpecification', content=Content}) ->
    hd(decode_xml(Content));
decode_xml(#xmlElement{name='FreeTextAnswer'} = Element) ->
    erlcloud_xml:decode(
      [
       {#mturk_free_text_answer.constraints, "Constraints", {single, fun decode_xml/1}},
       {#mturk_free_text_answer.default_text, "DefaultText", text},
       {#mturk_free_text_answer.number_of_lines_suggestion, "NumberOfLinesSuggestion", integer}
      ],
      Element,
      #mturk_free_text_answer{}
     );
decode_xml(#xmlElement{name='Constraints'} = Element) ->
    erlcloud_xml:decode(
      [
       {#mturk_constraints.is_numeric, "IsNumeric", present},
       {#mturk_constraints.min_value, {"IsNumeric", "minValue"}, optional_integer},
       {#mturk_constraints.max_value, {"IsNumeric", "maxValue"}, optional_integer},
       {#mturk_constraints.min_length, {"Length", "minLength"}, optional_integer},
       {#mturk_constraints.max_length, {"Length", "maxLength"}, optional_integer},
       {#mturk_constraints.answer_format_regex,
        {"AnswerFormatRegex", "regex"}, optional_text},
       {#mturk_constraints.answer_format_regex_error_text,
        {"AnswerFormatRegex", "errorText"}, optional_text},
       {#mturk_constraints.answer_format_regex_flags,
        {"AnswerFormatRegex", "flags"}, optional_text}
      ],
      Element,
      #mturk_constraints{}
     );
decode_xml(#xmlElement{name='SelectionAnswer'} = Element) ->
    erlcloud_xml:decode(
      [
       {#mturk_selection_answer.min_selection_count, "MinSelectionCount", integer},
       {#mturk_selection_answer.max_selection_count, "MaxSelectionCount", integer},
       {#mturk_selection_answer.style_suggestion, "StyleSuggestion", {value, fun (V) -> list_to_existing_atom(V) end}},
       {#mturk_selection_answer.selections, "Selections", {single, fun decode_xml/1}}
      ],
      Element,
      #mturk_selection_answer{}
     );
decode_xml(#xmlElement{name='Selections', content=Content}) ->
    decode_xml(Content);
decode_xml(#xmlElement{name='Selection'} = Element) ->
    erlcloud_xml:decode(
      [
       {#mturk_selection.selection_identifier, "SelectionIdentifier", text},
       {#mturk_selection.text, "Text", optional_text},
       {#mturk_selection.formatted_content, "FormattedContent", optional_text},
       {#mturk_selection.binary, "Binary", {single, fun decode_xml/1}}
      ],
      Element,
      #mturk_selection{}
     );
decode_xml(#xmlElement{name='Overview', content=Content}) ->
    #mturk_overview{content=decode_xml(Content)};
decode_xml(#xmlElement{name='Text'} = Element) ->
    {text, erlcloud_xml:get_text(Element)};
decode_xml(#xmlElement{name='Title'} = Element) ->
    {title, erlcloud_xml:get_text(Element)};
decode_xml(#xmlElement{name='Binary'} = Element) ->
    erlcloud_xml:decode(
      [
       {#mturk_binary.mime_type, "MimeType", {single, fun decode_xml/1}},
       {#mturk_binary.data_url, "DataURL", text},
       {#mturk_binary.alt_text, "AltText", text}
      ],
      Element,
      #mturk_binary{}
     );
decode_xml(#xmlElement{name=MimeType} = Element)
  when MimeType =:= 'MimeType'; MimeType =:= 'EmbeddedMimeType' ->
    erlcloud_xml:decode(
      [
       {#mturk_mime_type.type, "Type", text},
       {#mturk_mime_type.sub_type, "SubType", text}
      ],
      Element,
      #mturk_mime_type{}
     );
decode_xml(#xmlElement{name='FlashMovie'} = Element) ->
    erlcloud_xml:decode(
      [
       {#mturk_flash.flash_movie_url, "FlashMovieURL", text},
       {#mturk_flash.width, "Width", integer},
       {#mturk_flash.height, "Height", integer},
       {#mturk_flash.application_parameters, "ApplicationParameter", fun decode_xml/1}
      ],
      Element,
      #mturk_flash{}
     );
decode_xml(#xmlElement{name='JavaApplet'} = Element) ->
    erlcloud_xml:decode(
      [
       {#mturk_java_applet.applet_path, "AppletPath", text},
       {#mturk_java_applet.applet_filename, "AppletFilename", text},
       {#mturk_java_applet.width, "Width", integer},
       {#mturk_java_applet.height, "Height", integer},
       {#mturk_java_applet.application_parameters, "ApplicationParameter", fun decode_xml/1}
      ],
      Element,
      #mturk_java_applet{}
     );
decode_xml(#xmlElement{name='EmbeddedBinary'} = Element) ->
    erlcloud_xml:decode(
      [
       {#mturk_embedded_binary.embedded_mime_type, "MimeType", {single, fun decode_xml/1}},
       {#mturk_embedded_binary.data_url, "DataURL", text},
       {#mturk_embedded_binary.alt_text, "AltText", text},
       {#mturk_embedded_binary.width, "Width", integer},
       {#mturk_embedded_binary.height, "Height", integer},
       {#mturk_embedded_binary.application_parameters, "ApplicationParameter", fun decode_xml/1}
      ],
      Element,
      #mturk_embedded_binary{}
     );
decode_xml(#xmlElement{name='ApplicationParameter'} = Element) ->
    {erlcloud_xml:get_text("Name", Element), erlcloud_xml:get_text("Value", Element)};
decode_xml(#xmlElement{name='FormattedContent'} = Element) ->
    {formatted_content, erlcloud_xml:get_text(Element)}.

extract_qualification_requirements(Reqts) ->
    [extract_qualification_requirement(Reqt) || Reqt <- Reqts].

extract_qualification_requirement(Reqt) ->
    erlcloud_xml:decode(
      [
       {#mturk_qualification_requirement.qualification_type_id, "QualificationTypeId", text},
       {#mturk_qualification_requirement.comparator, "Comparator", {value, fun decode_comparator/1}},
       {#mturk_qualification_requirement.integer_value, "IntegerValue", integer},
       {#mturk_qualification_requirement.locale_value, "LocaleValue", {single, fun extract_locale_value/1}},
       {#mturk_qualification_requirement.required_to_preview, "RequiredToPreview", boolean}
      ],
      Reqt,
      #mturk_qualification_requirement{}
     ).

extract_locale_value([Item]) ->
    [#mturk_locale{country_code=erlcloud_xml:get_text("Country", Item)}].

extract_money(Money) ->
    erlcloud_xml:decode(
      [
       {#mturk_money.amount, "Amount", text},
       {#mturk_money.currency_code, "CurrencyCode", text},
       {#mturk_money.formatted_price, "FormattedPrice", text}
      ],
      Money,
      #mturk_money{}
     ).

encode_money(#mturk_money{amount=Amount, currency_code=CurrencyCode}) ->
    [{"Amount", Amount}, {"CurrencyCode", CurrencyCode}].

-spec notify_workers(string(), string(), [string()]) -> ok | no_return().
notify_workers(Subject, MessageText, WorkerIds) ->
    notify_workers(Subject, MessageText, WorkerIds, default_config()).

-spec notify_workers(string(), string(), [string()], aws_config()) -> ok | no_return().
notify_workers(Subject, MessageText, WorkerIds, Config)
  when is_list(Subject), is_list(MessageText),
       is_list(WorkerIds), length(WorkerIds) =< 100 ->
    mturk_simple_request(Config, "NotifyWorkers",
                         [
                          {"Subject", Subject},
                          {"MessageText", MessageText}|
                          erlcloud_aws:param_list(WorkerIds, "WorkerId")
                         ]
                        ).

-spec register_hit_type(#mturk_hit{}) -> proplist() | no_return().
register_hit_type(HIT) ->
    register_hit_type(HIT, default_config()).

-spec register_hit_type(#mturk_hit{}, aws_config()) -> proplist() | no_return().
register_hit_type(HIT, Config) ->
    Params = [
              {"Title", HIT#mturk_hit.title},
              {"Description", HIT#mturk_hit.description},
              {"AssignmentDurationInSeconds", HIT#mturk_hit.assignment_duration_in_seconds},
              {"Keywords", string:join(HIT#mturk_hit.keywords, ",")},
              {"AutoApprovalDelayInSeconds", HIT#mturk_hit.auto_approval_delay_in_seconds},
              {"RequesterAnnotation", HIT#mturk_hit.requester_annotation}] ++
        erlcloud_aws:param_list([encode_money(HIT#mturk_hit.reward)], "Reward") ++
        erlcloud_aws:param_list([encode_qualification_requirement(QR) || QR <- HIT#mturk_hit.qualification_requirements],
                                "QualificationRequirement"),
    Doc = mturk_xml_request(Config, "RegisterHITType", Params),
    erlcloud_xml:decode(
      [
       {hit_type_id, "HITTypeId", text}
      ],
      Doc
     ).

-spec reject_assignment(string()) -> ok | no_return().
reject_assignment(AssignmentId) ->
    reject_assignment(AssignmentId, none).

-spec reject_assignment(string(), string() | none | aws_config()) -> ok | no_return().
reject_assignment(AssignmentId, Config)
  when is_record(Config, aws_config) ->
    reject_assignment(AssignmentId, none, Config);
reject_assignment(AssignmentId, Reason) ->
    reject_assignment(AssignmentId, Reason, default_config()).

-spec reject_assignment(string(), string() | none, aws_config()) -> ok | no_return().
reject_assignment(AssignmentId, Reason, Config)
  when is_list(AssignmentId),
       is_list(Reason) orelse Reason =:= none ->
    mturk_simple_request(Config, "RejectAssignment",
                         [
                          {"AssignmentId", AssignmentId},
                          {"Reason", Reason}
                         ]
                        ).

-spec reject_qualification_request(string()) -> ok | no_return().
reject_qualification_request(QualificationRequestId) ->
    reject_qualification_request(QualificationRequestId, none).

-spec reject_qualification_request(string(), string() | none | aws_config()) -> ok | no_return().
reject_qualification_request(QualificationRequestId, Config)
  when is_record(Config, aws_config) ->
    reject_qualification_request(QualificationRequestId, none, Config);
reject_qualification_request(QualificationRequestId, Reason) ->
    reject_qualification_request(QualificationRequestId, Reason, default_config()).

-spec reject_qualification_request(string(), string() | none, aws_config()) -> ok | no_return().
reject_qualification_request(QualificationRequestId, Reason, Config)
  when is_list(QualificationRequestId),
       is_list(Reason) orelse Reason =:= none ->
    mturk_simple_request(Config, "RejectQualificationRequest",
                         [
                          {"QualificationRequestId", QualificationRequestId},
                          {"Reason", Reason}
                         ]
                        ).

-spec revoke_qualification(string(), string()) -> ok | no_return().
revoke_qualification(QualificationTypeId, WorkerId) ->
    revoke_qualification(QualificationTypeId, WorkerId, none).

-spec revoke_qualification(string(), string(), string() | none | aws_config()) -> ok | no_return().
revoke_qualification(QualificationTypeId, WorkerId, Config)
  when is_record(Config, aws_config) ->
    revoke_qualification(QualificationTypeId, WorkerId, none, Config);
revoke_qualification(QualificationTypeId, WorkerId, Reason) ->
    revoke_qualification(QualificationTypeId, WorkerId, Reason, default_config()).

-spec revoke_qualification(string(), string(), string() | none, aws_config()) -> ok | no_return().
revoke_qualification(QualificationTypeId, WorkerId, Reason, Config) ->
    mturk_simple_request(Config, "RevokeQualification",
                         [
                          {"SubjectId", WorkerId},
                          {"QualificationTypeId", QualificationTypeId},
                          {"Reason", Reason}
                         ]
                        ).

-spec search_hits() -> proplist() | no_return().
search_hits() ->
    search_hits([]).

-spec search_hits(proplist() | aws_config()) -> proplist() | no_return().
search_hits(Config)
  when is_record(Config, aws_config) ->
    search_hits([], Config);
search_hits(Options) ->
    search_hits(Options, default_config()).

-spec search_hits(proplist(), aws_config()) -> proplist() | no_return().
search_hits(Options, Config)
  when is_list(Options) ->
    Params = [
              {"SortProperty",
               case proplists:get_value(sort_property, Options) of
                   undefined -> undefined;
                   title -> "Title";
                   reward -> "Reward";
                   expiration -> "Expiration";
                   creation_time -> "CreationTime";
                   enumeration -> "Enumeration"
               end
              },
              {"SortDirection",
               case proplists:get_value(sort_direction, Options) of
                   undefined -> undefined;
                   ascending -> "Ascending";
                   descending -> "Descending"
               end
              },
              {"PageSize", proplists:get_value(page_size, Options)},
              {"PageNumber", proplists:get_value(page_number, Options)}
             ],
    Doc = mturk_xml_request(Config, "SearchHITs", Params),
    erlcloud_xml:decode(
      [
       {num_results, "NumResults", integer},
       {page_number, "PageNumber", integer},
       {total_num_results, "TotalNumResults", integer},
       {hits, "HIT", fun extract_hits/1}
      ],
      Doc
     ).

-spec search_qualification_types() -> proplist() | no_return().
search_qualification_types() ->
    search_qualification_types([]).

-spec search_qualification_types(proplist() | aws_config()) -> proplist() | no_return().
search_qualification_types(Config)
  when is_record(Config, aws_config) ->
    search_qualification_types([], Config);
search_qualification_types(Options) ->
    search_qualification_types(Options, default_config()).

-spec search_qualification_types(proplist(), aws_config()) -> proplist() | no_return().
search_qualification_types(Options, Config) ->
    Params = [
              {"Query", proplists:get_value(search_query, Options)},
              {"MustBeRequestable", proplists:get_value(must_be_requestable, Options)},
              {"MustBeOwnedByCaller", proplists:get_value(must_be_owned_by_caller, Options)},
              {"SortProperty",
               case proplists:get_value(sort_property, Options) of
                   undefined -> undefined;
                   name -> "Name"
               end
              },
              {"SortDirection",
               case proplists:get_value(sort_direction, Options) of
                   undefined -> undefined;
                   ascending -> "Ascending";
                   descending -> "Descending"
               end
              },
              {"PageSize", proplists:get_value(page_size, Options)},
              {"PageNumber", proplists:get_value(page_number, Options)}
             ],
    Doc = mturk_xml_request(Config, "SearchQualificationTypes", Params),
    erlcloud_xml:decode(
      [
       {num_results, "NumResults", integer},
       {page_number, "PageNumber", integer},
       {total_num_results, "TotalNumResults", integer},
       {qualification_types, "QualificationType", fun extract_qualification_types/1}
      ],
      Doc
     ).

-spec send_test_event_notification(proplist(), mturk_event_type()) -> ok | no_return().
send_test_event_notification(Notificaiton, TestEventType) ->
    send_test_event_notification(Notificaiton, TestEventType, default_config()).

-spec send_test_event_notification(proplist(), mturk_event_type(), aws_config()) -> ok | no_return().
send_test_event_notification(Notification, TestEventType, Config) ->
    mturk_simple_request(Config, "SendTestEventNotification",
                         [
                          {"Notification.Destination", proplists:get_value(destination, Notification)},
                          {"Notification.Transport", encode_transport(proplists:get_value(transport, Notification))},
                          {"Notification.Version", proplists:get_value(version, Notification, ?DEFAULT_NOTIFICATION_VERSION)},
                          {"Notification.EventType", encode_event_type(proplists:get_value(event_type, Notification))},
                          {"TestEventType", encode_event_type(TestEventType)}
                         ]
                        ).

-spec set_hit_as_reviewing(string()) -> ok | no_return().
set_hit_as_reviewing(HITId) ->
    set_hit_as_reviewing(HITId, false).

-spec set_hit_as_reviewing(string(), boolean() | aws_config()) -> ok | no_return().
set_hit_as_reviewing(HITId, Config)
  when is_record(Config, aws_config) ->
    set_hit_as_reviewing(HITId, false, Config);
set_hit_as_reviewing(HITId, Revert) ->
    set_hit_as_reviewing(HITId, Revert, default_config()).

-spec set_hit_as_reviewing(string(), boolean(), aws_config()) -> ok | no_return().
set_hit_as_reviewing(HITId, Revert, Config) ->
    mturk_simple_request(Config, "SetHITAsReviewing",
                         [
                          {"HITId", HITId},
                          {"Revert", Revert}
                         ]
                        ).

-spec set_hit_type_notification(string(), proplist()) -> ok | no_return().
set_hit_type_notification(HITTypeId, Notification) ->
    set_hit_type_notification(HITTypeId, Notification, undefined).

-spec set_hit_type_notification(string(), proplist(), boolean() | undefined | aws_config()) -> ok | no_return().
set_hit_type_notification(HITTypeId, Notification, Config)
  when is_record(Config, aws_config) ->
    set_hit_type_notification(HITTypeId, Notification, undefined, Config);
set_hit_type_notification(HITTypeId, Notification, Active) ->
    set_hit_type_notification(HITTypeId, Notification, Active, default_config()).

-spec set_hit_type_notification(string(), proplist(), boolean() | undefined, aws_config()) -> ok | no_return().
set_hit_type_notification(HITTypeId, Notification, Active, Config)
  when is_list(HITTypeId), is_list(Notification),
       is_boolean(Active) orelse Active =:= undefined ->
    mturk_simple_request(Config, "SetHITTypeNotification",
                         [
                          {"HITTypeId", HITTypeId},
                          {"Active", Active},
                          {"Notification.Destination", proplists:get_value(destination, Notification)},
                          {"Notification.Transport", encode_transport(proplists:get_value(transport, Notification))},
                          {"Notification.Version", proplists:get_value(version, Notification, ?DEFAULT_NOTIFICATION_VERSION)},
                          {"Notification.EventType", encode_event_type(proplists:get_value(event_type, Notification))}
                         ]
                        ).

encode_event_type(assignment_accepted) -> "AssignmentAccepted";
encode_event_type(assignment_abandoned) -> "AssignmentAbandoned";
encode_event_type(assignment_returned) -> "AssignmentReturned";
encode_event_type(assignment_submitted) -> "AssignmentSubmitted";
encode_event_type(hit_reviewable) -> "HITReviewable";
encode_event_type(hit_expired) -> "HITExpired".

encode_transport(email) -> "Email";
encode_transport(soap) -> "SOAP";
encode_transport(rest) -> "REST".

-spec unblock_worker(string()) -> ok | no_return().
unblock_worker(WorkerId) -> unblock_worker(WorkerId, none).

-spec unblock_worker(string(), string() | none | aws_config()) -> ok | no_return().
unblock_worker(WorkerId, Config)
  when is_record(Config, aws_config) ->
    unblock_worker(WorkerId, none, Config);
unblock_worker(WorkerId, Reason) ->
    unblock_worker(WorkerId, Reason, default_config()).

-spec unblock_worker(string(), string() | none, aws_config()) -> ok | no_return().
unblock_worker(WorkerId, Reason, Config)
  when is_list(WorkerId),
       is_list(Reason) orelse Reason =:= none ->
    mturk_simple_request(Config, "UnblockWorker",
                         [{"WorkerId", WorkerId}, {"Reason", Reason}]).

-spec update_qualification_score(string(), string(), integer()) -> ok | no_return().
update_qualification_score(QualificationTypeId, SubjectId, IntegerValue) ->
    update_qualification_score(QualificationTypeId, SubjectId,
                               IntegerValue, default_config()).

-spec update_qualification_score(string(), string(), integer(), aws_config()) -> ok | no_return().
update_qualification_score(QualificationTypeId, SubjectId, IntegerValue, Config)
  when is_list(SubjectId), is_list(QualificationTypeId),
       is_integer(IntegerValue) ->
    mturk_simple_request(Config, "UpdateQualificationScore",
                         [
                          {"SubjectId", SubjectId},
                          {"QualificationTypeId", QualificationTypeId},
                          {"IntegerValue", IntegerValue}
                         ]
                        ).

-spec update_qualification_type(#mturk_qualification_type{}) -> #mturk_qualification_type{} | no_return().
update_qualification_type(QType) ->
    update_qualification_type(QType, default_config()).

-spec update_qualification_type(#mturk_qualification_type{}, aws_config()) -> #mturk_qualification_type{} | no_return().
update_qualification_type(QType, Config) ->
    Doc = mturk_xml_request(Config, "UpdateQualificationType",
                            [
                             {"QualificationTypeId", QType#mturk_qualification_type.qualification_type_id}|
                             qualification_type_params(QType)
                            ]
                           ),
    extract_qualification_type(Doc).

encode_question_content(Items) ->
    [encode_xml(Item) || Item <- Items].

encode_xml_list(List) when is_list(List) ->
    [encode_xml(Item) || Item <- List];
encode_xml_list(undefined) -> [];
encode_xml_list(XML) -> [encode_xml(XML)].

encode_xml({title, Title}) ->
    {'Title', [Title]};
encode_xml({text, Text}) ->
    {'Text', [Text]};
encode_xml({list, List}) ->
    {'List', [{'ListItem', [Item]} || Item <- List]};
encode_xml({formatted_content, Content}) ->
    {'FormattedContent', [#xmlText{value=Content, type=cdata}]};
encode_xml(#mturk_question_form{content=Content}) ->
    {'QuestionForm', [{xmlns, ?XMLNS_QUESTIONFORM}], encode_xml_list(Content)};
encode_xml(#mturk_overview{content=Content}) ->
    {'Overview', encode_xml_list(Content)};
encode_xml(#mturk_external_question{external_url=ExternalURL, frame_height=FrameHeight}) ->
    {'ExternalQuestion', [{xmlns, ?XMLNS_EXTERNALQUESTION}],
     [
      {'ExternalURL', [ExternalURL]},
      {'FrameHeight', [integer_to_list(FrameHeight)]}
     ]};
encode_xml(#mturk_question{question_identifier=QuestionId,
                           display_name=DisplayName, is_required=IsRequired,
                           question_content=Content, answer_specification=AnswerSpec}) ->
    {'Question', filter_undefined([
                                   {'QuestionIdentifier', [QuestionId]},
                                   {'DisplayName', [DisplayName]},
                                   {'IsRequired', [atom_to_list(IsRequired)]},
                                   {'QuestionContent', encode_question_content(Content)},
                                   {'AnswerSpecification', [encode_xml(AnswerSpec)]}
                                  ])};
encode_xml(#mturk_answer_key{answers=Answers, qualification_value_mapping=QVMap}) ->
    {'AnswerKey', [{xmlns, ?XMLNS_ANSWERKEY}],
     encode_xml_list(Answers) ++
         case QVMap of
             undefined -> [];
             _ -> [{'QualificationValueMapping', encode_xml_list(QVMap)}]
         end
    };
encode_xml(#mturk_test_answer{question_identifier=QId, answer_options=Opts,
                              default_score=DefaultScore}) ->
    {'Question',
     [{'QuestionIdentifier', [QId]}] ++
         encode_xml_list(Opts) ++
         [{'DefaultScore', [integer_to_list(DefaultScore)]}]
    };
encode_xml(#mturk_answer_option{selection_identifier=SId, answer_score=Score}) ->
    {'AnswerOption', [
                      {'SelectionIdentifier', [SId]},
                      {'AnswerScore', [integer_to_list(Score)]}
                     ]};
encode_xml(#mturk_percentage_mapping{maximum_summed_score=MScore}) ->
    {'PercentageMapping', [{'MaximumSummedScore', [integer_to_list(MScore)]}]};
encode_xml(#mturk_scale_mapping{summed_score_multiplier=SMult}) ->
    {'ScaleMapping', [{'SummedScoreMultiplier', [float_to_list(SMult)]}]};
encode_xml(#mturk_free_text_answer{constraints=C, default_text=Text,
                                   number_of_lines_suggestion=NLines}) ->
    {'FreeTextAnswer', [
                        encode_xml(C),
                        {'DefaultText', case Text of undefined -> []; _ -> [Text] end},
                        {'NumberOfLinesSuggestion', [integer_to_list(NLines)]}
                       ]};
encode_xml(#mturk_constraints{is_numeric=IsNumeric, min_value=MinValue,
                              max_value=MaxValue, min_length=MinLength, max_length=MaxLength,
                              answer_format_regex=AnswerFormatRegex, answer_format_regex_error_text=ErrorText,
                              answer_format_regex_flags=Flags}) ->
    {'Constraints', lists:flatten([
                                   case IsNumeric of
                                       true ->
                                           [{'IsNumeric', filter_undefined([{minValue, MinValue}, {maxValue, MaxValue}]), []}];
                                       false -> []
                                   end,
                                   if
                                       MinLength =/= undefined; MaxLength =/= undefined ->
                                           [{'Length', filter_undefined([{minLength, MinLength}, {maxLength, MaxLength}]), []}];
                                       true -> []
                                   end,
                                   case AnswerFormatRegex of
                                       undefined -> [];
                                       _ ->
                                           [{'AnswerFormatRegex', filter_undefined([{regex, AnswerFormatRegex}, {errorText, ErrorText}, {flags, Flags}]), []}]
                                   end
                                  ])};
encode_xml(#mturk_file_upload_answer{min_file_size_in_bytes=MinSize,
                                     max_file_size_in_bytes=MaxSize}) ->
    {'FileUploadAnswer', [
                          {'MaxFileSizeInBytes', [integer_to_list(MaxSize)]},
                          {'MinFileSizeInBytes', [integer_to_list(MinSize)]}
                         ]};
encode_xml(#mturk_selection_answer{
              min_selection_count=MinSCount, max_selection_count=MaxSCount,
              style_suggestion=StyleSuggestion, selections=Selections}) ->
    {'SelectionAnswer', filter_undefined([
                                          {'MinSelectionCount', [integer_to_list(MinSCount)]},
                                          {'MaxSelectionCount', [integer_to_list(MaxSCount)]},
                                          {'StyleSuggestion',
                                           case StyleSuggestion of
                                               none -> undefined;
                                               _ -> [atom_to_list(StyleSuggestion)]
                                           end}
                                         ]) ++ [{'Selections', encode_xml_list(Selections)}]};
encode_xml(#mturk_selection{selection_identifier=SId, text=Text, formatted_content=Content, binary=Binary}) ->
    {'Selection', filter_undefined([
                                    {'SelectionIdentifier', [SId]},
                                    {'Text', [Text]},
                                    {'FormattedContent', [Content]}
                                   ]) ++ encode_xml_list(Binary)};
encode_xml(#mturk_java_applet{applet_path=AP, applet_filename=AF, width=Width,
                              height=Height, application_parameters=Params}) ->
    {'JavaApplet', [
                    {'AppletPath', [AP]},
                    {'AppletFilename', [AF]},
                    {'Width', [integer_to_list(Width)]},
                    {'Height', [integer_to_list(Height)]}|
                    [{'ApplicationParameter', [{'Name', [Name]}, {'Value', [Value]}]} ||
                        {Name, Value} <- Params]
                   ]};
encode_xml(#mturk_flash{flash_movie_url=URL, width=Width, height=Height,
                        application_parameters=Params}) ->
    {'FlashMovie', [
                    {'FlashMovieURL', [URL]},
                    {'Width', [integer_to_list(Width)]},
                    {'Height', [integer_to_list(Height)]}|
                    [{'ApplicationParameter', [{'Name', [Name]}, {'Value', [Value]}]} ||
                        {Name, Value} <- Params]
                   ]};
encode_xml(#mturk_binary{mime_type=MT, data_url=DataURL, alt_text=AltText}) ->
    {'Binary',
     [encode_xml(MT),
      {'DataURL', [DataURL]},
      {'AltText', [AltText]}
     ]};
encode_xml(#mturk_mime_type{type=T, sub_type=ST}) ->
    {'MimeType', [{'Type', [T]}, {'SubType', [ST]}]};
encode_xml(#mturk_embedded_binary{embedded_mime_type=#mturk_mime_type{type=T, sub_type=ST},
                                  data_url=DataURL, alt_text=AltText, width=Width, height=Height,
                                  application_parameters=Params}) ->
    {'EmbeddedBinary', [
                        {'EmbeddedMimeType', [{'Type', [T]}, {'SubType', [ST]}]},
                        {'DataURL', [DataURL]},
                        {'AltText', [AltText]},
                        {'Width', [integer_to_list(Width)]},
                        {'Height', [integer_to_list(Height)]}|
                        [{'ApplicationParameter', [{'Name', [Name]}, {'Value', [Value]}]} ||
                            {Name, Value} <- Params]
                       ]};
encode_xml(undefined) -> undefined.

xml_to_string(undefined) -> undefined;
xml_to_string(XML) ->
    "<?xml version=\"1.0\"?>" ++ XMLString = lists:flatten(xmerl:export_simple([XML], xmerl_xml)),
    XMLString.


encode_qualification_type_status(active) -> "Active";
encode_qualification_type_status(inactive) -> "Inactive".

decode_qualification_type_status("Active") -> active;
decode_qualification_type_status("Inactive") -> inactive;
decode_qualification_type_status("Disposing") -> disposing.

filter_undefined(List) ->
    [Elem || {_, Value} = Elem <- List, Value =/= undefined, Value =/= [undefined]].

mturk_simple_request(Config, Operation, Params) ->
    Doc = mturk_xml_request(Config, Operation, Params),
    case erlcloud_xml:get_text("//Request/IsValid", Doc) of
        "True" -> ok;
        "False" ->
            ErrorCode = erlcloud_xml:get_text("//Request/Errors/Error[1]/Code", Doc),
            ErrorMessage = erlcloud_xml:get_text("//Request/Errors/Error[1]/Message", Doc),
            erlang:error({mturk_error, ErrorCode, ErrorMessage})
    end.

mturk_xml_request(Config, Operation, Params) ->
    Doc = element(1, xmerl_scan:string(mturk_request(Config, Operation, Params))),
    Request = hd(xmerl_xpath:string("//Request", Doc)),
    case erlcloud_xml:get_text("IsValid", Request) of
        "True" -> hd(xmerl_xpath:string(atom_to_list(element(1,hd(Request#xmlElement.parents))), Doc));
        "False" ->
            ErrorCode = erlcloud_xml:get_text("//Request/Errors/Error[1]/Code", Doc),
            ErrorMessage = erlcloud_xml:get_text("//Request/Errors/Error[1]/Message", Doc),
            erlang:error({mturk_error, ErrorCode, ErrorMessage})
    end.

mturk_request(Config, Operation, Params) ->
    Timestamp = erlcloud_aws:format_timestamp(erlang:universaltime()),
    StringToSign = [?API_SERVICE, Operation, Timestamp],
    Signature = base64:encode(erlcloud_util:sha_mac(Config#aws_config.secret_access_key, StringToSign)),

    QParams = [{"Operation", Operation}, {"Version", ?API_VERSION},
               {"Service", ?API_SERVICE}, {"Timestamp", Timestamp},
               {"AWSAccessKeyId", Config#aws_config.access_key_id},
               {"Signature", Signature}|Params],

    URL = ["https://", Config#aws_config.mturk_host, "/"],

    Response = erlcloud_httpc:request(
                 lists:flatten(URL),
                 post,
                 [{<<"content-type">>, <<"application/x-www-form-urlencoded">>}],
                 list_to_binary(erlcloud_http:make_query_string(QParams)),
                 erlcloud_aws:get_timeout(Config), Config),

    case Response of
        {ok, {{200, _StatusLine}, _Headers, Body}} ->
            binary_to_list(Body);
        {ok, {{Status, _StatusLine}, _Headers, _Body}} ->
            erlang:error({aws_error, {http_error, Status, _StatusLine, _Body}});
        {error, Error} ->
            erlang:error({aws_error, {socket_error, Error}})
    end.
