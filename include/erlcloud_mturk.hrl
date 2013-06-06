-record(mturk_mime_type, {type=""::string(), sub_type=""::string()}).

-record(mturk_binary, {
          mime_type::#mturk_mime_type{},
          data_url=""::string(),
          alt_text=""::string()
         }).
-record(mturk_selection, {
          selection_identifier::string(),
          text::string(),
          formatted_content::string(),
          binary::#mturk_binary{}
         }).
-record(mturk_selection_answer, {
          min_selection_count=1::non_neg_integer(),
          max_selection_count=1::pos_integer(),
          style_suggestion=none::radiobutton | checkbox | list | dropdown | combobox | multichooser | none,
          selections=[]::[#mturk_selection{}]
         }).
-record(mturk_constraints, {
          is_numeric=false::boolean(),
          min_value::integer(),
          max_value::integer(),
          min_length=0::non_neg_integer(),
          max_length::non_neg_integer(),
          answer_format_regex::string(),
          answer_format_regex_error_text::string(),
          answer_format_regex_flags::string()
         }).
-record(mturk_free_text_answer, {
          constraints=#mturk_constraints{}::#mturk_constraints{},
          default_text::string(),
          number_of_lines_suggestion=1::pos_integer()
         }).
-record(mturk_file_upload_answer, {
          min_file_size_in_bytes=0::0..2000000000,
          max_file_size_in_bytes=2000000000::0..2000000000
         }).

-type mturk_answer_specification() ::
        #mturk_free_text_answer{} | #mturk_selection_answer{} |
        #mturk_file_upload_answer{}.

-record(mturk_java_applet, {
          applet_path::string(),
          applet_filename::string(),
          width::non_neg_integer(),
          height::non_neg_integer(),
          application_parameters::[{string(), string()}]
         }).

-record(mturk_flash, {
          flash_movie_url::string(),
          width::non_neg_integer(),
          height::non_neg_integer(),
          application_parameters::[{string(), string()}]
         }).

-type mturk_application() :: #mturk_java_applet{} | #mturk_flash{}.

-record(mturk_embedded_binary, {
          embedded_mime_type::#mturk_mime_type{},
          data_url::string(),
          alt_text::string(),
          width::non_neg_integer(),
          height::non_neg_integer(),
          application_parameters::[{string(), string()}]
         }).

-type mturk_question_content() ::
        {title, string()} | {text, string()} | {list, [string()]} | #mturk_binary{} |
        mturk_application() | #mturk_embedded_binary{} |
        {formatted_content, string()}.

-record(mturk_question, {
          question_identifier::string(),
          display_name::string(),
          is_required=false::boolean(),
          question_content=[]::[mturk_question_content()],
          answer_specification::mturk_answer_specification()
         }).
-record(mturk_overview, {
          content=[]::[mturk_question_content()]
         }).
-record(mturk_question_form, {
          content=[]::[#mturk_overview{} | #mturk_question{}]
         }).
-record(mturk_external_question, {
          external_url::string(),
          frame_height::pos_integer()
         }).
-type(mturk_question() :: #mturk_question_form{} | #mturk_external_question{}).

-record(mturk_answer_option, {
          selection_identifier::string(),
          answer_score::integer()
         }).

-record(mturk_test_answer, {
          question_identifier::string(),
          answer_options=[]::[#mturk_answer_option{}],
          default_score=0::integer()
         }).

-record(mturk_percentage_mapping, {maximum_summed_score::integer()}).
-record(mturk_scale_mapping, {summed_score_multiplier::float()}).
-type mturk_qualification_value_mapping() :: #mturk_percentage_mapping{} | #mturk_scale_mapping{}.

-record(mturk_answer_key, {
          answers=[]::[#mturk_test_answer{}],
          qualification_value_mapping::mturk_qualification_value_mapping()
         }).

-type(mturk_qualification_type_status() :: active | inactive).
-record(mturk_qualification_type, {
          qualification_type_id::string(),
          name::string(),
          description::string(),
          keywords=[]::[string()],
          retry_delay_in_seconds=none::pos_integer() | none,
          qualification_type_status=active::mturk_qualification_type_status(),
          test::#mturk_question_form{},
          answer_key::#mturk_answer_key{},
          test_duration_in_seconds=none::pos_integer() | none,
          creation_time::datetime(),
          auto_granted=false::boolean(),
          auto_granted_value=1::integer(),
          is_requestable=true::boolean()
         }).

-type(mturk_event_type() :: assignment_accepted | assignment_abandoned |
                            assignment_returned | assignment_submitted | hit_reviewable | hit_expired).

-record(mturk_money, {
          amount::string(),
          currency_code::string(),
          formatted_price::string()
         }).

-type(mturk_comparator() :: '<' | '=<' | '>' | '>=' | '==' | '/=' | exists).
-record mturk_locale, {
          country_code::string()
         }.
-record(mturk_qualification_requirement, {
          qualification_type_id::string(),
          comparator=exists::mturk_comparator(),
          integer_value::integer(),
          locale_value::#mturk_locale{},
          required_to_preview=false::boolean()
         }).

-type mturk_hit_status() :: assignable | unassignable | reviewable | reviewing | disposed.
-type mturk_hit_review_status() :: not_reviewed | marked_for_review | reviewed_appropriate | reviewed_inappropriate.
-record(mturk_hit, {
          hit_id::string(),
          hit_type_id::string(),
          creation_time::datetime(),
          hit_status::mturk_hit_status(),
          max_assignments=1::1..1000000000,
          auto_approval_delay_in_seconds=259200::0..2592000,
          lifetime_in_seconds::30..3153600,
          assignment_duration_in_seconds::30..3153600,
          reward::#mturk_money{},
          title::string(),
          description::string(),
          keywords::[string()],
          question::mturk_question(),
          qualification_requirements=[]::[#mturk_qualification_requirement{}],
          requester_annotation::string(),
          hit_review_status::mturk_hit_review_status(),
          expiration::integer(),
          number_of_assignments_pending::non_neg_integer(),
          number_of_assignments_available::non_neg_integer(),
          number_of_assignments_completed::non_neg_integer(),
          number_of_similar_hits::non_neg_integer()
         }).

-record(mturk_answer, {
          question_identifier::string(),
          free_text::string(),
          selection_identifier::string(),
          uploaded_file_size_in_bytes::non_neg_integer(),
          uploaded_file_key::string()
         }).
