
source("R/support_functions.R")

# output holder -----------------------------------------------------------

logic_seperate_output <- list()

# okay_arrange_marriage_1
df_disagree_child_marriage_but_agree_circumstances <- df_tool_data %>% 
  filter(okay_arrange_marriage %in% c("disagree", "strongly_disagree", "neither_agree_nor_agree") &
           (okay_arrange_marriage_money %in% c("agree", "strongly_agree") |
              okay_arrange_marriage_safety %in% c("agree", "strongly_agree"))) %>% 
  mutate(i.check.type = "change_response",
         i.check.name = "okay_arrange_marriage",
         i.check.current_value = okay_arrange_marriage,
         i.check.value = "",
         i.check.issue_id = "okay_arrange_marriage_1",
         i.check.issue = glue("okay_arrange_marriage_money: {okay_arrange_marriage_money}, okay_arrange_marriage_safety: {okay_arrange_marriage_safety}"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "accept", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.uuid_cl = "",
         i.check.so_sm_choices = "") %>% 
  dplyr::select(starts_with("i.check")) %>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

add_checks_data_to_list(input_list_name = "logic_seperate_output", input_df_name = "df_disagree_child_marriage_but_agree_circumstances")

# okay_arrange_marriage_2
df_agree_child_marriage_but_disagree_circumstances <- df_tool_data %>% 
  filter(okay_arrange_marriage %in% c("agree", "strongly_agree") &
           (okay_arrange_marriage_money %in% c("disagree", "strongly_disagree", "neither_agree_nor_agree") |
              okay_arrange_marriage_safety %in% c("disagree", "strongly_disagree", "neither_agree_nor_agree"))) %>% 
  mutate(i.check.type = "change_response",
         i.check.name = "okay_arrange_marriage",
         i.check.current_value = okay_arrange_marriage,
         i.check.value = "",
         i.check.issue_id = "okay_arrange_marriage_2",
         i.check.issue = glue("okay_arrange_marriage_money: {okay_arrange_marriage_money}, okay_arrange_marriage_safety: {okay_arrange_marriage_safety}"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "accept", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.uuid_cl = "",
         i.check.so_sm_choices = "") %>% 
  dplyr::select(starts_with("i.check")) %>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

add_checks_data_to_list(input_list_name = "logic_seperate_output", input_df_name = "df_agree_child_marriage_but_disagree_circumstances")

# okay_arrange_marriage_3
df_agree_child_marriage_but_disagree_girl_to_be_married <- df_tool_data %>% 
  filter(okay_arrange_marriage %in% c("agree", "strongly_agree") &
           okay_girl_to_be_married %in% c("disagree", "strongly_disagree", "neither_agree_nor_agree")) %>% 
  mutate(i.check.type = "change_response",
         i.check.name = "okay_arrange_marriage",
         i.check.current_value = okay_arrange_marriage,
         i.check.value = "",
         i.check.issue_id = "okay_arrange_marriage_3",
         i.check.issue = glue("okay_girl_to_be_married: {okay_girl_to_be_married}"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "accept", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.uuid_cl = "",
         i.check.so_sm_choices = "") %>% 
  dplyr::select(starts_with("i.check")) %>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

add_checks_data_to_list(input_list_name = "logic_seperate_output", input_df_name = "df_agree_child_marriage_but_disagree_girl_to_be_married")

# okay_arrange_marriage_4
df_disagree_child_marriage_but_agree_girl_to_be_married <- df_tool_data %>% 
  filter(okay_arrange_marriage %in% c("disagree", "strongly_disagree", " neiter_agree_not_agree") &
           okay_girl_to_be_married %in% c("agree", "strongly_agree")) %>% 
  mutate(i.check.type = "change_response",
         i.check.name = "okay_arrange_marriage",
         i.check.current_value = okay_arrange_marriage,
         i.check.value = "",
         i.check.issue_id = "okay_arrange_marriage_4",
         i.check.issue = glue("okay_girl_to_be_married: {okay_girl_to_be_married}"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "accept", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.uuid_cl = "",
         i.check.so_sm_choices = "") %>% 
  dplyr::select(starts_with("i.check")) %>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

add_checks_data_to_list(input_list_name = "logic_seperate_output", input_df_name = "df_disagree_child_marriage_but_agree_girl_to_be_married")

# inability_meet_needs_5
df_inability_meet_needs_no_lcsi <- df_tool_data %>% 
  filter(ability_meet_needs %in% c("none", "some", "about_half") &
           lcsi_intro == "no") %>% 
  mutate(i.check.type = "change_response",
         i.check.name = "ability_meet_needs",
         i.check.current_value = ability_meet_needs,
         i.check.value = "",
         i.check.issue_id = "inability_meet_needs_5",
         i.check.issue = glue("lcsi_intro: {lcsi_intro}"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "accept", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.uuid_cl = "",
         i.check.so_sm_choices = "") %>% 
  dplyr::select(starts_with("i.check")) %>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

add_checks_data_to_list(input_list_name = "logic_seperate_output", input_df_name = "df_inability_meet_needs_no_lcsi")

# ability_meet_needs_6
df_ability_meet_needs_yes_lcsi <- df_tool_data %>% 
  filter(ability_meet_needs %in% c("all", "most") &
           lcsi_intro == "yes") %>% 
  mutate(i.check.type = "change_response",
         i.check.name = "ability_meet_needs",
         i.check.current_value = ability_meet_needs,
         i.check.value = "",
         i.check.issue_id = "ability_meet_needs_6",
         i.check.issue = glue("lcsi_intro: {lcsi_intro}"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "accept", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.uuid_cl = "",
         i.check.so_sm_choices = "") %>% 
  dplyr::select(starts_with("i.check")) %>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

add_checks_data_to_list(input_list_name = "logic_seperate_output", input_df_name = "df_ability_meet_needs_yes_lcsi")

# lcsi_intro_7
df_yes_lcsi_but_no_other_circumstances <- df_tool_data %>% 
  filter(lcsi_intro == "yes"  &
           increase_number_search_work == "no" & 
           sell_assets == "no" &
           purchase_on_credit == "no" &
           spend_savings == "no" &
           borrow_money == "no" &
           sell_productive_assets == "no" &
           reduce_expenditure == "no" &
           withdraw_children == "no" &
           beg_charity == "no" &
           sell_more_than_usual == "no") %>% 
  mutate(i.check.type = "change_response",
         i.check.name = "lcsi_intro",
         i.check.current_value = lcsi_intro,
         i.check.value = "",
         i.check.issue_id = "lcsi_intro_7",
         i.check.issue = glue("increase_number_search_work: {increase_number_search_work}, sell_assets: {sell_assets}, purchase_on_credit: {purchase_on_credit}, spend_savings: {spend_savings}, borrow_money: {borrow_money}, sell_productive_assets: {sell_productive_assets}, reduce_expenditure: {reduce_expenditure}, withdraw_children: {withdraw_children}, beg_charity: {beg_charity}, sell_more_than_usual: {sell_more_than_usual}"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "accept", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.uuid_cl = "",
         i.check.so_sm_choices = "") %>% 
  dplyr::select(starts_with("i.check")) %>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

add_checks_data_to_list(input_list_name = "logic_seperate_output", input_df_name = "df_yes_lcsi_but_no_other_circumstances")