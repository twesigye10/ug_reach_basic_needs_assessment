
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

# no_hc_accessed_and_not_basic_need_8
df_no_hc_accessed_and_not_basic_need <- df_tool_data %>% 
  filter(hc_accessed == "no" &
           (ability_meet_needs == "all" | !str_detect(string = str_c(c("hc", "hc_children", "hc_women", "hc_disabilities"), collapse = " "),
                         pattern = str_replace_all(string = needs_not_met, pattern = " ", replacement = "|")))) %>% 
  mutate(i.check.type = "change_response",
         i.check.name = "hc_accessed",
         i.check.current_value = hc_accessed,
         i.check.value = "",
         i.check.issue_id = "no_hc_accessed_and_not_basic_need_8",
         i.check.issue = glue("ability_meet_needs: {ability_meet_needs}, needs_not_met: {needs_not_met}"),
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

add_checks_data_to_list(input_list_name = "logic_seperate_output", input_df_name = "df_no_hc_accessed_and_not_basic_need")

# hc_accessed_and_no_hc_needed_9
df_hc_accessed_and_no_hc_needed <- df_tool_data %>% 
  filter((hc_accessed == "yes" | hc_needed == "no") &  str_detect(string = str_c(c("hc", "hc_children", "hc_women", "hc_disabilities"), collapse = " "),
                         pattern = str_replace_all(string = needs_not_met, pattern = " ", replacement = "|"))) %>% 
  mutate(i.check.type = "change_response",
         i.check.name = "hc_accessed",
         i.check.current_value = hc_accessed,
         i.check.value = "",
         i.check.issue_id = "hc_accessed_and_no_hc_needed_9",
         i.check.issue = glue("hc_needed: {hc_needed}, needs_not_met: {needs_not_met}"),
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

add_checks_data_to_list(input_list_name = "logic_seperate_output", input_df_name = "df_hc_accessed_and_no_hc_needed")

# same_value_given_for_all_fcs_10
df_same_value_given_for_all_fcs <- df_tool_data %>% 
  filter(cereals == pulses == vegetables == fruits == tubers == protein == dairy == sugar == oils) %>% 
  mutate(i.check.type = "change_response",
         i.check.name = "cereals",
         i.check.current_value = cereals,
         i.check.value = "",
         i.check.issue_id = "same_vale_given_for_all_fcs_10",
         i.check.issue = glue("pulses: {pulses}, vegetables: {vegetables}, fruits: {fruits}, tubers: {tubers}, protein: {protein}, dairy: {dairy}, sugar: {sugar}, oils: {oils}"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "Same integer given for all FCS food groups", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.uuid_cl = "",
         i.check.so_sm_choices = "") %>% 
  dplyr::select(starts_with("i.check")) %>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

add_checks_data_to_list(input_list_name = "logic_seperate_output", input_df_name = "df_same_value_given_for_all_fcs")

# very_low_consumption_of_staple_foods_11
df_very_low_consumption_of_staple_foods <- df_tool_data %>% 
  filter(cereals <= 2 | tubers <= 2 | pulses <= 2 | vegetables <= 2) %>% 
  mutate(i.check.type = "change_response",
         i.check.name = "cereals",
         i.check.current_value = cereals,
         i.check.value = "",
         i.check.issue_id = "very_low_consumption_of_staple_foods_11",
         i.check.issue = glue("pulses: {pulses}, vegetables: {vegetables}, tubers: {tubers}"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "Same integer given for all FCS food groups", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.uuid_cl = "",
         i.check.so_sm_choices = "") %>% 
  dplyr::select(starts_with("i.check")) %>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

add_checks_data_to_list(input_list_name = "logic_seperate_output", input_df_name = "df_very_low_consumption_of_staple_foods")

# no_child_performs_domestic_labor_but_reports_domestic_labor

df_no_child_performs_domestic_labor_but_reports_domestic_labor <- df_repeat_children_school_aged_qns_data %>%
  group_by("_parent_index") %>% 
  filter(!str_detect(string = paste(child_domestic_labor, collapse = " : "), pattern = "yes"), 
         str_detect(string = non_attending_reason, pattern = "child_domestic_labor")) %>% 
  mutate(i.check.type = "remove_loop_entry",
         i.check.name = "child_domestic_labor",
         i.check.current_value = child_domestic_labor,
         i.check.value = "",
         i.check.issue_id = "no_child_performs_domestic_labor_but_reports_domestic_labor_13",
         i.check.issue = glue("non_attending_reason: {non_attending_reason}"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = " reports no child has to perform domestic labor, but reports domestic labor as the reason", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.uuid_cl = "",
         i.check.so_sm_choices = "") %>% 
  dplyr::select(starts_with("i.check")) %>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

add_checks_data_to_list(input_list_name = "logic_seperate_output", input_df_name = "df_no_child_performs_domestic_labor_but_reports_domestic_labor")

# no_child_performs_economic_labor_but_reports_economic_labor

df_no_child_performs_economic_labor_but_reports_economic_labor <- df_repeat_children_school_aged_qns_data %>%
  group_by("_parent_index") %>% 
  filter(!str_detect(string = paste(child_economic_labor, collapse = " : "), pattern = "yes"), 
         str_detect(string = non_attending_reason, pattern = "child_economic_labor")) %>% 
  mutate(i.check.type = "remove_loop_entry",
         i.check.name = "child_economic_labor",
         i.check.current_value = child_economic_labor,
         i.check.value = "",
         i.check.issue_id = "no_child_performs_economic_labor_but_reports_economic_labor_14",
         i.check.issue = glue("non_attending_reason: {non_attending_reason}"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = " reports no child has to perform economic labor, but reports economic labor as the reason", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.uuid_cl = "",
         i.check.so_sm_choices = "") %>% 
  dplyr::select(starts_with("i.check")) %>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

add_checks_data_to_list(input_list_name = "logic_seperate_output", input_df_name = "df_no_child_performs_economic_labor_but_reports_economic_labor")