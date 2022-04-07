# checks for data collection

library(tidyverse)
library(lubridate)
library(glue)

source("R/support_functions.R")

# read data ---------------------------------------------------------------

df_tool_data <- readxl::read_excel(path = "inputs/BNA_data.xlsx") %>% 
  mutate(i.check.uuid = `_uuid`,
         i.check.start_date = as_date(start),
         i.check.enumerator_id = enumerator_id,
         i.check.location = location,
         i.check.hh_id = hh_id,
         start = as_datetime(start),
         end = as_datetime(end)) %>% 
  filter(consent == "yes", age >= 18, i.check.start_date > as_date("2022-04-06"), 
         !str_detect(string = hh_id, pattern = fixed('test', ignore_case = TRUE))
  )

df_repeat_child_nutrition_qns_data <- readxl::read_excel(path = "inputs/BNA_data.xlsx", sheet = "child_nutrition_qns")
df_repeat_children_school_aged_qns_data <- readxl::read_excel(path = "inputs/BNA_data.xlsx", sheet = "children_school_aged_qns")

df_survey <- readxl::read_excel("inputs/BNA_quant_tool.xlsx", sheet = "survey")
df_choices <- readxl::read_excel("inputs/BNA_quant_tool.xlsx", sheet = "choices")

df_sample_data <- read_csv("inputs/bna_sampling_hhids.csv") %>% 
  janitor::clean_names() %>% 
  rename(unique_hhid_number = id)

# output holder -----------------------------------------------------------

logic_output <- list()

# check duplicate uuids ---------------------------------------------------

df_c_duplicate_uuid <-  check_duplicates_by_uuid(input_tool_data = df_tool_data)

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_c_duplicate_uuid")

# checks on hhids ----------------------------------------------------------

sample_hhid_nos <- df_sample_data %>% 
  pull(unique_hhid_number) %>% 
  unique()

# duplicate point numbers
df_c_duplicate_hhid_nos <- check_duplicate_hhid_numbers(input_tool_data = df_tool_data,
                                                        input_sample_hhid_nos_list = sample_hhid_nos)

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_c_duplicate_hhid_nos")

# pt id does not exist in sample
df_c_hhid_not_in_sample <- check_hhid_number_not_in_samples(input_tool_data = df_tool_data, 
                                                            input_sample_hhid_nos_list = sample_hhid_nos)

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_c_hhid_not_in_sample")

# Time checks -------------------------------------------------------------

# Time interval for the survey
min_time_of_survey <- 20
max_time_of_survey <- 120

df_c_survey_time <-  check_survey_time(input_tool_data = df_tool_data, 
                                       input_min_time = min_time_of_survey, 
                                       input_max_time = max_time_of_survey)

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_c_survey_time")

# check the time between surveys
min_time_btn_surveys <- 5

df_c_time_btn_survey <- check_time_interval_btn_surveys(input_tool_data = df_tool_data,
                                                        input_min_time = min_time_btn_surveys)

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_c_time_btn_survey")

# others checks

df_others_data <- extract_other_data(input_tool_data = df_tool_data, 
                                     input_survey = df_survey, 
                                     input_choices = df_choices)
add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_others_data")

# logical checks for different responses ---------------------------------------

# okay_arrange_marriage_1
df_disagree_child_marriage_but_agree_circumstances <- df_tool_data %>% 
  filter(okay_arrange_marriage %in% c("disagree", "strongly_disagree", "neither_agree_nor_agree") &
           (okay_arrange_marriage_money %in% c("agree", "strongly_agree") |
              okay_arrange_marriage_safety %in% c("agree", "strongly_agree"))) %>% 
  mutate(i.check.type = "change_response",
         i.check.name = "okay_arrange_marriage",
         i.check.current_value = okay_arrange_marriage,
         i.check.value = "",
         i.check.issue_id = "logic_c_okay_arrange_marriage_1",
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

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_disagree_child_marriage_but_agree_circumstances")

# okay_arrange_marriage_2
df_agree_child_marriage_but_disagree_circumstances <- df_tool_data %>% 
  filter(okay_arrange_marriage %in% c("agree", "strongly_agree") &
           (okay_arrange_marriage_money %in% c("disagree", "strongly_disagree", "neither_agree_nor_agree") |
              okay_arrange_marriage_safety %in% c("disagree", "strongly_disagree", "neither_agree_nor_agree"))) %>% 
  mutate(i.check.type = "change_response",
         i.check.name = "okay_arrange_marriage",
         i.check.current_value = okay_arrange_marriage,
         i.check.value = "",
         i.check.issue_id = "logic_c_okay_arrange_marriage_2",
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

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_agree_child_marriage_but_disagree_circumstances")

# okay_arrange_marriage_3
df_agree_child_marriage_but_disagree_girl_to_be_married <- df_tool_data %>% 
  filter(okay_arrange_marriage %in% c("agree", "strongly_agree") &
           okay_girl_to_be_married %in% c("disagree", "strongly_disagree", "neither_agree_nor_agree")) %>% 
  mutate(i.check.type = "change_response",
         i.check.name = "okay_arrange_marriage",
         i.check.current_value = okay_arrange_marriage,
         i.check.value = "",
         i.check.issue_id = "logic_c_okay_arrange_marriage_3",
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

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_agree_child_marriage_but_disagree_girl_to_be_married")

# okay_arrange_marriage_4
df_disagree_child_marriage_but_agree_girl_to_be_married <- df_tool_data %>% 
  filter(okay_arrange_marriage %in% c("disagree", "strongly_disagree", " neiter_agree_not_agree") &
           okay_girl_to_be_married %in% c("agree", "strongly_agree")) %>% 
  mutate(i.check.type = "change_response",
         i.check.name = "okay_arrange_marriage",
         i.check.current_value = okay_arrange_marriage,
         i.check.value = "",
         i.check.issue_id = "logic_c_okay_arrange_marriage_4",
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

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_disagree_child_marriage_but_agree_girl_to_be_married")

# inability_meet_needs_5
df_inability_meet_needs_no_lcsi <- df_tool_data %>% 
  filter(ability_meet_needs %in% c("none", "some", "about_half") &
           lcsi_intro == "no") %>% 
  mutate(i.check.type = "change_response",
         i.check.name = "ability_meet_needs",
         i.check.current_value = ability_meet_needs,
         i.check.value = "",
         i.check.issue_id = "logic_c_inability_meet_needs_5",
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

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_inability_meet_needs_no_lcsi")

# ability_meet_needs_6
df_ability_meet_needs_yes_lcsi <- df_tool_data %>% 
  filter(ability_meet_needs %in% c("all", "most") &
           lcsi_intro == "yes") %>% 
  mutate(i.check.type = "change_response",
         i.check.name = "ability_meet_needs",
         i.check.current_value = ability_meet_needs,
         i.check.value = "",
         i.check.issue_id = "logic_c_ability_meet_needs_6",
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

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_ability_meet_needs_yes_lcsi")

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
         i.check.issue_id = "logic_c_lcsi_intro_7",
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

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_yes_lcsi_but_no_other_circumstances")

# no_hc_accessed_and_not_basic_need_8
df_no_hc_accessed_and_not_basic_need <- df_tool_data %>% 
  filter(hc_accessed == "no" &
           (ability_meet_needs == "all" | !str_detect(string = str_c(c("hc", "hc_children", "hc_women", "hc_disabilities"), collapse = " "),
                                                      pattern = str_replace_all(string = needs_not_met, pattern = " ", replacement = "|")))) %>% 
  mutate(i.check.type = "change_response",
         i.check.name = "hc_accessed",
         i.check.current_value = hc_accessed,
         i.check.value = "",
         i.check.issue_id = "logic_c_no_hc_accessed_and_not_basic_need_8",
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

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_no_hc_accessed_and_not_basic_need")

# hc_accessed_and_no_hc_needed_9
df_hc_accessed_and_no_hc_needed <- df_tool_data %>% 
  filter((hc_accessed == "yes" | hc_needed == "no") &  str_detect(string = str_c(c("hc", "hc_children", "hc_women", "hc_disabilities"), collapse = " "),
                                                                  pattern = str_replace_all(string = needs_not_met, pattern = " ", replacement = "|"))) %>% 
  mutate(i.check.type = "change_response",
         i.check.name = "hc_accessed",
         i.check.current_value = hc_accessed,
         i.check.value = "",
         i.check.issue_id = "logic_c_hc_accessed_and_no_hc_needed_9",
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

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_hc_accessed_and_no_hc_needed")

# same_value_given_for_all_fcs_10
df_same_value_given_for_all_fcs <- df_tool_data %>% 
  filter(cereals == pulses, cereals == vegetables, cereals == fruits, cereals == tubers, 
         cereals == protein, cereals == dairy, cereals == sugar, cereals == oils) %>% 
  mutate(i.check.type = "change_response",
         i.check.name = "cereals",
         i.check.current_value = cereals,
         i.check.value = "",
         i.check.issue_id = "logic_c_same_vale_given_for_all_fcs_10",
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

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_same_value_given_for_all_fcs")

# very_low_consumption_of_staple_foods_11
df_very_low_consumption_of_staple_foods <- df_tool_data %>% 
  filter(cereals <= 2 | tubers <= 2 | pulses <= 2 | vegetables <= 2) %>% 
  mutate(i.check.type = "change_response",
         i.check.name = "cereals",
         i.check.current_value = cereals,
         i.check.value = "",
         i.check.issue_id = "logic_c_very_low_consumption_of_staple_foods_11",
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

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_very_low_consumption_of_staple_foods")

# reported_no_consumption_for_child
df_reported_no_consumption_for_child <- df_repeat_child_nutrition_qns_data %>% 
  filter(breast_fed %in% c("no", "dk"),
         infant_formula %in% c("no", "dk"),
         animal_milk  %in% c("no", "dk"),
         yoghurt %in% c("no", "dk"),
         grains  %in% c("no", "dk"),
         roots  %in% c("no", "dk"),
         pulses_nuts  %in% c("no", "dk"),
         yoghurt_other  %in% c("no", "dk"),
         cheese  %in% c("no", "dk"),
         meat  %in% c("no", "dk"),
         eggs  %in% c("no", "dk"),
         vita_veg  %in% c("no", "dk"),
         vita_fruits  %in% c("no", "dk"),
         other_veg  %in% c("no", "dk"),
         other_fruits  %in% c("no", "dk")) %>% 
  mutate(i.check.type = "remove_loop_entry",
         i.check.name = "",
         i.check.current_value = "",
         i.check.value = "",
         i.check.issue_id = "logic_c_reported_no_consumption_for_child_12",
         i.check.issue = glue("breast_fed: {breast_fed}, infant_formula: {infant_formula}, animal_milk: {animal_milk}, yoghurt: {yoghurt}, grains: {grains}, roots: {roots}, pulses_nuts: {pulses_nuts}, yoghurt_other: {yoghurt_other}, cheese: {cheese}, meat: {meat}, eggs: {eggs}, vita_fruits: {vita_fruits}, other_veg: {other_veg}, other_fruits: {other_fruits}"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "Respondent reports no consumption for child. Follow-up with enumerator", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.uuid_cl = "",
         i.check.so_sm_choices = "") %>% 
  dplyr::select(starts_with("i.check")) %>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_reported_no_consumption_for_child")

# no_child_performs_domestic_labor_but_reports_domestic_labor

df_no_child_performs_domestic_labor_but_reports_domestic_labor <- df_repeat_children_school_aged_qns_data %>%
  group_by(`_parent_index`) %>% 
  filter(!str_detect(string = paste(child_domestic_labor, collapse = " : "), pattern = "yes"), 
         str_detect(string = non_attending_reason, pattern = "child_domestic_labor")) %>% 
  mutate(i.check.type = "remove_loop_entry",
         i.check.name = "child_domestic_labor",
         i.check.current_value = child_domestic_labor,
         i.check.value = "",
         i.check.issue_id = "logic_c_no_child_performs_domestic_labor_but_reports_domestic_labor_13",
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

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_no_child_performs_domestic_labor_but_reports_domestic_labor")

# no_child_performs_economic_labor_but_reports_economic_labor

df_no_child_performs_economic_labor_but_reports_economic_labor <- df_repeat_children_school_aged_qns_data %>%
  group_by("_parent_index") %>% 
  filter(!str_detect(string = paste(child_economic_labor, collapse = " : "), pattern = "yes"), 
         str_detect(string = non_attending_reason, pattern = "child_economic_labor")) %>% 
  mutate(i.check.type = "remove_loop_entry",
         i.check.name = "child_economic_labor",
         i.check.current_value = child_economic_labor,
         i.check.value = "",
         i.check.issue_id = "logic_c_no_child_performs_economic_labor_but_reports_economic_labor_14",
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

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_no_child_performs_economic_labor_but_reports_economic_labor")

# Kyaka_reports_receiving_NRC_aid
df_Kyaka_reports_receiving_NRC_aid <- df_tool_data %>% 
  filter(location == "kyaka", str_detect(string = received_assistance_actor, pattern = "nrc")) %>% 
  mutate(i.check.type = "remove_option",
         i.check.name = "received_assistance_actor",
         i.check.current_value = received_assistance_actor,
         i.check.value = "nrc",
         i.check.issue_id = "logic_c_Kyaka_reports_receiving_NRC_aid_15",
         i.check.issue = glue("location: {location}"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "Respondent in Kyaka reports having received NRC aid, but consortia is not present in this location", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.uuid_cl = "",
         i.check.so_sm_choices = "") %>% 
  dplyr::select(starts_with("i.check")) %>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_Kyaka_reports_receiving_NRC_aid")

# nakivale_reports_receiving_care_apeal_aid
df_nakivale_reports_receiving_care_apeal_aid <- df_tool_data %>% 
  filter(location == "kyaka", str_detect(string = received_assistance_actor, pattern = "care_apeal")) %>% 
  mutate(i.check.type = "remove_option",
         i.check.name = "received_assistance_actor",
         i.check.current_value = received_assistance_actor,
         i.check.value = "nrc",
         i.check.issue_id = "logic_c_nakivale_reports_receiving_care_apeal_aid_16",
         i.check.issue = glue("location: {location}"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "Respondent in Nakivale reports having received APEAL aid, but consortia is not present in this location", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.uuid_cl = "",
         i.check.so_sm_choices = "") %>% 
  dplyr::select(starts_with("i.check")) %>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_nakivale_reports_receiving_care_apeal_aid")

# no_children_in_hh_but_reports_school_withdrawn_chilren
df_no_children_in_hh_but_reports_school_withdrawn_chilren <- df_tool_data %>% 
  filter(withdraw_children == "yes", num_children_school_aged == 0) %>% 
  mutate(i.check.type = "change_response",
         i.check.name = "withdraw_children",
         i.check.current_value = withdraw_children,
         i.check.value = "no",
         i.check.issue_id = "logic_c_no_children_in_hh_but_reports_school_withdrawn_chilren_17",
         i.check.issue = glue("num_children_school_aged: {num_children_school_aged}"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.uuid_cl = "",
         i.check.so_sm_choices = "") %>% 
  dplyr::select(starts_with("i.check")) %>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_no_children_in_hh_but_reports_school_withdrawn_chilren")

# combined logical checks ----------------------------------------------------------

df_combined_checks <- bind_rows(logic_output)

# output the resulting data frame
write_csv(x = df_combined_checks, file = paste0("outputs/", butteR::date_file_prefix(), "_combined_checks_bna.csv"), na = "")
