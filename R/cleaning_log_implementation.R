library(tidyverse)
library(lubridate)
library(glue)

source("R/support_functions.R")

# read data ---------------------------------------------------------------

# sheets

hh_roster <- readxl::read_excel(path = "inputs/BNA_data.xlsx", sheet = "hh_roster")

children_school_aged_qns <- readxl::read_excel(path = "inputs/BNA_data.xlsx", sheet = "children_school_aged_qns") %>% 
  mutate(across(.cols = everything(), .fns = ~ifelse(str_detect(string = ., pattern = fixed(pattern = "N/A", ignore_case = TRUE)), "NA", .)))

child_nutrition_qns <- readxl::read_excel(path = "inputs/BNA_data.xlsx", sheet = "child_nutrition_qns") %>% 
  mutate(across(.cols = everything(), .fns = ~ifelse(str_detect(string = ., pattern = fixed(pattern = "N/A", ignore_case = TRUE)), "NA", .)))

child_marriage_outside_hh_r <- readxl::read_excel(path = "inputs/BNA_data.xlsx", sheet = "child_marriage_outside_hh_r") %>% 
  mutate(across(.cols = everything(), .fns = ~ifelse(str_detect(string = ., pattern = fixed(pattern = "N/A", ignore_case = TRUE)), "NA", .)))

# main dataset
data_nms <- names(readxl::read_excel(path = "inputs/BNA_data.xlsx", sheet = "UGA2022 BNA_March2022_HH", n_max = 100))
c_types <- ifelse(str_detect(string = data_nms, pattern = "_other$"), "text", "guess")

df_raw_data <- readxl::read_excel(path = "inputs/BNA_data.xlsx", sheet = "UGA2022 BNA_March2022_HH", col_types = c_types) %>% 
  filter(consent == "yes", age >= 18, as_date(start) > as_date("2022-04-05"), 
         !str_detect(string = hh_id, pattern = fixed('test', ignore_case = TRUE))
  ) %>% 
  mutate(across(.cols = everything(), .fns = ~ifelse(str_detect(string = ., pattern = fixed(pattern = "N/A", ignore_case = TRUE)), "NA", .)))

df_raw_data_hh_roster <- df_raw_data %>% 
  select(-`_index`) %>% 
  inner_join(hh_roster, by = c("_uuid" = "_submission__uuid") ) 

df_raw_data_children_school_aged_qns <- df_raw_data %>% 
  select(-`_index`) %>% 
  inner_join(children_school_aged_qns, by = c("_uuid" = "_submission__uuid") ) 

df_raw_data_child_nutrition_qns <- df_raw_data %>% 
  select(-`_index`) %>% 
  inner_join(child_nutrition_qns, by = c("_uuid" = "_submission__uuid") ) 

df_raw_data_child_marriage_outside_hh_r <- df_raw_data %>% 
  select(-`_index`) %>% 
  inner_join(child_marriage_outside_hh_r, by = c("_uuid" = "_submission__uuid") ) 

# cleaning log
df_cleaning_log <- read_csv("inputs/combined_checks_bna.csv", col_types = cols(sheet = "c", index = "i")) %>% 
  mutate(adjust_log = ifelse(is.na(adjust_log), "apply_suggested_change", adjust_log),
         value = ifelse(is.na(value) & comment == "implement_logical_change", "blank", value),
         value = ifelse(is.na(value) & issue_id %in% c("logic_c_outlier"), "blank", value),
         value = ifelse(is.na(value) & type == "remove_survey", "blank", value)) %>%
  filter(adjust_log != "delete_log", !is.na(value), !is.na(uuid)) %>% 
  mutate(value = ifelse(value == "blank" & comment == "implement_logical_change", NA, value),
         relevant = NA) %>% 
  select(uuid, type, name, value, issue_id, sheet, index, relevant, issue)

# survey tool
df_survey <- readxl::read_excel("inputs/BNA_quant_tool.xlsx", sheet = "survey")
df_choices <- readxl::read_excel("inputs/BNA_quant_tool.xlsx", sheet = "choices")

# main dataset
df_cleaned_data <- implement_cleaning_support(input_df_raw_data = df_raw_data, 
                                              input_df_survey = df_survey, 
                                              input_df_choices = df_choices, 
                                              input_df_cleaning_log = df_cleaning_log %>% filter(name %in% colnames(df_raw_data)))

write_csv(df_cleaned_data, file = paste0("outputs/", butteR::date_file_prefix(), "_clean_data_bna.csv"))

# hh_roster
df_cleaning_log_hh_roster <- df_cleaning_log %>% 
  filter(uuid %in% df_raw_data_hh_roster$`_uuid`, name %in% colnames(df_raw_data_hh_roster))

df_cleaned_hh_roster_data <- implement_cleaning_support(input_df_raw_data = df_raw_data_hh_roster, 
                                                                                    input_df_survey = df_survey, 
                                                                                    input_df_choices = df_choices, 
                                                                                    input_df_cleaning_log = df_cleaning_log_hh_roster) %>% 
  select(any_of(colnames(hh_roster)), `_index` = index, `_submission__uuid` = uuid)

write_csv(df_cleaned_hh_roster_data, file = paste0("outputs/", butteR::date_file_prefix(), "_clean_hh_roster_data_bna.csv"))

# children_school_aged_qns
df_cleaning_log_children_school_aged_qns <- df_cleaning_log %>% 
  filter(uuid %in% df_raw_data_children_school_aged_qns$`_uuid`, name %in% colnames(df_raw_data_children_school_aged_qns))

df_cleaned_children_school_aged_qns_data <- implement_cleaning_support(input_df_raw_data = df_raw_data_children_school_aged_qns, 
                                                                                    input_df_survey = df_survey, 
                                                                                    input_df_choices = df_choices, 
                                                                                    input_df_cleaning_log = df_cleaning_log_children_school_aged_qns) %>% 
  select(any_of(colnames(children_school_aged_qns)), `_index` = index, `_submission__uuid` = uuid)

write_csv(df_cleaned_children_school_aged_qns_data, file = paste0("outputs/", butteR::date_file_prefix(), "_clean_children_school_aged_qns_data_bna.csv"))

# child_nutrition_qns
df_cleaning_log_child_nutrition_qns <- df_cleaning_log %>% 
  filter(uuid %in% df_raw_data_child_nutrition_qns$`_uuid`, name %in% colnames(df_raw_data_child_nutrition_qns))

df_cleaned_child_nutrition_qns_data <- implement_cleaning_support(input_df_raw_data = df_raw_data_child_nutrition_qns, 
                                                                                    input_df_survey = df_survey, 
                                                                                    input_df_choices = df_choices, 
                                                                                    input_df_cleaning_log = df_cleaning_log_child_nutrition_qns) %>% 
  select(any_of(colnames(child_nutrition_qns)), `_index` = index, `_submission__uuid` = uuid)

write_csv(df_cleaned_child_nutrition_qns_data, file = paste0("outputs/", butteR::date_file_prefix(), "_clean_child_nutrition_qns_data_bna.csv"))

# child_marriage_outside_hh_r
df_cleaning_log_child_marriage_outside_hh_r <- df_cleaning_log %>% 
  filter(uuid %in% df_raw_data_child_marriage_outside_hh_r$`_uuid`, name %in% colnames(df_raw_data_child_marriage_outside_hh_r))

df_cleaned_child_marriage_outside_hh_r_data <- implement_cleaning_support(input_df_raw_data = df_raw_data_child_marriage_outside_hh_r, 
                                                                                    input_df_survey = df_survey, 
                                                                                    input_df_choices = df_choices, 
                                                                                    input_df_cleaning_log = df_cleaning_log_child_marriage_outside_hh_r) %>% 
  select(any_of(colnames(child_marriage_outside_hh_r)), `_index` = index, `_submission__uuid` = uuid) 

write_csv(df_cleaned_child_marriage_outside_hh_r_data, file = paste0("outputs/", butteR::date_file_prefix(), "_clean_child_marriage_outside_hh_r_data_bna.csv"))


list_of_clean_datasets <- list("UGA2022 BNA_March2022_HH" = df_cleaned_data,
                               "hh_roster" = df_cleaned_hh_roster_data,
                               "children_school_aged_qns" = df_cleaned_children_school_aged_qns_data,
                               "child_nutrition_qns" = df_cleaned_child_nutrition_qns_data,
                               "child_marriage_outside_hh_r" = df_cleaned_child_marriage_outside_hh_r_data
                               )

openxlsx::write.xlsx(x = list_of_clean_datasets,
                     file = paste0("outputs/", butteR::date_file_prefix(), 
                                   "_clean_data_bna.xlsx"), 
                     overwrite = TRUE)
