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
  filter(consent == "yes", age >= 18, i.check.start_date > as_date("2022-03-20"), 
         !str_detect(string = hh_id, pattern = fixed('test', ignore_case = TRUE))
  )

df_survey <- readxl::read_excel("inputs/BNA_quant_tool.xlsx", sheet = "survey")
df_choices <- readxl::read_excel("inputs/BNA_quant_tool.xlsx", sheet = "choices")

# output holder -----------------------------------------------------------

logic_output <- list()

# check duplicate uuids ---------------------------------------------------

df_c_duplicate_uuid <-  check_duplicates_by_uuid(input_tool_data = df_tool_data)

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_c_duplicate_uuid")

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

# combined logical checks ----------------------------------------------------------

df_logic_checks <- bind_rows(logic_output)

# others checks

df_others_data <- extract_other_data(input_tool_data = df_tool_data, 
                                     input_survey = df_survey, 
                                     input_choices = df_choices)

# combine logic and others checks
df_combined_checks <- bind_rows(df_logic_checks, df_others_data)

# output the resulting data frame
write_csv(x = df_combined_checks, file = paste0("outputs/", butteR::date_file_prefix(), "_combined_checks_caregiver.csv"), na = "")
