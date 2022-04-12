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
  filter(consent == "yes", age >= 18, i.check.start_date > as_date("2022-04-06"), 
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