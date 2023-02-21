library(tidyverse)
library(janitor)
library(purrr)

# replacement function

support_replacement <- function(input_df, input_selection_str = "i.", input_replacement_str = "") {
  input_df |> 
    dplyr::select(starts_with(input_selection_str)) |> 
    dplyr::rename_with(.fn = ~str_replace(string = .x, pattern = input_selection_str, replacement =  input_replacement_str))
}

merged_data_list <- list()

# APEAL -------------------------------------------------------------------
db_loc_apeal <- "support_files/databases/APEAL IV Beneficiary Database.xlsx"
df_apeal <- readxl::read_excel(path = db_loc_apeal, sheet = "APEAL PROJECT", skip = 1) |> 
  clean_names() |> 
  mutate(i.beneficiary_name = beneficiary_name,
         i.age_band = age_band_0_59months_5_17_18_49_50,
         i.gender = sex_m_f,
         i.household_no = household_no,
         i.individual_no = individual_no_national_id_number,
         i.nationality = nationality,
         i.status = status_refugee_host,
         i.date_of_enrollment = date_of_enrollment_dd_mm_yy,
         i.settlement = settlement,
         i.current_place_residence = current_place_residence_zone_village_block_host_village_name,
         i.vulnerability_status = vulnerability_status_code,
         i.case_number = case_number,
         i.implementing_agency = implementing_agency
         ) |> 
  support_replacement()

add_checks_data_to_list(input_list_name = "merged_data_list", input_df_name = "df_apeal")


# DPR ---------------------------------------------------------------------
# db_loc_dpr1 <- "support_files/databases/DPR_List of beneficiaries for NFI assistance_Lamwo_ECHO DPR.xlsx"
# # df_dpr1 <- readxl::read_excel(path = db_loc_dpr1)
# # df_list <- map_df(set_names(excel_sheets(db_loc_dpr1)),
# #                ~read_excel(col_types = ), path = db_loc_dpr1)
# 
# df_dpr1 <- purrr::map2_df(.x = rio::import_list(db_loc_dpr1),
#                .y = readxl::excel_sheets(db_loc_dpr1),
#                ~{  .x  |>  
#                    dplyr::mutate(Block = as.character(Block),
#                                  sheet_name = .y )
#                }) |> 
#   clean_names() |> 
#   as_tibble() |> 
#   mutate(i.dataset_desc = "DPR_NFI_Lamwo_ECHO",
#          i.sheet_name = sheet_name,
#          i.beneficiary_name = name_in_full,
#          i.age = age,
#          i.gender = gender,
#          i.household_no = ration_card_number,
#          i.settlement = "Lamwo",
#          i.vulnerability_status = vulnerability_category,) |> 
#   support_replacement()
# 
# colnames(df_dpr1)

db_loc_dpr2 <- "support_files/databases/DPR_List of beneficiaries for NFIs Assistance_Lamwo_ECHO DPR_2.xlsx"
# df_dpr2 <- readxl::read_excel(path = db_loc_dpr2)
# other sheets exist but need investigation [General, Zone 7, Zone 8, Zone 8 (no ratio card captured), 
# Zone5A, Zone 5A (no ratio card captured]
df_dpr2 <- purrr::map2_df(.x = rio::import_list(db_loc_dpr2),
                          .y = readxl::excel_sheets(db_loc_dpr2),
                          ~{  .x |>  
                              dplyr::mutate(Block = as.character(Block),
                                            sheet_name = .y )
                          }) |> 
  clean_names() |> 
  as_tibble() |> 
  mutate(i.dataset_desc = "DPR_NFI_Lamwo_ECHO2",
         i.sheet_name = sheet_name,
         i.beneficiary_name = name_in_full,
         i.age = age,
         i.gender = gender,
         i.household_no = ration_card_number,
         i.settlement = "Lamwo",
         i.vulnerability_status = vulnerability_category,) |> 
  support_replacement() |> 
  filter(sheet_name == "General")
  
add_checks_data_to_list(input_list_name = "merged_data_list", input_df_name = "df_dpr2")

db_loc_dpr_mpc_rhino <- "support_files/databases/DPR_MPCT Beneficiary List for Rhino Camp Settlement.xlsx"
df_dpr_mpc_rhino <- readxl::read_excel(path = db_loc_dpr_mpc_rhino, skip = 2) |> 
  clean_names() |> 
  mutate(i.dataset_desc = "DPR_MPCT_Rhino Camp",
         i.beneficiary_name = fp_full_name,
         i.age = psn_age,
         i.age_band = age_group_date_of_birth,
         i.gender = sex,
         i.household_no = registration_group,
         i.individual_no = psn_individual_id,
         i.settlement = "Rhino camp",
         i.mobile_phone = record_mobile_phone_number_registered_for_mobile_money_if_not_record_no_phone
         ) |> 
  support_replacement()

add_checks_data_to_list(input_list_name = "merged_data_list", input_df_name = "df_dpr_mpc_rhino")

db_loc_dpr_mpc_imvepi <- "support_files/databases/DPR_MPCT Beneficiary List_Imvepi refugee settlement.xlsx"
df_dpr_mpc_imvepi <- readxl::read_excel(path = db_loc_dpr_mpc_imvepi, skip = 3) |> 
  clean_names() |> 
  mutate(i.dataset_desc = "DPR_MPCT_Imvepi Camp",
         i.beneficiary_name = beneficiary_name,
         i.age = age_dob,
         i.gender = gender,
         i.household_no = group_no,
         i.individual_no = individual_no,
         i.settlement = "Imvepi",
         i.mobile_phone = mo_mo_number
  ) |> 
  support_replacement()

add_checks_data_to_list(input_list_name = "merged_data_list", input_df_name = "df_dpr_mpc_imvepi")

# EQUATE ------------------------------------------------------------------

db_loc_equate <- "support_files/databases/EQUATE Beneficary List-23.01.2023.xlsx"

df_equate <- purrr::map2_df(.x = rio::import_list(db_loc_equate, skip =1),
                            .y = readxl::excel_sheets(db_loc_equate),
                            ~{  .x  |>  
                                dplyr::mutate(sheet_name = .y )
                            }) |> 
  clean_names() |> 
  as_tibble() |> 
  clean_names() |> 
  mutate(i.dataset_desc = "EQUATE Beneficary List",
         i.sheet_name = sheet_name,
         i.beneficiary_name = name,
         i.gender = gender,
         i.settlement = sheet_name,
         i.mobile_phone = contact
  ) |>
  support_replacement()
  
add_checks_data_to_list(input_list_name = "merged_data_list", input_df_name = "df_equate")

# INCLUDE -----------------------------------------------------------------
# df_include
db_loc_include_cash <- "support_files/databases/INCLUDE_Cash Beneficiary List Term III 2022_ Kyaka II Rhino and Imvepi v.xlsx"

df_include <- purrr::map2_df(.x = rio::import_list(db_loc_include_cash, skip = 3),
                            .y = readxl::excel_sheets(db_loc_include_cash),
                            ~{  .x  |>  
                                dplyr::mutate(sheet_name = .y )
                            }) |> 
  clean_names() |> 
  as_tibble() |> 
  clean_names() |> 
  mutate(i.dataset_desc = "INCLUDE_Cash Beneficiary List Term III 2022",
         i.sheet_name = sheet_name,
         i.beneficiary_name = name_of_household_head,
         i.household_no = household_number,
         i.individual_no = individual_number,
         i.settlement = settlement
  ) |>
  support_replacement()

add_checks_data_to_list(input_list_name = "merged_data_list", input_df_name = "df_include")

# db_loc_include_nrc
db_loc_include_nrc <- "support_files/databases/INCLUDE_NRC consolidated Cash beneficiary_Omugo-Rhino Camp  Nakivale.xlsx"

df_include_nrc <- purrr::map2_df(.x = rio::import_list(db_loc_include_nrc),
                             .y = readxl::excel_sheets(db_loc_include_nrc),
                             ~{  .x  |>  
                                 dplyr::mutate(sheet_name = .y )
                             }) |> 
  clean_names() |> 
  as_tibble() |> 
  clean_names() |> 
  mutate(i.dataset_desc = "INCLUDE_NRC consolidated Cash beneficiary",
         i.sheet_name = sheet_name,
         i.beneficiary_name = name_of_household_head,
         i.gender = gender,
         i.household_no = ifelse(is.na(group_household_number), group_id, group_household_number),
         i.individual_no = ifelse(is.na(refugee_number_of_household_head_or_caregiver), individual_id, refugee_number_of_household_head_or_caregiver),
         i.settlement = ifelse(is.na(settlement), location, settlement),
         i.mobile_phone = household_phone_number_mtn,
         i.vulnerability_status = vulnerability
  ) |>
  support_replacement()

add_checks_data_to_list(input_list_name = "merged_data_list", input_df_name = "df_include_nrc")

# db_loc_include_scholastics
db_loc_include_scholastics <- "support_files/databases/INCLUDE_Scholastics+EiE Paid (Equity)_ Kyangwali list 1_ Term II.xlsx"
# df_include_scholastics <- readxl::read_excel(path = db_loc_include_scholastics, skip = 4)

df_include_scholastics <- purrr::map2_df(.x = rio::import_list(db_loc_include_scholastics, skip = 4),
                                 .y = readxl::excel_sheets(db_loc_include_scholastics),
                                 ~{  .x  |>  
                                     dplyr::mutate(sheet_name = .y )
                                 }) |> 
  clean_names() |> 
  as_tibble() |> 
  clean_names() |> 
  mutate(i.dataset_desc = "INCLUDE_Scholastics+EiE Paid (Equity)_ Kyangwali list 1",
         i.sheet_name = sheet_name,
         i.beneficiary_name = full_name_focal_point_individual,
         i.gender = sex_focal_point,
         i.household_no = registration_group_id,
         i.settlement = "Kyangwali",
         i.zone = zone,
         i.purpose_of_cash = purpose_of_cash
  ) |>
  support_replacement()

add_checks_data_to_list(input_list_name = "merged_data_list", input_df_name = "df_include_scholastics")

# db_loc_include_scholastics2
db_loc_include_scholastics2 <- "support_files/databases/INCLUDE_Scholistics + EiE Paid Beyonic_ Kyangwali List 2_ Term II.xlsx"

df_include_scholastics2 <- purrr::map2_df(.x = rio::import_list(db_loc_include_scholastics2, skip = 4),
                                         .y = readxl::excel_sheets(db_loc_include_scholastics2),
                                         ~{  .x  |>  
                                             dplyr::mutate(sheet_name = .y )
                                         }) |> 
  clean_names() |> 
  as_tibble() |> 
  clean_names() |> 
  mutate(i.dataset_desc = "INCLUDE_Scholastics+EiE Paid Beyonic_ Kyangwali list 2",
         i.sheet_name = sheet_name,
         i.beneficiary_name = full_name_focal_point_individual,
         i.gender = sex_focal_point,
         i.household_no = registration_group_id,
         i.settlement = "Kyangwali",
         i.zone = zone,
         i.purpose_of_cash = purpose_of_cash
  ) |>
  support_replacement()

add_checks_data_to_list(input_list_name = "merged_data_list", input_df_name = "df_include_scholastics2")

# Legal Assistance --------------------------------------------------------
db_loc_la <- "support_files/databases/Legal Assistance Data base  Rhino Camp 2021-2022.xlsx"

# in some sheets, the first row is empty while others are completely empty

xl_sheets_list <- c("June 2021", "July 2021", "Aug 2021", "Sept 2021", "Oct 2021", "Nov 2021",
                     "April 2022", "May 2022", "June 2022", "July 2022", "August 2022", "September 2022", "October 2022"
                     )

# xl_sheets_to_consider <- readxl::excel_sheets(db_loc_la)[readxl::excel_sheets(db_loc_la) %in% xl_sheets_list]

df_la <- purrr::map2_df(.x = rio::import_list(db_loc_la, which = xl_sheets_list),
                                          .y = xl_sheets_list,
                                          ~{  .x  |>  
                                              dplyr::mutate(sheet_name = .y,
                                                            Age = as.character(Age)) |> 
                                              clean_names() |> 
                                              select(-c(21))
                                          }) |> 
  as_tibble() |> 
  mutate(i.dataset_desc = "Legal Assistance Data base  Rhino Camp",
         i.sheet_name = sheet_name,
         i.beneficiary_name = ifelse(is.na(beneficary_name) & !is.na(cleints_name), cleints_name, beneficary_name) ,
         i.age_band = age,
         i.gender = gender,
         i.individual_no = individual_number,
         i.settlement = area_settlement,
         i.zone = zone_village,
         i.mobile_phone = telephone,
         i.nationality = counrty_of_origin
  ) |>
  support_replacement()

add_checks_data_to_list(input_list_name = "merged_data_list", input_df_name = "df_la")

# NRC ---------------------------------------------------------------------
db_loc_nrc_isingiro <- "support_files/databases/NRC_Isingiro Beneficiaries' EXCEL database 2022.xlsx"
df_nrc_isingiro <- readxl::read_excel(path = db_loc_nrc_isingiro)
colnames(df_nrc_isingiro)

add_checks_data_to_list(input_list_name = "merged_data_list", input_df_name = "")

db_loc_nrc_la <- "support_files/databases/NRC_Legal Assistance Data base  Rhino Camp 2021-2022 copy.xlsx"
df_nrc_la <- readxl::read_excel(path = db_loc_nrc_la, skip = 1)
colnames(df_nrc_la)

add_checks_data_to_list(input_list_name = "merged_data_list", input_df_name = "")

# UCC ---------------------------------------------------------------------
db_loc_ucc_mpct_22_23 <- "support_files/databases/UCC MPCT Beneficiaries July 2022-2023.xlsx"
df_ucc_mpct_22_23 <- readxl::read_excel(path = db_loc_ucc_mpct_22_23)
colnames(df_ucc_mpct_22_23)

add_checks_data_to_list(input_list_name = "merged_data_list", input_df_name = "")

db_loc_ucc_mpct_21_22_kyangwali <- "support_files/databases/UCC MPCT Beneficiary List Aug 2021- June 2022 Kyangwali.xlsx"
df_ucc_mpct_21_22_kyangwali <- readxl::read_excel(path = db_loc_ucc_mpct_21_22_kyangwali, skip = 8)
colnames(df_ucc_mpct_21_22_kyangwali)

add_checks_data_to_list(input_list_name = "merged_data_list", input_df_name = "")

db_loc_ucc_mpct_22_23_palabek <- "support_files/databases/UCC MPCT Beneficiary List Aug 2022-2023 Palabek.xlsx"
df_ucc_mpct_22_23_palabek <- readxl::read_excel(path = db_loc_ucc_mpct_22_23_palabek, skip = 8)
colnames(df_ucc_mpct_22_23_palabek)

add_checks_data_to_list(input_list_name = "merged_data_list", input_df_name = "")

db_loc_ucc_mpct_21_22 <- "support_files/databases/UCC MPCT Beneficiary list JUN 2021-AUG 2022.xlsx"
df_ucc_mpct_21_22 <- readxl::read_excel(path = db_loc_ucc_mpct_21_22)
colnames(df_ucc_mpct_21_22)

add_checks_data_to_list(input_list_name = "merged_data_list", input_df_name = "")

db_loc_ucc_mpct_22_23_kyaka <- "support_files/databases/UCC MPCT Beneficiary List Kyaka Aug 2022- 2023.xlsx"
df_ucc_mpct_22_23_kyaka <- readxl::read_excel(path = db_loc_ucc_mpct_22_23_kyaka, skip = 9)
colnames(df_ucc_mpct_22_23_kyaka)

add_checks_data_to_list(input_list_name = "merged_data_list", input_df_name = "")

db_loc_ucc_mpct_22_23_kyangwali <- "support_files/databases/UCC MPCT Beneficiary List Kyangwali Aug 2022-2023.xlsx"
df_ucc_mpct_22_23_kyangwali <- readxl::read_excel(path = db_loc_ucc_mpct_22_23_kyangwali, skip = 1)
colnames(df_ucc_mpct_22_23_kyangwali)

add_checks_data_to_list(input_list_name = "merged_data_list", input_df_name = "")
