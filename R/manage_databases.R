library(tidyverse)
library(janitor)
library(purrr)

# replacement function

support_replacement <- function(input_df, input_selection_str = "i.", input_replacement_str = "") {
  input_df |> 
    dplyr::select(starts_with(input_selection_str)) |> 
    dplyr::rename_with(.fn = ~str_replace(string = .x, pattern = input_selection_str, replacement =  input_replacement_str))
}

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
colnames(df_apeal)


# DPR ---------------------------------------------------------------------
db_loc_dpr1 <- "support_files/databases/DPR_List of beneficiaries for NFI assistance_Lamwo_ECHO DPR.xlsx"
# df_dpr1 <- readxl::read_excel(path = db_loc_dpr1)
# df_list <- map_df(set_names(excel_sheets(db_loc_dpr1)),
#                ~read_excel(col_types = ), path = db_loc_dpr1)

df_dpr1 <- purrr::map2_df(.x = rio::import_list(db_loc_dpr1),
               .y = readxl::excel_sheets(db_loc_dpr1),
               ~{  .x  |>  
                   dplyr::mutate(Block = as.character(Block),
                                 sheet_name = .y )
               }) |> 
  clean_names() |> 
  as_tibble() |> 
  mutate(i.dataset_desc = "DPR_NFI_Lamwo_ECHO",
         i.sheet_name = sheet_name,
         i.beneficiary_name = name_in_full,
         i.age = age,
         i.gender = gender,
         i.household_no = ration_card_number,
         i.settlement = "Lamwo",
         i.vulnerability_status = vulnerability_category,) |> 
  support_replacement()

colnames(df_dpr1)

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
  
colnames(df_dpr2)

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

colnames(df_dpr_mpc_rhino)

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

colnames(df_dpr_mpc_imvepi)

# EQUATE ------------------------------------------------------------------

db_loc_equate <- "support_files/databases/EQUATE Beneficary List-23.01.2023.xlsx"
df_equate_Palabek <- readxl::read_excel(path = db_loc_equate, sheet = "Palabek", skip = 1) |> 
  mutate(settlement = "Palabek", sheet_name = "Palabek")
df_equate_Rhino <- readxl::read_excel(path = db_loc_equate, sheet = "Rhino", skip = 1) |> 
  mutate(settlement = "Rhino camp", sheet_name = "Rhino camp")
df_equate_Imvepi <- readxl::read_excel(path = db_loc_equate, sheet = "Imvepi", skip = 1) |> 
  mutate(settlement = "Imvepi", sheet_name = "Imvepi")

df_equate = bind_rows(df_equate_Palabek, df_equate_Rhino, df_equate_Imvepi) |> 
  clean_names() |> 
mutate(i.dataset_desc = "EQUATE Beneficary List",
       i.sheet_name = sheet_name,
       i.beneficiary_name = name,
       i.gender = gender,
       i.settlement = settlement,
       i.mobile_phone = contact
) |>
support_replacement()
  
colnames(df_equate)

# INCLUDE -----------------------------------------------------------------
# df_include
db_loc_include_cash <- "support_files/databases/INCLUDE_Cash Beneficiary List Term III 2022_ Kyaka II Rhino and Imvepi v.xlsx"
df_include_cash_Scholastic_Primary <- readxl::read_excel(path = db_loc_include_cash, sheet = "Cash for Scholastic-Primary", skip = 3) |> 
  mutate(sheet_name = "Cash for Scholastic-Primary")
df_include_cash_Education_Primary <- readxl::read_excel(path = db_loc_include_cash, sheet = "Cash for Education - Primary", skip = 3) |> 
  mutate(sheet_name = "Cash for Education - Primary")
df_include_cash_Scholastics_Secondary <- readxl::read_excel(path = db_loc_include_cash, sheet = "Cash for Scholastics Secondary", skip = 3) |> 
  mutate(sheet_name = "Cash for Scholastics Secondary")
df_include_cash_Education_Secondary <- readxl::read_excel(path = db_loc_include_cash, sheet = "Cash for Education Secondary", skip = 3) |> 
  mutate(sheet_name = "Cash for Education Secondary")

df_include <- bind_rows(df_include_cash_Scholastic_Primary,
                        df_include_cash_Education_Primary,
                        df_include_cash_Scholastics_Secondary,
                        df_include_cash_Education_Secondary) |> 
  clean_names() |> 
  mutate(i.dataset_desc = "INCLUDE_Cash Beneficiary List Term III 2022",
         i.sheet_name = sheet_name,
         i.beneficiary_name = name_of_household_head,
         i.household_no = household_number,
         i.individual_no = individual_number,
         i.settlement = settlement
  ) |>
  support_replacement()

colnames(df_include_cash)

# db_loc_include_nrc
db_loc_include_nrc <- "support_files/databases/INCLUDE_NRC consolidated Cash beneficiary_Omugo-Rhino Camp  Nakivale.xlsx"
# df_include_nrc <- readxl::read_excel(path = db_loc_include_nrc)

df_include_nrc_West_Nile <- readxl::read_excel(path = db_loc_include_nrc, sheet = "West Nile (EiE & Scholastic ben") |> 
  mutate(sheet_name = "West Nile (EiE & Scholastic ben")
df_include_nrc_South_West <- readxl::read_excel(path = db_loc_include_nrc, sheet = "South West (EiE & Scholatics be") |> 
  mutate(sheet_name = "South West (EiE & Scholatics be")

df_include_nrc <- bind_rows(df_include_nrc_West_Nile, df_include_nrc_South_West) |> 
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

colnames(df_include_nrc)

db_loc_include_scholastics <- "support_files/databases/INCLUDE_Scholastics+EiE Paid (Equity)_ Kyangwali list 1_ Term II.xlsx"
df_include_scholastics <- readxl::read_excel(path = db_loc_include_scholastics, skip = 4)
colnames(df_include_scholastics)

db_loc_include_scholastics2 <- "support_files/databases/INCLUDE_Scholistics + EiE Paid Beyonic_ Kyangwali List 2_ Term II.xlsx"
df_include_scholastics2 <- readxl::read_excel(path = db_loc_include_scholastics2, skip = 4)
colnames(df_include_scholastics)

# Legal Assistance --------------------------------------------------------
db_loc_la <- "support_files/databases/Legal Assistance Data base  Rhino Camp 2021-2022.xlsx"
df_la <- readxl::read_excel(path = db_loc_la, skip = 1)
colnames(df_la)

# NRC ---------------------------------------------------------------------
db_loc_nrc_isingiro <- "support_files/databases/NRC_Isingiro Beneficiaries' EXCEL database 2022.xlsx"
df_nrc_isingiro <- readxl::read_excel(path = db_loc_nrc_isingiro)
colnames(df_nrc_isingiro)

db_loc_nrc_la <- "support_files/databases/NRC_Legal Assistance Data base  Rhino Camp 2021-2022 copy.xlsx"
df_nrc_la <- readxl::read_excel(path = db_loc_nrc_la, skip = 1)
colnames(df_nrc_la)

# UCC ---------------------------------------------------------------------
db_loc_ucc_mpct_22_23 <- "support_files/databases/UCC MPCT Beneficiaries July 2022-2023.xlsx"
df_ucc_mpct_22_23 <- readxl::read_excel(path = db_loc_ucc_mpct_22_23)
colnames(df_ucc_mpct_22_23)

db_loc_ucc_mpct_21_22_kyangwali <- "support_files/databases/UCC MPCT Beneficiary List Aug 2021- June 2022 Kyangwali.xlsx"
df_ucc_mpct_21_22_kyangwali <- readxl::read_excel(path = db_loc_ucc_mpct_21_22_kyangwali, skip = 8)
colnames(df_ucc_mpct_21_22_kyangwali)

db_loc_ucc_mpct_22_23_palabek <- "support_files/databases/UCC MPCT Beneficiary List Aug 2022-2023 Palabek.xlsx"
df_ucc_mpct_22_23_palabek <- readxl::read_excel(path = db_loc_ucc_mpct_22_23_palabek, skip = 8)
colnames(df_ucc_mpct_22_23_palabek)

db_loc_ucc_mpct_21_22 <- "support_files/databases/UCC MPCT Beneficiary list JUN 2021-AUG 2022.xlsx"
df_ucc_mpct_21_22 <- readxl::read_excel(path = db_loc_ucc_mpct_21_22)
colnames(df_ucc_mpct_21_22)

db_loc_ucc_mpct_22_23_kyaka <- "support_files/databases/UCC MPCT Beneficiary List Kyaka Aug 2022- 2023.xlsx"
df_ucc_mpct_22_23_kyaka <- readxl::read_excel(path = db_loc_ucc_mpct_22_23_kyaka, skip = 9)
colnames(df_ucc_mpct_22_23_kyaka)

db_loc_ucc_mpct_22_23_kyangwali <- "support_files/databases/UCC MPCT Beneficiary List Kyangwali Aug 2022-2023.xlsx"
df_ucc_mpct_22_23_kyangwali <- readxl::read_excel(path = db_loc_ucc_mpct_22_23_kyangwali, skip = 1)
colnames(df_ucc_mpct_22_23_kyangwali)

