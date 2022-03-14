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

if(exists("df_disagree_child_marriage_but_agree_circumstances")){
  if(nrow(df_disagree_child_marriage_but_agree_circumstances) > 0){
    logic_seperate_output$df_disagree_child_marriage_but_agree_circumstances <- df_disagree_child_marriage_but_agree_circumstances
  }
}

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

if(exists("df_agree_child_marriage_but_disagree_circumstances")){
  if(nrow(df_agree_child_marriage_but_disagree_circumstances) > 0){
    logic_seperate_output$df_agree_child_marriage_but_disagree_circumstances <- df_agree_child_marriage_but_disagree_circumstances
  }
}

