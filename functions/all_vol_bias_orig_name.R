all_vol_bias_orig_name <- function(perma_names, starting_col, 
                         ending_col, df, species, sex_term) {
  
  if(species == "human") { 
    df <- df %>% mutate(Age_in_Yrs = Age_in_Yrs - mean(Age_in_Yrs)) %>% 
      mutate_at(vars(-c("Subject", "Age_in_Yrs",
                        "Gender", "euler")), scale) 
    
    df <- as.data.frame(df)
    
    as.data.frame(do.call(bind_rows,
                          lapply(starting_col:ending_col, function(x)
                            find_vol_bias(df, x, perma_names, sex_term)))) %>%
      mutate(p_adjust = p.adjust(p.value, "BH")) %>%
      mutate(signif = ifelse(p.value < 0.05, T, F),
             signif_adjust = ifelse(p_adjust < 0.05, T, F)) %>%
      rename(volume_x = Region) 
  }
  
  else{
    # replace NA mouse age with age = 60 days
    # per Elisa's code 
    df <- df %>% mutate(Mouse_Age = ifelse(is.na(Mouse_Age) == TRUE, 60, 
                                           Mouse_Age)) %>% 
      mutate(Mouse_Age = Mouse_Age - mean(Mouse_Age)) %>% 
      mutate_at(vars(-c("Mouse_ID", "Is_Wildtype",
                        "POND_Mouse_ID", "Study_Name",
                        "Mouse_Sex", "Mouse_Age", 
                        "Timepoint", "Background")), scale) 
    
    df <- as.data.frame(df)
    
    as.data.frame(do.call(bind_rows,
                          lapply(starting_col:ending_col, function(x)
                            find_vol_bias(df, x, perma_names, sex_term)))) %>%
      mutate(p_adjust = p.adjust(p.value, "BH")) %>%
      mutate(signif = ifelse(p.value < 0.05, T, F),
             signif_adjust = ifelse(p_adjust < 0.05, T, F)) %>%
      rename(volume_x = Region) 
  }
  
}