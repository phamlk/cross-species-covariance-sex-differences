all_vol_bias <- function(perma_names, starting_col, 
                         ending_col, df, species, sex_term) {
    
    if(species == "human") { 
    df <- df %>% 
          mutate_at(vars(-c("Subject", "Age_in_Yrs",
                            "Gender", "euler")), scale) 
    
    df <- as.data.frame(df)
    
    as.data.frame(do.call(bind_rows,
                          lapply(starting_col:ending_col, function(x)
                            find_vol_bias(df, x, perma_names, sex_term)))) %>%
    mutate(p_adjust = p.adjust(p.value, "BH")) %>%
    mutate(signif = ifelse(p.value < 0.05, T, F),
           signif_adjust = ifelse(p_adjust < 0.05, T, F)) %>%
    rename(volume_x = Region) %>% 
    mutate(hemi = case_when(grepl("_L$", volume_x) == TRUE ~ "L",
                              grepl("_R$", volume_x) == TRUE ~ "R",
                              str_starts(volume_x, "l_") == TRUE ~ "L",
                              str_starts(volume_x, "r_") == TRUE ~ "R",
                              grepl("Right.", volume_x) == TRUE ~ "R",
                              grepl("Left.", volume_x) == TRUE ~ "L",
                              .default = "other"), .before = volume_x) %>% 
    mutate(volume_x = str_replace_all(volume_x, 
                                        c("_L$" = "", "_R$" = "",
                                          "Right." = "", "Left." = ""))) %>%
    mutate(volume_x = str_replace(volume_x, "^l_", "")) %>%
    mutate(volume_x = str_replace(volume_x, "^r_", ""))
    }
  
  else{
    # replace NA mouse age with age = 60 days
    # per Elisa's code 
    df <- df %>% mutate(Mouse_Age = ifelse(is.na(Mouse_Age) == TRUE, 60, 
                                           Mouse_Age)) %>% 
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
    rename(volume_x = Region) %>%
    mutate(hemi = case_when(str_detect(volume_x, "^left") ~ "left",
                              str_detect(volume_x, "^right") ~ "right",
                              TRUE ~ "midline"), .before = volume_x) %>% 
    mutate(volume_x = str_replace_all(volume_x, 
                                        c("^left" = "", "^right" = ""))) 
  }
  
}