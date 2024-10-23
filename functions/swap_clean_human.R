swap_clean_human <- function(data) { 
  
  output <- data %>% 
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
  
  return(output)
  
  }