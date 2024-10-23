swap_clean_mouse <- function(data) { 
  
  output <- data %>% 
    mutate(hemi = case_when(str_detect(volume_x, "^left") ~ "left",
                            str_detect(volume_x, "^right") ~ "right",
                            TRUE ~ "midline"), .before = volume_x) %>% 
    mutate(volume_x = str_replace_all(volume_x, 
                                     c("^left" = "", "^right" = ""))) 
  
  return(output)
  }