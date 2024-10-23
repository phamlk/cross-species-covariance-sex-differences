get_volumes_age_normalized <- function(input) {
  
  final <- read_csv(input) %>% drop_na(Mouse_Sex) %>% 
  mutate(Mouse_Age = ifelse(is.na(Mouse_Age) == TRUE, 60, 
         Mouse_Age))
  
  
  vol_names <- data.frame(colnames = colnames(final)) %>%
    filter(!(colnames %in% c("Background", "Mouse_ID", 
                             "POND_Mouse_ID", "Mouse_Sex", 
                             "Is_Wildtype", "Study_Name", 
                             "Mouse_Age", "Timepoint","TBV"))) %>%
    pull(colnames)
  
  final_normalized_female <- final %>% filter(Mouse_Sex == "F") %>%
    mutate(across(vol_names, 
                  ~resid(lm(.x ~ Mouse_Age)))) 
  
  final_normalized_male <- final %>% filter(Mouse_Sex == "M") %>%
    mutate(across(vol_names, 
                  ~resid(lm(.x ~ Mouse_Age)))) 
  
  final_subjects_order <- final %>% select(Mouse_ID)
  
  final_normalized <- bind_rows(final_normalized_female, final_normalized_male) %>%
    left_join(final_subjects_order) 
  
  return(final_normalized)
}