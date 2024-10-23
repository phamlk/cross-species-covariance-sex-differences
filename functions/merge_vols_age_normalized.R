merge_vols_age_normalized <- function(cortical_data, subcortical_data) {
  
  cort <- cortical_data %>% select(-c("Age_in_Yrs", "Gender", "euler")) 
  
  final <- merge(cort, subcortical_data, by = "Subject") %>%
           relocate(TBV, .before = Age_in_Yrs) %>% ungroup() 
  
  vol_names <- data.frame(colnames = colnames(final)) %>%
               filter(!(colnames %in% c("TBV", "Age_in_Yrs",
                  "Gender", "euler", "Subject"))) %>%
               pull(colnames)
  
  final_normalized_female <- final %>% filter(Gender == "F") %>%
                      mutate(across(vol_names, 
                                    ~resid(lm(.x ~ Age_in_Yrs)))) 
                      
  final_normalized_male <- final %>% filter(Gender == "M") %>%
                      mutate(across(vol_names, 
                                    ~resid(lm(.x ~ Age_in_Yrs)))) 
  
  final_subjects_order <- final %>% select(Subject)
  
  final_normalized <- bind_rows(final_normalized_female, final_normalized_male) %>%
                      left_join(final_subjects_order)
                      
  return(final_normalized)
}