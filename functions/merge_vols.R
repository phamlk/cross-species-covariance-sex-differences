merge_vols <- function(cortical_data, subcortical_data) {
  
  cort <- cortical_data %>% select(-c("Age_in_Yrs", "Gender", "euler")) 
  
  final <- merge(cort, subcortical_data, by = "Subject") %>%
           relocate(TBV, .before = Age_in_Yrs)
}