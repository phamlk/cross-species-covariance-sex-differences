find_corr <- function(input_data, sex, species) {
  
  if(missing(sex)) {
    data <- input_data
  }
  else {
    if(species == "human") {
      data <- input_data %>% filter(Gender == sex)
    }
    else{ 
      data <- input_data %>% filter(Mouse_Sex == sex)}
  }
  
  if(species == "human") { 
    just_brain <- data %>% 
      select(-any_of(c("Subject", 
                       "TBV", "Age_in_Yrs", 
                       "Gender", "euler")))
  } else {
    
    just_brain <- data %>% 
      select(-any_of(c("Mouse_ID", "Is_Wildtype",
                       "POND_Mouse_ID", "Study_Name",
                       "Mouse_Sex", "Mouse_Age", "Timepoint",
                       "TBV", "Background")))
  }
  
  brain_cor <- cor(just_brain, method = "pearson")
  
  return(brain_cor)
}