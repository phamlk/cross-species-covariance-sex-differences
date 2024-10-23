split_sex <- function(data, sex_input) {
  
  data %>% filter(sex == sex_input)
  
}