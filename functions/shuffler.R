shuffler <- function(data) {
  
  data %>% mutate(sex = sample(sex))
}