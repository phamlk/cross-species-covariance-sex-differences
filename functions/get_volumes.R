get_volumes <- function(input) {
  
  read_csv(input) %>% drop_na(Mouse_Sex) 
  
}