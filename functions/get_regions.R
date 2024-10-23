get_regions <- function(whole_brain_data, to_exclude) {
  
  names <- whole_brain_data %>% select(-any_of(to_exclude)) %>% 
           colnames()
  
  return(names)
    
}