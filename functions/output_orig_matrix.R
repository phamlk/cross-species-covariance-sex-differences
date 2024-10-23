output_orig_matrix <- function(data_input, to_exclude, sex_colname, 
                             sex_init) {
  
  #----remove unnecessary columns from input data-------
  data <- data_input %>% 
    select(-any_of(to_exclude)) %>%
    rename(sex = any_of(sex_colname)) %>% 
    mutate(sex = tolower(sex))
  
  #----specifying correct format of input data
  sex_data <- data %>% filter(sex == sex_init) %>%
    as.data.frame(.)
  
  #---make initial correlation matrices by sex-------
  # "correlation..." is being used as column name of the correlation column
  # in the output. "sex" is the name of the sex column that needs to be
  # appended to the output data.
  cor_mat <- orig_matrix(sex_data,"correlation", "sex")
  
  return(cor_mat) 
}