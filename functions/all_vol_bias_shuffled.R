all_vol_bias_shuffled <- function(df, species, starting_col, ending_col, 
                                  perma_names, sex_term) { 
  
if(species == "human") { 
  
  df <- df %>% mutate(Age_in_Yrs = Age_in_Yrs - mean(Age_in_Yrs)) %>% 
    mutate_at(vars(-c("Age_in_Yrs",
                      "sex", "euler")), scale) 
  
}
else{
  
  df <- df %>% mutate(Mouse_Age = ifelse(is.na(Mouse_Age) == TRUE, 60, 
                                         Mouse_Age)) %>% 
    mutate(Mouse_Age = Mouse_Age - mean(Mouse_Age)) %>% 
    mutate_at(vars(-c("sex", "Mouse_Age")), scale) 
}

df <- as.data.frame(df)

output <- as.data.frame(do.call(bind_rows,
                      lapply(starting_col:ending_col, function(x)
                      find_vol_bias(df, x, perma_names, sex_term))))

return(output)
}

