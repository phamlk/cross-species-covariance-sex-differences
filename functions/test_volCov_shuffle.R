test_volCov_shuffle <- function(data_input, volCov_orig,
                                perma_names, sex_term, sex_colname,
                                species, exclude_col,
                                starting_col, ending_col, iter) { 
  

cor_orig <- cor(volCov_orig$estimate, volCov_orig$meanCov)
  
if(species == "human") {
data <-  data_input %>% mutate(Gender = sample(Gender)) 
}
else{
data <- data_input %>% mutate(Mouse_Sex = sample(Mouse_Sex)) 
}

bias <- all_vol_bias(perma_names = perma_names,
                     starting_col = starting_col,
                     ending_col = ending_col,
                     df = data,
                     species = species,
                     sex_term = sex_term)

#----specifying correct format of input data
if(species == "human") {
m <- data %>% filter(Gender == "M") %>%
     as.data.frame(.) %>% select(-any_of(exclude_col))

f <- data %>% filter(Gender == "F") %>%
     as.data.frame(.) %>% select(-any_of(exclude_col))
}
else {
m <- data %>% filter(Mouse_Sex == "M") %>%
    as.data.frame(.) %>% select(-any_of(exclude_col))

f <- data %>% filter(Mouse_Sex == "F") %>%
    as.data.frame(.) %>% select(-any_of(exclude_col))
}

#---make initial correlation matrices by sex-------
# "correlation..." is being used as column name of the correlation column
# in the output. "sex" is the name of the sex column that needs to be
# appended to the output data.
m_cor_mat <- orig_matrix(m,"correlation_m", sex_colname)
f_cor_mat <- orig_matrix(f,"correlation_f",sex_colname)

#----find original correlation differences--------
# this will find cor differences as male - female cor
differences <- orig_differences(m_cor_mat, f_cor_mat)

#----orienting the correlation difference matrix to merge with bias data----
differences_swapped <- swap_res(source_data = data,
                                to_exclude = exclude_col,
                                results_data = differences,
                                before_col = "p_val",
                                species = species) %>%
                        group_by(hemi,volume_x) %>%
                        reframe(meanCov = mean(`m-f`))

joined <- left_join(bias, differences_swapped)

result <- data.frame(iter = iter,
                     correlation = cor.test(joined$meanCov, joined$estimate)$estimate) %>% 
          mutate(orig_greater = ifelse(correlation <= cor_orig, TRUE, FALSE))

row.names(result) <- NULL

return(result)
}