find_vol_bias <- function(df, column_number, perma_names,
                          sex_term) {
  
        # data_mod <- df %>% select(colnames(df)[column_number], 
        #                           Age_in_Yrs, Gender, euler, 
        #                           BrainSegVolNotVent...183)
        
        
        lm(reformulate(perma_names, as.name(colnames(df)[column_number])),
        data = df) %>% tidy(.) %>%
        filter(p.value == p.value[term == sex_term]) %>%
        mutate(Region = colnames(df)[column_number]) %>%
        select(Region, estimate, p.value) %>%
        mutate(bias_vol = ifelse(estimate > 0, "M",
                          ifelse(estimate < 0, "F", "None")))
        
}