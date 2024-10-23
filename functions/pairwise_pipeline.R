pairwise_pipeline <- function(data_input, to_exclude, sex_colname) {
  
  #----remove unnecessary columns from input data-------
  data <- data_input %>% 
    select(-any_of(to_exclude)) %>%
    rename(sex = any_of(sex_colname)) %>% 
    mutate(sex = tolower(sex))
  
  #----specifying correct format of input data
  m <- data %>% filter(sex == "m") %>%
       as.data.frame(.)

  f <- data %>% filter(sex == "f") %>%
       as.data.frame(.)

  #---make initial correlation matrices by sex-------
  # "correlation..." is being used as column name of the correlation column
  # in the output. "sex" is the name of the sex column that needs to be
  # appended to the output data.
  m_cor_mat <- orig_matrix(m,"correlation_m", "sex")
  f_cor_mat <- orig_matrix(f,"correlation_f","sex")
  
  #----find original correlation differences--------
  # this will find cor differences as male - female cor
  differences <- orig_differences(m_cor_mat, f_cor_mat)

  #----permutation testing----

  #----1. shuffle the sex around 1000 times-----
  shuffled <- lapply(1:1000, function(x) shuffler(data))

  #----2. filter for only the male data from shuffled data------
  m_shuff <- lapply(1:length(shuffled),function(x)
                                       split_sex(shuffled[[x]], "m"))

  #----3. filter for only female data from shuffled data---------
  f_shuff <- lapply(1:length(shuffled),function(x)
                                        split_sex(shuffled[[x]], "f"))

  # ----4. make male matrices from the shuffled data-------
  m_shuffMat <- make_matrices(m_shuff)

  # ----5. make female matrices from the shuffled data------
  f_shuffMat <- make_matrices(f_shuff)

  # ---6. find correlation sex differences among the shuffled data-----
  shuffled_diff <- lapply(1:length(m_shuffMat), function(x)
                   m_shuffMat[[x]] - f_shuffMat[[x]])

  # ----7. cleaning up the shuffled differences to remove NA and self-corrs---
  shuffled_diffClean <- shuffled_cleaning(shuffled_diff)

  # ---8. merge with original differences data and find p-values (adjusted fdr)----
  pvals <- find_pval(shuffled_diffClean, differences, 1000)
  
# if(output == "pvals") {
# 
# return(pvals)
# }
# 
# else if(output == "data") {
# 
# return(shuffled)
# }
# 
# else{
# 
# return(shuffled_diffClean)
# }

output <- list(pvals, shuffled, shuffled_diffClean)

return(output)
}