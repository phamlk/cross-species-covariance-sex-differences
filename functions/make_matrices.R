make_matrices <- function(splits) {
  
        map(splits, function(x) {
                dat <- as.data.frame(x) %>% select(!sex) %>%
                cor(., use = "complete.obs") 
                
                dat[lower.tri(dat)] <- NA
                
                dat
          
                })
  
}