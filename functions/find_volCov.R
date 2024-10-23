find_volCov <- function(vol_bias, cov_bias, species, type, signif_type) {
  
  if(type == "mean" & species == "human" | 
     type == "mean" & species == "mouse" & signif_type == "all") { 
    
  cov_Bias <- cov_bias %>% group_by(hemi, volume_x) %>%
              summarise(meanCov = mean(`m-f`))  
  }
  
  else if (type == "mean_absolute" & species == "human" | 
            type == "mean_absolute" & species == "mouse" & signif_type == "all") {
    
    cov_Bias <- cov_bias %>% group_by(hemi, volume_x) %>%
      summarise(meanCov = mean(effect_size))  
    
  }
  else if (type == "mean" & species == "human" & signif_type == "signif"| 
           type == "mean" & species == "mouse" & signif_type == "signif") {
    
  cov_Bias <- cov_bias %>% filter(signif_adj == TRUE) %>% 
              group_by(hemi, volume_x) %>% 
              reframe(meanCov = mean(`m-f`))
  }
  
  else if(type == "percent" & species == "mouse") {
    
    cov_Bias <- cov_bias %>% 
                group_by(hemi, volume_x) %>% 
                mutate(total = n(), 
                female_biased = ifelse(`m-f` < 0, TRUE, FALSE)) %>% 
                group_by(hemi,volume_x, female_biased, total) %>%
                summarise(count = n()) %>%
                mutate(proportion_negative = 
                ifelse(female_biased == TRUE, count/total, 
                      (total-count)/total)) %>% ungroup() %>%
                select(-female_biased) %>% 
                distinct(hemi,volume_x, .keep_all = TRUE) %>% 
                ungroup() %>% 
                mutate(proportion_negative = proportion_negative*100) 
  }
  
   else{
     
     cov_Bias <- cov_bias %>% 
       group_by(volume_x, hemi) %>%
       mutate(direction = ifelse(`m-f` < 0,
                                 "negative", "positive"),
              count_total = n()) %>% ungroup() %>%
       group_by(volume_x, hemi, direction) %>%
       mutate(direction_count = n()) %>% 
       summarise(proportion_negative = 
                   ifelse(direction == "negative",
                          direction_count/count_total,
                          count_total - direction_count/count_total)) %>%
       ungroup() %>% distinct(hemi, volume_x, .keep_all = TRUE) %>%
       mutate(proportion_negative = proportion_negative*100) 
   }
  
  inner_join(vol_bias, cov_Bias)
}