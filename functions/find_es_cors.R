find_es_cors <- function(data, x) { 
  
  start <- p_values[x]
  end <- p_values[x + 1]
  
  if (start == 0) {
    
    correlation <- data %>% pivot_longer(cols = 
                                           c("correlation_m", "correlation_f"), 
                                         names_to = "sex", values_to = "correlation") %>% 
      #filter(sex == correlation_sex) %>% 
      filter(p_adjust > start & p_adjust <= end | p_adjust == 0) %>% group_by(sex) %>%
      summarise(cor_coef = cor.test(correlation, 
                                    effect_size)$estimate, p_val = cor.test(correlation, effect_size)$p.value) %>%
      mutate(p_values_range = paste(start,"-", end))
    
  }
  
  else ( 
  correlation <- data %>% pivot_longer(cols = 
                                         c("correlation_m", "correlation_f"), 
                                       names_to = "sex", values_to = "correlation") %>% 
    #filter(sex == correlation_sex) %>% 
    filter(p_adjust > start & p_adjust <= end) %>% group_by(sex) %>%
    summarise(cor_coef = cor.test(correlation, 
                                  effect_size)$estimate, p_val = cor.test(correlation, effect_size)$p.value) %>%
    mutate(p_values_range = paste(start,"-", end))
  )
  
  return(correlation)
}