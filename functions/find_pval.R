find_pval <- function(data1, data2, num_o_shuffles) {
  
  merge(data1, data2, by = c("volume_x", "volume_y")) %>% rowwise() %>%
  mutate(p_val = 
            sum(abs(c_across(3:(num_o_shuffles + 2))) >= abs(`m-f`), 
               na.rm = TRUE)/length(na.omit(c_across(3:(num_o_shuffles + 2))))) %>%
  select(volume_x, volume_y, p_val,correlation_m, correlation_f, `m-f`) %>% ungroup() %>%
  mutate(p_adjust = p.adjust(p_val, method = "BH")) %>%
  select(volume_x, volume_y, p_val, p_adjust, correlation_m, correlation_f, `m-f`) %>% 
  ungroup() %>%
  mutate(signif = ifelse(p_val < 0.05, TRUE, FALSE),
         signif_adj = ifelse(p_adjust < 0.05, TRUE, FALSE),
         effect_size = abs(`m-f`))
}