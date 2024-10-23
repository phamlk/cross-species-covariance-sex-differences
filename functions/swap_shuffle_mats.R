swap_shuffled_mats <- function(results_data, source_data, before_col,
                               to_exclude) {
  
  wb_names <- get_regions(source_data, to_exclude)
  
  wb_swap <- swap_all(results_data, wb_names, before_column = before_col) %>%
             group_by(volume_x) %>% select(-volume_y) %>%
             reframe(across(1:1000, mean))
  
  return(wb_swap)
  
}