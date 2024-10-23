orig_matrix <- function(input, title, sex_column) {
  
  data <- input %>% select(!any_of(sex_column))
  
  correlation <- cor(data, use = "pairwise.complete.obs")
  correlation[lower.tri(correlation)] <- NA
  
  correlation %>% as.data.frame(.) %>% 
  rownames_to_column(var = "volume_x") %>%
  pivot_longer(!volume_x, names_to = "volume_y", 
                 values_to = title) %>%
  filter(volume_x != volume_y) %>% drop_na()
  
}