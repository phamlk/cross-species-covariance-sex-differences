cluster_diff_test <- function(cluster_id, res_swap, species) {
  if(species == "human") { 
    cluster_names <- cluster_id %>% rename(volume_x = region) %>% 
      mutate(hemi = 
               case_when(grepl("_L$", volume_x) == TRUE ~ "L",
                         grepl("_R$", volume_x) == TRUE ~ "R",
                         str_starts(volume_x, "l_") == TRUE ~ "L",
                         str_starts(volume_x, "r_") == TRUE ~ "R",
                         grepl("Right.", volume_x) == TRUE ~ "R",
                         grepl("Left.", volume_x) == TRUE ~ "L",
                         .default = "other"), .before = volume_x) %>% 
      mutate(volume_x = str_replace_all(volume_x, 
                                        c("_L$" = "", "_R$" = "",
                                          "Right." = "", "Left." = ""))) %>%
      mutate(volume_x = str_replace(volume_x, "^l_", "")) %>%
      mutate(volume_x = str_replace(volume_x, "^r_", ""))
  }
  else {
    cluster_names <- cluster_id %>% rename(volume_x = region) %>%
      mutate(hemi = case_when(str_detect(volume_x, "^left") ~ "left",
                              str_detect(volume_x, "^right") ~ "right",
                              TRUE ~ "midline"), .before = volume_x) %>% 
      mutate(volume_x = str_replace_all(volume_x, 
                                        c("^left" = "", "^right" = "")))
  }
  
  left_join(cluster_names, res_swap) %>% 
           select(cluster_number, correlation_m, correlation_f) %>%
           pivot_longer(-cluster_number,
                       names_to = "correlation_type") %>% 
    ggplot() + aes(x = as.factor(cluster_number), 
                   y = value, fill = correlation_type) + geom_boxplot() +
    theme_bw() + theme(legend.position = "bottom")
  
  
  
}