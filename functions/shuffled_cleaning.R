shuffled_cleaning <- function(matrix.list) {
  
   melt(matrix.list) %>%
   as.data.frame(.) %>% 
   pivot_wider(names_from = L1,
               values_from = value) %>%
   rename("volume_x" = Var1,
          "volume_y" = Var2) %>%
   filter(volume_x != volume_y) %>%
   na.omit()
  }