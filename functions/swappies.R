swappies <- function(data, region, before_column) { 
  
y <- data %>% filter(volume_y == region) %>% 
     mutate(volume_x1 = volume_y, 
            volume_y1 = volume_x, .before = any_of(before_column)) %>%
     select(-any_of(c("volume_x", "volume_y"))) %>%
     rename(volume_x = volume_x1, volume_y = volume_y1)

x <- data %>% 
     filter(volume_x == region)

final <- bind_rows(x,y) 

return(final)
}




                        