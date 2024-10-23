swap_all <- function(data, regions_list, before_column) {
  
 final <- bind_rows(lapply(1:length(regions_list), function(x)
                    swappies(data, regions_list[x], before_column))) 
 
 return(final)
  
}