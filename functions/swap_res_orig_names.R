swap_res_orig_names <- function(source_data, to_exclude, results_data, before_col,
                     species) {
  
  wb_names <- get_regions(source_data, to_exclude)
  
  wb_swap <- swap_all(results_data, wb_names, before_column = before_col)
  
  return(wb_swap)
}