append <- function(node, data, id_col, default) {
  
  output <- default
  
  if(node$name %in% data[["volume_x"]]) 
    
    output <- subset(data, volume_x == node$name)[[id_col]]
  
  return(output)
}