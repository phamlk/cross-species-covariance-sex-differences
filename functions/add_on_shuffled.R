add_on_shuffled <- function(source, input, species) { 

if(species == "human") {
  
  add_ons <- source %>% select(c("Age_in_Yrs", "euler", "TBV"))
}
else{
  
  add_ons <- source %>% select(c("Mouse_Age", "TBV"))
  
}

  output <- lapply(1:length(input),
            function(x) bind_cols(input[[x]], 
            add_ons))
  
  return(output)
  
}
  


