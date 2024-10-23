make_shuffled_cors <- function(shuffled) { 
  
  #----2. filter for only the male data from shuffled data------
  m_shuff <- lapply(1:length(shuffled),function(x)
             split_sex(shuffled[[x]], sex = "m"))
  
  f_shuff <- lapply(1:length(shuffled),function(x)
             split_sex(shuffled[[x]], sex = "f"))
  
  # ----4. make male matrices from the shuffled data-------
  m_shuffMat <- make_matrices(m_shuff)
  
  # ----5. make female matrices from the shuffled data------
  f_shuffMat <- make_matrices(f_shuff)
  
  shuffled_mean <- lapply(1:length(m_shuffMat), function(x)
                          (m_shuffMat[[x]] + f_shuffMat[[x]])/2)
  
  # ----4. make male matrices from the shuffled data-------
  cleanMat <- shuffled_cleaning(shuffled_mean)
  
  return(cleanMat)
  }