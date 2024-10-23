function(names) { 
  
  glasser_names <- names %>% mutate(Lobe = fct_relevel(Lobe, c("Fr", "Par", "Ins", "Temp", "Occ"))) %>% 
                   arrange(Lobe)
  
  aseg_names <- as.data.frame(aseg$data) %>% select(hemi, region, side, label) %>%
                select(hemi, region, side, label) %>%
                na.omit() %>% 
                mutate(region = ifelse(!(region %in% c("caudate", "putamen", "pallidum")),
                                       region, "basal ganglia")) %>% 
                mutate(region = as.factor(region)) %>% 
                filter(region %in% c("basal ganglia", 
                                     "thalamus proper", "ventral DC", 
                                     "amygdala", "hippocampus", 
                                     "cerebellum cortex", "ventral DC")) %>%
                mutate(region = fct_relevel(region, 
                       c("basal ganglia", 
                         "thalamus proper",
                         "amygdala", "hippocampus", 
                         "cerebellum cortex", "ventral DC"))) %>% 
                arrange(region)
  
  # grab names from amygdala and hypothalamus data
  # need to import those data into here
  # then add a "region" column to all amygdala nuclei labeled "amygdala"
  # same for the hypothalamic nuclei but label "ventral DC" 
  # make sure the structure names column are called labels
  # append these rows to the aseg_names data and resort 
  # then bind both glasser and aseg_names together, extract the 
  # structure name (labels) and use that to arrange the correlation matrix
  
  # need to add accumbens on to here 
  lobe_colors <- c("Occ" = "#a6cee3", "Fr" = "#1f78b4" ,
                   "Par" = "#b2df8a", "Temp" = "#33a02c",
                   "Ins" = "#fb9a99", "thalamus proper" = "#e31a1c",
                   "hippocampus" = "#fdbf6f", "amygdala" = "#ff7f00", 
                   "caudate" = "#cab2d6", 
                   "putamen" = "#cab2d6", "pallidum" = "#cab2d6",
                   "brain stem" = "#6a3d9a", 
                   "cerebellum cortex" = "#ffff99", "ventral DC" = "#b15928")
  
  }