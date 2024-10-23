human_anatomy_map <- function(names) {
  
  # the cortex will be visualized as five lobes
  # subcortex is divided into 7 regions: thalamus proper, hippocampus, 
  # basal ganglia (putamen, caudate, pallidum), ventral DC (hypothalamic nucleis), 
  # amygdala (amygdala nuclei), brain stem, cerebellum cortex
  # makes for a total of 12 categories for colors. reasonable maximum number 
  # according to colorbrewer
  
  glasser_names <- names %>%
    mutate(label = ifelse(LR == "L", 
                          paste0("lh_", LR, "_", region),
                          paste0("rh_", LR, "_", region))) %>%
    mutate(label = case_when(label == "lh_L_7Pl" ~ "lh_L_7PL",
                             label == "rh_R_7Pl" ~ "rh_R_7PL",
                             TRUE ~ label),
           region = ifelse(region == "7Pl", "7PL", region)) %>%
    select(c(label,regionLongName, region, Lobe, cortex))
  
  aseg_names <- as.data.frame(aseg$data) %>% select(hemi, region, side, label) %>%
                select(hemi, region, side, label)
  
  # the accumbens (right and left) are not represented on the ggseg brain 
  # map but they are part of the basal ganglia, so assign them that light
  # purple color on the matrix 
  lobe_colors <- c("Occ" = "#a6cee3", "Fr" = "#1f78b4" ,
                   "Par" = "#b2df8a", "Temp" = "#33a02c",
                   "Ins" = "#fb9a99", "thalamus proper" = "#e31a1c",
                   "hippocampus" = "#fdbf6f", "amygdala" = "#ff7f00", 
                   "caudate" = "#cab2d6", 
                   "putamen" = "#cab2d6", "pallidum" = "#cab2d6",
                   "brain stem" = "#6a3d9a", 
                   "cerebellum cortex" = "#ffff99", "ventral DC" = "#b15928")
  
  left_lateral <- ggplot(glasser_names) +  
    geom_brain(atlas = glasser, 
               side = "lateral", hemi = "left", 
               colour="black",
               mapping = aes(fill = Lobe),
               show.legend = F, 
               alpha = 0.8) + theme_void() + 
    scale_fill_manual(values = lobe_colors) + 
    theme(axis.text.y = element_blank(),
          plot.title = element_text(hjust = 0.5)) + 
    ggtitle("Human atlas") 
  
  right_lateral <- ggplot(glasser_names) +  
    geom_brain(atlas = glasser, 
               side = "lateral", hemi = "right", 
               colour="black",
               mapping = aes(fill = Lobe),
               show.legend = F, 
               alpha = 0.8) + theme_void() + 
    scale_fill_manual(values = lobe_colors) + 
    theme(axis.text.y = element_blank()) 
  
  left_medial <- ggplot(glasser_names) +  
    geom_brain(atlas = glasser, 
               side = "medial", hemi = "left", 
               colour="black",
               mapping = aes(fill = Lobe),
               show.legend = F, 
               alpha = 0.8) + theme_void() + 
    scale_fill_manual(values = lobe_colors) + 
    theme(axis.text.y = element_blank(),
          plot.title = element_text(size = 18, hjust = 0.5))
  right_medial <- ggplot(glasser_names) +  
    geom_brain(atlas = glasser, 
               side = "medial", hemi = "right", 
               colour="black",
               mapping = aes(fill = Lobe),
               show.legend = F, 
               alpha = 0.8) + theme_void() + 
    scale_fill_manual(values = lobe_colors) + 
    theme(axis.text.y = element_blank()) 
  
  coronal <- ggplot(aseg_names) + 
    geom_brain(atlas = aseg, 
               side = "coronal",
               mapping = aes(fill = region), 
               show.legend = F,
               colour = "black",
               alpha = 0.8) +
    xlab("") + ylab("") + theme_void() + 
    scale_fill_manual(values = lobe_colors) + 
    theme(axis.text.y = element_blank()) +
    theme(axis.text.y = element_blank()) 
  
  sagittal <- ggplot(aseg_names) + 
    geom_brain(atlas = aseg, 
               side = "sagittal",
               mapping = aes(fill = region), 
               show.legend = F,
               colour = "black",
               alpha = 0.8) +
    xlab("") + ylab("") + theme_void() + 
    scale_fill_manual(values = lobe_colors) + 
    theme(axis.text.y = element_blank()) +
    theme(axis.text.y = element_blank()) 
  
  human_layout <- c(area(1,1), area(2,1), area(3,1),
                    area(4,1), area(5,1), area(6,1))
  
  human_maps <- left_lateral + right_lateral + left_medial + right_medial + 
                coronal + sagittal + 
                plot_layout(nrow = 6, ncol = 1, design = human_layout) & 
                theme(text = element_text(family = "mono", size = 14))
  
  return(as_grob(human_maps))
}