human_matrix_arrange <- function(names) {
  
  # the cortex will be visualized as five lobes
  # subcortex is divided into 7 regions: thalamus proper, hippocampus, 
  # basal ganglia (putamen, caudate, pallidum), ventral DC (hypothalamic nucleis), 
  # amygdala (amygdala nuclei), brain stem, cerebellum cortex
  # makes for a total of 12 categories for colors. reasonable maximum number 
  # according to colorbrewer
  
  hypothalamus_names <- tar_read(whole_brain_age_normalized) %>%
                        select(starts_with(c("l_", "r_"))) %>% colnames()
  
  amygdala_names <- tar_read(whole_brain_age_normalized) %>%
                    select(contains(c("-nucleus", "-AAA", "-transitio"))) %>%
                    colnames()
  
  subcortical_names <- as.data.frame(aseg$data) %>% 
                       mutate(region = ifelse(region %in% 
                             c("putamen", "caudate", "pallidum"), 
                           "basal ganglia", region)) %>% na.omit() %>%
                        filter(region %in% c("thalamus proper", "hippocampus", 
                         "basal ganglia", "ventral DC", 
                         "amygdala", "brain stem", 
                         "cerebellum cortex")) %>%
                       select(region, label) %>%
                       mutate(label = str_remove(label, "-Proper")) %>% 
                       mutate(region = 
                              fct_relevel(region, c("thalamus proper", "hippocampus",
                              "basal ganglia", "ventral DC", "amygdala", "brain stem",
                              "cerebellum cortex"))) %>%
                      add_row(label = c(hypothalamus_names,
                                        amygdala_names), 
                              region = c(rep("ventral DC", 42),
                                        rep("amygdala", 18))) %>%
                      rename("Lobe" = "region",
                             "regionLongName" = "label") 
  
  names_df <- names %>% select(regionLongName, Lobe) %>% bind_rows(subcortical_names) %>%
           filter(!(regionLongName %in% c("Hippocampus_L", "Hippocampus_R"))) %>% 
           add_row(Lobe = c("cerebellum cortex", "basal ganglia", "basal ganglia"), 
                   regionLongName = c("Left-Cerebellum-Cortex", 
                                      "Left-Accumbens-area", "Right-Accumbens-area")) %>% 
           mutate(Lobe =  fct_relevel(Lobe, c("Occ", "Fr", 
                                              "Par", "Temp",
                                              "Ins", "thalamus proper",
                                              "hippocampus", "amygdala", 
                                              "basal ganglia",
                                              "brain stem", 
                                              "cerebellum cortex", "ventral DC"))) %>% arrange(Lobe)
  
  names <- names_df %>% pull(regionLongName)
  
  humanM_matrix <- reshape2::melt(tar_read(corr_humanM_age_normalized)) %>%
                       arrange(factor(x = Var1, levels = names)) %>%
                       group_by(Var1) %>%
                       arrange(factor(x = Var2, levels = names))
  

  humanF_matrix <- reshape2::melt(tar_read(corr_humanF_age_normalized)) %>%
    arrange(factor(x = Var1, levels = names)) %>%
    group_by(Var1) %>%
    arrange(factor(x = Var2, levels = names))
  
  #------setting up colors for the matrix--------
  breaks1 <- seq(-1, 1, length.out = 200)
  
  clrsp <- colorRampPalette(c("navy","white", "firebrick3"))   
  clrs <- clrsp(99) 
  
  #-----setting up the matrices--------

  # anatomical atlas gap lines 
  gap_lines <- names_df %>% group_by(Lobe) %>% 
    summarise(count = n()) %>% mutate(row_number = cumsum(count)) %>%
    pull(row_number)
  
  gap_lines <- c(1, gap_lines)
  # making the matrix heatmaps
  
  humanM_heatmap <- humanM_matrix %>% ggplot() + aes(x = Var1, y = Var2, fill = value) +
  geom_raster() + scale_fill_gradient2(low = clrs[1:33], mid = clrs[34:67], 
  high = clrs[68:99], na.value = "grey", limits = c(-1,1)) + 
  expand_limits(x = -10, y = -10) + theme_classic() + 
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(), 
  axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.x = element_blank(), 
  axis.ticks.y = element_blank(), axis.line= element_blank(), 
  legend.title = element_text(size = 16, angle = 90, hjust = 0.5), 
  text = element_text(family = "mono", size = 14)) + 
  geom_hline(yintercept = gap_lines, linewidth = 0.1) + 
  geom_vline(xintercept = gap_lines, linewidth = 0.1) +
  # annotating the clusters on x-axis
  annotate("rect", xmin=gap_lines[1], xmax=gap_lines[2], ymin=-10 , ymax=1, fill="#a6cee3") + 
  annotate("rect", xmin=gap_lines[2], xmax=gap_lines[3], ymin=-10 , ymax=1, fill="#1f78b4") +
  annotate("rect", xmin=gap_lines[3], xmax=gap_lines[4], ymin=-10 , ymax=1, fill="#b2df8a") + 
  annotate("rect", xmin=gap_lines[4], xmax=gap_lines[5], ymin=-10 , ymax=1, fill="#33a02c") + 
  annotate("rect", xmin=gap_lines[5], xmax=gap_lines[6], ymin=-10 , ymax=1, fill="#fb9a99") + 
  annotate("rect", xmin=gap_lines[6], xmax=gap_lines[7], ymin=-10 , ymax=1, fill="#e31a1c") + 
  annotate("rect", xmin=gap_lines[7], xmax=gap_lines[8], ymin=-10 , ymax=1, fill="#fdbf6f") + 
  annotate("rect", xmin=gap_lines[8], xmax=gap_lines[9], ymin=-10 , ymax=1, fill="#ff7f00") + 
  annotate("rect", xmin=gap_lines[9], xmax=gap_lines[10], ymin=-10 , ymax=1, fill="#cab2d6") + 
  annotate("rect", xmin=gap_lines[10], xmax=gap_lines[11], ymin=-10 , ymax=1, fill="#6a3d9a") +
  annotate("rect", xmin=gap_lines[11], xmax=gap_lines[12], ymin=-10 , ymax=1, fill="#ffff99") +
  annotate("rect", xmin=gap_lines[12], xmax=gap_lines[13], ymin=-10 , ymax=1, fill="#b15928") +
    
    # annotating the clusters on y-axis
    annotate("rect", ymin=gap_lines[1], ymax=gap_lines[2], xmin=-10, xmax=1, fill="#a6cee3") + 
    annotate("rect", ymin=gap_lines[2], ymax=gap_lines[3], xmin=-10, xmax=1, fill="#1f78b4") +
    annotate("rect", ymin=gap_lines[3], ymax=gap_lines[4], xmin=-10, xmax=1, fill="#b2df8a") + 
    annotate("rect", ymin=gap_lines[4], ymax=gap_lines[5], xmin=-10, xmax=1, fill="#33a02c") + 
    annotate("rect", ymin=gap_lines[5], ymax=gap_lines[6], xmin=-10, xmax=1, fill="#fb9a99") + 
    annotate("rect", ymin=gap_lines[6], ymax=gap_lines[7], xmin=-10, xmax=1, fill="#e31a1c") + 
    annotate("rect", ymin=gap_lines[7], ymax=gap_lines[8], xmin=-10, xmax=1, fill="#fdbf6f") + 
    annotate("rect", ymin=gap_lines[8], ymax=gap_lines[9], xmin=-10, xmax=1, fill="#ff7f00") + 
    annotate("rect", ymin=gap_lines[9], ymax=gap_lines[10], xmin=-10, xmax=1, fill="#cab2d6") + 
    annotate("rect", ymin=gap_lines[10], ymax=gap_lines[11], xmin=-10, xmax=1, fill="#6a3d9a") +
    annotate("rect", ymin=gap_lines[11], ymax=gap_lines[12], xmin=-10, xmax=1, fill="#ffff99") +
    annotate("rect", ymin=gap_lines[12], ymax=gap_lines[13], xmin=-10, xmax=1, fill="#b15928") +
    
  guides(fill = guide_colourbar(barwidth = 0.8, barheight = 45,
  title.position = "right",
  label.position = "right")) + labs(fill = "Pearson correlation") +
  ggtitle("Human: Male structural covariance")
  
  humanF_heatmap <- humanF_matrix %>% ggplot() + aes(x = Var1, y = Var2, fill = value) +
    geom_raster() + scale_fill_gradient2(low = clrs[1:33], mid = clrs[34:67], 
                                         high = clrs[68:99], na.value = "grey", limits = c(-1,1)) + 
    expand_limits(x = -10, y = -10) + theme_classic() + 
    theme(axis.title.x = element_blank(), axis.text.x = element_blank(), 
          axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.x = element_blank(), 
          axis.ticks.y = element_blank(), axis.line= element_blank(), 
          legend.title = element_text(size = 16, angle = 90, hjust = 0.5), 
          text = element_text(family = "mono", size = 14)) + 
    geom_hline(yintercept = gap_lines, linewidth = 0.1) + 
    geom_vline(xintercept = gap_lines, linewidth = 0.1) + 
    # annotating the clusters on x-axis
    annotate("rect", xmin=gap_lines[1], xmax=gap_lines[2], ymin=-10 , ymax=1, fill="#a6cee3") + 
    annotate("rect", xmin=gap_lines[2], xmax=gap_lines[3], ymin=-10 , ymax=1, fill="#1f78b4") +
    annotate("rect", xmin=gap_lines[3], xmax=gap_lines[4], ymin=-10 , ymax=1, fill="#b2df8a") + 
    annotate("rect", xmin=gap_lines[4], xmax=gap_lines[5], ymin=-10 , ymax=1, fill="#33a02c") + 
    annotate("rect", xmin=gap_lines[5], xmax=gap_lines[6], ymin=-10 , ymax=1, fill="#fb9a99") + 
    annotate("rect", xmin=gap_lines[6], xmax=gap_lines[7], ymin=-10 , ymax=1, fill="#e31a1c") + 
    annotate("rect", xmin=gap_lines[7], xmax=gap_lines[8], ymin=-10 , ymax=1, fill="#fdbf6f") + 
    annotate("rect", xmin=gap_lines[8], xmax=gap_lines[9], ymin=-10 , ymax=1, fill="#ff7f00") + 
    annotate("rect", xmin=gap_lines[9], xmax=gap_lines[10], ymin=-10 , ymax=1, fill="#cab2d6") + 
    annotate("rect", xmin=gap_lines[10], xmax=gap_lines[11], ymin=-10 , ymax=1, fill="#6a3d9a") +
    annotate("rect", xmin=gap_lines[11], xmax=gap_lines[12], ymin=-10 , ymax=1, fill="#ffff99") +
    annotate("rect", xmin=gap_lines[12], xmax=gap_lines[13], ymin=-10 , ymax=1, fill="#b15928") +
    
    # annotating the clusters on y-axis
    annotate("rect", ymin=gap_lines[1], ymax=gap_lines[2], xmin=-10, xmax=1, fill="#a6cee3") + 
    annotate("rect", ymin=gap_lines[2], ymax=gap_lines[3], xmin=-10, xmax=1, fill="#1f78b4") +
    annotate("rect", ymin=gap_lines[3], ymax=gap_lines[4], xmin=-10, xmax=1, fill="#b2df8a") + 
    annotate("rect", ymin=gap_lines[4], ymax=gap_lines[5], xmin=-10, xmax=1, fill="#33a02c") + 
    annotate("rect", ymin=gap_lines[5], ymax=gap_lines[6], xmin=-10, xmax=1, fill="#fb9a99") + 
    annotate("rect", ymin=gap_lines[6], ymax=gap_lines[7], xmin=-10, xmax=1, fill="#e31a1c") + 
    annotate("rect", ymin=gap_lines[7], ymax=gap_lines[8], xmin=-10, xmax=1, fill="#fdbf6f") + 
    annotate("rect", ymin=gap_lines[8], ymax=gap_lines[9], xmin=-10, xmax=1, fill="#ff7f00") + 
    annotate("rect", ymin=gap_lines[9], ymax=gap_lines[10], xmin=-10, xmax=1, fill="#cab2d6") + 
    annotate("rect", ymin=gap_lines[10], ymax=gap_lines[11], xmin=-10, xmax=1, fill="#6a3d9a") +
    annotate("rect", ymin=gap_lines[11], ymax=gap_lines[12], xmin=-10, xmax=1, fill="#ffff99") +
    annotate("rect", ymin=gap_lines[12], ymax=gap_lines[13], xmin=-10, xmax=1, fill="#b15928") +
    
    guides(fill = guide_colourbar(barwidth = 0.8, barheight = 45,
           title.position = "right",
           label.position = "right")) + 
    labs(fill = "Pearson correlation") +
    ggtitle("Human: Female structural covariance")
  
  
  output <- humanF_heatmap / humanM_heatmap + plot_layout(guides = "collect")
  
  return(as_grob(output))
}