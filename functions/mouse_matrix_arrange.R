mouse_matrix_arrange <- function(tree_mouse) { 
  
  names_df <- ToDataFrameTree(tree_mouse$Get("color_hex_triplet", 
                                 filterFun = isLeaf)) %>%
              rownames_to_column %>%
              rename("region" = "rowname", 
                     "color" = "x") %>% 
              filter(region %in% colnames(tar_read(leaves_age_normalized)))
  
  names <- names_df %>% 
           pull(region)
  
  colors <- unique(names_df$color)
  
  # anatomical atlas gap lines 
  gap_lines <- names_df %>% group_by(color) %>% 
    summarise(count = n()) %>% mutate(row_number = cumsum(count)) %>%
  pull(row_number)
  
  gap_lines <- c(1, gap_lines)
  
  #------setting up colors for the matrix--------
  breaks1 <- seq(-1, 1, length.out = 200)
  
  clrsp <- colorRampPalette(c("navy","white", "firebrick3"))   
  clrs <- clrsp(99) 
  
  mouseM_matrix <- reshape2::melt(tar_read(corr_mouseM_age_normalized)) %>%
    arrange(factor(x = Var1, levels = names)) %>%
    group_by(Var1) %>%
    arrange(factor(x = Var2, levels = names))
  
  mouseF_matrix <- reshape2::melt(tar_read(corr_mouseF_age_normalized)) %>%
    arrange(factor(x = Var1, levels = names)) %>%
    group_by(Var1) %>%
    arrange(factor(x = Var2, levels = names))
  
  mouseM_heatmap <- mouseM_matrix %>% ggplot() + aes(x = Var1, y = Var2, fill = value) +
    geom_raster() + scale_fill_gradient2(low = clrs[1:33], mid = clrs[34:67], 
                                         high = clrs[68:99], na.value = "grey", limits = c(-1,1)) + 
    expand_limits(x = -5, y = -5) + theme_classic() + 
    theme(axis.title.x = element_blank(), axis.text.x = element_blank(), 
          axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.x = element_blank(), 
          axis.ticks.y = element_blank(), axis.line= element_blank(), 
          legend.title = element_text(size = 16, angle = 90, hjust = 0.5), 
          text = element_text(family = "mono", size = 14)) + 
    geom_hline(yintercept = gap_lines, linewidth = 0.1) + 
    geom_vline(xintercept = gap_lines, linewidth = 0.1) + 
    lapply(1:(length(gap_lines) - 1), function(i) {
      annotate("rect", xmin = gap_lines[i], xmax = gap_lines[i + 1], ymin = -5, ymax = 1, fill = colors[i])
    }) + 
    lapply(1:(length(gap_lines) - 1), function(i) {
      annotate("rect", ymin = gap_lines[i], ymax = gap_lines[i + 1], xmin = -5, xmax = 1, fill = colors[i])
    }) + 
    guides(fill = guide_colourbar(barwidth = 0.8, barheight = 45,
                                  title.position = "right",
                                  label.position = "right")) + labs(fill = "Pearson correlation") +
    ggtitle("Mouse: Male structural covariance")
  
  mouseF_heatmap <- mouseF_matrix %>% ggplot() + aes(x = Var1, y = Var2, fill = value) +
    geom_raster() + scale_fill_gradient2(low = clrs[1:33], mid = clrs[34:67], 
                                         high = clrs[68:99], na.value = "grey", limits = c(-1,1)) + 
    expand_limits(x = -5, y = -5) + theme_classic() + 
    theme(axis.title.x = element_blank(), axis.text.x = element_blank(), 
          axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.x = element_blank(), 
          axis.ticks.y = element_blank(), axis.line= element_blank(), 
          legend.title = element_text(size = 16, angle = 90, hjust = 0.5), 
          text = element_text(family = "mono", size = 14)) + 
    geom_hline(yintercept = gap_lines, linewidth = 0.1) + 
    geom_vline(xintercept = gap_lines, linewidth = 0.1) + 
    lapply(1:(length(gap_lines) - 1), function(i) {
      annotate("rect", xmin = gap_lines[i], xmax = gap_lines[i + 1], ymin = -5, ymax = 1, fill = colors[i])
    }) + 
    lapply(1:(length(gap_lines) - 1), function(i) {
      annotate("rect", ymin = gap_lines[i], ymax = gap_lines[i + 1], xmin = -5, xmax = 1, fill = colors[i])
    }) + 
    guides(fill = guide_colourbar(barwidth = 0.8, barheight = 45,
                                  title.position = "right",
                                  label.position = "right")) + labs(fill = "Pearson correlation") +
    ggtitle("Mouse: Female structural covariance")
  
  mouse_heatmaps <- mouseF_heatmap / mouseM_heatmap + plot_layout(guides = "collect")
  mouse_heatmaps <- as_grob(mouse_heatmaps)
  
  names_df <- names_df %>% rename("volume_x" = "region")
  
  tree_mouse$Do(function(node)
    node$color_hex_triplet_leaves <- NA)
  
  tree_mouse$Do(function(node)
    node$color_hex_triplet_leaves <- append(node, names_df, "color", NA))
  
  tree_mouse$Get("color_hex_triplet_leaves", filterFun = isLeaf)
  
  mouse_atlas_map <- sliceSeries(ncol = 1, nrow = 6, begin=70, end=350) %>%
    anatomy(anatVol, low=700, high=1400) %>%
    overlay(hanatToVolume(tree_mouse, labelVol, "color_hex_triplet_leaves"),
            low = 0, high = 1) %>% addtitle("Mouse atlas") %>%
    grobify(titlePars = gpar(fontsize = 20, cex = 0.88, fontfamily = "mono")) 
            
  layout_heatmaps <- c(area(1,1,6,4), area(1,5,6,6))
  
  mouse_matrices_and_atlases <- wrap_elements(mouse_heatmaps) + 
  wrap_elements(mouse_atlas_map) + plot_layout(design = layout_heatmaps)
  
  ggsave(plot = mouse_matrices_and_atlases, 
         "figures/supplementary/mouse_covariance_matrices.png", 
         height = 10, width = 10) # use 6.61 x 9.99
  }