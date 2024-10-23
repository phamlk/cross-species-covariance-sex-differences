human_mean_cov_maps <- function(names, res_swap, bias, glasser_coords, aseg_coords) {
  
  glasser_names <- names %>%
                   mutate(label = ifelse(LR == "L", 
                          paste0("lh_", LR, "_", region),
                          paste0("rh_", LR, "_", region))) %>%
                   mutate(label = case_when(label == "lh_L_7Pl" ~ "lh_L_7PL",
                             label == "rh_R_7Pl" ~ "rh_R_7PL",
                             TRUE ~ label),
                   region = ifelse(region == "7Pl", "7PL", region)) %>%
                   select(c(label,regionLongName, region))
  
  aseg_names <- aseg$data %>% select(hemi, region, side, label)
  
  res_swap_signif <- res_swap %>% filter(signif_adj == TRUE) %>% 
    group_by(hemi, volume_x) %>% 
    reframe(mean_CovDiff = mean(`m-f`)) 
  
  res_swap <- res_swap %>% group_by(hemi, volume_x) %>% 
              reframe(mean_CovDiff = mean(`m-f`)) 
  
  results_glasser <- res_swap %>% 
                     mutate(hemi = ifelse(hemi == "other", 
                                          "midline", hemi)) %>% 
                     mutate(regionLongName = paste0(volume_x,"_", hemi)) %>% 
                     inner_join(glasser_names) %>% select(-c(hemi, volume_x)) 
  
  biases_glasser <- bias %>% 
                    select(hemi, volume_x, estimate) %>% 
                    mutate(hemi = ifelse(hemi == "other", 
                         "midline", hemi)) %>% 
                    mutate(regionLongName = paste0(volume_x,"_", hemi)) %>% 
                    inner_join(glasser_names) %>% select(-c(hemi, volume_x))
  
  biases_glasser_signif <- bias %>% filter(signif_adjust == TRUE) %>% 
                         select(hemi, volume_x, estimate) %>% 
                          mutate(hemi = ifelse(hemi == "other", 
                         "midline", hemi)) %>% 
    mutate(regionLongName = paste0(volume_x,"_", hemi)) %>% 
    inner_join(glasser_names) %>% select(-c(hemi, volume_x))
  
  glasser_signif <- res_swap_signif %>% 
                    mutate(hemi = ifelse(hemi == "other", 
                         "midline", hemi)) %>% 
                    mutate(regionLongName = paste0(volume_x,"_", hemi)) %>% 
                    inner_join(glasser_names) %>% select(-volume_x) %>%
                    mutate(hemi = ifelse(hemi == "L", "left", 
                                  ifelse(hemi == "R", "right", hemi)))

  #-----setting star locations glasser-----
  glasser_stars_left_lateral <- biases_glasser_signif %>%
    select(any_of(c("regionLongName", "label", "region"))) %>%
    left_join(glasser_coords) %>% filter(side == "lateral") %>%
    filter(str_detect(label, "lh_") == TRUE) %>% 
    na.omit() %>% mutate(stars = "*") %>% 
    filter(regionLongName %in% glasser_signif$regionLongName) %>%
    mutate(Y = 42)
  
  glasser_stars_left_medial <- biases_glasser_signif %>%
    select(any_of(c("regionLongName", "label", "region"))) %>%
    left_join(glasser_coords) %>% filter(side == "medial") %>%
    filter(str_detect(label, "lh_") == TRUE) %>% 
    na.omit() %>% mutate(stars = "*") %>% 
    filter(regionLongName %in% glasser_signif$regionLongName) %>%
    mutate(Y = ifelse(regionLongName == "Entorhinal_Cortex_L", 4, 13))

  glasser_stars_right_lateral <- biases_glasser_signif %>%
    select(any_of(c("regionLongName", "label", "region"))) %>%
    left_join(glasser_coords) %>% filter(side == "lateral") %>%
    filter(str_detect(label, "rh_") == TRUE) %>% 
    na.omit() %>% mutate(stars = "*") %>%
    filter(regionLongName %in% glasser_signif$regionLongName)
  
  glasser_stars_right_medial <- biases_glasser_signif %>%
    select(any_of(c("regionLongName", "label", "region"))) %>%
    left_join(glasser_coords) %>% filter(side == "medial") %>%
    filter(str_detect(label, "rh_") == TRUE) %>% 
    na.omit() %>% mutate(stars = "*") %>% 
    filter(regionLongName %in% glasser_signif$regionLongName) 

  #------------------------------------#
  
  glasser_signif <- left_join(glasser$data, glasser_signif)
  
  results_aseg <- res_swap %>% mutate(hemi = case_when(
                  hemi == "other" ~ "midline",
                  hemi == "L" ~ "left",
                  hemi == "R" ~ "right",
                  TRUE ~ hemi)) %>%
                  mutate(region = volume_x) %>%
                  mutate(label = ifelse(volume_x != "Brain-Stem",
                          paste0(str_to_title(hemi),"-",volume_x),
                          volume_x)) %>% 
                  mutate(label = 
                         case_when(label == "Left-Thalamus" ~
                                  "Left-Thalamus-Proper", 
                                  label == "Right-Thalamus" ~
                                  "Right-Thalamus-Proper",
                                  .default = label)) %>% 
                  select(-c(hemi, volume_x,region)) %>% 
                  merge(aseg_names, by = "label") %>% select(-geometry)
   
  biases_aseg <- bias %>% 
                 select(hemi, volume_x, estimate) %>%
                 mutate(hemi = case_when(
                        hemi == "other" ~ "midline",
                        hemi == "L" ~ "left",
                        hemi == "R" ~ "right",
                        TRUE ~ hemi)) %>%
    mutate(region = volume_x) %>%
    mutate(label = ifelse(volume_x != "Brain-Stem",
                          paste0(str_to_title(hemi),"-",volume_x),
                          volume_x)) %>% 
    mutate(label = 
             case_when(label == "Left-Thalamus" ~
                         "Left-Thalamus-Proper", 
                       label == "Right-Thalamus" ~
                         "Right-Thalamus-Proper",
                       .default = label)) %>% 
    select(-c(hemi, volume_x,region)) %>% 
    merge(aseg_names, by = "label") %>% select(-geometry)
  
  biases_aseg_signif <- bias %>% filter(signif_adjust == TRUE) %>% 
    select(hemi, volume_x, estimate) %>%
    mutate(hemi = case_when(
      hemi == "other" ~ "midline",
      hemi == "L" ~ "left",
      hemi == "R" ~ "right",
      TRUE ~ hemi)) %>%
    mutate(region = volume_x) %>%
    mutate(label = ifelse(volume_x != "Brain-Stem",
                          paste0(str_to_title(hemi),"-",volume_x),
                          volume_x)) %>% 
    mutate(label = 
             case_when(label == "Left-Thalamus" ~
                         "Left-Thalamus-Proper", 
                       label == "Right-Thalamus" ~
                         "Right-Thalamus-Proper",
                       .default = label)) %>% 
    select(-c(hemi, volume_x,region)) %>% 
    merge(aseg_names, by = "label") %>% select(-geometry)
  
  aseg_signif <- res_swap_signif %>% mutate(hemi = case_when(
    hemi == "other" ~ "midline",
    hemi == "L" ~ "left",
    hemi == "R" ~ "right",
    TRUE ~ hemi)) %>%
    mutate(region = volume_x) %>%
    mutate(label = ifelse(volume_x != "Brain-Stem",
                          paste0(str_to_title(hemi),"-",volume_x),
                          volume_x)) %>% 
    mutate(label = 
             case_when(label == "Left-Thalamus" ~
                         "Left-Thalamus-Proper", 
                       label == "Right-Thalamus" ~
                         "Right-Thalamus-Proper",
                       .default = label)) %>% 
    select(-c(hemi, volume_x,region)) %>% 
    merge(aseg_names, by = "label") %>% select(-geometry)
  
  #------setting star locations aseg------
  aseg_stars <-   biases_aseg_signif %>%
    select(any_of(c("label"))) %>%
    left_join(aseg_coords) %>% 
    na.omit() %>% mutate(stars = "*") %>% 
    filter(label %in% aseg_signif$label) 
  #--------------------------------------#
  aseg_signif_coronal <- left_join(aseg$data, aseg_signif)
  
   set_range <- c(-1.2, 1.2)
   set_range_cov <- c(-0.45, 0.45)
  
   #-----map all volumetrically sex-biased areas-----
   
   cortical_left_lateral_bias <- ggplot(biases_glasser) +  
     geom_brain(atlas = glasser, 
                side = "lateral", hemi = "left", 
                colour="black",
                mapping = aes(fill = estimate),
                show.legend = F) + theme_void() + 
     scale_fill_gradient2(low = c("navyblue","blue","royalblue","dodgerblue1","deepskyblue1","turquoise3"),
                          mid = NULL,
                          high = c("goldenrod1","orange1","firebrick2", "firebrick3","firebrick","red4"),
                          midpoint = 0,na.value="grey",limits = set_range, 
                          n.breaks=5) + 
     theme(axis.text.y = element_blank(),
           plot.title = element_text(size = 18, hjust = 0.5)) + 
     ggtitle("all vol\nsex-bias") 

   cortical_right_lateral_bias <- ggplot(biases_glasser) +  
     geom_brain(atlas = glasser, 
                side = "lateral", hemi = "right", 
                colour="black",
                mapping = aes(fill = estimate),
                show.legend = F) + theme_void() + 
     scale_fill_gradient2(low = c("navyblue","blue","royalblue","dodgerblue1","deepskyblue1","turquoise3"),
                          mid = NULL,
                          high = c("goldenrod1","orange1","firebrick2", "firebrick3","firebrick","red4"),
                          midpoint = 0,na.value="grey",limits = set_range, 
                          n.breaks=5) + 
     theme(axis.text.y = element_blank(),
           plot.title = element_text(size = 18, hjust = 0.5)) +
     theme(axis.text.y = element_blank()) 

   cortical_left_medial_bias <- ggplot(biases_glasser) +  
     geom_brain(atlas = glasser, 
                side = "medial", hemi = "left", 
                colour="black",
                mapping = aes(fill = estimate),
                show.legend = F) + theme_void() + 
     scale_fill_gradient2(low = c("navyblue","blue","royalblue","dodgerblue1","deepskyblue1","turquoise3"),
                          mid = NULL,
                          high = c("goldenrod1","orange1","firebrick2", "firebrick3","firebrick","red4"),
                          midpoint = 0,na.value="grey",limits = set_range, 
                          n.breaks=5) + 
     theme(axis.text.y = element_blank(),
           plot.title = element_text(size = 18, hjust = 0.5)) +
     theme(axis.text.y = element_blank()) 
   
   cortical_right_medial_bias <- ggplot(biases_glasser) +  
     geom_brain(atlas = glasser, 
                side = "medial", hemi = "right", 
                colour="black",
                mapping = aes(fill = estimate),
                show.legend = F) + theme_void() + 
     scale_fill_gradient2(low = c("navyblue","blue","royalblue","dodgerblue1","deepskyblue1","turquoise3"),
                          mid = NULL,
                          high = c("goldenrod1","orange1","firebrick2", "firebrick3","firebrick","red4"),
                          midpoint = 0,na.value="grey",limits = set_range, 
                          n.breaks=5) + 
     theme(axis.text.y = element_blank(),
           plot.title = element_text(size = 18, hjust = 0.5)) +
     theme(axis.text.y = element_blank()) 
   
   noncortical_sagittal_bias <- ggplot(biases_aseg) + 
     geom_brain(atlas = aseg, 
                side = "sagittal", show.legend = F,
                mapping = aes(fill = estimate), colour = "black") +
     xlab("") + ylab("") + theme_void() + 
     scale_fill_gradient2(low = c("navyblue","blue","royalblue","dodgerblue1","deepskyblue1","turquoise3"),
                          mid = NULL,
                          high = c("goldenrod1","orange1","firebrick2", "firebrick3","firebrick","red4"),
                          midpoint = 0,na.value="grey",limits = set_range, 
                          n.breaks=5) + 
     theme(axis.text.y = element_blank(),
           plot.title = element_text(size = 18, hjust = 0.5)) +
     theme(axis.text.y = element_blank()) 
   
   noncortical_coronal_bias <- ggplot(biases_aseg) + 
     geom_brain(atlas = aseg, 
                side = "coronal",
                mapping = aes(fill = estimate), 
                show.legend = F,
                colour = "black") +
     xlab("") + ylab("") + theme_void() + 
     scale_fill_gradient2(low = c("navyblue","blue","royalblue","dodgerblue1","deepskyblue1","turquoise3"),
                          mid = NULL,
                          high = c("goldenrod1","orange1","firebrick2", "firebrick3","firebrick","red4"),
                          midpoint = 0,na.value="grey",limits = set_range, 
                          n.breaks=5) + 
     theme(axis.text.y = element_blank(),
           plot.title = element_text(size = 18, hjust = 0.5)) +
     theme(axis.text.y = element_blank()) 
   
    #---map signif volumetrically sex-biased areas---
   cortical_left_lateral_bias_signif <- ggplot(biases_glasser_signif) +  
     geom_brain(atlas = glasser, 
                side = "lateral", hemi = "left", 
                colour="black",
                mapping = aes(fill = estimate),
                show.legend = F) + theme_void() + 
     scale_fill_gradient2(low = c("navyblue","blue","royalblue","dodgerblue1","deepskyblue1","turquoise3"),
                          mid = NULL,
                          high = c("goldenrod1","orange1","firebrick2", "firebrick3","firebrick","red4"),
                          midpoint = 0,na.value="grey",limits = set_range, 
                          n.breaks=5) + 
     theme(axis.text.y = element_blank(),
           plot.title = element_text(size = 18, hjust = 0.5)) +
     ggtitle("signif vol\nsex-bias") 
   
   cortical_right_lateral_bias_signif <- ggplot(biases_glasser_signif) +  
     geom_brain(atlas = glasser, 
                side = "lateral", hemi = "right", 
                colour="black",
                mapping = aes(fill = estimate),
                show.legend = F) + theme_void() + 
     scale_fill_gradient2(low = c("navyblue","blue","royalblue","dodgerblue1","deepskyblue1","turquoise3"),
                          mid = NULL,
                          high = c("goldenrod1","orange1","firebrick2", "firebrick3","firebrick","red4"),
                          midpoint = 0,na.value="grey",limits = set_range, 
                          n.breaks=5) + 
     theme(axis.text.y = element_blank(),
           plot.title = element_text(size = 18, hjust = 0.5)) +
     theme(axis.text.y = element_blank()) 
   
   cortical_left_medial_bias_signif <- ggplot(biases_glasser_signif) +  
     geom_brain(atlas = glasser, 
                side = "medial", hemi = "left", 
                colour="black",
                mapping = aes(fill = estimate)) + theme_void() + 
     scale_fill_gradient2(low = c("navyblue","blue","royalblue","dodgerblue1","deepskyblue1","turquoise3"),
                          mid = NULL,
                          high = c("goldenrod1","orange1","firebrick2", "firebrick3","firebrick","red4"),
                          midpoint = 0,na.value="grey",limits = set_range, 
                          n.breaks=5,
                          labels = c("F > M", "-0.5", "0.0", "0.5", "M > F")) + 
     theme(axis.text.y = element_blank(), 
           legend.title = element_text(angle = 90, size = 18),
           legend.text = element_text(size = 18),
           legend.title.align = 0.5) + 
     labs(fill = "volumetric sex bias") + 
     guides(fill = guide_colourbar(barwidth = 1, barheight = 30,
                                   title.position = "right",
                                   label.position = "right")) 
   
   cortical_right_medial_bias_signif <- ggplot(biases_glasser_signif) +  
     geom_brain(atlas = glasser, 
                side = "medial", hemi = "right", 
                colour="black",
                mapping = aes(fill = estimate),
                show.legend = F) + theme_void() + 
     scale_fill_gradient2(low = c("navyblue","blue","royalblue","dodgerblue1","deepskyblue1","turquoise3"),
                          mid = NULL,
                          high = c("goldenrod1","orange1","firebrick2", "firebrick3","firebrick","red4"),
                          midpoint = 0,na.value="grey",limits = set_range, 
                          n.breaks=5) + 
     theme(axis.text.y = element_blank(),
           plot.title = element_text(size = 18, hjust = 0.5)) +
     theme(axis.text.y = element_blank()) 
   
   noncortical_sagittal_bias_signif <- ggplot(biases_aseg_signif) + 
     geom_brain(atlas = aseg, 
                side = "sagittal", show.legend = F,
                mapping = aes(fill = estimate), colour = "black") +
     xlab("") + ylab("") + theme_void() + 
     scale_fill_gradient2(low = c("navyblue","blue","royalblue","dodgerblue1","deepskyblue1","turquoise3"),
                          mid = NULL,
                          high = c("goldenrod1","orange1","firebrick2", "firebrick3","firebrick","red4"),
                          midpoint = 0,na.value="grey",limits = set_range, 
                          n.breaks=5) + 
     theme(axis.text.y = element_blank(),
           plot.title = element_text(size = 18, hjust = 0.5)) +
     theme(axis.text.y = element_blank()) 
   
   noncortical_coronal_bias_signif <- ggplot(biases_aseg_signif) + 
     geom_brain(atlas = aseg, 
                side = "coronal",
                mapping = aes(fill = estimate), 
                show.legend = F,
                colour = "black") +
     xlab("") + ylab("") + theme_void() + 
     scale_fill_gradient2(low = c("navyblue","blue","royalblue","dodgerblue1","deepskyblue1","turquoise3"),
                          mid = NULL,
                          high = c("goldenrod1","orange1","firebrick2", "firebrick3","firebrick","red4"),
                          midpoint = 0,na.value="grey",limits = set_range, 
                          n.breaks=5) + 
     theme(axis.text.y = element_blank(),
           plot.title = element_text(size = 18, hjust = 0.5)) +
     theme(axis.text.y = element_blank()) 
   
    #---maps all seeds cortical-----
    cortical_left_lateral <- ggplot(results_glasser) +  
                             geom_brain(atlas = glasser, 
                             side = "lateral", hemi = "left", 
                             colour="black",
                             mapping = aes(fill = mean_CovDiff),
                             show.legend = F) + theme_void() + 
     scale_fill_gradient2(low = c("navyblue","blue","royalblue","dodgerblue1","deepskyblue1","turquoise3"),
                          mid = NULL,
                          high = c("goldenrod1","orange1","firebrick2", "firebrick3","firebrick","red4"),
                          midpoint = 0,na.value="grey",limits = set_range_cov, 
                          n.breaks=5) + 
     theme(axis.text.y = element_blank(),
           plot.title = element_text(size = 18, hjust = 0.5)) +
    theme(axis.text.y = element_blank(),
          plot.title = element_text(size = 18, hjust = 0.5)) + ggtitle("all\ncomparisons") 

    cortical_right_lateral <- ggplot(results_glasser) +  
                              geom_brain(atlas = glasser, 
                              side = "lateral", hemi = "right", 
                              colour="black",
                              mapping = aes(fill = mean_CovDiff),
                              show.legend = F) + theme_void() + 
      scale_fill_gradient2(low = c("navyblue","blue","royalblue","dodgerblue1","deepskyblue1","turquoise3"),
                           mid = NULL,
                           high = c("goldenrod1","orange1","firebrick2", "firebrick3","firebrick","red4"),
                           midpoint = 0,na.value="grey",limits = set_range_cov, 
                           n.breaks=5) + 
      theme(axis.text.y = element_blank(),
           plot.title = element_text(size = 18, hjust = 0.5)) +
      theme(axis.text.y = element_blank())
    
    cortical_left_medial <- ggplot(results_glasser) +  
                 geom_brain(atlas = glasser, 
                 side = "medial", hemi = "left", 
                 colour="black",
                 mapping = aes(fill = mean_CovDiff),
                 show.legend = F) + theme_void() + 
      scale_fill_gradient2(low = c("navyblue","blue","royalblue","dodgerblue1","deepskyblue1","turquoise3"),
                           mid = NULL,
                           high = c("goldenrod1","orange1","firebrick2", "firebrick3","firebrick","red4"),
                           midpoint = 0,na.value="grey",limits = set_range_cov, 
                           n.breaks=5) + 
      theme(axis.text.y = element_blank(),
            plot.title = element_text(size = 18, hjust = 0.5)) +
      theme(axis.text.y = element_blank()) 
    
    cortical_right_medial <- ggplot(results_glasser) +  
                 geom_brain(atlas = glasser, 
                 side = "medial", hemi = "right", 
                 colour="black",
                 mapping = aes(fill = mean_CovDiff),
                 show.legend = F) + theme_void() + 
      scale_fill_gradient2(low = c("navyblue","blue","royalblue","dodgerblue1","deepskyblue1","turquoise3"),
                           mid = NULL,
                           high = c("goldenrod1","orange1","firebrick2", "firebrick3","firebrick","red4"),
                           midpoint = 0,na.value="grey",limits = set_range_cov, 
                           n.breaks=5) + 
      theme(axis.text.y = element_blank(),
            plot.title = element_text(size = 18, hjust = 0.5)) +
      theme(axis.text.y = element_blank()) 
    
    #-----maps significant cortical----
    cortical_left_lateral_sig <- glasser_signif %>% filter(hemi == "left" & side == "lateral") %>%
      ggplot() + geom_sf(aes(fill = mean_CovDiff), colour = "black", show.legend = F) +
      scale_fill_gradient2(low = c("navyblue","blue","royalblue","dodgerblue1","deepskyblue1","turquoise3"),
                           mid = NULL,
                           high = c("goldenrod1","orange1","firebrick2", "firebrick3","firebrick","red4"),
                           midpoint = 0,na.value="grey",limits = set_range_cov,
                           n.breaks=5) +
      geom_text(data = glasser_stars_left_lateral, aes(X, Y, label = stars), colour = "white",
                size = 7) + theme_void() + 
      theme(axis.text.y = element_blank(),
            plot.title = element_text(size = 18, hjust = 0.5)) +
      theme(axis.text.y = element_blank(),
            plot.title = element_text(size = 18, hjust = 0.5)) +
      ggtitle("signif\ncomparisons")
    
    cortical_right_lateral_sig <- glasser_signif %>% filter(hemi == "right" & side == "lateral") %>%
      ggplot() + geom_sf(aes(fill = mean_CovDiff), colour = "black", show.legend = F) +
      scale_fill_gradient2(low = c("navyblue","blue","royalblue","dodgerblue1","deepskyblue1","turquoise3"),
                           mid = NULL,
                           high = c("goldenrod1","orange1","firebrick2", "firebrick3","firebrick","red4"),
                           midpoint = 0,na.value="grey",limits = set_range_cov,
                           n.breaks=5) +
      geom_text(data = glasser_stars_right_lateral, aes(X,Y,label = stars), color = "white",
                size = 7) + 
      theme_void() + 
      theme(axis.text.y = element_blank(),
            plot.title = element_text(size = 18, hjust = 0.5)) +
      theme(axis.text.y = element_blank(),
            plot.title = element_text(size = 18, hjust = 0.5))
    
    cortical_left_medial_sig <- glasser_signif %>% filter(hemi == "left" & side == "medial") %>%
      ggplot() + geom_sf(aes(fill = mean_CovDiff), colour = "black", show.legend = F) +
      scale_fill_gradient2(low = c("navyblue","blue","royalblue","dodgerblue1","deepskyblue1","turquoise3"),
                           mid = NULL,
                           high = c("goldenrod1","orange1","firebrick2", "firebrick3","firebrick","red4"),
                           midpoint = 0,na.value="grey",limits = set_range_cov,
                           n.breaks=5) +
      geom_text(data = glasser_stars_left_medial, aes(X,Y,label = stars), color = "white",
                size = 7) + 
      theme_void() + 
      theme(axis.text.y = element_blank(),
            plot.title = element_text(size = 18, hjust = 0.5)) +
      theme(axis.text.y = element_blank(),
            plot.title = element_text(size = 18, hjust = 0.5))
    
    cortical_right_medial_sig <- glasser_signif %>% filter(hemi == "right" & side == "medial") %>%
      ggplot() + geom_sf(aes(fill = mean_CovDiff), colour = "black") +
      theme_void() +
      # set labels here
      scale_fill_gradient2(low = c("navyblue","blue","royalblue","dodgerblue1","deepskyblue1","turquoise3"),
                           mid = NULL,
                           high = c("goldenrod1","orange1","firebrick2", "firebrick3","firebrick","red4"),
                           midpoint = 0,na.value="grey",limits = set_range_cov, 
                           n.breaks=6,
                           labels = c("F > M", "-0.2", "0.0", "0.2", "M > F")) + 
      geom_text(data = glasser_stars_right_medial, 
                aes(X, Y, label = stars), colour = "white", 
                size = 7) + 
      theme(axis.text.y = element_blank(), 
            legend.title = element_text(angle = 90, size = 18),
            legend.text = element_text(size = 18),
            legend.title.align = 0.5) + 
      labs(fill = "mean covariance sex-bias") + 
      guides(fill = guide_colourbar(barwidth = 1, barheight = 30,
                                    title.position = "right",
                                    label.position = "right")) 
    
        #-----maps all seeds noncortical---------
    noncortical_sagittal <- ggplot(results_aseg) + 
                     geom_brain(atlas = aseg, 
                                side = "sagittal", show.legend = F,
                     mapping = aes(fill = mean_CovDiff), colour = "black") +
                     xlab("") + ylab("") + theme_void() + 
      scale_fill_gradient2(low = c("navyblue","blue","royalblue","dodgerblue1","deepskyblue1","turquoise3"),
                           mid = NULL,
                           high = c("goldenrod1","orange1","firebrick2", "firebrick3","firebrick","red4"),
                           midpoint = 0,na.value="grey",limits = set_range_cov, 
                           n.breaks=5) + 
      theme(axis.text.y = element_blank(),
            plot.title = element_text(size = 18, hjust = 0.5)) +
                     theme(axis.text.y = element_blank())
    
      noncortical_coronal <- ggplot(results_aseg) + 
                             geom_brain(atlas = aseg, 
                             side = "coronal",
                             mapping = aes(fill = mean_CovDiff), 
                             show.legend = F,
                             colour = "black") +
                             xlab("") + ylab("") + theme_void() + 
        scale_fill_gradient2(low = c("navyblue","blue","royalblue","dodgerblue1","deepskyblue1","turquoise3"),
                             mid = NULL,
                             high = c("goldenrod1","orange1","firebrick2", "firebrick3","firebrick","red4"),
                             midpoint = 0,na.value="grey",limits = set_range_cov, 
                             n.breaks=5) + 
        theme(axis.text.y = element_blank(),
              plot.title = element_text(size = 18, hjust = 0.5)) +
                             theme(axis.text.y = element_blank()) 
      
      #----maps significant noncortical----
      noncortical_sagittal_sig <- aseg_signif %>%
        ggplot() + 
        geom_brain(atlas = aseg, 
                   side = "sagittal", show.legend = F,
                   mapping = aes(fill = mean_CovDiff), colour = "black") +
        xlab("") + ylab("") + theme_void() + 
        scale_fill_gradient2(low = c("navyblue","blue","royalblue","dodgerblue1","deepskyblue1","turquoise3"),
                             mid = NULL,
                             high = c("goldenrod1","orange1","firebrick2", "firebrick3","firebrick","red4"),
                             midpoint = 0,na.value="grey",limits = set_range_cov, 
                             n.breaks=5) + 
        theme(axis.text.y = element_blank(),
              plot.title = element_text(size = 18, hjust = 0.5)) +
        theme(axis.text.y = element_blank()) 
      
      noncortical_coronal_sig <- aseg_signif_coronal %>% filter(side == "coronal") %>%
        ggplot() + geom_sf(aes(fill = mean_CovDiff), colour = "black", show.legend = F) +
        scale_fill_gradient2(low = c("navyblue","blue","royalblue","dodgerblue1","deepskyblue1","turquoise3"),
                             mid = NULL,
                             high = c("goldenrod1","orange1","firebrick2", "firebrick3","firebrick","red4"),
                             midpoint = 0,na.value="grey",limits = set_range_cov,
                             n.breaks=5) +
        geom_text(data = aseg_stars, aes(X, Y, label = stars), colour = "white", size = 7) + theme_void() +
        theme(axis.text.y = element_blank(),
              plot.title = element_text(size = 18, hjust = 0.5)) +
        theme(axis.text.y = element_blank(),
              plot.title = element_text(size = 18, hjust = 0.5)) 
       
      #----putting it all together-----
      # design <- c(
      #   area(1,1), area(2,1), area(3,1), 
      #   area(4,1), area(5,1), area(6,1), 
      #   area(1,2), area(2,2), area(3,2), 
      #   area(4,2), area(5,2), area(6,2),
      #   area(1,3), area(2,3), area(3,3), 
      #   area(4,3), area(5,3), area(6,3),
      #   area(1,4), area(2,4), area(3,4), 
      #   area(4,4), area(5,4), area(6,4))
      #  
      # semifinal <-  cortical_left_lateral_bias + cortical_right_lateral_bias + 
      #               cortical_left_medial_bias +  cortical_right_medial_bias +
      #               noncortical_coronal_bias + noncortical_sagittal_bias +
      #   
      #               cortical_left_lateral_bias_signif + cortical_right_lateral_bias_signif + 
      #               cortical_left_medial_bias_signif +  cortical_right_medial_bias_signif +
      #               noncortical_coronal_bias_signif + noncortical_sagittal_bias_signif +
      #   
      #              cortical_left_lateral + cortical_right_lateral + 
      #              cortical_left_medial +  cortical_right_medial +
      #              noncortical_coronal + noncortical_sagittal +
      # 
      #              cortical_left_lateral_sig + cortical_right_lateral_sig + 
      #              cortical_left_medial_sig + cortical_right_medial_sig + 
      #              noncortical_coronal_sig + noncortical_sagittal_sig + 
      #   
      #              plot_layout(nrow = 6, ncol = 4, design = design,
      #                          guides = "collect") &
      #              theme(text = element_text(family = "mono"),
      #                    legend.position = "right")
      
      design_biases <- c(area(1,1), area(2,1), area(3,1), 
                         area(4,1), area(5,1), area(6,1), 
                         area(1,2), area(2,2), area(3,2), 
                         area(4,2), area(5,2), area(6,2))
      
      design_covs <- c(area(1,1), area(2,1), area(3,1),
                       area(4,1), area(5,1), area(6,1),
                       area(1,2), area(2,2), area(3,2),
                       area(4,2), area(5,2), area(6,2))
      
      
      biases_maps <-  cortical_left_lateral_bias + cortical_right_lateral_bias + 
                      cortical_left_medial_bias +  cortical_right_medial_bias +
                      noncortical_coronal_bias + noncortical_sagittal_bias +

                      cortical_left_lateral_bias_signif + cortical_right_lateral_bias_signif +
                      cortical_left_medial_bias_signif +  cortical_right_medial_bias_signif +
                      noncortical_coronal_bias_signif + noncortical_sagittal_bias_signif +
        
                      plot_layout(nrow = 6, ncol = 2, design = design_biases,
                                 guides = "collect") &
                      theme(text = element_text(family = "mono"),
                           legend.position = "right")
      
      biases_maps <- as_grob(biases_maps)
      
      biases_covs <- cortical_left_lateral + cortical_right_lateral +
                     cortical_left_medial +  cortical_right_medial +
                     noncortical_coronal + noncortical_sagittal +

                     cortical_left_lateral_sig + cortical_right_lateral_sig +
                     cortical_left_medial_sig + cortical_right_medial_sig +
                     noncortical_coronal_sig + noncortical_sagittal_sig +

                     plot_layout(nrow = 6, ncol = 2, design = design_covs,
                                 guides = "collect") &
                     theme(text = element_text(family = "mono"),
                           legend.position = "right")

      # biases_covs <- cortical_left_lateral_sig + cortical_right_lateral_sig +
      #   cortical_left_medial_sig + cortical_right_medial_sig +
      #   noncortical_coronal_sig + noncortical_sagittal_sig +
      #   
      #   plot_layout(nrow = 6, ncol = 1, 
      #               guides = "collect") &
      #   theme(text = element_text(family = "mono"),
      #         legend.position = "right")
      # 
      # biases_covs <- as_grob(biases_covs) 
      
      #return(list(biases_maps, biases_covs))
      
      return(biases_covs)
}
