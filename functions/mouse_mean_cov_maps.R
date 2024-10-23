mouse_mean_cov_maps <- function(res_swap, tree, bias) { 
  
   mean_cov_bias_all <- res_swap %>% mutate(volume_x = 
                        ifelse(hemi != "midline",paste0(hemi, volume_x),
                        volume_x)) %>%
                        group_by(volume_x) %>% 
                        reframe(mean.CovDiff = mean(`m-f`)) %>% ungroup() 
   
   mean_cov_bias_signif <- res_swap %>% mutate(volume_x = 
                           ifelse(hemi != "midline",paste0(hemi, volume_x),
                           volume_x)) %>% filter(signif_adj == TRUE) %>% 
                           group_by(volume_x) %>% 
                           reframe(mean.CovDiff = mean(`m-f`)) %>% ungroup() 
   
  biases <- bias %>% mutate(volume_x = 
                     ifelse(hemi != "midline",paste0(hemi, volume_x),
                     volume_x)) 
  
  # try adding a column of biases regions that also overlap with covariance sex differences 
  # and then set all the numbers to 1 so that the contorus are made there
  # anything else is NA 
  # try to not draw another slice series and see if you can contour right on top
  biases_signif <- biases %>% filter(signif_adjust == TRUE) %>%
                   filter(volume_x %in% mean_cov_bias_signif$volume_x) %>%
                   mutate(estimate = 1)
  
  tree$Do(function(node)
    node$all <- NA)
  
  tree$Do(function(node)
    node$all <- append(node, mean_cov_bias_all, "mean.CovDiff", NA))
  
  tree$Do(function(node)
    node$signif <- NA)
  
  tree$Do(function(node)
    node$signif <- append(node, mean_cov_bias_signif,
                       "mean.CovDiff", NA))
  
  tree$Do(function(node)
    node$vol.bias <- NA)
  
  tree$Do(function(node)
    node$vol.bias <- append(node, biases,
                          "estimate", NA))
  
  tree$Do(function(node)
    node$vol.bias.signif <- append(node, biases_signif,
                            "estimate", NA))
  
  positive_cols <- function() {
    getOption("MRIcrotomeCol", colorRampPalette(c("yellow","red"))(255))
  }
  negative_cols <- function() {
    getOption("MRIcrotomeRcol", colorRampPalette(c("turquoise","blue"))(255))
  }
  
  # positive_cols_bias <- function() {
  #   getOption("MRIcrotomeCol", colorRampPalette(c("yellow","red"))(255))
  # }
  # negative_cols_bias <- function() {
  #   getOption("MRIcrotomeRcol", colorRampPalette(c("turquoise","blue"))(255))
  # }
  
  # biases_maps <- sliceSeries(ncol = 1, nrow = 6, begin=70, end=350) %>% 
  # anatomy(anatVol, low=700, high=1400) %>% 
  # overlay(hanatToVolume(tree, labelVol, "vol.bias"), low = 0, high = 1.2,
  #         col = positive_cols(), rCol=negative_cols(), 
  #         symmetric = T) %>% 
  # addtitle("all vol sex-bias") %>% 
  # sliceSeries() %>% anatomy() %>%
  # overlay(hanatToVolume(tree, labelVol, "vol.bias.signif"), low = 0, high = 1.2,
  #         col = positive_cols(), rCol=negative_cols(), symmetric = T) %>% 
  # addtitle("signif vol sex-bias") %>% 
  # legend("volumetric sex-bias") %>% grobify(titlePars = gpar(fontsize = 20, cex = 0.88, fontfamily = "mono"),
  #                                           legendPars = gpar(fontsize = 16, cex = 0.88, fontfamily = "mono")) 
  # 
  
  # biases_maps <- sliceSeries(ncol = 1, nrow = 6, begin=70, end=350) %>% 
  #   anatomy(anatVol, low=700, high=1400) %>% 
  #   overlay(hanatToVolume(tree, labelVol, "vol.bias.signif"), low = 0, high = 1.2,
  #           col = positive_cols(), rCol=negative_cols(), 
  #           symmetric = T) %>% 
  #   addtitle("significant volumetric sex-biases") 
  #   legend("volumetric sex-bias") %>% grobify(titlePars = gpar(fontsize = 20, cex = 0.88, fontfamily = "mono"),
  #                                             legendPars = gpar(fontsize = 16, cex = 0.88, fontfamily = "mono")) 
  # 
  biases_covs <- sliceSeries(ncol = 1, nrow = 6, begin=70, end=350) %>% 
    anatomy(anatVol, low=700, high=1400) %>% 
    overlay(hanatToVolume(tree, labelVol, "all"), low = 0, high = 0.45, 
          col = positive_cols(), rCol=negative_cols(), symmetric = T) %>% 
  addtitle("all comparisons") %>% 
  sliceSeries() %>% anatomy() %>% 
  overlay(hanatToVolume(tree, labelVol, "signif"), low = 0, high = 0.45,
  col = positive_cols(), rCol=negative_cols(), symmetric = T) %>%
  legend("mean covariance sex-bias") %>%
  contours(hanatToVolume(tree, labelVol, "vol.bias.signif"), levels=1, col = "white",
           lty = 3, lwd = 5) %>%
  addtitle("signif comparisons") %>%
  grobify(titlePars = gpar(fontsize = 20, cex = 0.88, fontfamily = "mono"),
          legendPars = gpar(fontsize = 16, cex = 0.88, fontfamily = "mono"))
  
  #return(list(biases_maps, biases_covs))
  
  return(biases_covs)
}