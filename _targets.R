# setting target libraries
library(targets)

set.seed(153)

# loading custom functions for pipeline 
sapply(list.files(pattern="[.]R$", path="functions/", full.names=TRUE), source)

# setting packages required for pipeline
options(tidyverse.quiet = TRUE)
options(clustermq.scheduler = "multicore")
tar_option_set(packages = c("reshape2","tidymodels",
                            "lme4", "patchwork", "RMINC", "MRIcrotome",
                            "grid","data.tree", "useful", "pheatmap",
                            "ggsignif", "ggseg", "ggsegGlasser",
                            "broom","tidyverse"),
               garbage_collection = TRUE,
               storage = "worker")

# begin laying down the pipeline
list(
  #---making data tree for eventual mapping of mouse results----
  tar_target(defs, 
             "data/Dorr_2008_Steadman_2013_Ullmann_2013_Richards_2011_Qiu_2016_Egan_2015_40micron/mappings/DSURQE_40micron_R_mapping.csv"),
  
  tar_target(allen, "data/allen.json"),
  
  tar_target(hdefs_allen, makeMICeDefsHierachical(defs, allen)),
  
  tar_target(anat, "data/Dorr_2008_Steadman_2013_Ullmann_2013_Richards_2011_Qiu_2016_Egan_2015_40micron/ex-vivo/DSURQE_40micron_average.mnc"),
  tar_target(labels, "data/Dorr_2008_Steadman_2013_Ullmann_2013_Richards_2011_Qiu_2016_Egan_2015_40micron/ex-vivo/DSURQE_40micron_labels.mnc"),
  tar_target(anatVol, mincArray(mincGetVolume(anat))),
  tar_target(labelVol, mincArray(mincGetVolume(labels))),
  
  #---- pre-processing mouse--------
  # defining the type of data being read in 
  tar_target(volume_leaves, "data-cleaned/mouse_wt_leaves_edit.csv",
             format = "file"),
  
  # reading and cleaning data
  # calling the mouse data leaves for all the leaves region included
  tar_target(leaves, get_volumes(volume_leaves)),
  tar_target(leaves_subcortical, get_mouse_subcor(leaves)),

  # -----pre-processing human-----
  # reading data directory 
  tar_target(cortical_dir, "data-cleaned/cortical.csv", format = "file"),
  tar_target(subcortical_dir, "data-cleaned/subcortical.csv", format = "file"),
  tar_target(map_names_dir, "data-cleaned/name_conversion.csv", format = "file"),

  # reading in the data
  tar_target(cortical, read_csv(cortical_dir)),
  tar_target(subcortical, read_csv(subcortical_dir)),
  tar_target(map_names, read_csv(map_names_dir) %>% 
             mutate(regionName = case_when(regionName == "7Pl_L" ~ "7PL_L",
                                           regionName == "7Pl_R" ~ "7PL_R",
                    TRUE ~ regionName))),

  # combining cortical and subcortical data in human
  tar_target(whole_brain, merge_vols(cortical, subcortical)),
  
  #---ANALYSES not age normalized----
  #----find volumetric sex biases----
  # human
  tar_target(biases_human, all_vol_bias(c("Age_in_Yrs", "Gender", "euler", "TBV"),
                                        2, 438, whole_brain, "human",
                                        "GenderM")),
  tar_target(biases_human_orig_names, all_vol_bias_orig_name(c("Age_in_Yrs", "Gender", "euler", "TBV"),
                                                   2, 438, whole_brain, "human",
                                                   "GenderM")),
  # mouse 
  tar_target(biases_mouse, all_vol_bias(c("Mouse_Sex", "Mouse_Age", "TBV", "Background"),
                                        9, 263, leaves, "mouse", 
                                        "Mouse_SexM")),
  
  tar_target(biases_mouse_orig_names, all_vol_bias_orig_name(c("Mouse_Sex", "Mouse_Age", "TBV"),
                                        9, 263, leaves, "mouse", 
                                        "Mouse_SexM")),
  
  #--- make correlation matrices DATA TABLE in mouse----
  # male
  tar_target(matM_ms, output_orig_matrix(data_input = leaves,
                                         to_exclude = c("Mouse_ID", "Is_Wildtype",
                                                        "POND_Mouse_ID", "Study_Name",
                                                        "Mouse_Age", "Timepoint",
                                                        "TBV", "Background"),
                                         sex_colname = "Mouse_Sex",
                                         sex_init = "m")),
  
  # making some alteration to the correlation results formatting 
  # output allows to see all correlations associated with a "seed" region
  # if that seed region is selected from volume_x
  tar_target(matM_ms_swap, swap_res(source_data = leaves,
                                    to_exclude = c("Mouse_ID", "Is_Wildtype",
                                                   "POND_Mouse_ID", "Study_Name",
                                                   "Mouse_Sex", "Mouse_Age", 
                                                   "Timepoint", "TBV", "Background"),
                                                   results_data = matM_ms, 
                                                   before_col = "correlation",
                                                   species = "mouse")),

  # female
  tar_target(matF_ms, output_orig_matrix(data_input = leaves,
                                           to_exclude = c("Mouse_ID", "Is_Wildtype",
                                                          "POND_Mouse_ID", "Study_Name",
                                                          "Mouse_Age", "Timepoint",
                                                          "TBV", "Background"),
                                           sex_colname = "Mouse_Sex",
                                           sex_init = "f")),
  
  tar_target(matF_ms_swap, swap_res(source_data = leaves,
                                    to_exclude = c("Mouse_ID", "Is_Wildtype",
                                                "POND_Mouse_ID", "Study_Name",
                                                "Mouse_Sex", "Mouse_Age", 
                                                "Timepoint", "TBV", "Background"),
                                    results_data = matF_ms, 
                                    before_col = "correlation",
                                    species = "mouse")),

  #--- make correlation matrices DATA TABLE in human----
  # male
  tar_target(matM_hm, output_orig_matrix(data_input = whole_brain,
                                         to_exclude = c("Subject", "TBV", "Age_in_Yrs",
                                                        "euler"),
                                         sex_colname = "Gender",
                                         sex_init = "m")),

  tar_target(matM_hm_swap, swap_res(source_data = whole_brain,
                                    to_exclude = c("Subject", "TBV",
                                                   "Age_in_Yrs", "Gender",
                                                   "euler"),
                                    results_data = matM_hm, 
                                    before_col = "correlation",
                                    species = "human")),
  
  # female
  tar_target(matF_hm, output_orig_matrix(data_input = whole_brain,
                                         to_exclude = c("Subject", "TBV", "Age_in_Yrs",
                                                        "euler"),
                                         sex_colname = "Gender",
                                         sex_init = "f")),
  
  tar_target(matF_hm_swap, swap_res(source_data = whole_brain,
                                    to_exclude = c("Subject", "TBV",
                                                   "Age_in_Yrs", "Gender",
                                                   "euler"),
                                    results_data = matF_hm, 
                                    before_col = "correlation",
                                    species = "human")),
  
 #---corr sex difference statistical testing mouse----
 tar_target(mouse_res,
            pairwise_pipeline(data_input = leaves,
                              to_exclude = c("Mouse_ID", "Is_Wildtype", 
                                             "POND_Mouse_ID", "Study_Name",
                                             "Mouse_Age", "Timepoint", 
                                             "TBV", "Background"), 
                              sex_colname = "Mouse_Sex")),
 
 
 tar_target(mouse_res_swap, swap_res(source_data = leaves,
                                   to_exclude = c("Mouse_ID", "Is_Wildtype",
                                                  "POND_Mouse_ID", "Study_Name",
                                                  "Mouse_Sex", "Mouse_Age",
                                                  "Timepoint", "TBV", "Background"),
                                   results_data = mouse_res[[1]],
                                   before_col = "p_val",
                                   species = "mouse")),

 tar_target(mouse_res_swap_orig_names, swap_res_orig_names(source_data = leaves,
                                     to_exclude = c("Mouse_ID", "Is_Wildtype",
                                                    "POND_Mouse_ID", "Study_Name",
                                                    "Mouse_Sex", "Mouse_Age",
                                                    "Timepoint", "TBV", "Background"),
                                     results_data = mouse_res[[1]],
                                     before_col = "p_val",
                                     species = "mouse")),

 #---corr sex difference statistical testing human----
 
 tar_target(human_res,
            pairwise_pipeline(data_input = whole_brain,
                              to_exclude = c("Subject", "TBV", "Age_in_Yrs",
                                             "euler"),
                              sex_colname = "Gender")),

 tar_target(human_res_swap, swap_res(source_data = whole_brain,
                                   to_exclude = c("Subject", "TBV",
                                                  "Age_in_Yrs", "Gender",
                                                  "euler"),
                                   results_data = human_res[[1]],
                                   before_col = "p_val",
                                   species = "human")),

 tar_target(human_res_swap_orig_names, swap_res_orig_names(source_data = whole_brain,
                                                to_exclude = c("Subject", "TBV",
                                                               "Age_in_Yrs", "Gender",
                                                               "euler"),
                                                results_data = human_res[[1]],
                                                before_col = "p_val",
                                                species = "human")),

 # #---analyses----
 # 
 # #-----volumetric vs covariance sex biases-----

 # mouse mean
 tar_target(vol_cov_mouse_mean, find_volCov(biases_mouse, mouse_res_swap,
                                       type = "mean", species = "mouse",
                                       signif_type = "all")),

 tar_target(vol_cov_mouse_mean_absolute, find_volCov(biases_mouse, mouse_res_swap,
                                            type = "mean_absolute", species = "mouse",
                                            signif_type = "all")),

 # human mean
 tar_target(vol_cov_human_mean, find_volCov(biases_human, human_res_swap,
                                       type = "mean", species = "human",
                                       signif_type = "all")),

 tar_target(vol_cov_human_mean_absolute, find_volCov(biases_human, human_res_swap,
                                            type = "mean_absolute", species = "human",
                                            signif_type = "all")),
 #----covariance matrices-------
 tar_target(corr_mouse, find_corr(leaves, species = "mouse")),
 tar_target(corr_mouseM, find_corr(leaves, species = "mouse", sex = "M")),
 tar_target(corr_mouseF, find_corr(leaves, species = "mouse", sex = "F")),
 tar_target(corr_mouseDiff, corr_mouseM - corr_mouseF),

 # human
 tar_target(corr_human, find_corr(whole_brain, species = "human")),
 tar_target(corr_humanM, find_corr(whole_brain, species = "human", sex = "M")),
 tar_target(corr_humanF, find_corr(whole_brain, species = "human", sex = "F")),
 tar_target(corr_humanDiff, corr_humanM - corr_humanF),

 # #-----AGE NORMALIZED ANALYSES----------
 # 
 # results are manipulated into different formats for easy calculations downstream (ie original matrices, data table, upper triangle only, 
 # upper and lower triangles [swap])
 # # reading and cleaning data
 # # calling the mouse data leaves for all the leaves region included
 tar_target(leaves_age_normalized, get_volumes_age_normalized(volume_leaves)),

 # # combining cortical and subcortical data in human
 tar_target(whole_brain_age_normalized, merge_vols_age_normalized(cortical, subcortical)),

 #--- make correlation matrices DATA TABLE in mouse----
 # male
 tar_target(matM_ms_age_normalized, output_orig_matrix(data_input = leaves_age_normalized,
                                                       to_exclude = c("Mouse_ID", "Is_Wildtype",
                                                                      "POND_Mouse_ID", "Study_Name",
                                                                      "Mouse_Age", "Timepoint",
                                                                      "TBV", "Background"),
                                                       sex_colname = "Mouse_Sex",
                                                       sex_init = "m")),
 #
 # # making some alteration to the correlation results formatting
 # # output allows to see all correlations associated with a "seed" region
 # # if that seed region is selected from volume_x
 tar_target(matM_ms_swap_age_normalized, swap_res(source_data = leaves_age_normalized,
                                                  to_exclude = c("Mouse_ID", "Is_Wildtype",
                                                                 "POND_Mouse_ID", "Study_Name",
                                                                 "Mouse_Sex", "Mouse_Age",
                                                                 "Timepoint", "TBV", "Background"),
                                                  results_data = matM_ms_age_normalized,
                                                  before_col = "correlation",
                                                  species = "mouse")),

 # # female
 tar_target(matF_ms_age_normalized, output_orig_matrix(data_input = leaves_age_normalized,
                                                       to_exclude = c("Mouse_ID", "Is_Wildtype",
                                                                      "POND_Mouse_ID", "Study_Name",
                                                                      "Mouse_Age", "Timepoint",
                                                                      "TBV", "Background"),
                                                       sex_colname = "Mouse_Sex",
                                                       sex_init = "f")),

 tar_target(matF_ms_swap_age_normalized, swap_res(source_data = leaves_age_normalized,
                                                  to_exclude = c("Mouse_ID", "Is_Wildtype",
                                                                 "POND_Mouse_ID", "Study_Name",
                                                                 "Mouse_Sex", "Mouse_Age",
                                                                 "Timepoint", "TBV", "Background"),
                                                  results_data = matF_ms_age_normalized,
                                                  before_col = "correlation",
                                                  species = "mouse")),

 # #--- make correlation matrices DATA TABLE in human----
 # # male
 tar_target(matM_hm_age_normalized, output_orig_matrix(data_input = whole_brain_age_normalized,
                                                       to_exclude = c("Subject", "TBV", "Age_in_Yrs",
                                                                      "euler"),
                                                       sex_colname = "Gender",
                                                       sex_init = "m")),

 tar_target(matM_hm_swap_age_normalized, swap_res(source_data = whole_brain_age_normalized,
                                                  to_exclude = c("Subject", "TBV",
                                                                 "Age_in_Yrs", "Gender",
                                                                 "euler"),
                                                  results_data = matM_hm_age_normalized,
                                                  before_col = "correlation",
                                                  species = "human")),
 #
 # # female
 tar_target(matF_hm_age_normalized, output_orig_matrix(data_input = whole_brain_age_normalized,
                                                       to_exclude = c("Subject", "TBV", "Age_in_Yrs",
                                                                      "euler"),
                                                       sex_colname = "Gender",
                                                       sex_init = "f")),

 tar_target(matF_hm_swap_age_normalized, swap_res(source_data = whole_brain_age_normalized,
                                                  to_exclude = c("Subject", "TBV",
                                                                 "Age_in_Yrs", "Gender",
                                                                 "euler"),
                                                  results_data = matF_hm_age_normalized,
                                                  before_col = "correlation",
                                                  species = "human")),

 # # #---corr sex difference statistical testing mouse----
 tar_target(mouse_res_age_normalized,
            pairwise_pipeline(data_input = leaves_age_normalized,
                              to_exclude = c("Mouse_ID", "Is_Wildtype",
                                             "POND_Mouse_ID", "Study_Name",
                                             "Mouse_Age", "Timepoint",
                                             "TBV", "Background"),
                              sex_colname = "Mouse_Sex")),

 tar_target(mouse_res_swap_age_normalized, swap_res(source_data = leaves_age_normalized,
                                                    to_exclude = c("Mouse_ID", "Is_Wildtype",
                                                                   "POND_Mouse_ID", "Study_Name",
                                                                   "Mouse_Sex", "Mouse_Age",
                                                                   "Timepoint", "TBV", "Background"),
                                                    results_data = mouse_res_age_normalized[[1]],
                                                    before_col = "p_val",
                                                    species = "mouse")),

 tar_target(mouse_res_swap_orig_names_age_normalized, swap_res_orig_names(source_data = leaves_age_normalized,
                                                                          to_exclude = c("Mouse_ID", "Is_Wildtype",
                                                                                         "POND_Mouse_ID", "Study_Name",
                                                                                         "Mouse_Sex", "Mouse_Age",
                                                                                         "Timepoint", "TBV", "Background"),
                                                                          results_data = mouse_res_age_normalized[[1]],
                                                                          before_col = "p_val",
                                                                          species = "mouse")),

 # ---finding correlation sex differences of demonstration regions in mice (BNST, medial amygdala, olfactory bulb)
 tar_target(demo_vols_exclude_age_normalized, make_demo_vols(input_normalized = leaves_age_normalized,
                                                             input_original = leaves)),
 tar_target(demo_res_age_normalized, pairwise_pipeline_demo(demo_vols_exclude_age_normalized,
                                                            to_exclude = "none",
                                                            sex_colname = "Mouse_Sex",
                                                            output = "pvals")),

 # # #---corr sex difference statistical testing human----
 tar_target(human_res_age_normalized,
            pairwise_pipeline(data_input = whole_brain_age_normalized,
                              to_exclude = c("Subject", "TBV", "Age_in_Yrs",
                                             "euler"),
                              sex_colname = "Gender")),

 tar_target(human_res_swap_age_normalized, swap_res(source_data = whole_brain_age_normalized,
                                                    to_exclude = c("Subject", "TBV",
                                                                   "Age_in_Yrs", "Gender",
                                                                   "euler"),
                                                    results_data = human_res_age_normalized[[1]],
                                                    before_col = "p_val",
                                                    species = "human")),

 tar_target(human_res_swap_orig_names_age_normalized, swap_res_orig_names(source_data = whole_brain_age_normalized,
                                                                          to_exclude = c("Subject", "TBV",
                                                                                         "Age_in_Yrs", "Gender",
                                                                                         "euler"),
                                                                          results_data = human_res_age_normalized[[1]],
                                                                          before_col = "p_val",
                                                                          species = "human")),

 # # #---analyses----
 # #
 # #-----volumetric vs covariance sex biases-----
 #
 # # mouse mean
 tar_target(vol_cov_mouse_mean_age_normalized, find_volCov(biases_mouse, mouse_res_swap_age_normalized,
                                                           type = "mean", species = "mouse",
                                                           signif_type = "all")),

 tar_target(vol_cov_mouse_mean_absolute_age_normalized, find_volCov(biases_mouse, mouse_res_swap_age_normalized,
                                                                    type = "mean_absolute", species = "mouse",
                                                                    signif_type = "all")),

 # # human mean
 tar_target(vol_cov_human_mean_age_normalized, find_volCov(biases_human, human_res_swap_age_normalized,
                                                           type = "mean", species = "human",
                                                           signif_type = "all")),

 tar_target(vol_cov_human_mean_absolute_age_normalized, find_volCov(biases_human, human_res_swap_age_normalized,
                                                                    type = "mean_absolute", species = "human",
                                                                    signif_type = "all")),
 # ---whole covariance matrices
 # mouse matrices (mixed, male, female, difference)
 tar_target(corr_mouse_age_normalized, find_corr(leaves_age_normalized, species = "mouse")),
 tar_target(corr_mouseM_age_normalized, find_corr(leaves_age_normalized, species = "mouse", sex = "M")),
 tar_target(corr_mouseF_age_normalized, find_corr(leaves_age_normalized, species = "mouse", sex = "F")),
 tar_target(corr_mouseDiff_age_normalized, corr_mouseM_age_normalized - corr_mouseF_age_normalized),

 # human matrices (mixed, male, female, difference)
 tar_target(corr_human_age_normalized, find_corr(whole_brain_age_normalized, species = "human")),
 tar_target(corr_humanM_age_normalized, find_corr(whole_brain_age_normalized, species = "human", sex = "M")),
 tar_target(corr_humanF_age_normalized, find_corr(whole_brain_age_normalized, species = "human", sex = "F")),
 tar_target(corr_humanDiff_age_normalized, corr_humanM_age_normalized - corr_humanF_age_normalized)
)

