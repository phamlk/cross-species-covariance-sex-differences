library(reshape2)
library(tidymodels)
library(tidyverse)
library(broom)
library(targets)

set.seed(153)

sapply(list.files(pattern="[.]R$", path="functions/", full.names=TRUE), source)

options(mc.cores = parallel::detectCores())

human_volCov_vals <- bind_rows(lapply(1:1000, function(x) test_volCov_shuffle(data_input =
                                                                                tar_read(whole_brain_age_normalized),
                                                                              volCov_orig = tar_read(vol_cov_human_mean_absolute_age_normalized),
                                                                              perma_names = c("Age_in_Yrs", "Gender", "euler", "TBV"),
                                                                              sex_term = "GenderM",
                                                                              sex_colname = "Gender",
                                                                              species = "human",
                                                                              exclude_col = c("Subject", "TBV",
                                                                                              "Age_in_Yrs", "Gender",
                                                                                              "euler"),
                                                                              starting_col = 2,
                                                                              ending_col = 438, 
                                                                              iter = x))) %>% group_by(orig_greater) %>%
  count(orig_greater) %>% 
  mutate(p_val = ifelse(orig_greater == TRUE, 
                        n/1000, 0))

print(human_volCov_vals)

mouse_volCov_vals <- bind_rows(lapply(1:1000, function(x)
  test_volCov_shuffle(data_input =
                        tar_read(leaves_age_normalized),
                      volCov_orig = tar_read(vol_cov_mouse_mean_absolute_age_normalized), 
                      perma_names = c("Mouse_Sex", "Mouse_Age", "TBV"),
                      sex_term = "Mouse_SexM",
                      sex_colname = "Mouse_Sex",
                      species = "mouse",
                      exclude_col = c("Mouse_ID", "Is_Wildtype",
                                      "POND_Mouse_ID", "Study_Name",
                                      "Mouse_Sex", "Mouse_Age",
                                      "Timepoint", "TBV", "Background"),
                      starting_col = 9,
                      ending_col = 263, 
                      iter = x))) %>% group_by(orig_greater) %>%
  count(orig_greater) %>% 
  mutate(p_val = ifelse(orig_greater == TRUE, 
                        n/1000, 0))

print(mouse_volCov_vals)
