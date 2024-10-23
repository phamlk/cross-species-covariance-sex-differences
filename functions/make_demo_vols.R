make_demo_vols <- function(input_normalized, input_original) { 
  
  
  original <- input_original %>% select(any_of(c("left Bed nuclei of the stria terminalis", 
                                    "right Bed nuclei of the stria terminalis", 
                                    "left Medial amygdalar nucleus", "right Medial amygdalar nucleus",
                                    "Mouse_Sex", "Mouse_ID", "Mouse_Age")), matches("olfactory bulb")) %>% 
    mutate(`Olfactory bulb` = rowSums(across(matches("olfactory bulb")))) %>% 
    mutate(BNST = rowSums(across(matches("Bed nuclei")))) %>% 
    mutate(`Medial amygdala` = rowSums(across(matches("Medial amygdalar")))) %>%
    select(Mouse_ID, Mouse_Sex, `Olfactory bulb`, BNST, `Medial amygdala`,
           Mouse_Age) %>% group_by(Mouse_Sex) %>% 
    mutate(Mouse_Age = ifelse(is.na(Mouse_Age) == TRUE, 60, Mouse_Age)) %>% 
    group_by(Mouse_Sex) %>% 
    mutate(mean_ob = mean(`Olfactory bulb`), mean_bnst = mean(BNST), 
           mean_ma = mean(`Medial amygdala`)) %>%
    mutate(across(c("Olfactory bulb", "BNST", "Medial amygdala"), 
                  ~resid(lm(.x ~ Mouse_Age)))) %>% 
    mutate(`Olfactory bulb` = `Olfactory bulb` + mean_ob, 
           `BNST` = `BNST` + mean_bnst,
           `Medial amygdala` = `Medial amygdala` + mean_ma) %>% 
    select(Mouse_Sex, `Olfactory bulb`, `BNST`, `Medial amygdala`)
  
  # normalized <- input_normalized %>% select(any_of(c("left Bed nuclei of the stria terminalis", 
  #                                      "right Bed nuclei of the stria terminalis", 
  #                                      "left Medial amygdalar nucleus", "right Medial amygdalar nucleus",
  #                                      "Mouse_Sex", "Mouse_ID")), matches("olfactory bulb")) %>% 
  # mutate(`Olfactory bulb` = rowSums(across(matches("olfactory bulb")))) %>% 
  # mutate(BNST = rowSums(across(matches("Bed nuclei")))) %>% 
  # mutate(`Medial amygdala` = rowSums(across(matches("Medial amygdalar")))) %>%
  # select(Mouse_ID, Mouse_Sex, `Olfactory bulb`, BNST, `Medial amygdala`) %>%
  # left_join(original, by = c("Mouse_Sex", "Mouse_ID")) %>%
  # mutate(`Olfactory bulb` = `Olfactory bulb` + `Olfactory bulb mean`,
  #        `BNST` = `BNST` + `BNST_mean`,
  #        `Medial amygdala` = `Medial amygdala` + `Medial amygdala mean`) %>%
  # select(Mouse_Sex, `Olfactory bulb`, `BNST`, `Medial amygdala`)
  
  return(original)
  
  } 