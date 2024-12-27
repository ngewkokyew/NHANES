library(tidyverse)
library(ggplot2)
library(haven)
library(gtsummary)

# Import files -----
lst_file <- list.files(pattern = "xpt")

lst_dfs <- map(lst_file, ~{
  
  read_xpt(.x)
  
}) 

sdtm <- reduce(lst_dfs, left_join) %>% 
  group_by(SEQN) %>%
  slice_head(n = 1) %>%
  ungroup()

# Inclusion and exclusion criteria -----
adam <- sdtm %>%
  filter(!is.na(RIDAGEYR) & (RIDAGEYR >= 18)) %>%
  filter(!is.na(RIAGENDR)) %>%
  filter(!is.na(CDQ001))

# Data manipulation -----
adam <- adam %>%
  rowwise() %>%
  mutate(
    ## Blood Pressue
    BP_sys = mean(c_across(c("BPXOSY1", "BPXOSY2", "BPXOSY3")), na.rm = TRUE),
    BP_dia = mean(c_across(c("BPXODI1", "BPXODI2", "BPXODI3")), na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    Gender = case_when(
      RIAGENDR == 1 ~ "Male",
      RIAGENDR == 2 ~ "Female",
      TRUE ~ NA_character_
    ),
    
    # Glucose and Insulin
    Glucose = LBDGLUSI,
    Insulin = LBDINSI,
    
    # Lipid Profile
    HDL = LBDHDDSI,
    LDL = LBDLDLSI,
    TRI = LBDTRSI,
    
    ## Cardiovascular Health
    Cardio = case_when(
      (CDQ001 == 1) | (CDQ010 == 1) ~ "Yes",
      TRUE ~ "No"
    ),
    
    Angina_G1 = case_when(
      (CDQ001 == 1) & (CDQ002 == 1) & (CDQ003 != 1) & (CDQ004 == 1) & (CDQ005 == 1) & (CDQ006 == 1) &
        (CDQ009D == 4 | CDQ009E == 5 | CDQ009F == 6 | CDQ009G == 7) ~ "Yes",
      TRUE ~ "No"
    ),
    
    Angina_G2 = case_when(
      (CDQ001 == 1) & (CDQ002 == 1) & (CDQ003 = 1) & (CDQ004 == 1) & (CDQ005 == 1) & (CDQ006 == 1) &
        (CDQ009D == 4 | CDQ009E == 5 | CDQ009F == 6 | CDQ009G == 7) ~ "Yes",
      TRUE ~ "No"
    ),
    
    ## Diabetes 
    Diabetes = factor(case_when(
      DIQ010 == 1 ~ "Diabetes",
      DIQ010 == 3 | DIQ160 == 1 ~ "Pre-Diabetes",
      DIQ010 == 2 ~ "No",
      TRUE ~ NA_character_
    ),
    levels = c("Diabetes", "Pre-Diabetes", "No")),
    
    ## Alcohol Use
    Alcohol = factor(case_when(
      (ALQ111 == 2) | (ALQ111 == 1 & ALQ121 == 0) ~ "Not at all",
      ALQ111 == 1 & ALQ121 %in% c(1, 2) ~ "Every (or nearly) day",
      ALQ111 == 1 & ALQ121 %in% c(3, 4, 5) ~ "> once a week",
      ALQ111 == 1 & ALQ121 %in% c(6, 7) ~ "> once a month",
      ALQ111 == 1 & ALQ121 %in% c(8, 9, 10) ~ "> once a year", 
      TRUE ~ NA_character_
    ),
    levels = c("Every (or nearly) day", "> once a week", "> once a month", "> once a year", "Not at all")),
    
    ## Smoking
    Smoke = factor(case_when(
      SMQ020 == 2 & SMQ890 == 2 ~ "Not at all",
      SMQ020 == 1 & SMQ040 == 3 ~ "Ex-smoker",
      SMQ020 == 1 & SMQ040 %in% c(1, 2) ~ "Current smoker",
      TRUE ~ NA_character_
    ),
    levels = c("Current smoker", "Ex-smoker", "Not at all"))
  )

adam_sub <- adam %>% filter(RIDRETH3 == 6)

# Analyses
adam %>%
  select(HDL, LDL, TRI, Cardio) %>%
  tbl_summary(by = Cardio)

# ggplot(data = adam, aes(x = Angina_G1, y = TRI, fill = Angina_G1)) + 
#   geom_boxplot() +
#   theme_bw() + 
#   scale_fill_viridis_d()