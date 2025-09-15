#------------------------------------------------------------------------------#
# PACE-TB - Outputs for CEA                                                    #
# Extract outputs for cost-effectiveness analysis                              #
# Code by Alvaro Schwalb (alvaro.schwalb@lshtm.ac.uk)                          #
# Last updated 2025-06-11 by KCH and ASC                                       #
#------------------------------------------------------------------------------#

# Packages
suppressPackageStartupMessages({
  library(here)
  library(rio)
  library(data.table)
  library(tidyverse)
  library(openxlsx)
  library(fst)
})

rm(list = ls())

# Checks outputs in folder
files <- list.files(here("outputs", "epi"), full.names = FALSE)

for (i in 1:length(files)) {
  file <- files[i]

  # Extract intervention code
  int <- strsplit(strsplit(file,"_")[[1]][3],"\\.")[[1]][1]
  if (!int %in% c("BAU", "VAX", "TPT", "NTN", "SCR", "DGN", "DST", "PRI", "SDS", "SDR")) {
    stop(paste("Error: Invalid intervention code in file name:", file))
  }
  
  # Extract ISO code
  iso <- strsplit(file,"_")[[1]][1]
  if (!iso %in% c("BRA", "IND", "ZAF")) {
    stop(paste("Error: Invalid ISO code in file name:", file))
  }
  
  # Extract UID
  UID <- strsplit(file,"_")[[1]][2]

  print(paste("Intervention:", int, "| ISO:", iso, "| UID:", UID))
  
  # Load the file
  stocks <- read_fst(here("outputs", "epi", paste0(file)))

  # Model outputs [n=year: 0-25] [n=age group: 1-16]
  # 1. Number of people with symptomatic TB 
  # Format: n_Dc_y[n]_ag[n]
  n_Dc_y_ag <- stocks %>% 
    mutate(year = ceiling(year)) %>% 
    filter(year >= 2026) %>% 
    filter(TB == 'Inc_Dc_count') %>% 
    filter(!(str_detect(RISK, 'dead') | str_detect(RISK, 'count'))) %>% 
    filter(!(str_detect(VXa, 'dead') | str_detect(VXa, 'count'))) %>% 
    filter(!(age_from == 0 & age_thru == 99)) %>% 
    mutate(age = (age_from/5) + 1) %>% 
    select(iso = country, year, TB, age, value) %>% 
    group_by(iso, year, age) %>% 
    summarise(val = sum(value) * 1e3, .groups = "drop") %>% 
    ungroup() %>% 
    mutate(var = paste0("n_Dc_y", year - 2025, "_ag", age)) %>% 
    mutate(intv = int) %>%     
    mutate(UIDv = UID) %>% 
    select(iso, intv, UIDv, var, val)
  
  # 2. Number of people who previously had symptomatic TB
  # Format: n_postTB_y0_ag[n]
  n_postTB_y0_ag <- stocks %>% 
    mutate(year = ceiling(year)) %>% 
    filter(year == 2025) %>% 
    filter(str_detect(TB, 'Tr') | str_detect(TB, 'Tc')) %>% 
    filter(!(str_detect(TB, 'count'))) %>% 
    filter(!(str_detect(RISK, 'dead') | str_detect(RISK, 'count'))) %>% 
    filter(!(str_detect(VXa, 'dead') | str_detect(VXa, 'count'))) %>% 
    filter(!(age_from == 0 & age_thru == 99)) %>% 
    mutate(age = (age_from/5) + 1) %>% 
    select(iso = country, year, TB, age, value) %>% 
    group_by(iso, year, age) %>% 
    summarise(val = sum(value) * 1e3, .groups = "drop") %>% 
    ungroup() %>% 
    mutate(var = paste0("n_postTB_y0_ag", age)) %>% 
    mutate(intv = int) %>%
    mutate(UIDv = UID) %>% 
    select(iso, intv, UIDv, var, val)
  
  # 3. Number of people who have never had symptomatic TB
  # Format: n_noTB_y[n]_ag[n]
  temp <- stocks %>%
    filter(year %% 1 != 0.5) %>%
    mutate(year = ceiling(year)) %>%
    filter(year >= 2026) %>% 
    filter(!(str_detect(TB, 'count')| str_detect(TB, 'dead'))) %>% 
    filter(!(str_detect(RISK, 'dead') | str_detect(RISK, 'count'))) %>%
    filter(!(str_detect(VXa, 'dead') | str_detect(VXa, 'count'))) %>% 
    filter(!(age_from == 0 & age_thru == 99)) %>% 
    mutate(age = (age_from/5) + 1) %>% 
    group_by(iso = country, year, age) %>% 
    summarise(val = sum(value), .groups = "drop") %>% 
    ungroup()%>%
    select(iso, year, age, pop = val)
  
  n_noTB_y_ag <- stocks %>%
    mutate(year = ceiling(year)) %>% 
    filter(year >= 2026) %>% 
    filter(str_detect(TB, 'Tr') | str_detect(TB, 'Tc')) %>% 
    filter(!(str_detect(TB, 'count'))) %>% 
    filter(!(str_detect(RISK, 'dead') | str_detect(RISK, 'count'))) %>%
    filter(!(str_detect(VXa, 'dead') | str_detect(VXa, 'count'))) %>% 
    filter(!(age_from == 0 & age_thru == 99)) %>% 
    mutate(age = (age_from/5) + 1) %>% 
    group_by(iso = country, year, age) %>% 
    summarise(val = sum(value), .groups = "drop") %>% 
    ungroup() %>% 
    left_join(temp, by = c('iso', 'year', 'age')) %>% 
    mutate(val = (pop - val) * 1e3) %>% 
    mutate(var = paste0("n_noTB_y", year - 2025, "_ag", age)) %>% 
    mutate(intv = int) %>% 
    mutate(UIDv = UID) %>% 
    select(iso, intv, UIDv, var, val)
  rm(temp)
  
  # 4. Number of TB-related deaths
  # Format: n_TBdeath_y[n]_ag[n]
  n_TBdeath_y_ag <- stocks %>% 
    mutate(year = ceiling(year)) %>% 
    filter(year >= 2026) %>% 
    filter(str_detect(TB, 'dead')) %>%
    filter(!(str_detect(RISK, 'dead') | str_detect(RISK, 'count'))) %>% 
    filter(!(str_detect(VXa, 'dead') | str_detect(VXa, 'count'))) %>% 
    filter(!(age_from == 0 & age_thru == 99)) %>% 
    mutate(age = (age_from/5) + 1) %>% 
    select(iso = country, year, TB, age, value) %>% 
    group_by(iso, year, age) %>% 
    summarise(val = sum(value) * 1e3, .groups = "drop") %>% 
    ungroup() %>% 
    mutate(var = paste0("n_TBdeath_y", year - 2025, "_ag", age)) %>% 
    mutate(intv = int) %>% 
    mutate(UIDv = UID) %>% 
    select(iso, intv, UIDv, var, val)
  
  # 5. Mean duration of symptomatic TB per person [DUMMY]
  # Format: duration_Dc_ag[n]
  duration_Dc_ag <- stocks %>% 
    filter(TB == 'Inc_Dc_count') %>% 
    filter(!(str_detect(RISK, 'dead') | str_detect(RISK, 'count'))) %>% 
    filter(!(str_detect(VXa, 'dead') | str_detect(VXa, 'count'))) %>% 
    filter(!(age_from == 0 & age_thru == 99)) %>% 
    mutate(age = (age_from/5) + 1) %>% 
    select(iso = country, age, value) %>% 
    group_by(iso, age) %>% 
    summarise(val = 1, .groups = "drop") %>% 
    ungroup() %>% 
    mutate(var = paste0("duration_Dc_ag", age)) %>% 
    mutate(intv = int) %>% 
    mutate(UIDv = UID) %>% 
    select(iso, intv, UIDv, var, val)
  
  # 2b. Number of people newly starting post-TB
  # Format: n_postTB_y[n]_ag[n]
  temp <- stocks %>% 
    mutate(year = ceiling(year)) %>% 
    filter(year >= 2026) %>% 
    filter((str_detect(TB, 'Dc') & str_detect(TB, 'dead'))) %>%
    filter(!(str_detect(RISK, 'dead') | str_detect(RISK, 'count'))) %>%
    filter(!(str_detect(VXa, 'dead') | str_detect(VXa, 'count'))) %>% 
    filter(!(age_from == 0 & age_thru == 99)) %>% 
    mutate(age = (age_from/5) + 1) %>% 
    select(iso = country, year, TB, age, value) %>% 
    group_by(iso, year, age) %>% 
    summarise(dead = sum(value), .groups = "drop") %>% 
    ungroup()
  
  n_postTB_y_ag <- stocks %>% 
    mutate(year = ceiling(year)) %>% 
    filter(year >= 2026) %>% 
    filter(TB == 'Inc_Dc_count') %>% 
    filter(!(str_detect(RISK, 'dead') | str_detect(RISK, 'count'))) %>% 
    filter(!(str_detect(VXa, 'dead') | str_detect(VXa, 'count'))) %>% 
    filter(!(age_from == 0 & age_thru == 99)) %>% 
    mutate(age = (age_from/5) + 1) %>% 
    select(iso = country, year, TB, age, value) %>% 
    group_by(iso, year, age) %>% 
    summarise(val = sum(value), .groups = "drop") %>% 
    ungroup() %>% 
    left_join(temp, by = c('iso', 'year', 'age')) %>% 
    mutate(val = (val - dead) * 1e3) %>% 
    mutate(var = paste0("n_postTB_y", year - 2025, "_ag", age)) %>% 
    mutate(intv = int) %>% 
    mutate(UIDv = UID) %>% 
    select(iso, intv, UIDv, var, val)
  rm(temp)
  
  # 6a. Whole population
  # Format: n_all_y[n]
  n_all_y <- stocks %>%
    filter(year %% 1 != 0.5) %>%
    mutate(year = ceiling(year)) %>%
    filter(year >= 2026) %>% 
    filter(!(str_detect(TB, 'count')| str_detect(TB, 'dead'))) %>% 
    filter(!(str_detect(RISK, 'dead') | str_detect(RISK, 'count'))) %>%
    filter(!(str_detect(VXa, 'dead') | str_detect(VXa, 'count'))) %>%
    filter(!(age_from == 0 & age_thru == 99)) %>% 
    mutate(age = (age_from/5) + 1) %>% 
    group_by(iso = country, year) %>% 
    summarise(val = sum(value)*1e3, .groups = "drop") %>% 
    mutate(var = paste0("n_all_y", year - 2025)) %>%
    mutate(intv = int) %>% 
    mutate(UIDv = UID) %>% 
    ungroup()%>%
    select(iso, intv, UIDv, var, val)
  
  # 6b. Number in each age group
  # Format: n_all_y[n]_ag[n]
  n_all_y_ag <- stocks %>%
    mutate(year = ceiling(year)) %>%
    filter(year >= 2026) %>% 
    filter(!(str_detect(TB, 'count')| str_detect(TB, 'dead'))) %>% 
    filter(!(str_detect(RISK, 'dead') | str_detect(RISK, 'count'))) %>%
    filter(!(str_detect(VXa, 'dead') | str_detect(VXa, 'count'))) %>%
    filter(!(age_from == 0 & age_thru == 99)) %>% 
    mutate(age = (age_from/5) + 1) %>% 
    group_by(iso = country, year, age) %>% 
    summarise(val = sum(value)*1e3, .groups = "drop") %>% 
    mutate(var = paste0("n_all_y", year - 2025, "_ag", age)) %>% 
    mutate(intv = int) %>% 
    mutate(UIDv = UID) %>% 
    ungroup()%>%
    select(iso, intv, UIDv, var, val) 
  
  # 7. Number entering the diagnostic pathway
  # Format: n_assess_y[n]
  if (int == "SCR" | int == "PRI") {
    n_assess_y <- stocks %>% 
      mutate(year = ceiling(year)) %>% 
      filter(year >= 2026) %>% 
      filter(!str_detect(TB, 'iTest')) %>%
      filter(str_detect(TB, 'Test')) %>%
      filter(!(str_detect(RISK, 'dead') | str_detect(RISK, 'count'))) %>% 
      filter(!(str_detect(VXa, 'dead') | str_detect(VXa, 'count'))) %>% 
      filter((age_from == 0 & age_thru == 99)) %>% 
      select(iso = country, year, TB, value) %>% 
      group_by(iso, year) %>% 
      summarise(val = sum(value) * 1e3, .groups = "drop") %>% 
      ungroup() %>% 
      mutate(var = paste0("n_assess_y", year - 2025)) %>% 
      mutate(intv = int) %>% 
      mutate(UIDv = UID) %>% 
      select(iso, intv, UIDv, var, val)
  
  } else {
    
    n_assess_y <- stocks %>% 
      mutate(year = ceiling(year)) %>% 
      filter(year >= 2026) %>% 
      filter(str_detect(TB, 'Test')) %>%
      filter(!(str_detect(RISK, 'dead') | str_detect(RISK, 'count'))) %>% 
      filter(!(str_detect(VXa, 'dead') | str_detect(VXa, 'count'))) %>% 
      filter((age_from == 0 & age_thru == 99)) %>% 
      select(iso = country, year, TB, value) %>% 
      group_by(iso, year) %>% 
      summarise(val = sum(value) * 1e3, .groups = "drop") %>% 
      ungroup() %>% 
      mutate(var = paste0("n_assess_y", year - 2025)) %>% 
      mutate(intv = int) %>% 
      mutate(UIDv = UID) %>% 
      select(iso, intv, UIDv, var, val)
  }
  
  # 8a. Those diagnosed with TB
  # Format: n_diag_y[n]
  n_diag_y <- stocks %>% 
    mutate(year = ceiling(year)) %>% 
    filter(year >= 2026) %>% 
    filter(str_detect(TB, 'Diag')) %>%
    filter(!(str_detect(RISK, 'dead') | str_detect(RISK, 'count'))) %>% 
    filter(!(str_detect(VXa, 'dead') | str_detect(VXa, 'count'))) %>% 
    filter((age_from == 0 & age_thru == 99)) %>% 
    select(iso = country, year, TB, value) %>% 
    group_by(iso, year) %>% 
    summarise(val = sum(value) * 1e3, .groups = "drop") %>% 
    ungroup() %>% 
    mutate(var = paste0("n_diag_y", year - 2025)) %>% 
    mutate(intv = int) %>%
    mutate(UIDv = UID) %>% 
    select(iso, intv, UIDv, var, val)
  
  # 8b. Those diagnosed with TB and not already tested using Xpert [ROUGH CALCULATION]
  # Format: n_diag_noXpert_y[n] 
  if (int == "DST") {
    pxpert <- switch(iso, 'BRA' = 0.50, 'IND' = 0.21, 'ZAF' = 0.61)
    
    n_diag_noXpert_y <- stocks %>% 
      mutate(year = ceiling(year)) %>% 
      filter(year >= 2026) %>% 
      filter(str_detect(TB, 'Diag')) %>%
      filter(!(str_detect(RISK, 'dead') | str_detect(RISK, 'count'))) %>%
      filter(!(str_detect(VXa, 'dead') | str_detect(VXa, 'count'))) %>%  
      filter((age_from == 0 & age_thru == 99)) %>% 
      select(iso = country, year, TB, value) %>% 
      group_by(iso, year) %>% 
      summarise(val = sum(value) * 1e3, .groups = "drop") %>% 
      ungroup() %>% 
      mutate(var = paste0("n_diag_noXpert_y", year - 2025)) %>% 
      mutate(intv = int) %>%
      mutate(UIDv = UID) %>% 
      select(iso, intv, UIDv, var, val) %>%
      mutate(val = val * (1 - pxpert)) 
    rm(pxpert)
  }
  
  # 9a. Number initiating treatment for DS-TB
  # Format: n_treat_DS_y[n]
  n_treat_DS_y <- stocks %>% 
    mutate(year = ceiling(year)) %>% 
    filter(year >= 2026) %>% 
    filter((str_detect(TB, 'ST') & str_detect(TB, 'count'))) %>% 
    filter(!(str_detect(TB, 'succ'))) %>% 
    filter(!(str_detect(RISK, 'dead') | str_detect(RISK, 'count'))) %>% 
    filter(!(str_detect(VXa, 'dead') | str_detect(VXa, 'count'))) %>% 
    filter((age_from == 0 & age_thru == 99)) %>% 
    select(iso = country, year, TB, value) %>% 
    group_by(iso, year) %>% 
    summarise(val = sum(value) * 1e3, .groups = "drop") %>% 
    ungroup() %>% 
    mutate(var = paste0("n_treat_DS_y", year - 2025)) %>% 
    mutate(intv = int) %>% 
    mutate(UIDv = UID) %>% 
    select(iso, intv, UIDv, var, val)
  
  # 9b. Number initiating treatment for DR-TB
  # Format: n_treat_DR_y[n]
  if (int == "SDR") {
    n_treat_DR_y <- stocks %>% 
      mutate(year = ceiling(year)) %>% 
      filter(year >= 2026) %>% 
      filter(!(str_detect(TB, 'iRT') & str_detect(TB, 'count'))) %>% 
      filter((str_detect(TB, 'RT') & str_detect(TB, 'count'))) %>% 
      filter(!(str_detect(TB, 'succ'))) %>% 
      filter(!(str_detect(RISK, 'dead') | str_detect(RISK, 'count'))) %>%
      filter(!(str_detect(VXa, 'dead') | str_detect(VXa, 'count'))) %>% 
      filter((age_from == 0 & age_thru == 99)) %>% 
      select(iso = country, year, TB, value) %>% 
      group_by(iso, year) %>% 
      summarise(val = sum(value) * 1e3, .groups = "drop") %>% 
      ungroup() %>% 
      mutate(var = paste0("n_treat_DR_y", year - 2025)) %>% 
      mutate(intv = int) %>% 
      mutate(UIDv = UID) %>% 
      select(iso, intv, UIDv, var, val)
    
  } else {
    
    n_treat_DR_y <- stocks %>% 
      mutate(year = ceiling(year)) %>% 
      filter(year >= 2026) %>% 
      filter((str_detect(TB, 'RT') & str_detect(TB, 'count'))) %>% 
      filter(!(str_detect(TB, 'succ'))) %>% 
      filter(!(str_detect(RISK, 'dead') | str_detect(RISK, 'count'))) %>%
      filter(!(str_detect(VXa, 'dead') | str_detect(VXa, 'count'))) %>% 
      filter((age_from == 0 & age_thru == 99)) %>% 
      select(iso = country, year, TB, value) %>% 
      group_by(iso, year) %>% 
      summarise(val = sum(value) * 1e3, .groups = "drop") %>% 
      ungroup() %>% 
      mutate(var = paste0("n_treat_DR_y", year - 2025)) %>% 
      mutate(intv = int) %>% 
      mutate(UIDv = UID) %>% 
      select(iso, intv, UIDv, var, val)
  }
  
  # 9c. Number initiating short treatment for DR-TB
  # Format: n_treat_DR_short_y[n]
  if (int == "SDR") {
    n_treat_DR_short_y <- stocks %>% 
      mutate(year = ceiling(year)) %>% 
      filter(year >= 2026) %>% 
      filter((str_detect(TB, 'iRT') & str_detect(TB, 'count'))) %>% 
      filter(!(str_detect(TB, 'succ'))) %>% 
      filter(!(str_detect(RISK, 'dead') | str_detect(RISK, 'count'))) %>%
      filter(!(str_detect(VXa, 'dead') | str_detect(VXa, 'count'))) %>% 
      filter((age_from == 0 & age_thru == 99)) %>% 
      select(iso = country, year, TB, value) %>% 
      group_by(iso, year) %>% 
      summarise(val = sum(value) * 1e3, .groups = "drop") %>% 
      ungroup() %>% 
      mutate(var = paste0("n_treat_DR_short_y", year - 2025)) %>% 
      mutate(intv = int) %>% 
      mutate(UIDv = UID) %>% 
      select(iso, intv, UIDv, var, val)
  }
  # 10a. Number of households with an index case of DS-TB
  # Format: n_HH_DS_y[n]_ag[n]
  # TBD
  
  # 10b. Number of households with an index case of DR-TB
  # Format: n_HH_DR_y[n]_ag[n]
  # TBD
  
  # 10c. Number of households with an index case of DS-TB
  # Format: n_HHC_DS_y[n]
  hh_all <- switch(iso, 'BRA' = 3.3, 'IND' = 4.4, 'ZAF' = 3.2)
  
  if (int == "TPT") {
    n_HHC_DS_y <- stocks %>% 
      mutate(year = ceiling(year)) %>% 
      filter(year >= 2026) %>% 
      filter(str_detect(TB, 'ST') & (str_detect(TB, 'adult') | str_detect(TB, 'children'))) %>% 
      filter(!(str_detect(RISK, 'dead') | str_detect(RISK, 'count'))) %>%
      filter(!(str_detect(VXa, 'dead') | str_detect(VXa, 'count'))) %>% 
      filter((age_from == 0 & age_thru == 99)) %>% 
      select(iso = country, year, TB, value) %>% 
      group_by(iso, year) %>% 
      summarise(val = sum(value), .groups = "drop") %>% 
      ungroup() %>% 
      mutate(val = val * (hh_all - 1) * 1e3) %>% 
      mutate(val = case_when(year == 2026 ~ val * 0.16, year == 2027 ~ val * 0.32,
                             year == 2028 ~ val * 0.48, year == 2029 ~ val * 0.64, 
                             year >= 2030 ~ val * 0.8)) %>% 
      mutate(var = paste0("n_HHC_DS_y", year - 2025)) %>% 
      mutate(intv = int) %>% 
      mutate(UIDv = UID) %>% 
      select(iso, intv, UIDv, var, val)
  }
  
  # 10d. Number of households with an index case of DR-TB
  # Format: n_HHC_DR_y[n]
  if (int == "TPT") {
    n_HHC_DR_y <- stocks %>% 
      mutate(year = ceiling(year)) %>% 
      filter(year >= 2026) %>% 
      filter(str_detect(TB, 'RT') & (str_detect(TB, 'adult') | str_detect(TB, 'children'))) %>% 
      filter(!(str_detect(RISK, 'dead') | str_detect(RISK, 'count'))) %>% 
      filter(!(str_detect(VXa, 'dead') | str_detect(VXa, 'count'))) %>% 
      filter((age_from == 0 & age_thru == 99)) %>% 
      select(iso = country, year, TB, value) %>% 
      group_by(iso, year) %>% 
      summarise(val = sum(value), .groups = "drop") %>% 
      ungroup() %>% 
      mutate(val = val * (hh_all - 1) * 1e3) %>% 
      mutate(val = case_when(year == 2026 ~ val * 0.16, year == 2027 ~ val * 0.32,
                             year == 2028 ~ val * 0.48, year == 2029 ~ val * 0.64, 
                             year >= 2030 ~ val * 0.8)) %>% 
      mutate(var = paste0("n_HHC_DR_y", year - 2025)) %>% 
      mutate(intv = int) %>% 
      mutate(UIDv = UID) %>% 
      select(iso, intv, UIDv, var, val)
  }
  rm(hh_all)

  # 11a. Number of household contacts receiving TPT for DS-TB
  # Format: n_HHC_DSTPT_y[n]
  if (int == "TPT") {
    n_HHC_DSTPT_y <- stocks %>% 
      mutate(year = ceiling(year)) %>% 
      filter(year >= 2026) %>% 
      filter(!(str_detect(TB, 'count') | str_detect(TB, 'dead'))) %>%
      filter(!(str_detect(RISK, 'dead') | str_detect(RISK, 'count'))) %>%
      filter(str_detect(VXa, 'tptDS_count')) %>% 
      filter(!(age_from == 0 & age_thru == 99)) %>% 
      mutate(age = (age_from/5) + 1) %>% 
      filter(age <= 3) %>% 
      select(iso = country, year, TB, value) %>% 
      group_by(iso, year) %>% 
      summarise(val = sum(value), .groups = "drop") %>% 
      ungroup() %>% 
      mutate(val = val / 0.89 * 1e3) %>% 
      mutate(var = paste0("n_HHC_DSTPT_y", year - 2025)) %>% 
      mutate(intv = int) %>% 
      mutate(UIDv = UID) %>% 
      select(iso, intv, UIDv, var, val)
  } 
  
  # 11b. Number of household contacts receiving TPT for DR-TB
  # Format: n_HHC_DRTPT_y[n]
  if (int == "TPT") {
    n_HHC_DRTPT_y <- stocks %>% 
      mutate(year = ceiling(year)) %>% 
      filter(year >= 2026) %>% 
      filter(!(str_detect(TB, 'count') | str_detect(TB, 'dead'))) %>%
      filter(!(str_detect(RISK, 'dead') | str_detect(RISK, 'count'))) %>%
      filter(str_detect(VXa, 'tptDR_count')) %>% 
      filter(!(age_from == 0 & age_thru == 99)) %>% 
      mutate(age = (age_from/5) + 1) %>% 
      filter(age <= 3) %>% 
      select(iso = country, year, TB, value) %>% 
      group_by(iso, year) %>% 
      summarise(val = sum(value), .groups = "drop") %>% 
      ungroup() %>% 
      mutate(val = val / 0.89 * 1e3) %>% 
      mutate(var = paste0("n_HHC_DRTPT_y", year - 2025)) %>% 
      mutate(intv = int) %>% 
      mutate(UIDv = UID) %>% 
      select(iso, intv, UIDv, var, val)
  }  
  
  # 11c. Number of household contacts treated for DS-TB
  # Format: n_HHC_DStreat_y[n]_ag[n]
  # TBD
  
  # 11d. Number of household contacts treated for DR-TB
  # Format: n_HHC_DRtreat_y[n]_ag[n]
  # TBD
  
  # 12a. Number of index cases receiving RATIONS
  # Format: n_nutrition_index_y[n]
  if (int == "NTN") {
    n_nutrition_index_y <- stocks %>% 
      mutate(year = ceiling(year)) %>% 
      filter(year >= 2026) %>% 
      filter((str_detect(TB, 'ST') & str_detect(TB, 'count')) | 
               (str_detect(TB, 'RT') & str_detect(TB, 'count'))) %>% 
      filter(!(str_detect(RISK, 'dead') | str_detect(RISK, 'count'))) %>%
      filter(!(str_detect(VXa, 'dead') | str_detect(VXa, 'count'))) %>% 
      filter((age_from == 0 & age_thru == 99)) %>% 
      select(iso = country, year, TB, value) %>% 
      group_by(iso, year) %>% 
      summarise(val = sum(value) * 1e3, .groups = "drop") %>% 
      ungroup() %>% 
      mutate(val = case_when(year == 2026 ~ val * 0.16, year == 2027 ~ val * 0.32,
                             year == 2028 ~ val * 0.48, year == 2029 ~ val * 0.64, 
                             year >= 2030 ~ val * 0.8)) %>% 
      mutate(var = paste0("n_nutrition_index_y", year - 2025)) %>% 
      mutate(intv = int) %>% 
      mutate(UIDv = UID) %>% 
      select(iso, intv, UIDv, var, val)
  }    
  
  # 12b. Number of household contacts of cases receiving RATIONS
  # Format: n_nutrition_HHC_y[n]
  if (int == "NTN") {
    n_nutrition_HHC_y <- stocks %>% 
      mutate(year = ceiling(year)) %>% 
      filter(year >= 2026) %>% 
      filter((str_detect(TB, 'ST') & str_detect(TB, 'count')) | 
               (str_detect(TB, 'RT') & str_detect(TB, 'count'))) %>% 
      filter(!(str_detect(RISK, 'dead') | str_detect(RISK, 'count'))) %>%
      filter(!(str_detect(VXa, 'dead') | str_detect(VXa, 'count'))) %>% 
      filter((age_from == 0 & age_thru == 99)) %>% 
      select(iso = country, year, TB, value) %>% 
      group_by(iso, year) %>% 
      summarise(val = sum(value) * 3.4 * 1e3, .groups = "drop") %>% 
      ungroup() %>% 
      mutate(val = case_when(year == 2026 ~ val * 0.16, year == 2027 ~ val * 0.32,
                             year == 2028 ~ val * 0.48, year == 2029 ~ val * 0.64, 
                             year >= 2030 ~ val * 0.8)) %>% 
      mutate(var = paste0("n_nutrition_HHC_y", year - 2025)) %>% 
      mutate(intv = int) %>% 
      mutate(UIDv = UID) %>% 
      select(iso, intv, UIDv, var, val)
  }    
  
  # 12c. Number of people with TB receiving nutrition 
  # Format: n_nutrition_index_y[n]
  
  
  # 12d. Number of household contacts receiving nutrition
  # Format: n_nutrition_HHC_y[n]
  
  
  # 13. Number of people screened in prison
  # Format: n_screen_prison_y[n]
  if (iso == "BRA") {
    n_screen_prison_y <- stocks %>% 
      mutate(year = ceiling(year)) %>% 
      filter(year >= 2026) %>% 
      filter(TB == "iTest_count") %>% 
      filter(RISK == 'prison') %>% 
      filter(!(str_detect(RISK, 'dead') | str_detect(RISK, 'count'))) %>% 
      filter(!(str_detect(VXa, 'dead') | str_detect(VXa, 'count'))) %>% 
      filter((age_from == 0 & age_thru == 99)) %>% 
      select(iso = country, year, TB, value) %>% 
      group_by(iso, year) %>% 
      summarise(val = sum(value) * 1e3, .groups = "drop") %>% 
      ungroup() %>% 
      mutate(var = paste0("n_screen_prison_y", year - 2025)) %>% 
      mutate(intv = int) %>% 
      mutate(UIDv = UID) %>% 
      select(iso, intv, UIDv, var, val)
  } 
    
  # 14a. Duration of standard treatment for DS-TB
  # Format: t_treat_DS_standard
  # Durations pre-defined
  
  # 14b. Duration of standard treatment for DR-TB
  # Format: t_treat_DR_standard
  # Durations pre-defined
  
  # 14c. Duration of shortened treatment for DS-TB
  # Format: t_treat_DS_short
  # Durations pre-defined
  
  # 14d. Duration of shortened treatment for DR-TB
  # Format: t_treat_DR_short
  # Durations pre-defined
  
  # 15a. Number of people screened with CXR in community screening
  # Format: n_screen_y[n]
  if (int == "SCR") {
    n_screen_y <- stocks %>% 
      mutate(year = ceiling(year)) %>%
      filter(year >= 2026) %>%
      filter(TB == "iTest_count") %>% 
      filter(!(str_detect(RISK, 'dead') | str_detect(RISK, 'count'))) %>% 
      filter(!(str_detect(VXa, 'dead') | str_detect(VXa, 'count'))) %>% 
      filter((age_from == 0 & age_thru == 99)) %>% 
      select(iso = country, year, TB, value) %>% 
      group_by(iso, year) %>% 
      summarise(val = sum(value) * 1e3, .groups = "drop") %>% 
      ungroup() %>% 
      mutate(var = paste0("n_screen_y", year - 2025)) %>% 
      mutate(intv = int) %>%
      mutate(UIDv = UID) %>% 
      select(iso, intv, UIDv, var, val)
  }
  
  # 15b. Number showing signs of TB on CXR in community screening
  # Format: n_screen_CXRpositive_y[n]
  if (int == "SCR") {
    n_screen_CXRpositive_y <- stocks %>% 
      mutate(year = ceiling(year)) %>%
      filter(year >= 2026) %>%
      filter(TB == "n_screen_CXRpositive_count") %>% 
      filter(!(str_detect(RISK, 'dead') | str_detect(RISK, 'count'))) %>% 
      filter(!(str_detect(VXa, 'dead') | str_detect(VXa, 'count'))) %>% 
      filter((age_from == 0 & age_thru == 99)) %>% 
      select(iso = country, year, TB, value) %>% 
      group_by(iso, year) %>% 
      summarise(val = sum(value) * 1e3, .groups = "drop") %>% 
      ungroup() %>% 
      mutate(var = paste0("n_screen_CXRpositive_y", year - 2025)) %>% 
      mutate(intv = int) %>%
      mutate(UIDv = UID) %>% 
      select(iso, intv, UIDv, var, val)
  }

  # 16. Number of people vaccinated
  # Format: n_vaccinated_y[n]
  if (int == "VAX"){
    n_vaccinated_y <- stocks %>%
      mutate(year = ceiling(year)) %>%
      filter(year >= 2026) %>%
      filter(!(str_detect(TB, 'count') | str_detect(TB, 'dead'))) %>%
      filter(!(str_detect(RISK, 'dead') | str_detect(RISK, 'count'))) %>%
      filter(VXa == "vaxcount") %>%
      filter((age_from == 0 & age_thru == 99)) %>% 
      select(iso = country, year, TB, value) %>% 
      group_by(iso, year) %>% 
      summarise(val = sum(value) * 1e3, .groups = "drop") %>% 
      ungroup() %>% 
      mutate(var = paste0("n_vaccinated_y", year - 2025)) %>% 
      mutate(intv = int) %>% 
      mutate(UIDv = UID) %>% 
      select(iso, intv, UIDv, var, val)
  }
  
  # Drop stocks
  rm(stocks)

  # List data frames created
  dfs <- Filter(function(x) is.data.frame(get(x)), ls())

  # Save RData file
  save(list = dfs, file = here("outputs", "cea", paste0("CEA_", int, "_", iso, "_", UID, ".RData")))

  # Remove everything except the file list
  rm(list = setdiff(ls(), "files"))
}

rm(list = ls())
