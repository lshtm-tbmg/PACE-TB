##### NIH2 - Epidemiological impact ###########
# R script to plot intermediate output of interventions
###############################################
# Packages
suppressPackageStartupMessages({
  library(here)
  library(rio)
  library(data.table)
  library(tidyverse)
  library(extrafont)
  library(ggplot2)
  library(cowplot)
  library(patchwork)
  library(dplyr)
  library(stringr)
  library(reshape2)
  library(tidyverse)
  library(here)
  library(png)
  library(fst)
})

#################################################################################
################################ CREATE DATASETS ################################
# Checks outputs in folder
files <- list.files(here("countries", "Calibration_BAU_outputs"), full.names = FALSE)
prev <- data.table()
inc <- data.table()
Ds <- data.table()
mort <- data.table()
not <- data.table()
mdr <- data.table()
inc_risk <- data.table()
mort_risk <- data.table()
not_risk <- data.table()
pris_admis <- data.table()
pris_prev <- data.table()
HIV_prev <- data.table()
ART_cov <- data.table()
HIV_mort <- data.table()
pop <- data.table()
pop_risk <- data.table()
prev_PAF <- data.table()
inc_PAF <- data.table()

for (i in files) {
  file <- basename(i)
  
  # Define a regex pattern: ISO (3 letters) + Unique ID (any length) + Intervention (3 letters)
  pattern <- "^(\\w{3})_(\\w+)_([A-Z]{3})\\.fst$"
  
  # Apply the regex to extract components
  matches <- regmatches(file, regexec(pattern, file))[[1]]
  
  # Assign extracted values to separate objects
  iso <- matches[2]          # Extract ISO
  if (!iso %in% c("BRA", "IND", "ZAF")) {
    stop(paste("Error: Invalid ISO code in file name:", file))
  }
  
  unique_id <- matches[3]    # Extract Unique ID
  
  int <- matches[4] # Extract Intervention
  if (!int %in% c("BAU")) {
    stop(paste("Error: Invalid intervention code in file name:", file))
  }
  
  print(paste("Intervention:", int, "| ISO:", iso, "| ID:", unique_id))
  
  # Importing output
  output <- read_fst(paste("countries/Calibration_BAU_outputs/",file, sep = ""))
  
  output <- output[grepl("\\.999$", output$year), ]
  output_trial <- setDT(output)
  TB_trends_99 <- output_trial[, age_group := fcase(
    age_from == 0 & age_thru == 4, "[0,4]",
    age_from == 5 & age_thru == 9, "[5,9]",
    age_from == 10 & age_thru == 14, "[10,14]",
    age_from == 15 & age_thru == 19, "[15,19]",
    age_from == 20 & age_thru == 24, "[20,24]",
    age_from == 25 & age_thru == 29, "[25,29]",
    age_from == 30 & age_thru == 34, "[30,34]",
    age_from == 35 & age_thru == 39, "[35,39]",
    age_from == 40 & age_thru == 44, "[40,44]",
    age_from == 45 & age_thru == 49, "[45,49]",
    age_from == 50 & age_thru == 54, "[50,54]",
    age_from == 55 & age_thru == 59, "[55,59]",
    age_from == 60 & age_thru == 64, "[60,64]",
    age_from == 65 & age_thru == 69, "[65,69]",
    age_from == 70 & age_thru == 74, "[70,74]",
    age_from == 75 & age_thru == 99, "[75,99]",
    age_from == 0 & age_thru == 99, "[0,99]")]
  
  TB_trends_99 <- setDT(TB_trends_99)
  
  usual_TB_trends <- TB_trends_99 %>%
    mutate(age_group = case_when(
      age_group %in% c("[15,19]","[20,24]","[25,29]", "[30,34]",
                       "[35,39]", "[40,44]", "[45,49]","[50,54]","[55,59]",
                       "[60,64]", "[65,69]","[70,74]", "[75,99]") ~ "[15,99]",
      TRUE ~ age_group
    )) %>%
    mutate(age_group = case_when(
      age_group %in% c("[0,4]","[5,9]","[10,14]") ~ "[0,14]",
      TRUE ~ age_group
    )) %>%
    group_by(country, year, RISK, VXa, TB, age_group) %>%
    summarise(value = sum(value), .groups = "drop") 
  
  usual_TB_trends <- setDT(usual_TB_trends)
  
  # Dataset 1: % Prevalence
  temp_prev <- usual_TB_trends %>% 
    mutate(year = floor(year)) %>% 
    filter(year <= 2025) %>% 
    filter(str_detect(TB, 'Ds') | str_detect(TB, 'Dc')) %>% 
    filter(!(str_detect(TB, 'ST') | str_detect(TB, 'RT'))) %>% # Remove if need to include ST and RT
    filter(!(str_detect(TB, 'dead') | str_detect(TB, 'count'))) %>% 
    filter(!(str_detect(RISK, 'dead') | str_detect(RISK, 'count'))) %>% 
    select(iso = country, year, age_group, TB, value) %>% 
    group_by(iso, year, age_group) %>% 
    summarise(val = sum(value) * 1e3, .groups = "drop") %>% 
    ungroup() %>% 
    mutate(var = "prev") %>% 
    mutate(uid = unique_id) %>% 
    select(iso, uid, year, age_group, var, val)
  
  prev <- rbind(prev, temp_prev, fill = TRUE)
  rm(temp_prev)
  
  # Dataset 2: % Ds
  temp_Ds <- usual_TB_trends %>% 
    mutate(year = floor(year)) %>% 
    filter(year <= 2025) %>% 
    filter(str_detect(TB, 'Ds')) %>% 
    filter(!(str_detect(TB, 'ST') | str_detect(TB, 'RT'))) %>% # Remove if need to include ST and RT
    filter(!(str_detect(TB, 'dead') | str_detect(TB, 'count'))) %>% 
    filter(!(str_detect(RISK, 'dead') | str_detect(RISK, 'count'))) %>% 
    select(iso = country, year, age_group, TB, value) %>% 
    group_by(iso, year, age_group) %>% 
    summarise(Ds_val = sum(value) * 1e3, .groups = "drop") %>% 
    ungroup() %>% 
    mutate(var = "Ds") %>% 
    mutate(uid = unique_id) %>% 
    select(iso, uid, year, age_group, var, Ds_val)
  
  Ds <- rbind(Ds, temp_Ds, fill = TRUE)
  rm(temp_Ds)
  
  # Dataset 3: Incidence 
  temp_inc <- usual_TB_trends %>% 
    mutate(year = floor(year)) %>% 
    filter(year <= 2025) %>% 
    filter(str_detect(TB, 'Inc_Dc_count')) %>% 
    filter(!(str_detect(VXa, 'dead') | str_detect(VXa, 'count'))) %>% 
    filter(!(str_detect(RISK, 'dead') | str_detect(RISK, 'count'))) %>% 
    select(iso = country, age_group, year, TB, value) %>% 
    group_by(iso, year, age_group) %>% 
    summarise(val = sum(value) * 1e3, .groups = "drop") %>% 
    ungroup() %>% 
    mutate(var = "inc") %>% 
    mutate(uid = unique_id) %>% 
    select(iso, uid, age_group, year, var, val)
  
  inc <- rbind(inc, temp_inc, fill = TRUE)
  rm(temp_inc)
  
  # Dataset 4: TB Mortality
  temp_mort <- usual_TB_trends %>% 
    mutate(year = floor(year)) %>% 
    filter(year <= 2025) %>% 
    filter(str_detect(TB, 'dead')) %>% 
    filter(!(str_detect(TB, 'count'))) %>% 
    filter(!(str_detect(RISK, 'dead') | str_detect(RISK, 'count'))) %>% 
    select(iso = country, year, age_group, TB, value) %>% 
    group_by(iso, year, age_group) %>% 
    summarise(val = sum(value) * 1e3, .groups = "drop") %>% 
    ungroup() %>% 
    mutate(var = "mort") %>% 
    mutate(uid = unique_id) %>% 
    select(iso, uid, year, age_group, var, val)
  
  mort <- rbind(mort, temp_mort, fill = TRUE)
  rm(temp_mort)
  
  # Dataset 5: TB Notifications
  temp_not <- usual_TB_trends %>% 
    mutate(year = floor(year)) %>% 
    filter(year <= 2025) %>% 
    filter(str_detect(TB, 'Not')) %>% 
    filter(!(str_detect(TB, 'ST') | str_detect(TB, 'RT'))) %>%
    filter(!(str_detect(TB, 'dead'))) %>% 
    filter(!(str_detect(RISK, 'dead') | str_detect(RISK, 'count'))) %>% 
    select(iso = country, year, age_group, TB, value) %>% 
    group_by(iso, year, age_group) %>% 
    summarise(val = sum(value) * 1e3, .groups = "drop") %>% 
    ungroup() %>% 
    mutate(var = "not") %>% 
    mutate(uid = unique_id) %>% 
    select(iso, uid, year, age_group, var, val)
  
  not <- rbind(not, temp_not, fill = TRUE)
  rm(temp_not)
  
  # Dataset 6: MDR
  temp_mdr <- usual_TB_trends %>% 
    mutate(year = floor(year)) %>% 
    filter(year <= 2025) %>% 
    filter(str_detect(TB, 'MDR') ) %>% 
    filter(!(str_detect(TB, 'ST') | str_detect(TB, 'RT'))) %>% # Remove if need to include ST and RT
    filter(!(str_detect(TB, 'dead'))) %>% 
    filter(!(str_detect(RISK, 'dead') | str_detect(RISK, 'count'))) %>% 
    select(iso = country, year, age_group, TB, value) %>% 
    group_by(iso, year, age_group) %>% 
    summarise(mdr_val = sum(value) * 1e3, .groups = "drop") %>% 
    ungroup() %>% 
    mutate(var = "mdr") %>% 
    mutate(uid = unique_id) %>% 
    select(iso, uid, year, age_group, var, mdr_val)
  
  mdr <- rbind(mdr, temp_mdr, fill = TRUE)
  rm(temp_mdr)
  
  if (iso=="BRA") {
    # Dataset 7: Incidence by RISK
    temp_inc_risk <- usual_TB_trends %>% 
      mutate(year = floor(year)) %>% 
      filter(year <= 2025) %>% 
      filter(str_detect(TB, 'Inc_Dc_count')) %>% 
      filter(str_detect(RISK, 'prison')) %>% 
      filter(!(str_detect(RISK, 'dead') | str_detect(RISK, 'count'))) %>%
      select(iso = country, year, age_group, TB, value) %>% 
      group_by(iso, year, age_group) %>% 
      summarise(val = sum(value) * 1e3, .groups = "drop") %>% 
      ungroup() %>% 
      mutate(var = "inc_risk") %>% 
      mutate(uid = unique_id) %>% 
      select(iso, uid, age_group, year, var, val)
    
    inc_risk <- rbind(inc_risk, temp_inc_risk, fill = TRUE)
    rm(temp_inc_risk)
    
    # Dataset 8: Notifications by RISK
    temp_not_risk <- usual_TB_trends %>% 
      mutate(year = floor(year)) %>% 
      filter(year <= 2025) %>% 
      filter(str_detect(TB, 'Not')) %>% 
      filter(str_detect(RISK, 'prison')) %>% 
      filter(!(str_detect(TB, 'ST') | str_detect(TB, 'RT'))) %>%
      filter(!(str_detect(TB, 'dead'))) %>% 
      filter(!(str_detect(RISK, 'dead') | str_detect(RISK, 'count'))) %>% 
      select(iso = country, year, age_group, TB, value) %>% 
      group_by(iso, year, age_group) %>% 
      summarise(val = sum(value) * 1e3, .groups = "drop") %>% 
      ungroup() %>% 
      mutate(var = "not_risk") %>% 
      mutate(uid = unique_id) %>% 
      select(iso, uid, year, age_group, var, val)
    
    not_risk <- rbind(not_risk, temp_not_risk, fill = TRUE)
    rm(temp_not_risk)
    
    # Dataset 9: Admission rate
    temp_pris_admis <- usual_TB_trends %>% 
      mutate(year = floor(year)) %>% 
      filter(year <= 2025) %>% 
      filter(str_detect(RISK, 'priscount')) %>% 
      filter(!(str_detect(TB, 'dead') | str_detect(TB, 'count'))) %>% 
      filter(!(str_detect(RISK, 'dead'))) %>% 
      select(iso = country, year, age_group, TB, value) %>% 
      group_by(iso, year, age_group) %>% 
      summarise(val = sum(value) * 1e3, .groups = "drop") %>% 
      ungroup() %>% 
      mutate(var = "pris_admis") %>% 
      mutate(uid = unique_id) %>% 
      select(iso, uid, year, age_group, var, val)
   
    pris_admis <- rbind(pris_admis, temp_pris_admis, fill = TRUE)
    rm(temp_pris_admis)
    
    # Dataset 10: Incarceration prevalence
    temp_pris_prev <- usual_TB_trends %>% 
      mutate(year = floor(year)) %>% 
      filter(year <= 2025) %>% 
      filter(str_detect(RISK, 'prison')) %>% 
      filter(!(str_detect(TB, 'dead') | str_detect(TB, 'count'))) %>% 
      filter(!(str_detect(RISK, 'dead') | str_detect(RISK, 'count'))) %>% 
      select(iso = country, year, age_group, TB, value) %>% 
      group_by(iso, year, age_group) %>% 
      summarise(val = sum(value) * 1e3, .groups = "drop") %>% 
      ungroup() %>% 
      mutate(var = "pris_prev") %>% 
      mutate(uid = unique_id) %>% 
      select(iso, uid, year, age_group, var, val)
    
    pris_prev <- rbind(pris_prev, temp_pris_prev, fill = TRUE)
    rm(temp_pris_prev)
    
  } else if (iso=="ZAF"){
    # Dataset 7: Incidence by RISK
    temp_inc_risk <- usual_TB_trends %>% 
      mutate(year = floor(year)) %>% 
      filter(year <= 2025) %>% 
      filter(str_detect(TB, 'Inc_Dc_count')) %>% 
      filter(str_detect(RISK, 'HIVnt') | str_detect(RISK, 'HIVart')) %>% 
      filter(!(str_detect(RISK, 'dead') | str_detect(RISK, 'count'))) %>%
      select(iso = country, year, age_group, TB, value) %>% 
      group_by(iso, year, age_group) %>% 
      summarise(val = sum(value) * 1e3, .groups = "drop") %>% 
      ungroup() %>% 
      mutate(var = "inc_risk") %>% 
      mutate(uid = unique_id) %>% 
      select(iso, uid, age_group, year, var, val)
    
    inc_risk <- rbind(inc_risk, temp_inc_risk, fill = TRUE)
    rm(temp_inc_risk)
    
    # Dataset 8: Notifications by RISK
    temp_not_risk <- usual_TB_trends %>% 
      mutate(year = floor(year)) %>% 
      filter(year <= 2025) %>% 
      filter(str_detect(TB, 'Not')) %>% 
      filter(str_detect(RISK, 'HIVnt') | str_detect(RISK, 'HIVart')) %>% 
      filter(!(str_detect(TB, 'ST') | str_detect(TB, 'RT'))) %>%
      filter(!(str_detect(TB, 'dead'))) %>% 
      filter(!(str_detect(RISK, 'dead') | str_detect(RISK, 'count'))) %>% 
      select(iso = country, year, age_group, TB, value) %>% 
      group_by(iso, year, age_group) %>% 
      summarise(val = sum(value) * 1e3, .groups = "drop") %>% 
      ungroup() %>% 
      mutate(var = "not_risk") %>% 
      mutate(uid = unique_id) %>% 
      select(iso, uid, year, age_group, var, val)
    
    not_risk <- rbind(not_risk, temp_not_risk, fill = TRUE)
    rm(temp_not_risk)
    
    # Dataset 9: TB Mortality by RISK
    temp_mort_risk <- usual_TB_trends %>% 
      mutate(year = floor(year)) %>% 
      filter(year <= 2025) %>% 
      filter(str_detect(TB, 'dead')) %>% 
      filter(str_detect(RISK, 'HIVnt') | str_detect(RISK, 'HIVart')) %>% 
      filter(!(str_detect(TB, 'count'))) %>% 
      filter(!(str_detect(RISK, 'dead') | str_detect(RISK, 'count'))) %>% 
      select(iso = country, year, age_group, TB, value) %>% 
      group_by(iso, year, age_group) %>% 
      summarise(val = sum(value) * 1e3, .groups = "drop") %>% 
      ungroup() %>% 
      mutate(var = "mort_risk") %>% 
      mutate(uid = unique_id) %>% 
      select(iso, uid, year, age_group, var, val)
    
    mort_risk <- rbind(mort_risk, temp_mort_risk, fill = TRUE)
    rm(temp_mort_risk)
    
    # Dataset 10: HIV prevalence
    temp_HIV_prev <- usual_TB_trends %>% 
      mutate(year = floor(year)) %>% 
      filter(year <= 2025) %>% 
      filter(str_detect(RISK, 'HIVnt') | str_detect(RISK, 'HIVart')) %>% 
      filter(!(str_detect(TB, 'dead') | str_detect(TB, 'count'))) %>% 
      filter(!(str_detect(RISK, 'dead') | str_detect(RISK, 'count'))) %>% 
      select(iso = country, year, age_group, TB, value) %>% 
      group_by(iso, year, age_group) %>% 
      summarise(val = sum(value) * 1e3, .groups = "drop") %>% 
      ungroup() %>% 
      mutate(var = "HIVprev") %>% 
      mutate(uid = unique_id) %>% 
      select(iso, uid, year, age_group, var, val)
    
    HIV_prev <- rbind(HIV_prev, temp_HIV_prev, fill = TRUE)
    rm(temp_HIV_prev)
    
    # Dataset 11: ART coverage
    temp_ART_cov <- usual_TB_trends %>% 
      mutate(year = floor(year)) %>% 
      filter(year <= 2025) %>% 
      filter(str_detect(RISK, 'HIVart')) %>% 
      filter(!(str_detect(TB, 'dead') | str_detect(TB, 'count'))) %>% 
      filter(!(str_detect(RISK, 'dead') | str_detect(RISK, 'count'))) %>% 
      select(iso = country, year, age_group, TB, value) %>% 
      group_by(iso, year, age_group) %>% 
      summarise(val = sum(value) * 1e3, .groups = "drop") %>% 
      ungroup() %>% 
      mutate(var = "ARTcov") %>% 
      mutate(uid = unique_id) %>% 
      select(iso, uid, year, age_group, var, val)
    
    ART_cov <- rbind(ART_cov, temp_ART_cov, fill = TRUE)
    rm(temp_ART_cov)
    
    # Dataset 12: TB Mortality by RISK
    temp_HIV_mort <- usual_TB_trends %>% 
      mutate(year = floor(year)) %>% 
      filter(year <= 2025) %>% 
      filter(str_detect(RISK, 'dead')) %>% 
      filter(!(str_detect(TB, 'count') | str_detect(TB, 'dead'))) %>% 
      filter(!(str_detect(RISK, 'count'))) %>% 
      select(iso = country, year, age_group, TB, value) %>% 
      group_by(iso, year, age_group) %>% 
      summarise(val = sum(value) * 1e3, .groups = "drop") %>% 
      ungroup() %>% 
      mutate(var = "HIVmort") %>% 
      mutate(uid = unique_id) %>% 
      select(iso, uid, year, age_group, var, val)
    
    HIV_mort <- rbind(HIV_mort, temp_HIV_mort, fill = TRUE)
    rm(temp_HIV_mort)
    
  } else if (iso == "IND") {
    # Dataset X: % Prevalence by RISK
    temp_prev_PAF <- usual_TB_trends %>% 
      mutate(year = floor(year)) %>% 
      filter(year <= 2025) %>% 
      filter(str_detect(TB, 'Ds') | str_detect(TB, 'Dc')) %>% 
      filter(!(str_detect(TB, 'ST') | str_detect(TB, 'RT'))) %>% # Remove if need to include ST and RT
      filter(!(str_detect(TB, 'dead') | str_detect(TB, 'count'))) %>% 
      filter(!(str_detect(RISK, 'dead') | str_detect(RISK, 'count'))) %>% 
      select(iso = country, year, age_group, RISK, TB, value) %>% 
      group_by(iso, year, age_group, RISK) %>% 
      summarise(val = sum(value) * 1e3, .groups = "drop") %>% 
      ungroup() %>% 
      mutate(var = "prev") %>% 
      mutate(uid = unique_id) %>% 
      select(iso, uid, year, age_group, RISK, var, val)
    
    prev_PAF <- rbind(prev_PAF, temp_prev_PAF, fill = TRUE)
    rm(temp_prev_PAF)
    
    # Dataset Y: Incidence by RISK
    temp_inc_PAF <- usual_TB_trends %>% 
      mutate(year = floor(year)) %>% 
      filter(year <= 2025) %>% 
      filter(str_detect(TB, 'Inc_Dc_count')) %>% 
      filter(!(str_detect(VXa, 'dead') | str_detect(VXa, 'count'))) %>% 
      filter(!(str_detect(RISK, 'dead') | str_detect(RISK, 'count'))) %>% 
      select(iso = country, age_group, year, RISK, TB, value) %>% 
      group_by(iso, year, age_group, RISK) %>% 
      summarise(val = sum(value) * 1e3, .groups = "drop") %>% 
      ungroup() %>% 
      mutate(var = "inc") %>% 
      mutate(uid = unique_id) %>% 
      select(iso, uid, age_group, year, RISK, var, val)
    
    inc_PAF <- rbind(inc_PAF, temp_inc_PAF, fill = TRUE)
    rm(temp_inc_PAF)
    
  }
  
  # Dataset 10: Population
  temp_pop <- usual_TB_trends %>% 
    mutate(year = floor(year)) %>% 
    filter(year <= 2025) %>% 
    filter(!(str_detect(TB, 'dead') | str_detect(TB, 'count'))) %>% 
    filter(!(str_detect(VXa, 'dead') | str_detect(VXa, 'count'))) %>% 
    filter(!(str_detect(RISK, 'dead') | str_detect(RISK, 'count'))) %>% 
    select(iso = country, year, age_group, TB, value) %>% 
    group_by(iso, year, age_group) %>% 
    summarise(pop = sum(value) * 1e3, .groups = "drop") %>% 
    ungroup() %>% 
    mutate(var = "pop") %>% 
    mutate(uid = unique_id) %>% 
    select(iso, uid, year, age_group, var, pop)
  
  pop <- rbind(pop, temp_pop, fill = TRUE)
  rm(temp_pop)
  
  if (iso %in% c("BRA", "IND")) {
    # Dataset 11: Population by RISK
    temp_pop_risk <- usual_TB_trends %>% 
      mutate(year = floor(year)) %>% 
      filter(year <= 2025) %>% 
      filter(!(str_detect(TB, 'dead') | str_detect(TB, 'count'))) %>% 
      filter(!(str_detect(VXa, 'dead') | str_detect(VXa, 'count'))) %>% 
      filter(!(str_detect(RISK, 'dead') | str_detect(RISK, 'count'))) %>% 
      select(iso = country, year, age_group, RISK, TB, value) %>% 
      group_by(iso, year, RISK, age_group) %>% 
      summarise(pop = sum(value) * 1e3, .groups = "drop") %>% 
      ungroup() %>% 
      mutate(var = "pop_risk") %>% 
      mutate(uid = unique_id) %>% 
      select(iso, uid, year, age_group,  RISK, var, pop)
    
    pop_risk <- rbind(pop_risk, temp_pop_risk, fill = TRUE)
    rm(temp_pop_risk)
  }
  
  
}

rm(list = setdiff(ls(), c("inc", "prev", "mort", "not", "mdr", "Ds", "inc_risk", 
                          "mort_risk", "not_risk", "pop", "pop_risk", "pris_admis",
                          "pris_prev", "HIV_prev", "HIV_mort", "ART_cov", "inc_PAF", "prev_PAF")))

save(inc, prev, mort, not, mdr, Ds, inc_risk, 
     mort_risk, not_risk, pop, pop_risk, pris_admis,
     pris_prev, HIV_prev, HIV_mort, ART_cov, inc_PAF, prev_PAF,
     file = "countries/calibration_Rdata/cali_dataset.RData")

load("countries/calibration_Rdata/cali_dataset.RData")

##########################################################################################

# 1. TB Prevalence ----------------------------------------------------------------
prev_trial <- setDT(prev)
pop_trial <- setDT(pop)
tb_prev_pop <- left_join(prev_trial, pop_trial, by = c("uid","iso", "year", "age_group"))
tb_prev_pop <- tb_prev_pop[, total_value := ((val/pop))*100000]

prevs_median <- tb_prev_pop %>%
  group_by(iso, age_group, year) %>%
  summarise(median = median(total_value),         #Calculate median
            ci_lo = quantile(total_value, 0), #Calculate 2.5 percentile as CI low
            ci_hi = quantile(total_value, 1)) #Calculate 97.5 percentile as CI high


# 2. % subclinical of infectious TB ----------------------------------------------------------------
Ds_trial <- setDT(Ds)
Ds_pct <- left_join(prev_trial, Ds_trial, by = c("uid","iso", "year", "age_group"))
Ds_pct <- Ds_pct[, total_value := ((Ds_val/val))*100]

Ds_pct_median <- Ds_pct %>%
  group_by(iso, age_group, year) %>%
  summarise(median = median(total_value),         #Calculate median
            ci_lo = quantile(total_value, 0), #Calculate 2.5 percentile as CI low
            ci_hi = quantile(total_value, 1)) #Calculate 97.5 percentile as CI high

# 3. TB incidence ----------------------------------------------------------------
Dc_inc_adjust <- 0.86 #Double counting
inc$adjusted_val <- inc$val*Dc_inc_adjust
inc_risk$adjusted_val <- inc_risk$val*Dc_inc_adjust

inc_trial <- setDT(inc)
inc_risk_trial <- setDT(inc_risk)

tb_inc_pop <- left_join(inc_trial, pop_trial, by = c("uid","iso", "year", "age_group"))
tb_inc_pop <- tb_inc_pop[, total_value := ((adjusted_val/pop))*100000]

tb_inc_pop_median <- tb_inc_pop %>%
  group_by(iso, age_group, year) %>%
  summarise(median = median(total_value),         #Calculate median
            ci_lo = quantile(total_value, 0), #Calculate 2.5 percentile as CI low
            ci_hi = quantile(total_value, 1)) #Calculate 97.5 percentile as CI high


  # 3a) BRA
  inc_risk_BRA <- subset(inc_risk_trial, iso =="BRA")
  pop_BRA <- subset(pop_risk, iso == "BRA")
  pop_prison <- subset(pop_BRA, RISK == "prison")
  pop_prison_trial <- setDT(pop_prison)
  tb_inc_BRA <- left_join(inc_risk_BRA, pop_prison_trial, by = c("uid","iso", "year", "age_group"))
  tb_inc_BRA <- tb_inc_BRA[, total_value := ((adjusted_val/pop))*100000]
  tb_inc_BRA <- subset(tb_inc_BRA, age_group != "[0,14]")
    
  tb_inc_BRA_median <- tb_inc_BRA %>%
    group_by(iso, age_group, year) %>%
    summarise(median = median(total_value),         #Calculate median
              ci_lo = quantile(total_value, 0), #Calculate 2.5 percentile as CI low
              ci_hi = quantile(total_value, 1)) #Calculate 97.5 percentile as CI high
  
  # 3b) ZAF
  inc_risk_ZAF <- subset(inc_risk_trial, iso =="ZAF")
  tb_inc_ZAF <- left_join(inc_risk_ZAF, pop_trial, by = c("uid","iso", "year", "age_group"))
  tb_inc_ZAF <- tb_inc_ZAF[, total_value := ((adjusted_val/pop))*100000]
  
  tb_inc_ZAF_median <- tb_inc_ZAF %>%
    group_by(iso, age_group, year) %>%
    summarise(median = median(total_value),         #Calculate median
              ci_lo = quantile(total_value, 0), #Calculate 2.5 percentile as CI low
              ci_hi = quantile(total_value, 1)) #Calculate 97.5 percentile as CI high
  
# 4. TB mortality
mort_trial <- setDT(mort)
tb_mort_pop <- left_join(mort_trial, pop_trial, by = c("uid","iso", "year", "age_group"))
tb_mort_pop <- tb_mort_pop[, total_value := (val/pop)*100000]

tb_mort_pop_median <- tb_mort_pop %>%
  group_by(iso, age_group, year) %>%
  summarise(median = median(total_value),         #Calculate median
            ci_lo = quantile(total_value, 0), #Calculate 2.5 percentile as CI low
            ci_hi = quantile(total_value, 1)) #Calculate 97.5 percentile as CI high
  # 4b) ZAF
  mort_risk_trial <- setDT(mort_risk)
  tb_mort_ZAF <- merge(mort_risk, pop_trial, by = c("uid","iso", "year", "age_group"))
  tb_mort_ZAF <- tb_mort_ZAF[, total_value := ((val/pop))*100000]
  
  tb_mort_ZAF_median <- tb_mort_ZAF %>%
    group_by(iso, age_group, year) %>%
    summarise(median = median(total_value),         #Calculate median
              ci_lo = quantile(total_value, 0), #Calculate 2.5 percentile as CI low
              ci_hi = quantile(total_value, 1)) #Calculate 97.5 percentile as CI high

# 5. TB notifications
not_trial <- setDT(not)
not_risk_trial <- setDT(not_risk)
tb_not_pop <- left_join(not_trial, pop_trial, by = c("uid","iso", "year", "age_group"))
tb_not_pop <- tb_not_pop[, total_value := ((val/pop))*100000]

tb_not_pop_median <- tb_not_pop %>%
  group_by(iso, age_group, year) %>%
  summarise(median = median(total_value),         #Calculate median
            ci_lo = quantile(total_value, 0), #Calculate 2.5 percentile as CI low
            ci_hi = quantile(total_value, 1)) #Calculate 97.5 percentile as CI high
  
  # 5a) BRA
  not_risk_BRA <- subset(not_risk_trial, iso =="BRA")
  tb_not_BRA <- left_join(not_risk_BRA, pop_prison_trial, by = c("uid","iso", "year", "age_group"))
  tb_not_BRA <- tb_not_BRA[, total_value := ((val/pop))*100000]
  tb_not_BRA <- subset(tb_not_BRA, age_group != "[0,14]")
  
  tb_not_BRA_median <- tb_not_BRA %>%
    group_by(iso, age_group, year) %>%
    summarise(median = median(total_value),         #Calculate median
              ci_lo = quantile(total_value, 0), #Calculate 2.5 percentile as CI low
              ci_hi = quantile(total_value, 1)) #Calculate 97.5 percentile as CI high
  
  
  # 5b) ZAF
  not_risk_ZAF <- subset(not_risk_trial, iso =="ZAF")
  tb_not_ZAF <- left_join(not_risk_ZAF, pop_trial, by = c("uid","iso", "year", "age_group"))
  tb_not_ZAF <- tb_not_ZAF[, total_value := ((val/pop))*100000]
  
  tb_not_ZAF_median <- tb_not_ZAF %>%
    group_by(iso, age_group, year) %>%
    summarise(median = median(total_value),         #Calculate median
              ci_lo = quantile(total_value, 0), #Calculate 2.5 percentile as CI low
              ci_hi = quantile(total_value, 1)) #Calculate 97.5 percentile as CI high
  

# 6. MDR among notifications
mdr_trial <- setDT(mdr)
tb_mdr_pop <- left_join(mdr_trial, not_trial, by = c("uid","iso", "year", "age_group"))
tb_mdr_pop <- tb_mdr_pop[, total_value := ((mdr_val/val))*100]

tb_mdr_pop_median <- tb_mdr_pop %>%
  group_by(iso, age_group, year) %>%
  summarise(median = median(total_value),         #Calculate median
            ci_lo = quantile(total_value, 0), #Calculate 2.5 percentile as CI low
            ci_hi = quantile(total_value, 1)) #Calculate 97.5 percentile as CI high

# 7. BRA's prison targets

  # 7a) Prison's admission rate
  pris_admis_trial <- setDT(pris_admis)
  pris_admis_pop <- left_join(pris_admis_trial, pop_trial, by = c("uid","iso", "year", "age_group"))
  pris_admis_pop <- pris_admis_pop[, total_value := ((val/pop))*100000]

  pris_admis_pop_median <- pris_admis_pop %>%
    group_by(iso, age_group, year) %>%
    summarise(median = median(total_value),         #Calculate median
             ci_lo = quantile(total_value, 0), #Calculate 2.5 percentile as CI low
             ci_hi = quantile(total_value, 1)) #Calculate 97.5 percentile as CI high

  # 7b) Incarceration prevalence
  pris_prev_trial <- setDT(pris_prev)
  pris_prev_pop <- left_join(pris_prev_trial, pop_trial, by = c("uid","iso", "year", "age_group"))
  pris_prev_pop <- pris_prev_pop[, total_value := ((val/pop))*100000]

  pris_prev_pop_median <- pris_prev_pop %>%
    group_by(iso, age_group, year) %>%
    summarise(median = median(total_value),         #Calculate median
              ci_lo = quantile(total_value, 0), #Calculate 2.5 percentile as CI low
              ci_hi = quantile(total_value, 1)) #Calculate 97.5 percentile as CI high

# 8. ZAF's HIV targets
  
  # 8a) ART coverage 
  ART_cov_trial <- setDT(ART_cov)
  HIV_prev_trial <- setDT(HIV_prev)
  ART_cov_pop <- left_join(ART_cov_trial, HIV_prev_trial, by = c("uid","iso", "year", "age_group"))
  ART_cov_pop <- ART_cov_pop[, total_value := ((val.x/val.y))*100]
  
  ART_cov_pop_median <- ART_cov_pop %>%
    group_by(iso, age_group, year) %>%
    summarise(median = median(total_value),         #Calculate median
              ci_lo = quantile(total_value, 0), #Calculate 2.5 percentile as CI low
              ci_hi = quantile(total_value, 1)) #Calculate 97.5 percentile as CI high
  
  # 8b) HIV prevalence
  HIV_prev_pop <- left_join(HIV_prev_trial, pop_trial, by = c("uid","iso", "year", "age_group"))
  HIV_prev_pop <- HIV_prev_pop[, total_value := ((val/pop))*100]
  
  HIV_prev_pop_median <- HIV_prev_pop %>%
    group_by(iso, age_group, year) %>%
    summarise(median = median(total_value),         #Calculate median
              ci_lo = quantile(total_value, 0), #Calculate 2.5 percentile as CI low
              ci_hi = quantile(total_value, 1)) #Calculate 97.5 percentile as CI high
  
  # 8b) HIV mortality
  HIV_mort_trial <- setDT(HIV_mort)
  HIV_mort_pop <- left_join(HIV_mort_trial, pop_trial, by = c("uid","iso", "year", "age_group"))
  HIV_mort_pop <- HIV_mort_pop[, total_value := ((val/pop))*100000]
  
  HIV_mort_pop_median <- HIV_mort_pop %>%
    group_by(iso, age_group, year) %>%
    summarise(median = median(total_value),         #Calculate median
              ci_lo = quantile(total_value, 0), #Calculate 2.5 percentile as CI low
              ci_hi = quantile(total_value, 1)) #Calculate 97.5 percentile as CI high
  
  # IND specific plot
  inc_PAFall <- subset(inc_PAF, age_group == "[0,99]")
  inc_PAFall <- setDT(inc_PAFall)
  
  inc_thin <- inc_PAFall %>% 
    filter(str_detect(RISK, 'moderate') | str_detect(RISK, 'mild')) %>%  
    group_by(iso, uid, year) %>% 
    summarise(val = sum(val))
  
  pop_IND <- subset(pop_risk, iso == "IND")
  pop_IND <- subset(pop_IND, age_group == "[0,99]")
  pop_thin <- pop_IND %>% 
    filter(str_detect(RISK, 'moderate') | str_detect(RISK, 'mild')) %>% 
    group_by(iso, uid, year) %>% 
    summarise(pop_thin = sum(pop))
  pop_thin_trial <- setDT(pop_thin)
  
  inc_pop_thin <- left_join(inc_thin, pop_thin_trial, by = c("uid","iso", "year"))
  inc_pop_thin <- setDT(inc_pop_thin)
  inc_pop_thin <- inc_pop_thin[, inc_thin := ((val/pop_thin))]
  #-------------
  inc_NOthin <- inc_PAFall %>% 
    filter(str_detect(RISK, 'normal') | str_detect(RISK, 'over')) %>%  
    group_by(iso, uid, year) %>% 
    summarise(val = sum(val))
  
  pop_NOthin <- pop_IND %>% 
    filter(str_detect(RISK, 'normal') | str_detect(RISK, 'over')) %>% 
    group_by(iso, uid, year) %>% 
    summarise(pop_NOthin = sum(pop))
  pop_NOthin_trial <- setDT(pop_NOthin)
  
  inc_pop_NOthin <- left_join(inc_NOthin, pop_NOthin_trial, by = c("uid","iso", "year"))
  inc_pop_NOthin <- setDT(inc_pop_NOthin)
  inc_pop_NOthin <- inc_pop_NOthin[, inc_NOthin := ((val/pop_NOthin))]
  #-------------
  
  pop_all <- subset(pop_trial, age_group == "[0,99]")
  
  PAF_data <- inc_pop_thin %>% 
    left_join(inc_pop_NOthin, by=c("iso", "uid", "year")) %>% 
    left_join(pop_all, by=c("iso", "uid", "year")) #%>% 
    #left_join(pop_thin_trial, by=c("iso", "uid", "year"))
  PAF_data <- setDT(PAF_data)
  
  PAF_data <- PAF_data [, total_value := (((pop_thin/pop)*((inc_thin/inc_NOthin)-1))/((pop_thin/pop)*((inc_thin/inc_NOthin)-1)+1))]
  
  PAF_median2 <- PAF_data %>%
    group_by(iso, year) %>%
    summarise(median = median(total_value),         #Calculate median
              ci_lo = quantile(total_value, 0), #Calculate 2.5 percentile as CI low
              ci_hi = quantile(total_value, 1)) #Calculate 97.5 percentile as CI high
  
################################################################################
################################## PLOTS #######################################

plot_single <- function(dataset, specific_col, target_name, main_title, y_title){
  plot_graph <- ggplot(dataset) +
        geom_line(aes(x = year, y = median), linewidth = 0.4, na.rm=TRUE, colour = specific_col ) + #change for colour=specific_col if there is an error
        geom_ribbon(aes(x = year, ymin = ci_lo, ymax = ci_hi, fill = specific_col), alpha = 0.2) +
        ylim(c(0, NA)) +
        labs(title = main_title) + ylab(y_title) + xlab("Year") +
        scale_fill_manual(values = c(age = specific_col)) +
        geom_errorbar(data = subset(targets, name == target_name), aes(ymin = lo, ymax = hi, x= year), 
                      size = 0.4, width = 0.5, colour = specific_col) +
        geom_point(data = subset(targets, name == target_name), 
                   aes(y = value, x=year), size = 2, colour = specific_col) +
        scale_fill_manual(values = specific_col) +
        theme_bw() +
        theme(text = element_text(family = "sans"),
              axis.line = element_line(),
              axis.line.x.bottom = element_line(linewidth = 0.6),
              axis.line.y.left = element_line(linewidth = 0.6),
              axis.ticks = element_line(linewidth = 0.6),
              axis.title.x.bottom = element_text(size= 11, vjust = -2, margin = margin(b = 20)),
              axis.title.y.left = element_text(size= 11, vjust= 3, margin = margin(l = 15)),
              axis.text.x.bottom = element_text(size= 9, vjust = 0.3, hjust = 0, angle = 90)) +
          theme(legend.position = "none")
      return(plot_graph)
} 

plot_many <- function(dataset, target_name, main_title, y_title){
  plot_graph <- ggplot(dataset) +
    geom_line(aes(x = year, y = median, group = age_group, colour = age_group), linewidth = 0.4, na.rm=TRUE ) + #change for colour=specific_col if there is an error
    geom_ribbon(aes(x = year, ymin = ci_lo, ymax = ci_hi, group = age_group, fill = age_group), alpha = 0.2) +
    ylim(c(0, NA)) +
    geom_errorbar(data = subset(targets, name == paste(target_name,"_0_99", sep="")),
                  aes(ymin = lo, ymax = hi, x=year), size = 0.4, width = 0.5, colour = "#8DA0CB") +
    geom_point(data = subset(targets, name == paste(target_name,"_0_99", sep="")), 
               aes(y = value, x=year), size = 2, colour = "#8DA0CB") +
    geom_errorbar(data = subset(targets, name == paste(target_name,"_0_14", sep="")),
                  aes(ymin = lo, ymax = hi, x=year), size = 0.4, width = 0.5, colour = "#FC8D62") +
    geom_point(data = subset(targets, name == paste(target_name,"_0_14", sep="")), 
               aes(y = value, x=year), size = 2, colour = "#FC8D62") +
    geom_errorbar(data = subset(targets, name == paste(target_name,"_15_99", sep="")),
                  aes(ymin = lo, ymax = hi, x=year), size = 0.4, width = 0.5, colour = "#66C2A5") +
    geom_point(data = subset(targets, name == paste(target_name,"_15_99", sep="")), 
               aes(y = value, x=year), size = 2, colour = "#66C2A5") +
    labs(title = main_title) + ylab(y_title) + xlab("Year") +
    scale_color_manual(name = "Age", values = c( "#D95F02", "#7570B3" , "#1B9E77"))+
    scale_fill_manual(values = c( "#D95F02", "#7570B3" , "#1B9E77")) +
    #scale_y_continuous(labels = scales::label_number(scale = scale_value, suffix = suffix_name)) +
    #coord_cartesian(ylim = c(0, NA)) +
    theme_bw() +
    theme(text = element_text(family = "sans"),
          axis.line = element_line(),
          axis.line.x.bottom = element_line(linewidth = 0.6),
          axis.line.y.left = element_line(linewidth = 0.6),
          axis.ticks = element_line(linewidth = 0.6),
          axis.title.x.bottom = element_text(size= 11, vjust = -2, margin = margin(b = 20)),
          axis.title.y.left = element_text(size= 11, vjust= 3, margin = margin(l = 15)),
          axis.text.x.bottom = element_text(size= 9, vjust = 0.3, hjust = 0, angle = 90)) +
    theme(legend.text = element_text(size = 10),
          legend.title = element_text(size = 10))
  return(plot_graph)
} 

plot_HIV <- function(dataset, target_name, main_title, y_title){
  plot_graph <- ggplot(dataset) +
    geom_line(aes(x = year, y = median, group = age_group, colour = age_group), linewidth = 0.4, na.rm=TRUE ) + #change for colour=specific_col if there is an error
    geom_ribbon(aes(x = year, ymin = ci_lo, ymax = ci_hi, group = age_group, fill = age_group), alpha = 0.2) +
    ylim(c(0, NA)) +
    geom_errorbar(data = subset(targets, name == paste(target_name,"_0_14", sep="")),
                  aes(ymin = lo, ymax = hi, x=year), size = 0.4, width = 0.5, colour = "#FC8D62") +
    geom_point(data = subset(targets, name == paste(target_name,"_0_14", sep="")), 
               aes(y = value, x=year), size = 2, colour = "#FC8D62") +
    geom_errorbar(data = subset(targets, name == paste(target_name,"_15_99", sep="")),
                  aes(ymin = lo, ymax = hi, x=year), size = 0.4, width = 0.5, colour = "#66C2A5") +
    geom_point(data = subset(targets, name == paste(target_name,"_15_99", sep="")), 
               aes(y = value, x=year), size = 2, colour = "#66C2A5") +
    labs(title = main_title) + ylab(y_title) + xlab("Year") +
    scale_color_manual(name = "Age", values = c( "#D95F02" , "#1B9E77"))+
    scale_fill_manual(values = c( "#D95F02", "#1B9E77")) +
    #scale_y_continuous(labels = scales::label_number(scale = scale_value, suffix = suffix_name)) +
    #coord_cartesian(ylim = c(0, NA)) +
    theme_bw() +
    theme(text = element_text(family = "sans"),
          axis.line = element_line(),
          axis.line.x.bottom = element_line(linewidth = 0.6),
          axis.line.y.left = element_line(linewidth = 0.6),
          axis.ticks = element_line(linewidth = 0.6),
          axis.title.x.bottom = element_text(size= 11, vjust = -2, margin = margin(b = 20)),
          axis.title.y.left = element_text(size= 11, vjust= 3, margin = margin(l = 15)),
          axis.text.x.bottom = element_text(size= 9, vjust = 0.3, hjust = 0, angle = 90)) +
    theme(legend.text = element_text(size = 10),
          legend.title = element_text(size = 10))
  return(plot_graph)
} 

      
# BRA's plots -----------------------------------------------------------------
#Targets file
countries_isos <- c("BRA", "IND", "ZAF")

for (isos in countries_isos) {
  
  if (isos=="BRA"){
    targets <- read.csv("countries/BRA/parameters/BRA_target_plotCali.csv")
  } else if (isos == "IND") {
    targets <- read.csv("countries/IND/parameters/IND_targets_plotCali.csv")
  } else if (isos == "ZAF") {
    targets <- read.csv("countries/ZAF/parameters/ZAF_target_plotCali_PrevAll.csv")
  }
  
  # a) Prevalence
  prevs_trial_median1 <- subset(prevs_median, iso == isos)
  prevs_trial_median2 <- subset(prevs_trial_median1, age_group == "[0,99]")
  
  if (isos == "BRA") {
    plot_prev <- plot_single(prevs_trial_median2, "#8DA0CB", "", "TB prevalence \nin the population", "Prevalence per 100,000")
  } else {
    plot_prev <- plot_single(prevs_trial_median2, "#8DA0CB", "TB_prev_0_99", "TB prevalence \nin the population", "Prevalence per 100,000")
  }
    
  # b) % asymptomatic TB 
  Ds_trial_median1 <- subset(Ds_pct_median, iso == isos)
  Ds_trial_median2 <- subset(Ds_trial_median1, age_group == "[15,99]")
  
  if (isos == "BRA") {
  plot_Ds <- plot_single(Ds_trial_median2, "#1B9E77", "", "% asymptomatic TB in prevalent \ninfectious TB in adults", "Percentage (%)")
  } else {
    plot_Ds <- plot_single(Ds_trial_median2, "#1B9E77", "Ds_pct", "% asymptomatic TB in prevalent \ninfectious TB in adults", "Percentage (%)")
  }
  
  # c) Incidence
  inc_trial_median1 <- subset(tb_inc_pop_median, iso == isos)
  plot_inc <- plot_many(inc_trial_median1, "TB_inc", "Symptomatic TB incidence \nin the population", "Incidence per 100,000")
  
  # d) Notifications
  not_trial_median1 <- subset(tb_not_pop_median, iso == isos)
  plot_not <- plot_many(not_trial_median1, "TB_not", "TB notifications in \nthe population", "Notifications per 100,000")
  
  # e) Mortality
  mort_trial_median1 <- subset(tb_mort_pop_median, iso == isos)
  mort_trial_median2 <- subset(mort_trial_median1, age_group == "[0,99]")
  plot_mort <- plot_single(mort_trial_median2, "#8DA0CB", "TB_mort_0_99", 
                          "TB mortality in \nthe population", "Mortality per 100,000")
  
  # f) MDR
  mdr_trial_median1 <- subset(tb_mdr_pop_median, iso == isos)
  mdr_trial_median2 <- subset(mdr_trial_median1, age_group == "[0,99]")
  plot_mdr <- plot_single(mdr_trial_median2, "#8DA0CB", "MDR_0_99", 
                          "% MDR among notifications", "Percentage (%)")
  
  if (isos=="BRA"){
    # TB-Prison plots
    inc_BRA_median1 <- subset(tb_inc_BRA_median, age_group == "[15,99]")
    plot_inc_pris <- plot_single(inc_BRA_median1, "#1B9E77", "TB_incpris_15_99", "Symptomatic TB incidence in prisons", "Incidence per 100,000")
    
    not_BRA_median1 <- subset(tb_not_BRA_median, age_group == "[15,99]")
    plot_not_pris <- plot_single(not_BRA_median1, "#1B9E77", "TB_notpris_15_99", "TB notifications in prisons", "Notifications per 100,000")
    
    # Prison specific plots
    admiss_trial_median1 <- subset(pris_admis_pop_median, age_group == "[15,99]")
    plot_pris_admis <- plot_single(admiss_trial_median1, "#1B9E77", "Pris_admis", "Admissions in prisons", "Admissions per 100,000")
    
    pris_prev_trial_median1 <- subset(pris_prev_pop_median, age_group == "[15,99]")
    plot_pris_prev <- plot_single(pris_prev_trial_median1, "#1B9E77", "Pris_prev", "Incarceration prevalence", "Prevalence per 100,000")
    
    # Add plots into one
    plots_calibration <- (plot_inc + plot_mort + plot_not) /
      (plot_prev + plot_Ds + plot_mdr) / 
      (plot_inc_pris + plot_not_pris + plot_spacer()) /
      (plot_pris_admis + plot_pris_prev + plot_spacer()) +
      plot_layout(guides = 'collect') +
      plot_annotation(title = paste(isos),
                      theme = theme(plot.title = element_text(hjust = 0.5, size = 14)))
      
      plots_risks <- (plot_pris_admis + plot_pris_prev + plot_spacer()) +
        plot_layout(guides = 'collect') +
        plot_annotation(title = paste(isos),
                        theme = theme(plot.title = element_text(hjust = 0.5, size = 14)))
      
    
  } else if (isos == "IND") {
    plot_PAF <- plot_single(PAF_median2, "#8DA0CB", "PAF", "Population Attributable Fraction \nto Undernutrition", "Proportion Attributable")
    
    plots_calibration <- (plot_inc + plot_mort + plot_not) /
      (plot_prev + plot_Ds + plot_mdr) / 
      (plot_PAF + plot_spacer() + plot_spacer()) /
      (plot_spacer()) +
      plot_layout(guides = 'collect') +
      plot_annotation(title = paste(isos),
                      theme = theme(plot.title = element_text(hjust = 0.5, size = 14)))
    
  } else if (isos == "ZAF") {
    # TB-HIV plots
    inc_ZAF_median1 <- subset(tb_inc_ZAF_median, age_group == "[0,99]")
    plot_inc_HIV <- plot_single(inc_ZAF_median1, "#8DA0CB", "TB_inchiv_0_99", "Symptomatic TB incidence in HIV+", "Incidence per 100,000")
    
    not_ZAF_median1 <- subset(tb_inc_ZAF_median, age_group == "[0,99]")
    plot_not_HIV <- plot_single(inc_ZAF_median1, "#8DA0CB", "TB_nothiv_0_99", "TB notifications in HIV+", "Notifications per 100,000")
    
    mort_ZAF_median1 <- subset(tb_mort_ZAF_median, age_group == "[0,99]")
    plot_mort_HIV <- plot_single(mort_ZAF_median1, "#8DA0CB", "TB_morthiv_0_99", "TB mortality in HIV+", "Mortality per 100,000")
    
    # HIV specific plots
    HIV_prev_median1 <- subset(HIV_prev_pop_median, age_group != "[0,99]")
    plot_HIV_prev <- plot_HIV(HIV_prev_median1, "HIV_prev", "HIV+ prevalence", "Prevalence (%)")
    
    ART_cov_median1 <- subset(ART_cov_pop_median, age_group != "[0,99]")
    plot_ART_cov <- plot_HIV(ART_cov_median1, "HIV_artcov", "ART coverage", "Coverage (%)")
    
    HIV_mort_median1 <- subset(HIV_mort_pop_median, age_group == "[0,99]")
    plot_HIV_mort <- plot_single(HIV_mort_median1, "#8DA0CB", "HIV_mort_0_99", "HIV mortality", "Mortality per 100,000")
    
    # Add plots into one
    plots_calibration <- (plot_inc + plot_mort + plot_not) /
      (plot_prev + plot_Ds + plot_mdr) / 
      (plot_inc_HIV + plot_mort_HIV + plot_not_HIV ) /
      (plot_HIV_prev + plot_ART_cov + plot_HIV_mort) +
    plot_layout(guides = 'collect') +
      plot_annotation(title = paste(isos),
                      theme = theme(plot.title = element_text(hjust = 0.5, size = 14)))
    
    plots_risks <- (plot_HIV_prev + plot_ART_cov + plot_HIV_mort) +
    plot_layout(guides = 'collect') +
      plot_annotation(title = paste(isos),
                      theme = theme(plot.title = element_text(hjust = 0.5, size = 14)))
    
  }
    
  file_path_cali <- paste("countries/",isos,"/output/",isos,"_Calibration(floor)_CI.png", sep="")
  ggsave(filename=file_path_cali,plot=plots_calibration,width=29.7,height=21, unit="cm", bg = "white")
  
  file_path_risk <- paste("countries/",isos,"/output/",isos,"_RISKs(floor)_CI.png", sep="")
  ggsave(filename=file_path_risk,plot=plots_risks,width=29.7,height=7, unit="cm", bg = "white")
}
