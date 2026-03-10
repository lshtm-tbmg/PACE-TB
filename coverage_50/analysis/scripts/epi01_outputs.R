#------------------------------------------------------------------------------#
# PACE-TB - Epidemiological impact                                             #
# Calculate epidemiological outputs across ISO/UID/INT                         #
# Code by Katherine C. Horton (katherine.horton@lshtm.ac.uk)                   #
# Last updated 2025-03-27 by KCH                                               #
#------------------------------------------------------------------------------#

# Clear environment
rm(list = ls())

# Packages
suppressPackageStartupMessages({
  library(here)
  library(rio)
  library(data.table)
  library(tidyverse)
  library(openxlsx)
  library(fst)
})

# Checks outputs in folder
files <- list.files(here("outputs", "epi"), full.names = FALSE)
epiout <- data.table()

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

  # Population
  temp_pop <- stocks %>% 
    mutate(year = ceiling(year)) %>% 
    filter(year >= 2000) %>% 
    filter(!(str_detect(TB, 'dead') | str_detect(TB, 'count'))) %>% 
    filter(!(str_detect(VXa, 'dead') | str_detect(VXa, 'count'))) %>% 
    filter(!(str_detect(RISK, 'dead') | str_detect(RISK, 'count'))) %>% 
    filter((age_from == 0 & age_thru == 99)) %>% 
    select(iso = country, year, TB, value) %>% 
    group_by(iso, year) %>% 
    summarise(val = sum(value) * 1e3, .groups = "drop") %>% 
    ungroup() %>% 
    mutate(var = "pop") %>% 
    mutate(intv = int) %>% 
    mutate(uid = UID) %>% 
    select(iso, intv, uid, year, var, val)
    
  # Prevalence of infTB
  temp_prev <- stocks %>% 
    mutate(year = ceiling(year)) %>% 
    filter(year >= 2000) %>% 
    filter(str_detect(TB, 'Ds') | str_detect(TB, 'Dc')) %>% 
    filter(!(str_detect(TB, 'ST') | str_detect(TB, 'RT'))) %>%
    filter(!(str_detect(TB, 'dead') | str_detect(TB, 'count'))) %>% 
    filter(!(str_detect(VXa, 'dead') | str_detect(VXa, 'count'))) %>% 
    filter(!(str_detect(RISK, 'dead') | str_detect(RISK, 'count'))) %>% 
    filter((age_from == 0 & age_thru == 99)) %>% 
    select(iso = country, year, TB, value) %>% 
    group_by(iso, year) %>% 
    summarise(val = sum(value) * 1e3, .groups = "drop") %>% 
    ungroup() %>% 
    mutate(var = "prev") %>% 
    mutate(intv = int) %>% 
    mutate(uid = UID) %>% 
    select(iso, intv, uid, year, var, val)
  
  # Incident episodes of sTB
  temp_inc <- stocks %>% 
    mutate(year = ceiling(year)) %>% 
    filter(year >= 2000) %>% 
    filter(str_detect(TB, 'Inc_Dc_count')) %>% 
    filter(!(str_detect(VXa, 'dead') | str_detect(VXa, 'count'))) %>% 
    filter(!(str_detect(RISK, 'dead') | str_detect(RISK, 'count'))) %>% 
    filter((age_from == 0 & age_thru == 99)) %>% 
    select(iso = country, year, TB, value) %>% 
    group_by(iso, year) %>% 
    summarise(val = sum(value) * 1e3, .groups = "drop") %>% 
    ungroup() %>% 
    mutate(var = "inc") %>% 
    mutate(intv = int) %>% 
    mutate(uid = UID) %>% 
    select(iso, intv, uid, year, var, val)
  
  # Deaths from sTB
  temp_death <- stocks %>% 
    mutate(year = ceiling(year)) %>% 
    filter(year >= 2000) %>% 
    filter(str_detect(TB, 'dead')) %>% 
    filter(!(str_detect(VXa, 'dead') | str_detect(VXa, 'count'))) %>% 
    filter(!(str_detect(RISK, 'dead') | str_detect(RISK, 'count'))) %>% 
    filter((age_from == 0 & age_thru == 99)) %>% 
    select(iso = country, year, TB, value) %>% 
    group_by(iso, year) %>% 
    summarise(val = sum(value) * 1e3, .groups = "drop") %>% 
    ungroup() %>% 
    mutate(var = "death") %>% 
    mutate(intv = int) %>% 
    mutate(uid = UID) %>% 
    select(iso, intv, uid, year, var, val)
  
  epiout <- rbind(epiout, temp_pop, temp_prev, temp_inc, temp_death)
  rm(temp_pop,temp_prev, temp_inc, temp_death)
  
} 

epioutrun <- split(epiout, list(epiout$uid, epiout$iso), drop = TRUE)
names(epioutrun) <- sapply(epioutrun, function(df) {
  paste0(unique(df$iso), "_", unique(df$uid))
})

for (i in 1:length(epioutrun)){
  write_fst(epioutrun[[i]], here('outputs', 'res', 'epi', paste0(strsplit(names(epioutrun[i]),".",fixed=TRUE)[[1]][1],".fst")))
}
