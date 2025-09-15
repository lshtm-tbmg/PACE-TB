#------------------------------------------------------------------------------#
# PACE-TB - Epidemiological impact                                             #
# Calculate epidemiological impact relative to BAU across ISO/UID/INT          #
# Code by Katherine C. Horton (katherine.horton@lshtm.ac.uk)                   #
# Last updated 2025-06-02 by KCH                                               #
#------------------------------------------------------------------------------#

rm(list = ls())

# Packages
suppressPackageStartupMessages({
  library(here)
  library(rio)
  library(data.table)
  library(tidyverse)
  library(openxlsx)
  library(fst)
  library(extrafont)
  library(scales)
  library(gt)
  library(chromote)
})

# Checks outputs in folder
files <- list.files(here("outputs", "res", "epi"), full.names = FALSE)
epicalc <- data.table()

for (i in 1:length(files)) {
  file <- files[i]
  
  # Extract ISO code
  iso <- strsplit(file,"_")[[1]][1]
  if (!iso %in% c("BRA", "IND", "ZAF")) {
    stop(paste("Error: Invalid ISO code in file name:", file))
  }
 
  # Extract UID
  uid <- strsplit(strsplit(file,"_")[[1]][2],"\\.")[[1]][1]
  
  print(paste("ISO:", iso, "| UID:", uid))
  
  # Load the file
  resepi <- read_fst(here('outputs', 'res', 'epi', file))
  
  resepi_bra <- resepi %>%
    pivot_wider(names_from='var',values_from='val') %>%
    filter(iso == "BRA") %>% 
    mutate(prevtimeraw  = prev  / pop * 1e5,            # Prevalence (raw)
           prevtimepulm = prev  / pop * 1e5,            # Prevalence of pulmonary TB
           prevtimeall  = prev/0.905/ pop * 1e5,        # Prevalence of all forms (0.905 = proportion pTB incidence)
           inctimeraw   = inc / pop * 1e5,              # Incidence of sTB (raw)
           inctimestb   = (inc*0.86) / pop * 1e5,       # Incidence of sTB (0.86 = adjust for double-counting episodes)
           inctimeall   = (inc*0.86/0.905) / pop * 1e5, # Incidence of all forms (0.86 = adjust for double-counting episodes; 0.905 = proportion pTB incidence)
           morttime     = death / pop * 1e5) %>%        # Mortality 
    select(iso, uid, intv, year, prevtimeraw, prevtimepulm, prevtimeall, inctimeraw, inctimestb, inctimeall, morttime) %>%
    pivot_longer(cols=contains("time"),names_to='var',values_to='val')
  
  resepi_ind <- resepi %>%
    pivot_wider(names_from='var',values_from='val') %>%
    filter(iso == "IND") %>% 
    mutate(prevtimeraw  = prev / pop * 1e5,                # Prevalence (raw)
           prevtimepulm = (prev/0.88) / pop * 1e5,         # Prevalence of pulmonary TB (0.88 = proportion not on treatment)
           prevtimeall  = (prev/(0.88*0.819)) / pop * 1e5, # Prevalence of all forms (0.88 = proportion not on treatment; 0.819 = proportion pTB incidence)
           inctimeraw   = inc / pop * 1e5,                 # Incidence of sTB (raw)
           inctimestb   = (inc*0.86) / pop * 1e5,          # Incidence of sTB (0.86 = adjust for double-counting episodes)
           inctimeall   = (inc*0.86/0.819) / pop * 1e5,    # Incidence of all forms (0.86 = adjust for double-counting episodes; 0.819 = proportion pTB incidence)
           morttime     = death / pop * 1e5) %>%           # Mortality 
    select(iso, uid, intv, year, prevtimeraw, prevtimepulm, prevtimeall, inctimeraw, inctimestb, inctimeall, morttime) %>%
    pivot_longer(cols=contains("time"),names_to='var',values_to='val')
  
  resepi_zaf <- resepi %>%
    pivot_wider(names_from='var',values_from='val') %>%
    filter(iso == "ZAF") %>% 
    mutate(prevtimeraw  = prev / pop * 1e5,                 # Prevalence (raw)
           prevtimepulm = (prev/0.953) / pop * 1e5,         # Prevalence of pulmonary TB (0.953 = proportion not on treatment)
           prevtimeall  = (prev/(0.953*0.923)) / pop * 1e5, # Prevalence of all forms (0.953 = proportion not on treatment; 0.923 = proportion pTB incidence)
           inctimeraw   = inc / pop * 1e5,                  # Incidence of sTB (raw)
           inctimestb   = (inc*0.86) / pop * 1e5,           # Incidence of sTB (0.86 = adjust for double-counting episodes)
           inctimeall   = (inc*0.86/0.923) / pop * 1e5,     # Incidence of all forms (0.86 = adjust for double-counting episodes; 0.923 = proportion pTB incidence)
           morttime     = death / pop * 1e5) %>%            # Mortality 
    select(iso, uid, intv, year, prevtimeraw, prevtimepulm, prevtimeall, inctimeraw, inctimestb, inctimeall, morttime) %>%
    pivot_longer(cols=contains("time"),names_to='var',values_to='val')
  
  # Prevalence of infTB
  prev_temp <- resepi %>% 
    pivot_wider(names_from='var',values_from='val') %>%
    filter(year == 2050) %>% 
    mutate(var = "prev",
           prevpop = prev / pop,
           val_bau = prevpop[intv == "BAU"],
           pct = (val_bau - prevpop) / val_bau) %>% 
    select(iso, uid, intv, year, var = var, val = pct)
  
  # Incident episodes of sTB
  inc_temp <- resepi %>% 
    filter(var == 'inc') %>% 
    filter(year >= 2025) %>% 
    group_by(intv) %>% 
    mutate(val = cumsum(val)) %>% 
    ungroup() %>%
    filter(year == 2050) %>% 
    mutate(val_bau = val[intv == "BAU"],
           diff = (val_bau - val)) %>% 
    select(iso, uid, intv, year, var, val = diff)
  
  # Proportion incident episodes of sTB
  incpct_temp <- resepi %>% 
    filter(var == 'inc') %>% 
    filter(year >= 2025) %>% 
    group_by(intv) %>% 
    mutate(val = cumsum(val)) %>% 
    ungroup() %>%
    filter(year == 2050) %>% 
    mutate(val_bau = val[intv == "BAU"],
           pct = (val_bau - val)/val_bau) %>% 
    mutate(var = 'incpct') %>%
    select(iso, uid, intv, year, var, val = pct)

  # Deaths from TB
  death_temp <- resepi %>% 
    filter(var == 'death') %>% 
    filter(year >= 2025) %>% 
    group_by(intv) %>% 
    mutate(val = cumsum(val)) %>% 
    ungroup() %>%
    filter(year == 2050) %>% 
    mutate(val_bau = val[intv == "BAU"],
           diff = (val_bau - val)) %>% 
    select(iso, uid, intv, year, var, val = diff)

  # Proportion deaths from TB
  deathpct_temp <- resepi %>% 
    filter(var == 'death') %>% 
    filter(year >= 2025) %>% 
    group_by(intv) %>% 
    mutate(val = cumsum(val)) %>% 
    ungroup() %>%
    filter(year == 2050) %>% 
    mutate(val_bau = val[intv == "BAU"],
           pct = (val_bau - val)/val_bau) %>% 
    mutate(var = 'deathpct') %>%
    select(iso, uid, intv, year, var, val = pct)
  
  epicalc <- rbind(epicalc, resepi_bra, resepi_ind, resepi_zaf, prev_temp, inc_temp, incpct_temp, death_temp, deathpct_temp)
}

write_fst(epicalc, here('outputs', 'res', 'summ', "epicalc.fst"))

episumm <- epicalc  %>% 
  group_by(iso, intv, var, year) %>%
  summarise(med = quantile(val, 0.5),
            low = quantile(val, 0.025),
            upp = quantile(val, 0.975))
  
write_fst(episumm, here('outputs', 'res', 'summ', "episumm.fst"))
