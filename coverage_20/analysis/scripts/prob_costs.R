#------------------------------------------------------------------------------#
# PACE-TB - Cost uncertainty                                                   #
# Assigns cost uncertainty linked to UID                                       #
# Code by Alvaro Schwalb (alvaro.schwalb@lshtm.ac.uk)                          #
# Last updated 2025-03-22 by ASC                                               #
#------------------------------------------------------------------------------#

# Packages
suppressMessages({
  library(tidyverse)
  library(openxlsx)
  library(here)
  library(rio)
  library(fst)
})

rm(list = ls())

# Setting seed
set.seed(104046)

# Set ISO codes
isos <- c("BRA", "IND", "ZAF")

# Checks outputs in folder
files <- list.files(here("outputs", "epi"), full.names = FALSE)

# Initialise costs
costs <- list()

for (iso in isos) {
  print(paste("Cost uncertainty:", iso))
  
  # Read in cost data
  cost <- read.xlsx(here("outputs", "res", "summ", "prob_costs.xlsx"), sheet = iso) %>% 
    filter(complete.cases(.)) 
  
  # Extract unique UIDs
  iso_files <- files[grep(paste0("^", iso), files)]
  UIDs <- unique(sapply(strsplit(iso_files, "_"), function(x) x[2]))
  
  # Random costs from gamma distribution
  for (uid in UIDs) { 
    costs[[iso]][[uid]] <- cost
    costs[[iso]][[uid]]$cost <- rgamma(n = nrow(cost), shape = cost$alpha, scale = cost$theta)
  }
}

rm(list = setdiff(ls(), "costs"))

# Row bind list and specify ISO and UID
costs <- imap_dfr(costs, function(iso_list, iso) {
  imap_dfr(iso_list, function(uid_list, uid) {
    uid_list %>% mutate(iso = iso, uid = uid)
  })
})

costs <- costs %>% 
  select(iso, uid, actv, cost) %>% 
  pivot_wider(names_from = actv, values_from = cost)
  
export(costs, here("outputs", "res", "summ", "costs.csv"))
