#------------------------------------------------------------------------------#
# PACE-TB - Health economic analysis                                           #
# Launches health economic analysis for all countries                          #
# Code by Lara Goscé (lara.gosce@lshtm.ac.uk)                                  #
# Last updated 2025-05-13 by ASC                                               #
#------------------------------------------------------------------------------#

# Packages
suppressMessages({
  library(ggplot2)
  library(tidyverse)
  library(openxlsx)
  library(data.table)
  library(scales)
  library(here)
  library(rio)
  library(fst)
})

rm(list = ls())

# Define country ISOs
isos <- c("BRA", "IND", "ZAF")

# Combined-run data
data <- list() 

# Load model outputs
for (i in 1:length(isos)){
  iso <- isos[i]
  print(paste0(iso, " | Loading model outputs"))
  source(here("scripts", paste0("cea00_outputs.R")))
}

# Health economic values
he_all <- list() 
he_values <- read.xlsx(here("outputs", "res", "summ", "ceacalc.xlsx"), sheet = "VALUES")

# Extract values from XLSX
he_all[["dis_w"]] <- data.frame(dis_w = he_values[4:5,4:6], row.names = he_values$X3[4:5]) # Disability weights
he_all[["uti_w"]] <- data.frame(uti_w = he_values[8:10,4:6], row.names = he_values$X3[8:10]) # Utility weights
he_all[["cet"]] <- data.frame(cet = he_values[13:14,4:6], row.names = he_values$X3[13:14]) # Cost-effectiveness thresholds
he_all[["disc_rate"]] <- data.frame(disc = he_values[17:18,4:6], row.names = he_values$X3[17:18]) # Discount rates
he_all[["ly_healthy"]] <- data.frame(ly_healthy = he_values[42:441,3:5], row.names = he_values$X2[42:441]) # Healthy life years
he_all[["ly_postTB"]] <- data.frame(ly_postTB = he_values[42:441,8:10], row.names = he_values$X7[42:441]) # Post TB life years
he_all[["ly_postTB_lost"]] <- data.frame(ly_postTB_lost = he_values[42:441,12:14], row.names = he_values$X11[42:441]) # Post TB life years lost

# Apply ISO column names
he_all <- lapply(he_all, function(df) {
  colnames(df) <- isos
  return(df)
})

# Restructure HE values by ISO
he <- list()

for (var in names(he_all)) {
  df <- he_all[[var]]
  
  for (i in seq_along(isos)) {
    iso <- isos[i]
    he[[iso]][[var]] <- data.frame(df[, i, drop = FALSE])
    he[[iso]][[var]][, 1] <- as.numeric(he[[iso]][[var]][, 1])
  }
}

rm(list = setdiff(ls(), c("data", "he", "isos")))

# Activity costs with uncertainty from gamma distribution
costs <- import(here("outputs", "res", "summ", "costs.csv"))

# Cost-effectiveness analysis
cea <- list() 
cea_sum <- list()

for (i in 1:length(isos)){
  iso <- isos[i]
  
  if ("SCR" %in% names(data[[iso]])) {
    # Add SCR sensitivity analyses
    data[[iso]][["SCRlo"]] <- data[[iso]][["SCR"]]
    data[[iso]][["SCRhi"]] <- data[[iso]][["SCR"]]
    data[[iso]][["SCR"]] <- NULL
    
    # Reorder intervention names alphabetically
    data[[iso]] <- data[[iso]][sort(names(data[[iso]]))]
  }
  
  print(paste0(iso, " | Running analysis"))
  source(here("scripts", paste0("cea00_analysis.R")))
  
}

rm(list = setdiff(ls(), c("cea", "cea_sum", "isos")))

# Summary CE table
cea_total <- list()

for (iso in names(cea)) {

  for (intv in names(cea[[iso]])) {
    
    n_runs <- ncol(cea[[iso]][[intv]][["c_Int"]])
    
    for (run in 1:n_runs) {
      
      cea_total[[length(cea_total) + 1]] <- list(
        iso = iso,
        intv = intv,
        run = run,
        c_Int = cea[[iso]][[intv]][["c_Int"]][, run],
        DALY = cea[[iso]][[intv]][["DALY"]][, run],
        QALY = cea[[iso]][[intv]][["QALY"]][, run],
        Inc_costs = cea_sum[[iso]][[intv]][["Inc_costs"]][, run],
        DALY_avert = cea_sum[[iso]][[intv]][["DALY_avert"]][, run],
        QALY_gains = cea_sum[[iso]][[intv]][["QALY_gains"]][, run],
        ICERs = cea_sum[[iso]][[intv]][["ICERs"]][, run],
        NB_low = cea_sum[[iso]][[intv]][["NB_low"]][, run],
        NB_high = cea_sum[[iso]][[intv]][["NB_high"]][, run]
      )
    }
  }
}

rm(list = setdiff(ls(), c("cea_total")))

# Convert to data frame
cea_df <- rbindlist(cea_total)

write_fst(cea_df, here("outputs", "res", "summ", "ceasumm.fst"))
