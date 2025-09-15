#------------------------------------------------------------------------------#
# PACE-TB - CEA analysis                                                       #
# Run cost-effectiveness analysis                                              #
# Code by Lara Goscé (lara.gosce@lshtm.ac.uk)                                  #
# Last updated 2025-05-13 by ASC                                               #
#------------------------------------------------------------------------------#

# Extract intervention types
intvs <- unique(names(data[[iso]]))

he_temp <- he[[iso]]
cost <- filter(costs, iso == .env$iso)
uids <- unique(cost$uid)
  
for (intv in 1:length(intvs)) {
  intv <- intvs[intv]
  data_temp <- data[[iso]][[intv]]
  n_runs <- length(uids)
  n_years <- 25
  n_agegp <- 16
  
  # Initialise discount_y
  cea[[iso]][[intv]][["disc_y"]] <- matrix(0, nrow = n_years, ncol = 1)
  
  for (yr in 1:n_years){ 
    cea[[iso]][[intv]][["disc_y"]][1 + (yr - 1), ] <- (1 + he_temp[["disc_rate"]]["disc", ])^(-(yr - 1))
  } 
  
  # Initialise discount_y_ag
  cea[[iso]][[intv]][["disc_y_ag"]] <- matrix(0, nrow = n_agegp * n_years, ncol = 1)
  
  for (yr in 1:n_years){ 
    cea[[iso]][[intv]][["disc_y_ag"]][(1:16) + (n_agegp * (yr - 1)), ] <- (1 + he_temp[["disc_rate"]]["disc", ])^(-(yr - 1))
  } 
  
  # Initialise DALY counts
  cea[[iso]][[intv]][["DALY_mort_TB"]] <- matrix(0, nrow = 1, ncol = n_runs)
  cea[[iso]][[intv]][["DALY_mort_postTB"]] <- matrix(0, nrow = 1, ncol = n_runs)
  cea[[iso]][[intv]][["DALY_dis_TB"]] <- matrix(0, nrow = 1, ncol = n_runs)
  cea[[iso]][[intv]][["DALY_dis_postTB"]] <- matrix(0, nrow = 1, ncol = n_runs)
  
  for (run in 1:n_runs){
    cea[[iso]][[intv]][["DALY_mort_TB"]][, run] <- sum(data_temp[["n_TBdeath_y_ag"]][, run] * he_temp[["ly_healthy"]])
    cea[[iso]][[intv]][["DALY_mort_postTB"]][, run] <- sum(data_temp[["n_postTB_y_ag"]][, run] * he_temp[["ly_postTB_lost"]])
    cea[[iso]][[intv]][["DALY_dis_TB"]][, run] <- sum(data_temp[["n_Dc_y_ag"]][, run] * data_temp[["duration_Dc_ag"]][, run] * he_temp[["dis_w"]]["dw_TB", ] * cea[[iso]][[intv]][["disc_y_ag"]])
    cea[[iso]][[intv]][["DALY_dis_postTB"]][, run] <- sum(data_temp[["n_postTB_y_ag"]][, run] * he_temp[["ly_postTB"]] * he_temp[["dis_w"]]["dw_postTB", ])
  }
  
  # Initialise total DALYs
  cea[[iso]][[intv]][["DALY"]] <- matrix(0, nrow = 1, ncol = n_runs)
  
  for (run in 1:n_runs){
    cea[[iso]][[intv]][["DALY"]][, run] <- 
      cea[[iso]][[intv]][["DALY_mort_TB"]][, run] + cea[[iso]][[intv]][["DALY_mort_postTB"]][, run] +
      cea[[iso]][[intv]][["DALY_dis_TB"]][, run] + cea[[iso]][[intv]][["DALY_dis_postTB"]][, run]
  }
  
  # Initialise QALY counts
  cea[[iso]][[intv]][["QALY_annual_noTB"]] <- matrix(0, nrow = 1, ncol = n_runs)
  cea[[iso]][[intv]][["QALY_annualTB"]] <- matrix(0, nrow = 1, ncol = n_runs)
  cea[[iso]][[intv]][["QALY_future_noTB"]] <- matrix(0, nrow = 1, ncol = n_runs)
  cea[[iso]][[intv]][["QALY_future_postTB"]] <- matrix(0, nrow = 1, ncol = n_runs)
  
  for (run in 1:n_runs){
    cea[[iso]][[intv]][["QALY_annual_noTB"]][, run] <- sum(data_temp[["n_noTB_y_ag"]][, run] * he_temp[["uti_w"]]["u_healthy", ] * cea[[iso]][[intv]][["disc_y_ag"]])
    cea[[iso]][[intv]][["QALY_annualTB"]][, run] <- sum(data_temp[["n_Dc_y_ag"]][, run] * data_temp[["duration_Dc_ag"]][, run] * he_temp[["uti_w"]]["u_TB", ] * cea[[iso]][[intv]][["disc_y_ag"]])
    cea[[iso]][[intv]][["QALY_future_noTB"]][, run] <- sum(data_temp[["n_noTB_y_ag"]][385:400, run] * he_temp[["ly_healthy"]][385:400,] * he_temp[["uti_w"]]["u_healthy", ])
    cea[[iso]][[intv]][["QALY_future_postTB"]][, run] <- sum(data_temp[["n_postTB_y_ag"]][, run] * he_temp[["ly_postTB"]] * he_temp[["uti_w"]]["u_postTB", ])
  }
  
  # Initialise total QALYs
  cea[[iso]][[intv]][["QALY"]] <- matrix(0, nrow = 1, ncol = n_runs)
  
  for (run in 1:n_runs){
    cea[[iso]][[intv]][["QALY"]][, run] <- 
      cea[[iso]][[intv]][["QALY_annual_noTB"]][, run] + cea[[iso]][[intv]][["QALY_annualTB"]][, run] +
      cea[[iso]][[intv]][["QALY_future_noTB"]][, run] + cea[[iso]][[intv]][["QALY_future_postTB"]][, run]
  }
  
  # Initialise c_Int
  cea[[iso]][[intv]][["c_Int"]] <- matrix(0, nrow = n_years, ncol = n_runs)
  
  if (intv == "BAU") {
    for (run in 1:n_runs) {
      uid <- uids[run]
      for (yr in 1:n_years) {
        # Compute c_Int for each year and run
        cea[[iso]][[intv]][["c_Int"]][yr, run] <- 
          (data_temp[["n_assess_y"]][yr, run] * cost[cost$uid == uid, "c_a_diag_standard"] + # Cost of standard evaluation
             data_temp[["n_treat_DS_y"]][yr, run] * cost[cost$uid == uid, "c_a_treat_DS_standard"] + # Cost of DS treatment
             data_temp[["n_treat_DR_y"]][yr, run] * cost[cost$uid == uid, "c_a_treat_DR_standard"]) * # Cost of DR treatment
          (1 + he_temp[["disc_rate"]]["disc", ])^(-(yr - 1)) # Discounting
      }
    }
    
    # Summarise c_Int by column
    cea[[iso]][[intv]][["c_Int"]] <- matrix(colSums(cea[[iso]][[intv]][["c_Int"]]), nrow = 1)
  }
  
  if (intv == "DGN") {
    for (run in 1:n_runs) {
      uid <- uids[run]
      for (yr in 1:n_years) {
        if (yr <= 5) {
          # Compute c_Int for first 5 years and runs
          cea[[iso]][[intv]][["c_Int"]][yr, run] <- 
            (data_temp[["n_assess_y"]][yr, run] * ((1 - (0.16 * yr)) * cost[cost$uid == uid, "c_a_diag_standard"] + (0.16 * yr) * cost[cost$uid == uid, "c_a_diag_improved"]) + # Cost of standard and improved evaluation
               data_temp[["n_treat_DS_y"]][yr, run] * cost[cost$uid == uid, "c_a_treat_DS_standard"] + # Cost of DS treatment
               data_temp[["n_treat_DR_y"]][yr, run] * cost[cost$uid == uid, "c_a_treat_DR_standard"]) * # Cost of DR treatment
            (1 + he_temp[["disc_rate"]]["disc", ])^(-(yr - 1)) # Discounting
          
        } else {
          # Compute c_Int for the rest of the years and runs
          cea[[iso]][[intv]][["c_Int"]][yr, run] <- 
            (data_temp[["n_assess_y"]][yr, run] * (0.2 * cost[cost$uid == uid, "c_a_diag_standard"] + 0.8 * cost[cost$uid == uid, "c_a_diag_improved"]) + # Cost of standard and improved evaluation
               data_temp[["n_treat_DS_y"]][yr, run] * cost[cost$uid == uid, "c_a_treat_DS_standard"] + # Cost of DS treatment
               data_temp[["n_treat_DR_y"]][yr, run] * cost[cost$uid == uid, "c_a_treat_DR_standard"]) * # Cost of DR treatment
            (1 + he_temp[["disc_rate"]]["disc", ])^(-(yr - 1)) # Discounting
        }
      }
    }
    
    # Summarise c_Int by column
    cea[[iso]][[intv]][["c_Int"]] <- matrix(colSums(cea[[iso]][[intv]][["c_Int"]]), nrow = 1)
  }
  
  if (intv == "DST") {
    for (run in 1:n_runs) {
      uid <- uids[run]
      for (yr in 1:n_years) {
        if (yr <= 5) {
          # Compute c_Int for first 5 years and runs
          cea[[iso]][[intv]][["c_Int"]][yr, run] <- 
            (data_temp[["n_assess_y"]][yr, run] * cost[cost$uid == uid, "c_a_diag_standard"] + # Cost of standard evaluation
               data_temp[["n_treat_DS_y"]][yr, run] * cost[cost$uid == uid, "c_a_treat_DS_standard"] + # Cost of DS treatment
               data_temp[["n_treat_DR_y"]][yr, run] * cost[cost$uid == uid, "c_a_treat_DR_standard"] + # Cost of DR treatment
               (0.16 * yr) * data_temp[["n_diag_noXpert_y"]][yr, run] * cost[cost$uid == uid, "c_a_DST"]) * # Cost of performing DST
            (1 + he_temp[["disc_rate"]]["disc", ])^(-(yr - 1)) # Discounting
          
        } else {
          # Compute c_Int for the rest of the years and runs
          cea[[iso]][[intv]][["c_Int"]][yr, run] <- 
            (data_temp[["n_assess_y"]][yr, run] * cost[cost$uid == uid, "c_a_diag_standard"] + # Cost of standard evaluation
               data_temp[["n_treat_DS_y"]][yr, run] * cost[cost$uid == uid, "c_a_treat_DS_standard"] + # Cost of DS treatment
               data_temp[["n_treat_DR_y"]][yr, run] * cost[cost$uid == uid, "c_a_treat_DR_standard"] + # Cost of DR treatment
               0.8 * data_temp[["n_diag_noXpert_y"]][yr, run] * cost[cost$uid == uid, "c_a_DST"]) * # Cost of performing DST
            (1 + he_temp[["disc_rate"]]["disc", ])^(-(yr - 1)) # Discounting
        }
      }
    }
    
    # Summarise c_Int by column
    cea[[iso]][[intv]][["c_Int"]] <- matrix(colSums(cea[[iso]][[intv]][["c_Int"]]), nrow = 1)
  }
  
  if (intv == "NTN") {
    for (run in 1:n_runs) {
      uid <- uids[run]
      for (yr in 1:n_years) {
        # Compute c_Int
        cea[[iso]][[intv]][["c_Int"]][yr, run] <- 
          (data_temp[["n_assess_y"]][yr, run] * cost[cost$uid == uid, "c_a_diag_standard"] + # Cost of standard evaluation
             data_temp[["n_treat_DS_y"]][yr, run] * cost[cost$uid == uid, "c_a_treat_DS_standard"] + # Cost of DS treatment
             data_temp[["n_treat_DR_y"]][yr, run] * cost[cost$uid == uid, "c_a_treat_DR_standard"] + # Cost of DR treatment
             data_temp[["n_nutrition_index_y"]][yr, run] * cost[cost$uid == uid, "c_a_nutrition_index"] + # Cost of providing nutrition to index case
             data_temp[["n_nutrition_HHC_y"]][yr, run] * cost[cost$uid == uid, "c_a_nutrition_contact"]) * # Cost of providing nutrition to contacts
          (1 + he_temp[["disc_rate"]]["disc", ])^(-(yr - 1)) # Discounting
      }
    }
    
    # Summarise c_Int by column
    cea[[iso]][[intv]][["c_Int"]] <- matrix(colSums(cea[[iso]][[intv]][["c_Int"]]), nrow = 1)
  }
  
  if (intv == "PRI") {
    for (run in 1:n_runs) {
      uid <- uids[run]
      for (yr in 1:n_years) {
        cea[[iso]][[intv]][["c_Int"]][yr, run] <- 
          (data_temp[["n_assess_y"]][yr, run] * cost[cost$uid == uid, "c_a_diag_standard"] + # Cost of standard evaluation
             data_temp[["n_treat_DS_y"]][yr, run] * cost[cost$uid == uid, "c_a_treat_DS_standard"] + # Cost of DS treatment
             data_temp[["n_treat_DR_y"]][yr, run] * cost[cost$uid == uid, "c_a_treat_DR_standard"] + # Cost of DR treatment
             data_temp[["n_screen_prison_y"]][yr, run] * cost[cost$uid == uid, "c_a_prison"]) * # Cost of screening in prison
          (1 + he_temp[["disc_rate"]]["disc", ])^(-(yr - 1)) # Discounting
      }
    }
    
    # Summarise c_Int by column
    cea[[iso]][[intv]][["c_Int"]] <- matrix(colSums(cea[[iso]][[intv]][["c_Int"]]), nrow = 1)
  }
  
  if (intv == "SCRhi") { 
    for (run in 1:n_runs) {
      uid <- uids[run]
      for (yr in 1:n_years) {
        if (yr <= 10) {
          # Compute c_Int for first 10 years and runs
          cea[[iso]][[intv]][["c_Int"]][yr, run] <- 
            (data_temp[["n_assess_y"]][yr, run] * cost[cost$uid == uid, "c_a_diag_standard"] + # Cost of standard evaluation
               data_temp[["n_treat_DS_y"]][yr, run] * cost[cost$uid == uid, "c_a_treat_DS_standard"] + # Cost of DS treatment
               data_temp[["n_treat_DR_y"]][yr, run] * cost[cost$uid == uid, "c_a_treat_DR_standard"] + # Cost of DR treatment
               data_temp[["n_screen_y"]][yr, run] * cost[cost$uid == uid, "c_a_screen_CXR_hi"] + #  Cost of CXR screening
               data_temp[["n_screen_CXRpositive_y"]][yr, run] * cost[cost$uid == uid, "c_a_screen_Xpert"]) * # Cost of Xpert screening
            (1 + he_temp[["disc_rate"]]["disc", ])^(-(yr - 1)) # Discounting
          
        } else {
          # Compute c_Int for the rest of the years and runs
          cea[[iso]][[intv]][["c_Int"]][yr, run] <- 
            (data_temp[["n_assess_y"]][yr, run] * cost[cost$uid == uid, "c_a_diag_standard"] + # Cost of standard evaluation
               data_temp[["n_treat_DS_y"]][yr, run] * cost[cost$uid == uid, "c_a_treat_DS_standard"] + # Cost of DS treatment
               data_temp[["n_treat_DR_y"]][yr, run] * cost[cost$uid == uid, "c_a_treat_DR_standard"]) * # Cost of DR treatment
            (1 + he_temp[["disc_rate"]]["disc", ])^(-(yr - 1)) # Discounting
        }
      }
    }
    
    # Summarise c_Int by column
    cea[[iso]][[intv]][["c_Int"]] <- matrix(colSums(cea[[iso]][[intv]][["c_Int"]]), nrow = 1)
  }
  
  if (intv == "SCRlo") { 
    for (run in 1:n_runs) {
      uid <- uids[run]
      for (yr in 1:n_years) {
        if (yr <= 10) {
          # Compute c_Int for first 10 years and runs
          cea[[iso]][[intv]][["c_Int"]][yr, run] <- 
            (data_temp[["n_assess_y"]][yr, run] * cost[cost$uid == uid, "c_a_diag_standard"] + # Cost of standard evaluation
               data_temp[["n_treat_DS_y"]][yr, run] * cost[cost$uid == uid, "c_a_treat_DS_standard"] + # Cost of DS treatment
               data_temp[["n_treat_DR_y"]][yr, run] * cost[cost$uid == uid, "c_a_treat_DR_standard"] + # Cost of DR treatment
               data_temp[["n_screen_y"]][yr, run] * cost[cost$uid == uid, "c_a_screen_CXR_lo"] + #  Cost of CXR screening
               data_temp[["n_screen_CXRpositive_y"]][yr, run] * cost[cost$uid == uid, "c_a_screen_Xpert"]) * # Cost of Xpert screening
            (1 + he_temp[["disc_rate"]]["disc", ])^(-(yr - 1)) # Discounting
          
        } else {
          # Compute c_Int for the rest of the years and runs
          cea[[iso]][[intv]][["c_Int"]][yr, run] <- 
            (data_temp[["n_assess_y"]][yr, run] * cost[cost$uid == uid, "c_a_diag_standard"] + # Cost of standard evaluation
               data_temp[["n_treat_DS_y"]][yr, run] * cost[cost$uid == uid, "c_a_treat_DS_standard"] + # Cost of DS treatment
               data_temp[["n_treat_DR_y"]][yr, run] * cost[cost$uid == uid, "c_a_treat_DR_standard"]) * # Cost of DR treatment
            (1 + he_temp[["disc_rate"]]["disc", ])^(-(yr - 1)) # Discounting
        }
      }
    }
    
    # Summarise c_Int by column
    cea[[iso]][[intv]][["c_Int"]] <- matrix(colSums(cea[[iso]][[intv]][["c_Int"]]), nrow = 1)
  }
  
  if (intv == "SDR") {
    for (run in 1:n_runs) {
      uid <- uids[run]
      for (yr in 1:n_years) {
        # Compute c_Int for each year and run
        cea[[iso]][[intv]][["c_Int"]][yr, run] <- 
          (data_temp[["n_assess_y"]][yr, run] * cost[cost$uid == uid, "c_a_diag_standard"] + # Cost of standard evaluation
             data_temp[["n_treat_DS_y"]][yr, run] * cost[cost$uid == uid, "c_a_treat_DS_standard"] + # Cost of DS treatment
             (data_temp[["n_treat_DR_y"]][yr, run] - data_temp[["n_treat_DR_short_y"]][yr, run]) * cost[cost$uid == uid, "c_a_treat_DR_standard"] + # Cost of standard DR treatment
             data_temp[["n_treat_DR_short_y"]][yr, run] * cost[cost$uid == uid, "c_a_treat_DR_improved"]) * # Cost of improved DR treatment
          (1 + he_temp[["disc_rate"]]["disc", ])^(-(yr - 1)) # Discounting
      }
    }
    
    # Summarise c_Int by column
    cea[[iso]][[intv]][["c_Int"]] <- matrix(colSums(cea[[iso]][[intv]][["c_Int"]]), nrow = 1)
  }
  
  if (intv == "SDS") {
    for (run in 1:n_runs) {
      uid <- uids[run]
      for (yr in 1:n_years) {
        if (yr <= 5) {
          # Compute c_Int for first 5 years and runs
          cea[[iso]][[intv]][["c_Int"]][yr, run] <- 
            (data_temp[["n_assess_y"]][yr, run] * cost[cost$uid == uid, "c_a_diag_standard"] + # Cost of standard evaluation
               data_temp[["n_treat_DS_y"]][yr, run] * ((1 - (0.16 * yr)) * cost[cost$uid == uid, "c_a_treat_DS_standard"] + (0.16 * yr) * cost[cost$uid == uid, "c_a_treat_DS_improved"]) + # Cost of improved and standard DS treatment
               data_temp[["n_treat_DR_y"]][yr, run] * cost[cost$uid == uid, "c_a_treat_DR_standard"]) # Cost of DR treatment
            (1 + he_temp[["disc_rate"]]["disc", ])^(-(yr - 1)) # Discounting
          
        } else {
          # Compute c_Int for the rest of the years and runs
          cea[[iso]][[intv]][["c_Int"]][yr, run] <- 
            (data_temp[["n_assess_y"]][yr, run] * cost[cost$uid == uid, "c_a_diag_standard"] + # Cost of standard evaluation
               data_temp[["n_treat_DS_y"]][yr, run] * (0.2 * cost[cost$uid == uid, "c_a_treat_DS_standard"] + 0.8 * cost[cost$uid == uid, "c_a_treat_DS_improved"]) + # Cost of improved and standard DS treatment
               data_temp[["n_treat_DR_y"]][yr, run] * cost[cost$uid == uid, "c_a_treat_DR_standard"]) # Cost of DR treatment
          (1 + he_temp[["disc_rate"]]["disc", ])^(-(yr - 1)) # Discounting
        }
      }
    }
    
    # Summarise c_Int by column
    cea[[iso]][[intv]][["c_Int"]] <- matrix(colSums(cea[[iso]][[intv]][["c_Int"]]), nrow = 1)
  }
  
  if (intv == "TPT") {
    for (run in 1:n_runs) {
      uid <- uids[run]
      for (yr in 1:n_years) {
        # Compute c_Int for each year and run
        cea[[iso]][[intv]][["c_Int"]][yr, run] <- 
          (data_temp[["n_assess_y"]][yr, run] * cost[cost$uid == uid, "c_a_diag_standard"] + # Cost of standard evaluation
             data_temp[["n_treat_DS_y"]][yr, run] * cost[cost$uid == uid, "c_a_treat_DS_standard"] + # Cost of DS treatment
             data_temp[["n_treat_DR_y"]][yr, run] * cost[cost$uid == uid, "c_a_treat_DR_standard"] + # Cost of DR treatment
             (data_temp[["n_HHC_DS_y"]][yr, run] + data_temp[["n_HHC_DR_y"]][yr, run]) * cost[cost$uid == uid, "c_a_HHCM"] + # Cost of assessment of HHC
             data_temp[["n_HHC_DSTPT_y"]][yr, run] * cost[cost$uid == uid, "c_a_TPT_DS"] + # Cost of DS TPT
             data_temp[["n_HHC_DRTPT_y"]][yr, run] * cost[cost$uid == uid, "c_a_TPT_DR"]) * # Cost of DR TPT
          (1 + he_temp[["disc_rate"]]["disc", ])^(-(yr - 1)) # Discounting
      }
    }
    
    # Summarise c_Int by column
    cea[[iso]][[intv]][["c_Int"]] <- matrix(colSums(cea[[iso]][[intv]][["c_Int"]]), nrow = 1)
  }
  
  if (intv == "VAX") {
    for (run in 1:n_runs) {
      uid <- uids[run]
      for (yr in 1:n_years) {
        # Compute c_Int for each year and run
        cea[[iso]][[intv]][["c_Int"]][yr, run] <- 
          (data_temp[["n_assess_y"]][yr, run] * cost[cost$uid == uid, "c_a_diag_standard"] + # Cost of standard evaluation
             data_temp[["n_treat_DS_y"]][yr, run] * cost[cost$uid == uid, "c_a_treat_DS_standard"] + # Cost of DS treatment
             data_temp[["n_treat_DR_y"]][yr, run] * cost[cost$uid == uid, "c_a_treat_DR_standard"] + # Cost of DR treatment
             data_temp[["n_vaccinated_y"]][yr, run] * cost[cost$uid == uid, "c_a_vax"]) * # Cost of vaccination
          (1 + he_temp[["disc_rate"]]["disc", ])^(-(yr - 1)) # Discounting
      }
    }
    
    # Summarise c_Int by column
    cea[[iso]][[intv]][["c_Int"]] <- matrix(colSums(cea[[iso]][[intv]][["c_Int"]]), nrow = 1)
  }
  
  cea[[iso]][[intv]] <- list(
    c_Int = cea[[iso]][[intv]][["c_Int"]],
    DALY = cea[[iso]][[intv]][["DALY"]],
    QALY = cea[[iso]][[intv]][["QALY"]]
  )
  
  print(paste0(iso, " | Finished: ", intv))
}

# Overall calculations
for (intv in 1:length(intvs)) {
  intv <- intvs[intv]
  cea_sum[[iso]][[intv]][["Inc_costs"]] <- matrix(0, nrow = 1, ncol = n_runs) # Incremental costs
  cea_sum[[iso]][[intv]][["DALY_avert"]] <- matrix(0, nrow = 1, ncol = n_runs) # DALYs averted
  cea_sum[[iso]][[intv]][["QALY_gains"]] <- matrix(0, nrow = 1, ncol = n_runs) # QALYs gained
  cea_sum[[iso]][[intv]][["ICERs"]] <- matrix(0, nrow = 1, ncol = n_runs) # ICERs
  cea_sum[[iso]][[intv]][["NB_low"]] <- matrix(0, nrow = 1, ncol = n_runs) # Net benefit (low CET)
  cea_sum[[iso]][[intv]][["NB_high"]] <- matrix(0, nrow = 1, ncol = n_runs) # Net benefit (high CET)
  
  for (run in 1:n_runs) {
    cea_sum[[iso]][[intv]][["Inc_costs"]][, run] <- cea[[iso]][[intv]][["c_Int"]][, run] - cea[[iso]][["BAU"]][["c_Int"]][, run]
    cea_sum[[iso]][[intv]][["DALY_avert"]][, run] <- cea[[iso]][["BAU"]][["DALY"]][, run] - cea[[iso]][[intv]][["DALY"]][, run]
    cea_sum[[iso]][[intv]][["QALY_gains"]][, run] <- cea[[iso]][[intv]][["QALY"]][, run] - cea[[iso]][["BAU"]][["QALY"]][, run]
    cea_sum[[iso]][[intv]][["ICERs"]][, run] <- cea_sum[[iso]][[intv]][["Inc_costs"]][, run] / cea_sum[[iso]][[intv]][["DALY_avert"]][, run]
    cea_sum[[iso]][[intv]][["ICERs"]][, run] <- ifelse(is.finite(cea_sum[[iso]][[intv]][["ICERs"]][, run]), cea_sum[[iso]][[intv]][["ICERs"]][, run], NA)
    cea_sum[[iso]][[intv]][["NB_low"]][, run] <- cea_sum[[iso]][[intv]][["DALY_avert"]][, run] - (cea_sum[[iso]][[intv]][["Inc_costs"]][, run] / he_temp[["cet"]]["cet_low", ])
    cea_sum[[iso]][[intv]][["NB_low"]][, run] <- ifelse(is.finite(cea_sum[[iso]][[intv]][["NB_low"]][, run]), cea_sum[[iso]][[intv]][["NB_low"]][, run], NA)
    cea_sum[[iso]][[intv]][["NB_high"]][, run] <- cea_sum[[iso]][[intv]][["DALY_avert"]][, run] - (cea_sum[[iso]][[intv]][["Inc_costs"]][, run] / he_temp[["cet"]]["cet_high", ])
    cea_sum[[iso]][[intv]][["NB_high"]][, run] <- ifelse(is.finite(cea_sum[[iso]][[intv]][["NB_high"]][, run]), cea_sum[[iso]][[intv]][["NB_high"]][, run], NA)
  }
}
