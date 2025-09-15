# ----------------------------------------------------
# Running the R scripts with the CORE model plots (NIH2_GeneralPlots.R) and the RISK specific plots
# (NIH2_RISKplots_ISO.R) and saving the outputs
# Elena Venero Garcia (with some code from Hira Tanvir)
#-----------------------------------------------------

################### Steps required before running the script ###################

# 1. Download the target file for your country (and save it in the parameter file of your country)
# 2. Check that in the XML in detailed.output:
# - The age.group.lower.limits = "0,15,50" and not "0,15" (to be able to calculate annual risk 
# of infection by the age brackets specified in the document) 
# 3. Save the 3 R scripts in the same folder with a R project
# 4. Specify the ISO (3 letter code) of your country (line 45) 
# 5. Specify the XML file name (used on line 46)
# ----------------------------------------------------
#### Currently using version 3.7.1

##############################################################################

# load packages
suppressPackageStartupMessages({
  rm(list=ls())
  #library(tbmodepitest)
  library(tbmoddev)
  library(data.table)
  library(ggplot2)
  library(cowplot)
  library(patchwork)
  library(dplyr)
  library(stringr)
  library(reshape2)
  library(tidyverse)
  library(here)
  #library(rio)
  library(readxl)
  library(grid)
  library(gridExtra)
  library(gtable)
  library(png)
  
  theme_set(theme_minimal_grid() + panel_border(color = "black"))
})

#Specify ISO
ISO <- "ZAF"
#XML_NAME <- "NIH2-CORE-COMPv1.2_age15to99_20240902.xml"
XML_NAME <- "NIH2-CORE-COMPv1.2_20240902.xml"

#ISO <- "BRA"
#XML_NAME <- "XML_NIH2-CORE-COMPv1.2-02_09_v2_mornmeeting.xml"

#ISO <- "IND"
#XML_NAME <- "NIH2-CORE-COMPv1.2withNUT.xml"




if (!exists("ISO") || !is.character(ISO) || nchar(ISO) != 3) {
  stop("You must specify and run ISO (with a 3-letter code) before running this script.")
}

# set.paths = initialize the params
paths = set.paths(countries   = "countries", 
                  countrycode = paste(ISO), 
                  xml         = XML_NAME)
                 # parameters  = "input_CORE-COMPv1.2_HIV_20240829.csv",
                 #targets = "ZAF_calibration_targets.csv")

# Run the model, sample parameters from input.csv
output = run(paths, sample.parameters = F, output.flows = F, write.to.file = F)

#Load target file
targets <- read.csv(paste("countries/",ISO,"/parameters/",ISO,"_targets.csv", sep = ""))

# Maybe use this for automated calibration (Hira Tanvir's code) --------------------------------
## Read in data
#IND_params = fread(here("processing_files/param_sets/IND_params.csv"))
#target_hits = fread(here("countries/IND/parameters/target_count.csv"))


## Run the model for each param set that hit max targets
#stocks <- list()
#hits <- list()
#for(i in 1:nrow(IND_params)){
  
#  params = IND_params[i,]
#  params_uid = params$uid
  # params = params[, !c("uid", "nhits")] # for param.csv produced from automated calibration
#  params = unlist(params)
  
#  paths = set.paths(countries   = "countries",
#                    countrycode = "IND",
#                    xml         = "XMLinput_plot.xml",
#                    parameters  = "input_all.csv",
#                    targets = "target_count.csv")
  
  ## Run the model, sample parameters from the input.csv each time
#  output <- run(paths, new.parameter.values = params, output.flows = F)
  
#  counts <- output$stocks
#  target_hits <- as.data.table(output$hits)
#  stocks[[i]] <- counts[, `:=`(UID = params_uid)]
#  hits[[i]] <- target_hits[, `:=`(UID = params_uid)]
#}

## Combine stocks into one data.frame
#stock_data = rbindlist(stocks)
#target_hits = rbindlist(hits)

## sum number of hits for each UID
#target_hits[, nhits := sum(fit), by = UID]

# -------------------------------------------------------------------------

# Get the current date in the desired format (e.g., "20240807")
current_date <- format(Sys.Date(), "%Y%m%d")

# Create a unique identifier based on the current time (e.g., "140512" for 14:05:12)
time_id <- format(Sys.time(), "%H%M%S")

# Create an environment and pass the ISO variable to it
env1 <- new.env()
assign("ISO", ISO, envir = env1)
assign("output", output, envir = env1)
assign("targets", targets, envir = env1)


env2 <- new.env()
assign("ISO", ISO, envir = env2)
assign("output", output, envir = env2)
assign("targets", targets, envir = env2)

# Source the first script and capture the plot
source("NIH2_GeneralPlots.R", local = env1)

plot_cali1 <- get("plot_TBprev_pop", envir = env1)
plot_cali2 <- get("plot_Dc_inc_pop", envir = env1)
plot_cali3 <- get("plot_Not_pop", envir = env1)
plot_cali4 <- get("plot_mort_pop", envir = env1)
plot_cali5 <- get("plot_Ds_pct", envir = env1)
plot_cali6 <- get("plot_MDR", envir = env1)

plot_analytic1 <- get("plot_TBprev_pop_age", envir = env1)
plot_analytic2 <- get("plot_mort_pop_age", envir = env1)
plot_analytic3 <- get("plot_ARI_s", envir = env1)
plot_analytic4 <- get("plot_ARI_r", envir = env1)
plot_analytic5 <- get("plot_inf_prev", envir = env1)
plot_analytic6 <- get("plot_ratio", envir = env1)
plot_analytic7 <- get("plot_Tsuccess_age_ST", envir = env1)
plot_analytic8 <- get("plot_Tsuccess_age_RT", envir = env1)
plot_analytic9 <- get("plot_population", envir = env1)
plot_analytic10 <- get("plot_mort_prop", envir = env1)
plot_analytic11 <- get("plot_mort_age_prop", envir = env1)


if ("uid" %in% names(output$stocks)) {
  # Plot if UID is present
  plots_analytics <- "Too many lines in one plot showing different things, not just id"
} else {
  plots_analytics <- (plot_analytic1 + plot_analytic2 + plot_analytic3) / 
    ( plot_analytic4 + plot_analytic5 + plot_analytic6)/ 
    (plot_analytic7 + plot_analytic8 + plot_analytic9)/
    (plot_analytic10 + plot_analytic11 + plot_spacer()) +
    plot_layout(guides = 'collect') +
    plot_annotation(title = paste(ISO),
                    theme = theme(plot.title = element_text(hjust = 0.5, size = 14)))
  # Analytic graphs
  file_path_analytics <- paste("countries/",ISO,"/output/Manual calibration/",ISO,"_Figures_Analytics_",current_date,"_",time_id,".png", sep="")
  ggsave(filename=file_path_analytics,plot=plots_analytics,width=29.7,height=28, unit="cm", bg = "white")
  
}

# Source the second script and capture the plot
source(paste("NIH2_RISKplots_",ISO,".R", sep = ""), local = env2)

if ("uid" %in% names(output$stocks)) {
  if (ISO == "ZAF") {
    plot_cali_risk1 <- get("plot_TB_risk1", envir = env2)
    plot_cali_risk2 <- get("plot_TB_risk2", envir = env2)
    plot_cali_risk3 <- get("plot_TB_risk3", envir = env2)
    plot_cali_risk4 <- get("plot_TB_risk4", envir = env2)
    
    plots_calibration <- plot_cali1 + plot_cali2 + plot_cali3 +
      plot_cali4 + plot_cali5 + plot_cali6 +
      plot_annotation(title = paste(ISO),
                      theme = theme(plot.title = element_text(hjust = 0.5, size = 14)))
    
    plot_analytic_risk1 <- get("plot_risk1", envir = env2)
    plot_analytic_risk2 <- get("plot_risk2", envir = env2)
    plot_analytic_risk3 <- get("plot_risk3", envir = env2)
    
    plots_risks <- (plot_cali_risk1 + plot_cali_risk2 + plot_cali_risk3)/
      (plot_cali_risk4 + plot_analytic_risk1 + plot_analytic_risk2)/
      (plot_analytic_risk3 + plot_spacer() + plot_spacer()) +
      plot_layout(guides = 'collect') +
      plot_annotation(title = paste(ISO),
                      theme = theme(plot.title = element_text(hjust = 0.5, size = 14)))
    # Save graphs
    # General Calibration graphs
    file_path_cali <- paste("countries/",ISO,"/output/Automated calibration/",ISO,"_Figures_Calibration_General_",current_date,"_",time_id,".png", sep="")
    ggsave(filename=file_path_cali,plot=plots_calibration,width=29.7,height=28, unit="cm", bg = "white")
    
    
  } else if (ISO == "BRA"){
    plot_cali_risk1 <- get("plot_TB_risk1", envir = env2)
    plot_cali_risk2 <- get("plot_TB_risk2", envir = env2)
    
    plots_calibration <- plot_cali1 + plot_cali2 + plot_cali3 +
      plot_cali4 + plot_cali5 + plot_cali6 +
      plot_annotation(title = paste(ISO),
                      theme = theme(plot.title = element_text(hjust = 0.5, size = 14)))
    
    plot_analytic_risk1 <- get("plot_risk1", envir = env2)
    plot_analytic_risk2 <- get("plot_risk2", envir = env2)
    plot_analytic_risk3 <- get("plot_risk3", envir = env2)
    
    plots_risks <- (plot_cali_risk1 + plot_cali_risk2 + plot_spacer())/
      (plot_analytic_risk1 + plot_analytic_risk2 + plot_spacer())/
      (plot_analytic_risk3) +
      plot_layout(guides = 'collect') +
      plot_annotation(title = paste(ISO),
                      theme = theme(plot.title = element_text(hjust = 0.5, size = 14)))
    # Save graphs
    # General Calibration graphs
    file_path_cali <- paste("countries/",ISO,"/output/Automated calibration/",ISO,"_Figures_Calibration_General_",current_date,"_",time_id,".png", sep="")
    ggsave(filename=file_path_cali,plot=plots_calibration,width=29.7,height=21, unit="cm", bg = "white")
    
    
  } else {
    plot_cali_risk1 <- get("plot_TB_risk1", envir = env2)
    
    plots_calibration <- plot_cali1 + plot_cali2 + plot_cali3 +
      plot_cali4 + plot_cali5 + plot_cali6 +
      plot_annotation(title = paste(ISO),
                      theme = theme(plot.title = element_text(hjust = 0.5, size = 14)))
    
    plot_analytic_risk1 <- get("plot_risk1", envir = env2)
    
    plots_risks <- (plot_cali_risk1 + plot_spacer() + plot_spacer())/
      (plot_analytic_risk1 + plot_spacer() + plot_spacer())/
      (plot_spacer() + plot_spacer() + plot_spacer())+
      plot_layout(guides = 'collect') +
      plot_annotation(title = paste(ISO),
                      theme = theme(plot.title = element_text(hjust = 0.5, size = 14)))
    
    # Save graphs
    # General Calibration graphs
    file_path_cali <- paste("countries/",ISO,"/output/Automated calibration/",ISO,"_Figures_Calibration_General_",current_date,"_",time_id,".png", sep="")
    ggsave(filename=file_path_cali,plot=plots_calibration,width=29.7,height=21, unit="cm", bg = "white")
    
    
  } 
  
  # Risk graphs
  file_path_risks <- paste("countries/",ISO,"/output/Automated calibration/",ISO,"_Figures_Calibration_Risk_",current_date,"_",time_id,".png", sep="")
  ggsave(filename=file_path_risks,plot=plots_risks,width=29.7,height=21, unit="cm", bg = "white")
  
  
# If no UID
} else {
  if (ISO == "ZAF") {
    plot_cali_risk1 <- get("plot_TB_risk1", envir = env2)
    plot_cali_risk2 <- get("plot_TB_risk2", envir = env2)
    plot_cali_risk3 <- get("plot_TB_risk3", envir = env2)
    plot_cali_risk4 <- get("plot_TB_risk4", envir = env2)
    plot_cali_risk5 <- get("plot_TB_risk5", envir = env2)
    plot_cali_risk6 <- get("plot_TB_risk6", envir = env2)
    
    plots_calibration <- (plot_cali1 + plot_cali2 + plot_cali3)/
      (plot_cali4 + plot_cali5 + plot_cali6)/
      (plot_cali_risk1 + plot_cali_risk2 + plot_cali_risk3)/
      (plot_cali_risk4 + plot_cali_risk5 + plot_cali_risk6)+
      plot_layout(guides = 'collect') +
      plot_annotation(title = paste(ISO),
                      theme = theme(plot.title = element_text(hjust = 0.5, size = 14)))
    
    plot_analytic_risk1 <- get("plot_risk1", envir = env2)
    plot_analytic_risk2 <- get("plot_risk2", envir = env2)
    plot_analytic_risk3 <- get("plot_risk3", envir = env2)
    
    plots_risks <- (plot_analytic_risk1 + plot_analytic_risk2 + plot_analytic_risk3)/
      (plot_spacer() + plot_spacer() + plot_spacer()) +
      plot_layout(guides = 'collect') +
      plot_annotation(title = paste(ISO),
                      theme = theme(plot.title = element_text(hjust = 0.5, size = 14)))
    #Save graphs
    # Calibration graphs
    file_path_cali <- paste("countries/",ISO,"/output/Manual calibration/",ISO,"_Figures_Calibration_",current_date,"_",time_id,".png", sep="")
    ggsave(filename=file_path_cali,plot=plots_calibration,width=29.7,height=28, unit="cm", bg = "white")
    
  } else if (ISO == "BRA"){
    plot_cali_risk1 <- get("plot_TB_risk1", envir = env2)
    plot_cali_risk2 <- get("plot_TB_risk2", envir = env2)
    
    plots_calibration <- (plot_cali1 + plot_cali2 + plot_cali3)/
      (plot_cali4 + plot_cali5 + plot_cali6)/
      (plot_cali_risk1 + plot_cali_risk2 + plot_spacer()) + 
      plot_layout(guides = 'collect') +
      plot_annotation(title = paste(ISO),
                    theme = theme(plot.title = element_text(hjust = 0.5, size = 14)))
    
    plot_analytic_risk1 <- get("plot_risk1", envir = env2)
    plot_analytic_risk2 <- get("plot_risk2", envir = env2)
    plot_analytic_risk3 <- get("plot_risk3", envir = env2)
    
    plots_risks <- (plot_analytic_risk1 + plot_analytic_risk2 + plot_spacer()) / (plot_analytic_risk3) +
      plot_layout(guides = 'collect') +
      plot_annotation(title = paste(ISO),
                      theme = theme(plot.title = element_text(hjust = 0.5, size = 14)))
    
    #Save graphs
    # Calibration graphs
    file_path_cali <- paste("countries/",ISO,"/output/Manual calibration/",ISO,"_Figures_Calibration_",current_date,"_",time_id,".png", sep="")
    ggsave(filename=file_path_cali,plot=plots_calibration,width=29.7,height=21, unit="cm", bg = "white")
    
  } else {
    
      plots_calibration <- (plot_cali1 + plot_cali2 + plot_cali3)/
      (plot_cali4 + plot_cali5 + plot_cali6)/
      (plot_spacer() + plot_spacer() + plot_spacer()) +
      plot_layout(guides = 'collect') +
      plot_annotation(title = paste(ISO),
                    theme = theme(plot.title = element_text(hjust = 0.5, size = 14)))
    
      plot_analytic_risk1 <- get("plot_TB_risk1", envir = env2)
      plot_analytic_risk2 <- get("plot_risk1", envir = env2)
    
      plots_risks <- (plot_analytic_risk1 + plot_analytic_risk2 + plot_spacer())/ 
        (plot_spacer() + plot_spacer() + plot_spacer())+
        plot_layout(guides = 'collect') +
        plot_annotation(title = paste(ISO),
                      theme = theme(plot.title = element_text(hjust = 0.5, size = 14)))
      
      #Save graphs
      # Calibration graphs
      file_path_cali <- paste("countries/",ISO,"/output/Manual calibration/",ISO,"_Figures_Calibration_",current_date,"_",time_id,".png", sep="")
      ggsave(filename=file_path_cali,plot=plots_calibration,width=29.7,height=21, unit="cm", bg = "white")
    
  } 
  
  # Risk graphs
  file_path_risks <- paste("countries/",ISO,"/output/Manual calibration/",ISO,"_Figures_Risk_",current_date,"_",time_id,".png", sep="")
  ggsave(filename=file_path_risks,plot=plots_risks,width=29.7,height=14, unit="cm", bg = "white")
  
}


