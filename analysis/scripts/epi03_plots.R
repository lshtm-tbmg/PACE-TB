#------------------------------------------------------------------------------#
# PACE-TB - Epidemiological impact                                             #
# Produce epidemiological plots and tables                                     #
# Code by Katherine C. Horton (katherine.horton@lshtm.ac.uk)                   #
# Last updated 2025-07-15 by KCH                                               #
#------------------------------------------------------------------------------#

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
  library(ggplot2)
  library(ggbreak)
  library(gt)
  library(chromote)
  library(patchwork)
})

# Load the file
episumm <- read_fst(here('outputs', 'res', 'summ', "episumm.fst"))

# Set colours and labels
intv_col <- c("BAU" = "black", "VAX" = "#537D8D", "TPT" = "#CB4C15", 
              "SCR" = "#754668", "DGN" = "#CBA715", "DST" = "#1F4E79", 
              "SDS" = "#AE0D0A", "SDR" = "#2C6E49", "PRI" = "#545454", 
              "NTN" = "#8A8A8A") 
intv_ord  <- c("VAX", "TPT", "SCR", "DGN", "DST", "SDS", "SDR", "PRI", "NTN")
intv_name <- c("Vacc", "TPT", "Comm\nScr", "Impr\nDiag", "DST\nfor all", 
               "Short\nDS", "Short\nDR", "Pri\nScr", "Nutr")
intv_col2 <- c("BAU" = "black", "Vacc" = "#537D8D", "TPT" = "#CB4C15", 
               "Comm Scr" = "#754668", "Impr Diag" = "#CBA715", "DST for all" = "#1F4E79", 
               "Short DS" = "#AE0D0A", "Short DR" = "#2C6E49", "Pri Scr" = "#545454", "Nutr" = "#8A8A8A") 
intv_ord2  <- c("BAU","VAX", "TPT", "SCR", "DGN", "DST", "SDS", "SDR", "PRI", "NTN")
intv_name2 <- c("BAU", "Vacc", "TPT", "Comm Scr", "Impr Diag", "DST for all", 
               "Short DS", "Short DR", "Pri Scr", "Nutr")

## EPI1 - PREVALENCE ## 

# Epi1Fig1: Proportional decline in infectious TB prevalence in 2050 (relative to BAU)    
  png(here('outputs', 'res', 'plots', paste("epi1fig1_prev_",format(Sys.time(), "%Y%m%d_%H-%M"),".png")), width = 16, height = 7, units = 'in', res = 1000)
  ggplot(filter(episumm, var == "prev", intv != "BAU")) +
    facet_grid(~factor(iso,levels=c("BRA","IND","ZAF")), space = "free", scales = "free_x", labeller = as_labeller(c("BRA" = "Brazil", "IND" = "India", "ZAF" = "South Africa"))) +
    geom_col(aes(x = factor(intv, levels = intv_ord, labels = intv_name), y = -med, fill = intv), position = position_dodge(width = 1)) + 
    geom_errorbar(aes(x = factor(intv, levels = intv_ord, labels = intv_name), ymin = -upp, ymax = -low), 
                  position = position_dodge(width = 1), width = 0.5, size = 0.5) +
    geom_hline(yintercept = 0, size = 1) +
    scale_fill_manual(values = intv_col) +
    scale_x_discrete(position = "top") +
    scale_y_continuous(#lim = c(-0.5, 0), 
                       expand=c(0, 0),
                       labels = scales::label_number(scale = 100, suffix = '%')) +
    labs(x = NULL, y = "Proportion relative to BAU", fill = "Intervention") +
    theme_classic(base_size = 18) +
    theme(plot.title = element_text(hjust = 0.55),
          legend.position = "none",
          axis.ticks.x = element_blank(),
          axis.line.x = element_line(colour = "white", size = 0),
          strip.placement = "outside", 
          strip.background.x = element_rect(color = "white", fill = NULL), 
          strip.text = element_text(size = 18))#,
          #text = element_text(family = "Open sans"))
  dev.off()
   
# Epi1Fig2: Proportional decline in infectious TB prevalence in 2050 (relative to BAU) by intervention
  png(here('outputs', 'res', 'plots', paste("epi1fig2_prevbyint_",format(Sys.time(), "%Y%m%d_%H-%M"),".png")), width = 16, height = 7, units = 'in', res = 1000)
  ggplot(filter(episumm, var == "prev", intv != "BAU")) +
    facet_grid(~factor(intv, levels = intv_ord, labels = intv_name), space = "free", scales = "free_x") + #, labeller = as_labeller(c("BRA" = "Brazil", "IND" = "India", "ZAF" = "South Africa"))) +
    geom_col(aes(x = factor(iso,levels=c("BRA","IND","ZAF")), y = -med, fill = intv), position = position_dodge(width = 1)) + 
    geom_errorbar(aes(x = factor(iso,levels=c("BRA","IND","ZAF")), ymin = -upp, ymax = -low), 
                  position = position_dodge(width = 1), width = 0.5, size = 0.5) +
    geom_hline(yintercept = 0, size = 1) +
    scale_fill_manual(values = intv_col) +
    scale_x_discrete(position = "top") +
    scale_y_continuous(lim = c(-0.55, 0), expand=c(0, 0),
                       labels = scales::label_number(scale = 100, suffix = '%')) +
    labs(x = NULL, y = "Proportion relative to BAU", fill = "Intervention") +
    theme_classic(base_size = 18) +
    theme(plot.title = element_text(hjust = 0.5),
          legend.position = "none",
          axis.ticks.x = element_blank(),
          axis.line.x = element_line(colour = "white", size = 0),
          strip.placement = "outside", 
          strip.background.x = element_rect(color = "white", fill = NULL), 
          strip.text = element_text(size = 18))#,
          #text = element_text(family = "Open sans"))
  dev.off()
      
# Epi1Tab1: Proportional decline in infectious TB prevalence in 2050 (relative to BAU)    
  prevs <- episumm %>%
    filter(var == "prev") %>%
    select(iso, intv, med, low, upp)
  
  prevt <- reshape(prevs, idvar = "intv", timevar = "iso", direction = "wide")
  for (i in 1:nrow(prevt)){
    prevt$BRA[i] <- paste(round(prevt$med.BRA[i]*100, digits = 1), "%", "\n(", 
                          round(prevt$low.BRA[i]*100, digits = 1), "-",
                          round(prevt$upp.BRA[i]*100, digits = 1), "%", ")", sep = "")
    prevt$IND[i] <- paste(round(prevt$med.IND[i]*100, digits = 1), "%", "\n(", 
                          round(prevt$low.IND[i]*100, digits = 1), "-",
                          round(prevt$upp.IND[i]*100, digits = 1), "%", ")", sep = "")
    prevt$ZAF[i] <- paste(round(prevt$med.ZAF[i]*100, digits = 1), "%", "\n(", 
                          round(prevt$low.ZAF[i]*100, digits = 1), "-",
                          round(prevt$upp.ZAF[i]*100, digits = 1), "%", ")", sep = "")
  }
  
  prevt2 <- prevt %>%
    filter(!(str_detect(intv, 'BAU'))) %>%
    select(intv, BRA, IND, ZAF) %>%
    arrange(factor(intv, levels = intv_ord)) %>%
    mutate(intvn = intv_name) %>%
    mutate(BRA = ifelse(BRA == 'NA%\n(NA-NA%)', '-', BRA)) %>%
    mutate(IND = ifelse(IND == 'NA%\n(NA-NA%)', '-', IND)) %>%
    mutate(ZAF = ifelse(ZAF == 'NA%\n(NA-NA%)', '-', ZAF)) %>%
    relocate(5) %>%
    select(intvn, BRA, IND, ZAF)
  colnames(prevt2) <- c("Intervention", "Brazil", "India", "South Africa")
  
  prevt2 %>% 
    gt() %>% 
    tab_header(title = md("Decline in infectious TB prevalence in 2050<br>(percent relative to BAU)")) %>%
    cols_align(align = "center") %>%
    cols_width(everything() ~ px(200)) %>%
    cols_width(Intervention ~ px(150)) %>%
    opt_table_font(font = 'Open sans') %>%
    gtsave(filename = here('outputs', 'res', 'plots', paste("epi1tab1_prev_",format(Sys.time(), "%Y%m%d_%H-%M"),".png")))

## EPI2 - INCIDENT EPISODES ##
    
# Epi2Fig1: Incident symptomatic TB episodes averted by 2050 (relative to BAU)
  png(here('outputs', 'res', 'plots', paste("epi2fig1_inc_",format(Sys.time(), "%Y%m%d_%H-%M"),".png")), width = 16, height = 7, units = 'in', res = 1000)
  ggplot(filter(episumm, var == "inc", intv != "BAU")) +
    facet_grid(~factor(iso,levels=c("BRA","IND","ZAF")), space = "free", scales = "free_x", labeller = as_labeller(c("BRA" = "Brazil", "IND" = "India", "ZAF" = "South Africa"))) +
    geom_col(aes(x = factor(intv, levels = intv_ord, labels = intv_name), y = med, fill = intv), position = position_dodge(width = 1)) + 
    geom_errorbar(aes(x = factor(intv, levels = intv_ord, labels = intv_name), ymin = low, ymax = upp), 
                  position = position_dodge(width = 1), width = 0.5, size = 0.5) +
    geom_hline(yintercept = 0, size = 1) +
    scale_fill_manual(values = intv_col) +
    scale_x_discrete(position = "bottom") +
    scale_y_continuous(expand = c(0, 0), labels = scales::label_number(scale = 1e-6, suffix = 'M')) +
    labs(x = NULL, y = "Number relative to BAU", fill = "Intervention") +
    theme_classic(base_size = 18) +
    theme(plot.title = element_text(hjust = 0.5),
          legend.position = "none",
          axis.ticks.x = element_blank(),
          axis.line.x = element_line(colour = "white", size = 0),
          strip.placement = "outside", 
          strip.background.x = element_rect(color = "white", fill = NULL), 
          strip.text = element_text(size = 18))#,
          #text = element_text(family = "Open sans"))
  dev.off()

# Epi2Fig2: Incident symptomatic TB episodes averted by 2050 (relative to BAU) 
  episummtemp <- episumm
  
  high_inc <- which(episummtemp$var == "inc" & episummtemp$med > 1800000)
  
  if (length(high_inc) > 0) {
    og_values <- episummtemp$med[high_inc]
    episummtemp$med[high_inc] <- 1750000
    episummtemp$low[high_inc] <- NA
    episummtemp$upp[high_inc] <- NA
    
    intv_mapping <- setNames(seq_along(intv_ord), intv_ord)
    
    anntext <- data.frame(x = intv_mapping[episummtemp$intv[high_inc]], y = 1800000,
                          label = paste0(format(round(og_values / 1e6, digits = 1), big.mark = ",", scientific = FALSE), "M"), 
                          iso = episummtemp$iso[high_inc])
    
    anntext["NTN","x"] <- anntext["NTN","x"]-1 # Quick fix to align NTN
    
  } else {
    anntext <- data.frame(x = character(0), y = numeric(0), label = character(0), iso = character(0))
  }
  
  png(here('outputs', 'res', 'plots', paste("epi2fig2_inc_",format(Sys.time(), "%Y%m%d_%H-%M"),".png")), width = 16, height = 7, units = 'in', res = 1000)
  ggplot(filter(episummtemp, var == "inc", intv != "BAU")) +
    facet_grid(~factor(iso,levels=c("BRA","IND","ZAF")), space = "free", scales = "free_x", labeller = as_labeller(c("BRA" = "Brazil", "IND" = "India", "ZAF" = "South Africa"))) +
    geom_col(aes(x = factor(intv, levels = intv_ord, labels = intv_name), y = med, fill = intv), position = position_dodge(width = 1)) + 
    geom_errorbar(aes(x = factor(intv, levels = intv_ord, labels = intv_name), ymin = low, ymax = upp), 
                  position = position_dodge(width = 1), width = 0.5, size = 0.5) +
    geom_hline(yintercept = 0, size = 1) +
    scale_fill_manual(values = intv_col) +
    scale_x_discrete(position = "bottom") +
    scale_y_continuous(lim = c(0, 1.85e6), expand=c(0, 0),
                       labels = scales::label_number(scale = 1e-6, suffix = 'M')) +
    geom_text(data = anntext, aes(x = x, y = y, label = label), inherit.aes = FALSE) +
    labs(x = NULL, y = "Number relative to BAU", fill = "Intervention") +
    theme_classic(base_size = 18) +
    theme(plot.title = element_text(hjust = 0.5),
          legend.position = "none",
          axis.ticks.x = element_blank(),
          axis.line.x = element_line(colour = "white", size = 0),
          strip.placement = "outside", 
          strip.background.x = element_rect(color = "white", fill = NULL), 
          strip.text = element_text(size = 18))
  dev.off()
        
# Epi2Fig3: Proportion of incident symptomatic TB episodes (relative to BAU)
  png(here('outputs', 'res', 'plots', paste("epi2fig3_",format(Sys.time(), "%Y%m%d_%H-%M"),"incpct.png")), width = 16, height = 7, units = 'in', res = 1000)
  ggplot(filter(episumm, var == "incpct", intv != "BAU")) +
    facet_grid(~factor(iso,levels=c("BRA","IND","ZAF")), space = "free", scales = "free_x", labeller = as_labeller(c("BRA" = "Brazil", "IND" = "India", "ZAF" = "South Africa"))) +
    geom_col(aes(x = factor(intv, levels = intv_ord, labels = intv_name), y = med, fill = intv), position = position_dodge(width = 1)) + 
    geom_errorbar(aes(x = factor(intv, levels = intv_ord, labels = intv_name), ymin = low, ymax = upp), 
                  position = position_dodge(width = 1), width = 0.5, size = 0.5) +
    geom_hline(yintercept = 0, size = 1) +
    scale_fill_manual(values = intv_col) +
    scale_x_discrete(position = "bottom") +
    scale_y_continuous(expand = c(0, 0),
                       labels = scales::label_number(scale = 100, suffix = '%')) +
    labs(x = NULL, y = "Percent relative to BAU", fill = "Intervention") +
    theme_classic(base_size = 18) +
    theme(plot.title = element_text(hjust = 0.5),
          legend.position = "none",
          axis.ticks.x = element_blank(),
          axis.line.x = element_line(colour = "white", size = 0),
          strip.placement = "outside", 
          strip.background.x = element_rect(color = "white", fill = NULL), 
          strip.text = element_text(size = 18))
  dev.off()
        
# Epi2Fig4: Proportion of incident symptomatic TB episodes (relative to BAU) by intervention 
  png(here('outputs', 'res', 'plots', paste("epi2fig4_incpctbyint_",format(Sys.time(), "%Y%m%d_%H-%M"),".png")), width = 16, height = 7, units = 'in', res = 1000)
  ggplot(filter(episumm, var == "incpct", intv != "BAU")) +
    facet_grid(~factor(intv, levels = intv_ord, labels = intv_name), space = "free", scales = "free_x") +
    geom_col(aes(x = factor(iso,levels=c("BRA","IND","ZAF")), y = med, fill = intv), position = position_dodge(width = 1)) + 
    geom_errorbar(aes(x = factor(iso,levels=c("BRA","IND","ZAF")), ymin = low, ymax = upp), 
                  position = position_dodge(width = 1), width = 0.5, size = 0.5) +
    geom_hline(yintercept = 0, size = 1) +
    scale_fill_manual(values = intv_col) +
    scale_x_discrete(position = "bottom") +
    scale_y_continuous(expand = c(0, 0),
                       labels = scales::label_number(scale = 100, suffix = '%')) +
    labs(x = NULL, y = "Percent relative to BAU", fill = "Intervention") +
    theme_classic(base_size = 18) +
    theme(plot.title = element_text(hjust = 0.5),
          legend.position = "none",
          axis.ticks.x = element_blank(),
          axis.line.x = element_line(colour = "white", size = 0),
          strip.placement = "outside", 
          strip.background.x = element_rect(color = "white", fill = NULL), 
          strip.text = element_text(size = 18))
  dev.off()
        
# Epi2Tab1: Incident symptomatic TB episodes averted by 2050 (relative to BAU) 
  incs <- episumm %>%
    filter(var == "inc") %>%
    select(iso, intv, med, low, upp)
  
  inct <- reshape(incs, idvar = "intv", timevar = "iso", direction = "wide")
  for (i in 1:nrow(inct)){
    inct$BRA[i] <- paste(format(round(inct$med.BRA[i] * 1e-3, digits = 0), big.mark = ",", scientific = FALSE), "K\n(",
                         format(round(inct$low.BRA[i] * 1e-3, digits = 0), big.mark = ",", scientific = FALSE), "-",
                         format(round(inct$upp.BRA[i] * 1e-3, digits = 0), big.mark = ",", scientific = FALSE), "K)", sep = "")
    inct$IND[i] <- paste(format(round(inct$med.IND[i] * 1e-3, digits = 0), big.mark = ",", scientific = FALSE), "K\n(",
                         format(round(inct$low.IND[i] * 1e-3, digits = 0), big.mark = ",", scientific = FALSE), "-",
                         format(round(inct$upp.IND[i] * 1e-3, digits = 0), big.mark = ",", scientific = FALSE), "K)", sep = "")
    inct$ZAF[i] <- paste(format(round(inct$med.ZAF[i] * 1e-3, digits = 0), big.mark = ",", scientific = FALSE), "K\n(",
                         format(round(inct$low.ZAF[i] * 1e-3, digits = 0), big.mark = ",", scientific = FALSE), "-",
                         format(round(inct$upp.ZAF[i] * 1e-3, digits = 0), big.mark = ",", scientific = FALSE), "K)", sep = "")
  }
  
  inct2 <- inct %>%
    filter(!(str_detect(intv, 'BAU'))) %>%
    select(intv, BRA, IND, ZAF) %>%
    arrange(factor(intv, levels = intv_ord)) %>%
    mutate(intvn = intv_name) %>%
    mutate(BRA = ifelse(BRA == 'NAK\n(NA-NAK)', '-', BRA)) %>%
    mutate(IND = ifelse(IND == 'NAK\n(NA-NAK)', '-', IND)) %>%
    mutate(ZAF = ifelse(ZAF == 'NAK\n(NA-NAK)', '-', ZAF)) %>%
    relocate(5) %>%
    select(intvn, BRA, IND, ZAF)
  colnames(inct2) <- c("Intervention", "Brazil", "India", "South Africa")
  
  inct2 %>% 
    gt() %>% 
    tab_header(title = md("Incident symptomatic TB episodes averted by 2050<br>(number relative to BAU, in thousands)")) %>%
    cols_align(align = "center") %>%
    cols_width(everything() ~ px(200)) %>%
    cols_width(Intervention ~ px(150)) %>%
    opt_table_font(font = 'Open sans') %>%
    gtsave(filename = here('outputs', 'res', 'plots', paste("epi2tab1_inc_",format(Sys.time(), "%Y%m%d_%H-%M"),".png")))

# Epi2Tab2: Proportion incident symptomatic TB episodes averted by 2050 (relative to BAU) 
  incs <- episumm %>%
    filter(var == "incpct") %>%
    select(iso, intv, med, low, upp)
  
  inct <- reshape(incs, idvar = "intv", timevar = "iso", direction = "wide")
  for (i in 1:nrow(inct)){
    inct$BRA[i] <- paste(format(round(inct$med.BRA[i]*100, digits = 1), big.mark = ",", scientific = FALSE), "%\n(",
                         format(round(inct$low.BRA[i]*100, digits = 1), big.mark = ",", scientific = FALSE), "-",
                         format(round(inct$upp.BRA[i]*100, digits = 1), big.mark = ",", scientific = FALSE), "%)", sep = "")
    inct$IND[i] <- paste(format(round(inct$med.IND[i]*100, digits = 1), big.mark = ",", scientific = FALSE), "%\n(",
                         format(round(inct$low.IND[i]*100, digits = 1), big.mark = ",", scientific = FALSE), "-",
                         format(round(inct$upp.IND[i]*100, digits = 1), big.mark = ",", scientific = FALSE), "%)", sep = "")
    inct$ZAF[i] <- paste(format(round(inct$med.ZAF[i]*100, digits = 1), big.mark = ",", scientific = FALSE), "%\n(",
                         format(round(inct$low.ZAF[i]*100, digits = 1), big.mark = ",", scientific = FALSE), "-",
                         format(round(inct$upp.ZAF[i]*100, digits = 1), big.mark = ",", scientific = FALSE), "%)", sep = "")
  }
  
  inct2 <- inct %>%
    filter(!(str_detect(intv, 'BAU'))) %>%
    select(intv, BRA, IND, ZAF) %>%
    arrange(factor(intv, levels = intv_ord)) %>%
    mutate(intvn = intv_name) %>%
    mutate(BRA = ifelse(BRA == 'NA%\n(NA-NA%)', '-', BRA)) %>%
    mutate(IND = ifelse(IND == 'NA%\n(NA-NA%)', '-', IND)) %>%
    mutate(ZAF = ifelse(ZAF == 'NA%\n(NA-NA%)', '-', ZAF)) %>%
    relocate(5) %>%
    select(intvn, BRA, IND, ZAF)
  colnames(inct2) <- c("Intervention", "Brazil", "India", "South Africa")
  
  inct2 %>% 
    gt() %>% 
    tab_header(title = md("Incident symptomatic TB episodes averted by 2050<br>(percent relative to BAU)")) %>%
    cols_align(align = "center") %>%
    cols_width(everything() ~ px(200)) %>%
    cols_width(Intervention ~ px(150)) %>%
    opt_table_font(font = 'Open sans') %>%
    gtsave(filename = here('outputs', 'res', 'plots', paste("epi2tab2_incpct_",format(Sys.time(), "%Y%m%d_%H-%M"),".png")))

## EPI3 - DEATHS ##

# Epi3Fig1: TB deaths averted by 2050 (relative to BAU)
  png(here('outputs', 'res', 'plots', paste("epi3fig1_death_",format(Sys.time(), "%Y%m%d_%H-%M"), ".png")), width = 16, height = 7, units = 'in', res = 1000)
  ggplot(filter(episumm, var == "death", intv != "BAU")) +
    facet_grid(~factor(iso,levels=c("BRA","IND","ZAF")), space = "free", scales = "free_x", labeller = as_labeller(c("BRA" = "Brazil", "IND" = "India", "ZAF" = "South Africa"))) +
    geom_col(aes(x = factor(intv, levels = intv_ord, labels = intv_name), y = med, fill = intv), position = position_dodge(width = 1)) + 
    geom_errorbar(aes(x = factor(intv, levels = intv_ord, labels = intv_name), ymin = low, ymax = upp), 
                  position = position_dodge(width = 1), width = 0.5, size = 0.5) +
    geom_hline(yintercept = 0, size = 1) +
    scale_fill_manual(values = intv_col) +
    scale_x_discrete(position = "bottom") +
    scale_y_continuous(expand=c(0,0), labels = scales::label_number(scale = 1e-6, suffix = 'M')) +
    labs(x = NULL, y = "Number relative to BAU", fill = "Intervention") +
    theme_classic(base_size = 18) +
    theme(plot.title = element_text(hjust = 0.5),
          legend.position = "none",
          axis.ticks.x = element_blank(),
          axis.line.x = element_line(colour = "white", size = 0),
          strip.placement = "outside", 
          strip.background.x = element_rect(color = "white", fill = NULL), 
          strip.text = element_text(size = 18))#,
          #text = element_text(family = "Open sans"))
  dev.off()

# Epi3Fig2: TB deaths averted by 2050 (relative to BAU) - For results 05/03
  episummtemp <- episumm
  
  high_deaths <- which(episummtemp$var == "death" & episummtemp$med > 380000)
  
  if (length(high_deaths) > 0) {
    og_values <- episummtemp$med[high_deaths]
    episummtemp$med[high_deaths] <- 365000
    episummtemp$low[high_deaths] <- NA
    episummtemp$upp[high_deaths] <- NA
    
    intv_mapping <- setNames(seq_along(intv_ord), intv_ord)
    
    anntext <- data.frame(x = intv_mapping[episummtemp$intv[high_deaths]], y = 375000,
                          label = paste0(format(round(og_values / 1e6, digits = 1), big.mark = ",", scientific = FALSE), "M"), 
                          iso = episummtemp$iso[high_deaths])
  
    anntext[2,"x"] <- anntext[2,"x"]-1 # Quick fix to align NTN
    
  } else {
    anntext <- data.frame(x = character(0), y = numeric(0), label = character(0), iso = character(0))
  }
  
  png(here('outputs', 'res', 'plots', paste("epi3fig2_death_",format(Sys.time(), "%Y%m%d_%H-%M"),".png")), width = 16, height = 7, units = 'in', res = 1000)
  ggplot(filter(episummtemp, var == "death", intv != "BAU")) +
    facet_grid(~factor(iso,levels=c("BRA","IND","ZAF")), space = "free", scale = 'free_x', labeller = as_labeller(c("BRA" = "Brazil", "IND" = "India", "ZAF" = "South Africa"))) +
    geom_col(aes(x = factor(intv, levels = intv_ord, labels = intv_name), y = med, fill = intv), position = position_dodge(width = 1)) + 
    geom_errorbar(aes(x = factor(intv, levels = intv_ord, labels = intv_name), ymin = low, ymax = upp), 
                  position = position_dodge(width = 1), width = 0.5, size = 0.5) +
    geom_hline(yintercept = 0, size = 1) +
    scale_fill_manual(values = intv_col) +
    scale_x_discrete(position = "bottom") +
    scale_y_continuous(expand = c(0, 0), lim = c(0, 380000),
                       labels = scales::label_number(scale = 1e-6, suffix = 'M')) +
    geom_text(data = anntext, aes(x = x, y = y, label = label), inherit.aes = FALSE) +
    labs(x = NULL, y = "Number relative to BAU", fill = "Intervention") +
    theme_classic(base_size = 18) +
    theme(plot.title = element_text(hjust = 0.5),
          legend.position = "none",
          axis.ticks.x = element_blank(),
          axis.line.x = element_line(colour = "white", size = 0),
          strip.placement = "outside", 
          strip.background.x = element_rect(color = "white", fill = NULL), 
          strip.text = element_text(size = 18))
  dev.off()

# Epi3Fig3: Proportional TB deaths (relative to BAU) - For results 10/03
  png(here('outputs', 'res', 'plots', paste("epi3fig3_deathpct_",format(Sys.time(), "%Y%m%d_%H-%M"),".png")), width = 16, height = 7, units = 'in', res = 1000)
  ggplot(filter(episumm, var == "deathpct", intv != "BAU")) +
    facet_grid(~factor(iso,levels=c("BRA","IND","ZAF")), space = "free", scales = "free_x", labeller = as_labeller(c("BRA" = "Brazil", "IND" = "India", "ZAF" = "South Africa"))) +
    geom_col(aes(x = factor(intv, levels = intv_ord, labels = intv_name), y = med, fill = intv), position = position_dodge(width = 1)) + 
    geom_errorbar(aes(x = factor(intv, levels = intv_ord, labels = intv_name), ymin = low, ymax = upp), 
                  position = position_dodge(width = 1), width = 0.5, size = 0.5) +
    geom_hline(yintercept = 0, size = 1) +
    scale_fill_manual(values = intv_col) +
    scale_x_discrete(position = "bottom") +
    scale_y_continuous(expand = c(0, 0),
                       labels = scales::label_number(scale = 100, suffix = '%')) +
    labs(x = NULL, y = "Percent relative to BAU", fill = "Intervention") +
    theme_classic(base_size = 18) +
    theme(plot.title = element_text(hjust = 0.5),
          legend.position = "none",
          axis.ticks.x = element_blank(),
          axis.line.x = element_line(colour = "white", size = 0),
          strip.placement = "outside", 
          strip.background.x = element_rect(color = "white", fill = NULL), 
          strip.text = element_text(size = 18))
  dev.off()
      
# Epi3Fig4: Proportional TB deaths (relative to BAU) by intervention - For results 10/03
  png(here('outputs', 'res', 'plots', paste("epi3fig4_deathpctbyint_",format(Sys.time(), "%Y%m%d_%H-%M"),".png")), width = 16, height = 7, units = 'in', res = 1000)
  ggplot(filter(episumm, var == "deathpct", intv != "BAU")) +
    facet_grid(~factor(intv, levels = intv_ord, labels = intv_name), space = "free", scales = "free_x") +
    geom_col(aes(x = factor(iso,levels=c("BRA","IND","ZAF")), y = med, fill = intv), position = position_dodge(width = 1)) + 
    geom_errorbar(aes(x = factor(iso,levels=c("BRA","IND","ZAF")), ymin = low, ymax = upp), 
                  position = position_dodge(width = 1), width = 0.5, size = 0.5) +
    geom_hline(yintercept = 0, size = 1) +
    scale_fill_manual(values = intv_col) +
    scale_x_discrete(position = "bottom") +
    scale_y_continuous(expand = c(0, 0),
                       labels = scales::label_number(scale = 100, suffix = '%')) +
    labs(x = NULL, y = "Percent relative to BAU", fill = "Intervention") +
    theme_classic(base_size = 18) +
    theme(plot.title = element_text(hjust = 0.5),
          legend.position = "none",
          axis.ticks.x = element_blank(),
          axis.line.x = element_line(colour = "white", size = 0),
          strip.placement = "outside", 
          strip.background.x = element_rect(color = "white", fill = NULL), 
          strip.text = element_text(size = 18))
  dev.off()
          
# Epi3Tab1: TB deaths averted by 2050 (relative to BAU)
  deaths <- episumm %>%
    filter(var == "death") %>%
    select(iso, intv, med, low, upp)
  
  deatht <- reshape(deaths, idvar = "intv", timevar = "iso", direction = "wide")
  for (i in 1:nrow(deatht)){
    deatht$BRA[i] <- paste(format(round(deatht$med.BRA[i] * 1e-3, digits = 0), big.mark = ",", scientific = FALSE), "K\n(",
                         format(round(deatht$low.BRA[i] * 1e-3, digits = 0), big.mark = ",", scientific = FALSE), "-",
                         format(round(deatht$upp.BRA[i] * 1e-3, digits = 0), big.mark = ",", scientific = FALSE), "K)", sep = "")
    deatht$IND[i] <- paste(format(round(deatht$med.IND[i] * 1e-3, digits = 0), big.mark = ",", scientific = FALSE), "K\n(",
                         format(round(deatht$low.IND[i] * 1e-3, digits = 0), big.mark = ",", scientific = FALSE), "-",
                         format(round(deatht$upp.IND[i] * 1e-3, digits = 0), big.mark = ",", scientific = FALSE), "K)", sep = "")
    deatht$ZAF[i] <- paste(format(round(deatht$med.ZAF[i] * 1e-3, digits = 0), big.mark = ",", scientific = FALSE), "K\n(",
                         format(round(deatht$low.ZAF[i] * 1e-3, digits = 0), big.mark = ",", scientific = FALSE), "-",
                         format(round(deatht$upp.ZAF[i] * 1e-3, digits = 0), big.mark = ",", scientific = FALSE), "K)", sep = "")
  }
  
  deatht2 <- deatht %>%
    filter(!(str_detect(intv,'BAU'))) %>%
    select(intv, BRA, IND, ZAF) %>%
    arrange(factor(intv, levels = intv_ord)) %>%
    mutate(intvn = intv_name) %>%
    mutate(BRA = ifelse(BRA == 'NAK\n(NA-NAK)', '-', BRA)) %>%
    mutate(IND = ifelse(IND == 'NAK\n(NA-NAK)', '-', IND)) %>%
    mutate(ZAF = ifelse(ZAF == 'NAK\n(NA-NAK)', '-', ZAF)) %>%
    relocate(5) %>%
    select(intvn, BRA, IND, ZAF)
  colnames(deatht2) <- c("Intervention", "Brazil", "India", "South Africa")
  
  deatht2 %>% 
    gt() %>% 
    tab_header(title = md("TB deaths averted by 2050<br>(number relative to BAU, in thousands)")) %>%
    cols_align(align = "center") %>%
    cols_width(everything() ~ px(200)) %>%
    cols_width(Intervention ~ px(150)) %>%
    opt_table_font(font = 'Open sans') %>%
    gtsave(filename = here('outputs', 'res', 'plots', paste("epi3tab1_death_",format(Sys.time(), "%Y%m%d_%H-%M"),".png", sep = "")))

# Epi3Tab2: Percent TB deaths averted by 2050 (relative to BAU)
  deaths <- episumm %>%
    filter(var == "deathpct") %>%
    select(iso, intv, med, low, upp)
  
  deatht <- reshape(deaths, idvar = "intv", timevar = "iso", direction = "wide")
  for (i in 1:nrow(deatht)){
    deatht$BRA[i] <- paste(format(round(deatht$med.BRA[i] * 100, digits = 1), big.mark = ",", scientific = FALSE), "%\n(",
                           format(round(deatht$low.BRA[i] * 100, digits = 1), big.mark = ",", scientific = FALSE), "-",
                           format(round(deatht$upp.BRA[i] * 100, digits = 1), big.mark = ",", scientific = FALSE), "%)", sep = "")
    deatht$IND[i] <- paste(format(round(deatht$med.IND[i] * 100, digits = 1), big.mark = ",", scientific = FALSE), "%\n(",
                           format(round(deatht$low.IND[i] * 100, digits = 1), big.mark = ",", scientific = FALSE), "-",
                           format(round(deatht$upp.IND[i] * 100, digits = 1), big.mark = ",", scientific = FALSE), "%)", sep = "")
    deatht$ZAF[i] <- paste(format(round(deatht$med.ZAF[i] * 100, digits = 1), big.mark = ",", scientific = FALSE), "%\n(",
                           format(round(deatht$low.ZAF[i] * 100, digits = 1), big.mark = ",", scientific = FALSE), "-",
                           format(round(deatht$upp.ZAF[i] * 100, digits = 1), big.mark = ",", scientific = FALSE), "%)", sep = "")
  }
  
  deatht2 <- deatht %>%
    filter(!(str_detect(intv,'BAU'))) %>%
    select(intv, BRA, IND, ZAF) %>%
    arrange(factor(intv, levels = intv_ord)) %>%
    mutate(intvn = intv_name) %>%
    mutate(BRA = ifelse(BRA == 'NA%\n(NA-NA%)', '-', BRA)) %>%
    mutate(IND = ifelse(IND == 'NA%\n(NA-NA%)', '-', IND)) %>%
    mutate(ZAF = ifelse(ZAF == 'NA%\n(NA-NA%)', '-', ZAF)) %>%
    relocate(5) %>%
    select(intvn, BRA, IND, ZAF)
  colnames(deatht2) <- c("Intervention", "Brazil", "India", "South Africa")
  
  deatht2 %>% 
    gt() %>% 
    tab_header(title = md("TB deaths averted by 2050<br>(percent relative to BAU)")) %>%
    cols_align(align = "center") %>%
    cols_width(everything() ~ px(200)) %>%
    cols_width(Intervention ~ px(150)) %>%
    opt_table_font(font = 'Open sans') %>%
    gtsave(filename = here('outputs', 'res', 'plots', paste("epi3tab2_deathpct_",format(Sys.time(), "%Y%m%d_%H-%M"),".png", sep = "")))

## EPI4 - OUTPUTS OVER TIME ##

# Temp for legend
  tempforlegend <- ggplot(filter(episumm, var == "prevtimeraw")) +
                   facet_wrap(~iso) + 
                   geom_line(aes(x=year,y=med,col=factor(intv,levels = intv_ord2,labels=intv_name2)),size=1) +
                   scale_color_manual(values = intv_col2) +
                   theme_classic(base_size=18) +
                   labs(color = "Intervention") +
                   theme(plot.title = element_text(hjust = 0.5),
                         legend.position = "bottom",
                         axis.ticks.x = element_blank(),
                         strip.placement = "outside", 
                         plot.margin = margin(0, 0.6, 0, 0, "cm"),
                         strip.background.x = element_rect(color = "white", fill = NULL), 
                         strip.text = element_text(size = 18)) +
                   guides(colour = guide_legend(nrow = 2, byrow = TRUE))
  g_legend <- function(a.gplot){ 
    tmp <- ggplot_gtable(ggplot_build(a.gplot)) 
    leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box") 
    legend <- tmp$grobs[[leg]] 
    legend
  } 
  legend <- g_legend(tempforlegend)
  
  
# Epi4Fig1: Change in prevalence over time (raw)
  prevtime_bra <- ggplot(filter(episumm, var == "prevtimeraw", iso == "BRA")) +
                  geom_line(aes(x=year,y=med,col=intv),size=1) +
                  geom_ribbon(aes(x=year,min=low,max=upp,fill=intv),size=1,alpha=.15) +
                  geom_line(data=filter(episumm, var == "prevtimeraw", iso == "BRA", intv == "BAU"), aes(x=year,y=med), col="black",linetype=2, size=1.5) +
                  scale_fill_manual(values = intv_col) +
                  scale_color_manual(values = intv_col) +
                  scale_x_continuous("",
                                     lim=c(2020,2050),expand=c(0,0),breaks=c(2020,2030,2040,2050)) + 
                  scale_y_continuous("infTB prevalence per 100,000",expand=c(0,0),lim=c(0,210),breaks=c(100,200)) +
                  labs(title="Brazil") +
                  theme_classic(base_size=18) +
                  theme(plot.title = element_text(hjust = 0.5),
                        legend.position = "none",
                        axis.ticks.x = element_blank(),
                        strip.placement = "outside", 
                        plot.margin = margin(0, 0.6, 0, 0, "cm"),
                        strip.background.x = element_rect(color = "white", fill = NULL), 
                        strip.text = element_text(size = 18))
  prevtime_ind <- ggplot(filter(episumm, var == "prevtimeraw", iso == "IND")) +
                  geom_line(aes(x=year,y=med,col=intv),size=1) +
                  geom_ribbon(aes(x=year,min=low,max=upp,fill=intv),size=1,alpha=.15) +
                  geom_line(data=filter(episumm, var == "prevtimeraw", iso == "IND", intv == "BAU"), aes(x=year,y=med), col="black",linetype=2, size=1.5) +
                  scale_fill_manual(values = intv_col) +
                  scale_color_manual(values = intv_col) +
                  scale_x_continuous("Year",lim=c(2020,2050),expand=c(0,0),breaks=c(2020,2030,2040,2050)) + 
                  scale_y_continuous("",
                                     expand=c(0,0),lim=c(0,450),breaks=c(100,200,300,400)) +
                  labs(title="India") +
                  theme_classic(base_size=18) +
                  theme(plot.title = element_text(hjust = 0.5),
                        legend.position = "none",
                        axis.ticks.x = element_blank(),
                        strip.placement = "outside", 
                        plot.margin = margin(0, 0.6, 0, 0, "cm"),
                        strip.background.x = element_rect(color = "white", fill = NULL), 
                        strip.text = element_text(size = 18))
  prevtime_zaf <- ggplot(filter(episumm, var == "prevtimeraw", iso == "ZAF")) +
                  geom_line(aes(x=year,y=med,col=intv),size=1) +
                  geom_ribbon(aes(x=year,min=low,max=upp,fill=intv),size=1,alpha=.15) +
                  geom_line(data=filter(episumm, var == "prevtimeraw", iso == "ZAF", intv == "BAU"), aes(x=year,y=med), col="black",linetype=2, size=1.5) +
                  scale_fill_manual(values = intv_col) +
                  scale_color_manual(values = intv_col) +
                  scale_x_continuous("",
                                     lim=c(2020,2050),expand=c(0,0),breaks=c(2020,2030,2040,2050)) + 
                  scale_y_continuous("",
                                     expand=c(0,0),lim=c(0,760),breaks=c(100,200,300,400,500,600,700)) +
                  labs(title="South Africa") +
                  theme_classic(base_size=18) +
                  theme(plot.title = element_text(hjust = 0.5),
                        legend.position = "none",
                        axis.ticks.x = element_blank(),
                        #axis.line.x = element_line(colour = "white", size = 0),
                        strip.placement = "outside", 
                        plot.margin = margin(0, 0.6, 0, 0, "cm"),
                        strip.background.x = element_rect(color = "white", fill = NULL), 
                        strip.text = element_text(size = 18))
  png(here('outputs', 'res', 'plots', paste("epi4fig1_prev_",format(Sys.time(),"%Y%m%d_%H-%M"),".png")), width = 16, height = 8, units = 'in', res = 1000)
  (prevtime_bra | prevtime_ind | prevtime_zaf) / legend + plot_layout(heights = c(2, 0.2))
  dev.off()

  # Epi4Fig2: Change in incidence (sTB) over time
  inctime_bra <- ggplot(filter(episumm, var == "inctimestb", iso == "BRA")) +
                 geom_line(aes(x=year,y=med,col=intv),size=1) +
                 geom_ribbon(aes(x=year,min=low,max=upp,fill=intv),size=1,alpha=.15) +
                 geom_errorbar(aes(x=2022,ymin=37,ymax=51),colour="black",size=1) +
                 scale_fill_manual(values = intv_col) +
                 scale_color_manual(values = intv_col) +
                 geom_line(data=filter(episumm, var == "inctimestb", iso == "BRA", intv == "BAU"), aes(x=year,y=med), col="black",linetype=2, size=1.5) +
                 scale_x_continuous("",
                                    lim=c(2020,2050),expand=c(0,0),breaks=c(2020,2030,2040,2050)) + 
                 scale_y_continuous("sTB incidence per 100,000",expand=c(0,0),lim=c(0,85)) +
                 labs(title="Brazil") +
                 theme_classic(base_size=18) +
                 theme(plot.title = element_text(hjust = 0.5),
                       legend.position = "none",
                       axis.ticks.x = element_blank(),
                       strip.placement = "outside", 
                       plot.margin = margin(0, 0.6, 0, 0, "cm"),
                       strip.background.x = element_rect(color = "white", fill = NULL), 
                       strip.text = element_text(size = 18))
  inctime_ind <- ggplot(filter(episumm, var == "inctimestb", iso == "IND")) +
                 geom_line(aes(x=year,y=med,col=intv),size=1) +
                 geom_ribbon(aes(x=year,min=low,max=upp,fill=intv),size=1,alpha=.15) +
                 geom_errorbar(aes(x=2022,ymin=139,ymax=189),colour="black",size=1) +
                 geom_line(data=filter(episumm, var == "inctimestb", iso == "IND", intv == "BAU"), aes(x=year,y=med), col="black",linetype=2, size=1.5) +
                 scale_fill_manual(values = intv_col) +
                 scale_color_manual(values = intv_col) +
                 scale_x_continuous("Year",lim=c(2020,2050),expand=c(0,0),breaks=c(2020,2030,2040,2050)) + 
                 scale_y_continuous("",expand=c(0,0),lim=c(0,200)) +
                 labs(title="India") +
                 theme_classic(base_size=18) +
                 theme(plot.title = element_text(hjust = 0.5),
                       legend.position = "none",
                       axis.ticks.x = element_blank(),
                       strip.placement = "outside", 
                       plot.margin = margin(0, 0.6, 0, 0, "cm"),
                       strip.background.x = element_rect(color = "white", fill = NULL), 
                       strip.text = element_text(size = 18))
  inctime_zaf <- ggplot(filter(episumm, var == "inctimestb", iso == "ZAF")) +
                 geom_line(aes(x=year,y=med,col=intv),size=1) +
                 geom_ribbon(aes(x=year,min=low,max=upp,fill=intv),size=1,alpha=.15) +
                 geom_errorbar(aes(x=2022,ymin=280,ymax=614),colour="black",size=1) +
                 geom_line(data=filter(episumm, var == "inctimestb", iso == "ZAF", intv == "BAU"), aes(x=year,y=med), col="black",linetype=2, size=1.5) +
                 scale_fill_manual(values = intv_col) +
                 scale_color_manual(values = intv_col) +
                 scale_x_continuous("",
                                    lim=c(2020,2050),expand=c(0,0),breaks=c(2020,2030,2040,2050)) + 
                 scale_y_continuous("",expand=c(0,0),lim=c(0,620)) + 
                 labs(title="South Africa") +
                 theme_classic(base_size=18) +
                 theme(plot.title = element_text(hjust = 0.5),
                       legend.position = "none",
                       axis.ticks.x = element_blank(),
                       strip.placement = "outside", 
                       plot.margin = margin(0, 0.6, 0, 0, "cm"),
                       strip.background.x = element_rect(color = "white", fill = NULL), 
                       strip.text = element_text(size = 18))
  png(here('outputs', 'res', 'plots', paste("epi4fig2_inc_",format(Sys.time(), "%Y%m%d_%H-%M"),".png")), width = 16, height = 8, units = 'in', res = 1000)
  (inctime_bra | inctime_ind | inctime_zaf) / legend + plot_layout(heights = c(2, 0.2))
  dev.off()
  
# Epi4Fig3: Change in mortality over time
  morttime_bra <- ggplot(filter(episumm, var == "morttime", iso == "BRA")) +
                  geom_line(aes(x=year,y=med,col=intv),size=1) +
                  geom_ribbon(aes(x=year,min=low,max=upp,fill=intv),size=1,alpha=.15) +
                  geom_line(data=filter(episumm, var == "morttime", iso == "BRA", intv == "BAU"), aes(x=year,y=med), col="black",linetype=2, size=1.5) +
                  scale_fill_manual(values = intv_col) +
                  scale_color_manual(values = intv_col) +
                  scale_x_continuous("",lim=c(2020,2050),expand=c(0,0),breaks=c(2020,2030,2040,2050)) + 
                  scale_y_continuous("sTB mortality per 100,000",expand=c(0,0),lim=c(0,11),breaks=c(2,4,6,8,10)) +
                  labs(title="Brazil") +
                  theme_classic(base_size=18) +
                  theme(plot.title = element_text(hjust = 0.5),
                        legend.position = "none",
                        axis.ticks.x = element_blank(),
                        strip.placement = "outside", 
                        plot.margin = margin(0, 0.6, 0, 0, "cm"),
                        strip.background.x = element_rect(color = "white", fill = NULL), 
                        strip.text = element_text(size = 18))
  morttime_ind <- ggplot(filter(episumm, var == "morttime", iso == "IND")) +
                  geom_line(aes(x=year,y=med,col=intv),size=1) +
                  geom_ribbon(aes(x=year,min=low,max=upp,fill=intv),size=1,alpha=.15) +
                  geom_line(data=filter(episumm, var == "morttime", iso == "IND", intv == "BAU"), aes(x=year,y=med), col="black",linetype=2, size=1.5) +
                  scale_fill_manual(values = intv_col) +
                  scale_color_manual(values = intv_col) +
                  scale_x_continuous("Year",lim=c(2020,2050),expand=c(0,0),breaks=c(2020,2030,2040,2050)) + 
                  scale_y_continuous("",expand=c(0,0),lim=c(0,33)) +
                  labs(title="India") +
                  theme_classic(base_size=18) +
                  theme(plot.title = element_text(hjust = 0.5),
                        legend.position = "none",
                        axis.ticks.x = element_blank(),
                        strip.placement = "outside", 
                        plot.margin = margin(0, 0.6, 0, 0, "cm"),
                        strip.background.x = element_rect(color = "white", fill = NULL), 
                        strip.text = element_text(size = 18))
  morttime_zaf <- ggplot(filter(episumm, var == "morttime", iso == "ZAF")) +
                  geom_line(aes(x=year,y=med,col=intv),size=1) +
                  geom_ribbon(aes(x=year,min=low,max=upp,fill=intv),size=1,alpha=.15) +
                  geom_line(data=filter(episumm, var == "morttime", iso == "ZAF", intv == "BAU"), aes(x=year,y=med), col="black",linetype=2, size=1.5) +
                  scale_fill_manual(values = intv_col) +
                  scale_color_manual(values = intv_col) +
                  scale_x_continuous("",lim=c(2020,2050),expand=c(0,0),breaks=c(2020,2030,2040,2050)) + 
                  scale_y_continuous("",expand=c(0,0),lim=c(0,180)) +
                  labs(title="South Africa") +
                  theme_classic(base_size=18) +
                  theme(plot.title = element_text(hjust = 0.5),
                        legend.position = "none",
                        axis.ticks.x = element_blank(),
                        strip.placement = "outside", 
                        plot.margin = margin(0, 0.6, 0, 0, "cm"),
                        strip.background.x = element_rect(color = "white", fill = NULL), 
                        strip.text = element_text(size = 18))
  png(here('outputs', 'res', 'plots', paste("epi4fig3_mort_",format(Sys.time(), "%Y%m%d_%H-%M"),".png")), width = 16, height = 8, units = 'in', res = 1000)
  (morttime_bra | morttime_ind | morttime_zaf) / legend + plot_layout(heights = c(2, 0.2))
  dev.off()
