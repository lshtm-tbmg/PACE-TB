#------------------------------------------------------------------------------#
# PACE-TB - Cost-effectiveness plots preparation                               #
# Curates cost-effectiveness outputs for plots.                                #
# Code by Katherine C. Horton (katherine.horton@lshtm.ac.uk)                   #
# Last updated 2025-08-07 by ASC                                               #
#------------------------------------------------------------------------------#

# Packages
suppressPackageStartupMessages({
  library(here)
  library(rio)
  library(data.table)
  library(tidyverse)
  library(fst)
  library(extrafont)
  library(scales)
  library(ggbreak)
  library(gt)
  library(chromote)
  library(readxl)
  library(openxlsx)
  library(patchwork)
})

rm(list = ls())

# Load the file
cea <- as.data.table(read_fst(here("outputs", "res", "summ", "ceasumm.fst")))
cea_mean <- cea # For summaries using mean and local currency (see end of script)

# Summarise runs using medians
cea <- cea %>% 
  pivot_longer(cols = -c(iso, intv, run), names_to = "var", values_to = "val") %>% 
  group_by(iso, intv, var) %>% 
  summarise(med = quantile(val, 0.5, na.rm = TRUE),
            low = quantile(val, 0.025, na.rm = TRUE),
            upp = quantile(val, 0.975, na.rm = TRUE),
            .groups = "drop")

intv_col <- c("BAU" = "#C0C0C0", "VAX" = "#537D8D", "TPT" = "#CB4C15", 
              "SCRhi" = "#754668", "SCRlo" = "#754668",
              "DGN" = "#CBA715", "DST" = "#1F4E79", "PRI" = "#545454",
              "SDS" = "#AE0D0A", "SDR" = "#2C6E49", "NTN" = "#8A8A8A") 
intv_ord  <- c("VAX", "TPT", "SCRhi", "SCRlo", "DGN", "DST", "SDS", "SDR", "PRI", "NTN")
intv_name <- c("Vacc", "TPT", "Comm\nScr\n(high)", "Comm\nScr\n(low)",
               "Impr\nDiag", "DST\nfor all", "Short\nDS", "Short\nDR", "Pri\nScr", "Nutr")
intv_col2 <- c("BAU" = "#C0C0C0", "Vacc" = "#537D8D", "TPT" = "#CB4C15", 
              "Comm Scr (high)" = "#754668", "Comm Scr (low)" = "#754668", 
              "Impr Diag" = "#CBA715", "DST for all" = "#1F4E79", "Pri Scr" = "#545454", 
              "Short DS" = "#AE0D0A", "Short DR" = "#2C6E49", "Nutr" = "#8A8A8A") 
intv_name2 <- c("Vacc", "TPT", "Comm Scr (high)", "Comm Scr (low)", 
                "Impr Diag", "DST for all", "Short DS", "Short DR", "Pri Scr", "Nutr")
intv_col3 <- c("BAU" = "#C0C0C0", "Vacc" = "#537D8D", "TPT" = "#CB4C15", 
               "Comm Scr (low)" = "#754668", 
               "Impr Diag" = "#CBA715", "DST for all" = "#1F4E79", "Pri Scr" = "#545454", 
               "Short DS" = "#AE0D0A", "Short DR" = "#2C6E49", "Nutr" = "#8A8A8A") 
intv_name3 <- c("Vacc", "TPT", "Comm Scr (low)", 
                "Impr Diag", "DST for all", "Short DS", "Short DR", "Pri Scr", "Nutr")
intv_ord3  <- c("VAX", "TPT", "SCRlo", "DGN", "DST", "SDS", "SDR", "PRI", "NTN")
intv_ord4  <- c("BAU","VAX", "TPT", "SCRlo", "DGN", "DST", "SDS", "SDR", "PRI", "NTN")
intv_name4 <- c("BAU","Vacc", "TPT", "Comm\nScr", 
               "Impr\nDiag", "DST\nfor all", "Short\nDS", "Short\nDR", "Pri\nScr", "Nutr")

# Cea1Fig1: Incremental budget (relative to BAU)
  png(here('outputs', 'res', 'plots', paste0("cea1fig1_incbudget_",format(Sys.time(),"%Y%m%d_%H-%M"),".png")), width = 16, height = 7, units = 'in', res = 1000)
  ggplot(filter(cea, var == "Inc_costs", intv != "BAU")) +
    facet_grid(~iso, space = "free", scales = "free_x", labeller = as_labeller(c("BRA" = "Brazil", "IND" = "India", "ZAF" = "South Africa"))) +
    geom_col(aes(x = factor(intv, levels = intv_ord, labels = intv_name), y = med, fill = intv), position = position_dodge(width = 1)) +
    geom_errorbar(aes(x = factor(intv, levels = intv_ord, labels = intv_name), ymin = low, ymax = upp),
                  position = position_dodge(width = 1), width = 0.5, size = 0.5) +
    geom_hline(yintercept = 0, linewidth = 1) +
    scale_fill_manual(values = intv_col) +
    scale_x_discrete(position = "bottom") +
    scale_y_continuous(labels = scales::label_number(scale = 1e-9, suffix = 'B')) +
    labs(x = NULL, y = "Incremental budget (USD)", fill = "Intervention") +
    theme_classic(base_size = 18) +
    theme(plot.title = element_text(hjust = 0.5),
          legend.position = "none",
          axis.ticks.x = element_blank(),
          axis.line.x = element_line(colour="white",size=0),
          strip.placement = "outside",
          strip.background.x = element_rect(colour = "white", fill=NULL),
          strip.text = element_text(size = 18))
  dev.off()

# Cea1Fig2: Incremental budget (relative to BAU)
  ceatemp <- cea
  high_costs <- which(ceatemp$var == "Inc_costs" & ceatemp$med > 19e9)
  if (length(high_costs) > 0) {
    og_values <- ceatemp$med[high_costs]
    ceatemp$med[high_costs] <- 19.5e9
    intv_mapping <- setNames(seq_along(intv_ord), intv_ord)
    anntext <- data.frame(x = intv_mapping[ceatemp$intv[high_costs]], y = 20.25e9,
      label = paste0(format(round(og_values / 1e9, digits = 0), big.mark = ",", scientific = FALSE), "B"), 
      iso = ceatemp$iso[high_costs])
  } else {
    anntext <- data.frame(x = character(0), y = numeric(0), label = character(0), iso = character(0))
  }
  
  high_upp <- which(ceatemp$var == "Inc_costs" & ceatemp$med < 19e9 & ceatemp$upp > 19e9)
  if (length(high_upp) > 0) {
    og_values <- ceatemp$upp[high_upp]
    ceatemp$upp[high_upp] <- 19.5e9
    intv_mapping <- setNames(seq_along(intv_ord), intv_ord)
  } 
  
  arrows <- ceatemp %>%
    filter(var == "Inc_costs") %>%
    filter(upp == 19.5e9) %>%
    filter(med != 19.5e9)
  
  png(here('outputs', 'res', 'plots', paste0("cea1fig2_incbudget_",format(Sys.time(),"%Y%m%d_%H-%M"),".png")), width = 16, height = 7, units = 'in', res = 1000)
  ggplot(filter(ceatemp, var == "Inc_costs", intv != "BAU")) +
    facet_grid(~iso, space = "free", scales = "free_x", labeller = as_labeller(c("BRA" = "Brazil", "IND" = "India", "ZAF" = "South Africa"))) +
    geom_col(aes(x = factor(intv, levels = intv_ord, labels = intv_name), y = med, fill = intv), position = position_dodge(width = 1)) + 
    geom_errorbar(aes(x = factor(intv, levels = intv_ord, labels = intv_name), ymin = low, ymax = upp), 
                  position = position_dodge(width = 1), width = 0.5, size = 0.5) +
    geom_hline(yintercept = 0, linewidth = 1) +
    geom_segment(arrows,mapping = aes(x = factor(intv, levels = intv_ord, labels = intv_name),
                                      xend = factor(intv, levels = intv_ord, labels = intv_name),
                                      y = 20.45e9-1,
                                      yend = 20.45e9), arrow = arrow()) +
    scale_fill_manual(values = intv_col) +
    scale_x_discrete(position = "bottom") +
    scale_y_continuous(lim = c(-1.005e9, 20.5e9),
                       labels = scales::label_number(scale = 1e-9, suffix = 'B')) +
    geom_text(data = anntext, aes(x = x, y = y, label = label), inherit.aes = FALSE) +
    labs(x = NULL, y = "Incremental budget (USD)", fill = "Intervention") +
    theme_classic(base_size = 18) +
    theme(plot.title = element_text(hjust = 0.5),
          legend.position = "none",
          axis.ticks.x = element_blank(),
          axis.line.x = element_line(colour="white",size=0),
          strip.placement = "outside", 
          strip.background.x = element_rect(colour = "white", fill=NULL), 
          strip.text = element_text(size = 18))
  dev.off()

# Cea1Tab1: Incremental budget (relative to BAU) 
  costs <- cea %>%
    filter(var == "Inc_costs") %>%
    select(iso, intv, med, low, upp) %>% 
    pivot_wider(names_from = iso, values_from = c(med, low, upp)) %>% 
    mutate_at(vars(-intv),
              ~ . * 1e-9, TRUE ~ as.numeric(.)) %>% 
    mutate(across(-intv, ~ round(.x, digits = 1)),
           across(-intv, ~ format(.x, big.mark = ",", scientific = FALSE))) %>% 
    rowwise() %>%
    mutate(across(matches("^med_"), ~ trimws(paste0(.x, "B\n(", 
                                                    trimws(get(sub("med_", "low_", cur_column()))), "-",
                                                    trimws(get(sub("med_", "upp_", cur_column()))), "B)")))) %>%
    select(intv, matches("^med_")) %>%
    rename_with(~ sub("^med_", "", .), matches("^med_"))
  
  costt2 <- costs %>%
    filter(!(str_detect(intv, 'BAU'))) %>%
    select(intv, BRA, IND, ZAF) %>%
    arrange(factor(intv, levels = intv_ord)) %>%
    mutate(intvn = intv_name[match(intv, intv_ord)]) %>%
    mutate(BRA = ifelse(BRA == 'NAB\n(NA-NAB)', '-', BRA)) %>%
    mutate(IND = ifelse(IND == 'NAB\n(NA-NAB)', '-', IND)) %>%
    mutate(ZAF = ifelse(ZAF == 'NAB\n(NA-NAB)', '-', ZAF)) %>%
    relocate(5) %>%
    select(intvn, BRA, IND, ZAF)
  colnames(costt2) <- c("Intervention", "Brazil", "India", "South Africa")
  costt2 %>% 
    gt() %>% 
    tab_header(title = md("Incremental cost<br>(USD relative to BAU)")) %>%
    cols_align(align = "center") %>%
    cols_width(everything() ~ px(200)) %>%
    cols_width(Intervention ~ px(150)) %>%
    opt_table_font(font = 'Open sans') %>%
    gtsave(filename = here('outputs', 'res', 'plots', paste0("cea1tab1_incbudget_",format(Sys.time(), "%Y%m%d_%H-%M"),".png")))

# Cea2Fig1: Cost v DALY
  he_values <- t(read.csv(here('outputs', 'res', 'summ', "cethresh.csv")))
  thresh <- as.data.frame(he_values[-1,])
  colnames(thresh) <- c("cet_low", "cet_high")
  thresh$iso <- c("BRA", "IND", "ZAF")
  thresh$cet_high <- as.numeric(thresh$cet_high)
  thresh$cet_low  <- as.numeric(thresh$cet_low)
  
  costdaly <- filter(cea, var %in% c('Inc_costs', 'DALY_avert')) %>% 
    pivot_wider(names_from = var, values_from = c(med, low, upp), names_glue = "{var}.{.value}") %>%
    arrange(factor(intv))
    
  g_legend <- function(a.gplot){ 
    tmp <- ggplot_gtable(ggplot_build(a.gplot)) 
    leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box") 
    legend <- tmp$grobs[[leg]] 
    legend
  } 

  tempforlegend <- ggplot(filter(costdaly, intv != "BAU")) +
    facet_wrap(~iso, scales = "free", labeller = as_labeller(c("BRA" = "Brazil", "IND" = "India", "ZAF" = "South Africa"))) +
    geom_hline(yintercept = 0, size = 1) +
    geom_vline(xintercept = 0, size = 1) +
    geom_point(aes(x = DALY_avert.med, y = Inc_costs.med, colour = factor(intv,levels = intv_ord,labels=intv_name2)), alpha=0.5, size=2.5) + 
    geom_abline(aes(intercept = 0, slope = cet_high), colour = "grey40", data = thresh, size = 1, lty=5) + 
    geom_abline(aes(intercept = 0, slope = cet_low ), colour = "grey40", data = thresh, size = 1, lty=3) + 
    geom_errorbar(aes(x = DALY_avert.med, ymin = Inc_costs.low, ymax = Inc_costs.upp, colour = factor(intv,levels = intv_ord, labels = intv_name2)), 
                  width = 0, size = 1) +
    geom_errorbar(aes(y = Inc_costs.med, xmin = DALY_avert.low, xmax = DALY_avert.upp, , colour = factor(intv,levels = intv_ord, labels = intv_name2)), 
                  width = 0, size = 1) +
    scale_colour_manual(values = intv_col2) +
    scale_y_continuous(labels = scales::label_number(scale = 1e-9, suffix = 'B')) +
    scale_x_continuous(labels = scales::label_number(scale = 1e-6, suffix = 'M')) +
    labs(x = "DALYs averted", y = "Incremental budget (USD)", colour = "Intervention") +
    theme_classic(base_size = 18) +
    theme(plot.title = element_text(hjust = 0.5),
          axis.line.x = element_line(colour = "white", size = 0),
          axis.line.y = element_line(colour = "white", size = 0),
          strip.placement = "outside", 
          strip.background.x = element_rect(colour = "white", fill=NULL), 
          strip.text = element_text(size = 18),
          legend.position = "bottom") +
    guides(colour = guide_legend(nrow = 2, byrow = TRUE))
  
  legend <- g_legend(tempforlegend)

  cea_zaf <- ggplot(filter(costdaly, intv != "BAU", iso == "ZAF")) +
    geom_hline(yintercept = 0, size = 1) +
    geom_vline(xintercept = 0, size = 1) +
    geom_point(aes(x = DALY_avert.med, y = Inc_costs.med, colour = factor(intv,levels = intv_ord,labels=intv_name2)), alpha=0.5, size=2.5) + 
    geom_abline(data=filter(thresh, iso == "ZAF"), aes(intercept = 0, slope = cet_high), colour = "grey40", size = 1, lty=5) + 
    geom_abline(data=filter(thresh, iso == "ZAF"), aes(intercept = 0, slope = cet_low ), colour = "grey40", size = 1, lty=3) + 
    geom_errorbar(aes(x = DALY_avert.med, ymin = Inc_costs.low, ymax = Inc_costs.upp, colour = factor(intv,levels = intv_ord, labels = intv_name2)), 
                  width = 0, size = 1) +
    geom_errorbar(aes(y = Inc_costs.med, xmin = DALY_avert.low, xmax = DALY_avert.upp, , colour = factor(intv,levels = intv_ord, labels = intv_name2)), 
                  width = 0, size = 1) +
    scale_colour_manual(values = intv_col2) +
    scale_y_continuous(labels = scales::label_number(scale = 1e-9, suffix = 'B')) +
    scale_x_continuous(labels = scales::label_number(scale = 1e-6, suffix = 'M')) +
    labs(title= "South Africa", x = "", y = "", colour = "Intervention") +
    theme_classic(base_size = 18) +
    theme(plot.title = element_text(hjust = 0.5),
          axis.line.x = element_line(colour = "white", size = 0),
          axis.line.y = element_line(colour = "white", size = 0),
          strip.placement = "outside", 
          strip.background.x = element_rect(colour = "white", fill=NULL), 
          strip.text = element_text(size = 18),
          legend.position = "none") +
    guides(colour = guide_legend(nrow = 2, byrow = TRUE))
  
  negscal <- layer_scales(cea_zaf)$y$range$range[1]/layer_scales(cea_zaf)$y$range$range[2]
  
  cea_bra_temp <- ggplot(filter(costdaly, intv != "BAU", iso == "BRA")) +
    geom_hline(yintercept = 0, size = 1) +
    geom_vline(xintercept = 0, size = 1) +
    geom_point(aes(x = DALY_avert.med, y = Inc_costs.med, colour = factor(intv,levels = intv_ord,labels=intv_name2)), alpha=0.5, size=2.5) + 
    geom_abline(data=filter(thresh, iso == "BRA"), aes(intercept = 0, slope = cet_high), colour = "grey40", size = 1, lty=5) + 
    geom_abline(data=filter(thresh, iso == "BRA"), aes(intercept = 0, slope = cet_low),  colour = "grey40", size = 1, lty=3) + 
    geom_errorbar(aes(x = DALY_avert.med, ymin = Inc_costs.low, ymax = Inc_costs.upp, colour = factor(intv,levels = intv_ord, labels = intv_name2)), 
                  width = 0, size = 1) +
    geom_errorbar(aes(y = Inc_costs.med, xmin = DALY_avert.low, xmax = DALY_avert.upp, , colour = factor(intv,levels = intv_ord, labels = intv_name2)), 
                  width = 0, size = 1) +
    scale_colour_manual(values = intv_col2) +
    scale_y_continuous(labels = scales::label_number(scale = 1e-9, suffix = 'B')) +
    scale_x_continuous(labels = scales::label_number(scale = 1e-6, suffix = 'M')) +
    theme_classic(base_size = 18) +
    theme(plot.title = element_text(hjust = 0.5),
          axis.line.x = element_line(colour = "white", size = 0),
          axis.line.y = element_line(colour = "white", size = 0),
          strip.placement = "outside", 
          strip.background.x = element_rect(colour = "white", fill=NULL), 
          strip.text = element_text(size = 18),
          legend.position = "none") +
    guides(colour = guide_legend(nrow = 2, byrow = TRUE))
  
  bra_min <- negscal*layer_scales(cea_bra_temp)$y$range$range[2]
  bra_max <- layer_scales(cea_bra_temp)$y$range$range[2]
  
  cea_bra <- ggplot(filter(costdaly, intv != "BAU", iso == "BRA")) +
    geom_hline(yintercept = 0, size = 1) +
    geom_vline(xintercept = 0, size = 1) +
    geom_point(aes(x = DALY_avert.med, y = Inc_costs.med, colour = factor(intv,levels = intv_ord,labels=intv_name2)), alpha=0.5, size=2.5) + 
    geom_abline(data=filter(thresh, iso == "BRA"), aes(intercept = 0, slope = cet_high), colour = "grey40", size = 1, lty=5) + 
    geom_abline(data=filter(thresh, iso == "BRA"), aes(intercept = 0, slope = cet_low),  colour = "grey40", size = 1, lty=3) + 
    geom_errorbar(aes(x = DALY_avert.med, ymin = Inc_costs.low, ymax = Inc_costs.upp, colour = factor(intv,levels = intv_ord, labels = intv_name2)), 
                  width = 0, size = 1) +
    geom_errorbar(aes(y = Inc_costs.med, xmin = DALY_avert.low, xmax = DALY_avert.upp, , colour = factor(intv,levels = intv_ord, labels = intv_name2)), 
                  width = 0, size = 1) +
    scale_colour_manual(values = intv_col2) +
    scale_y_continuous(labels = scales::label_number(scale = 1e-9, suffix = 'B'),lim=c(bra_min,bra_max)) +
    scale_x_continuous(labels = scales::label_number(scale = 1e-6, suffix = 'M')) +
    labs(title="Brazil", x = "", y = "Incremental budget (USD)", colour = "Intervention") +
    theme_classic(base_size = 18) +
    theme(plot.title = element_text(hjust = 0.5),
          axis.line.x = element_line(colour = "white", size = 0),
          axis.line.y = element_line(colour = "white", size = 0),
          strip.placement = "outside", 
          strip.background.x = element_rect(colour = "white", fill=NULL), 
          strip.text = element_text(size = 18),
          legend.position = "none") +
    guides(colour = guide_legend(nrow = 2, byrow = TRUE))
  
  cea_ind_temp <- ggplot(filter(costdaly, intv != "BAU", iso == "IND")) +
    geom_hline(yintercept = 0, size = 1) +
    geom_vline(xintercept = 0, size = 1) +
    geom_point(aes(x = DALY_avert.med, y = Inc_costs.med, colour = factor(intv,levels = intv_ord,labels=intv_name2)), alpha=0.5, size=2.5) + 
    geom_abline(data=filter(thresh, iso == "IND"), aes(intercept = 0, slope = cet_high), colour = "grey40", size = 1, lty=5) + 
    geom_abline(data=filter(thresh, iso == "IND"), aes(intercept = 0, slope = cet_low),  colour = "grey40", size = 1, lty=3) + 
    geom_errorbar(aes(x = DALY_avert.med, ymin = Inc_costs.low, ymax = Inc_costs.upp, colour = factor(intv,levels = intv_ord, labels = intv_name2)), 
                  width = 0, size = 1) +
    geom_errorbar(aes(y = Inc_costs.med, xmin = DALY_avert.low, xmax = DALY_avert.upp, , colour = factor(intv,levels = intv_ord, labels = intv_name2)), 
                  width = 0, size = 1) +
    scale_colour_manual(values = intv_col2) +
    scale_y_continuous(labels = scales::label_number(scale = 1e-9, suffix = 'B')) +
    scale_x_continuous(labels = scales::label_number(scale = 1e-6, suffix = 'M')) +
    theme_classic(base_size = 18) +
    theme(plot.title = element_text(hjust = 0.5),
          axis.line.x = element_line(colour = "white", size = 0),
          axis.line.y = element_line(colour = "white", size = 0),
          strip.placement = "outside", 
          strip.background.x = element_rect(colour = "white", fill=NULL), 
          strip.text = element_text(size = 18),
          #text = element_text(family = "Open sans"),
          legend.position = "none") +
    guides(colour = guide_legend(nrow = 2, byrow = TRUE))
  
  ind_min <- negscal*layer_scales(cea_ind_temp)$y$range$range[2]
  ind_max <- layer_scales(cea_ind_temp)$y$range$range[2]
  
  cea_ind <- ggplot(filter(costdaly, intv != "BAU", iso == "IND")) +
    geom_hline(yintercept = 0, size = 1) +
    geom_vline(xintercept = 0, size = 1) +
    geom_point(aes(x = DALY_avert.med, y = Inc_costs.med, colour = factor(intv,levels = intv_ord,labels=intv_name2)), alpha=0.5, size=2.5) + 
    geom_abline(data=filter(thresh, iso == "IND"), aes(intercept = 0, slope = cet_high), colour = "grey40", size = 1, lty=5) + 
    geom_abline(data=filter(thresh, iso == "IND"), aes(intercept = 0, slope = cet_low),  colour = "grey40", size = 1, lty=3) + 
    geom_errorbar(aes(x = DALY_avert.med, ymin = Inc_costs.low, ymax = Inc_costs.upp, colour = factor(intv,levels = intv_ord, labels = intv_name2)), 
                  width = 0, size = 1) +
    geom_errorbar(aes(y = Inc_costs.med, xmin = DALY_avert.low, xmax = DALY_avert.upp, , colour = factor(intv,levels = intv_ord, labels = intv_name2)), 
                  width = 0, size = 1) +
    scale_colour_manual(values = intv_col2) +
    scale_y_continuous(labels = scales::label_number(scale = 1e-9, suffix = 'B'),lim=c(ind_min,ind_max)) +
    scale_x_continuous(labels = scales::label_number(scale = 1e-6, suffix = 'M')) +
    labs(title="India", x = "DALYs averted", y = "", colour = "Intervention") +
    theme_classic(base_size = 18) +
    theme(plot.title = element_text(hjust = 0.5),
          axis.line.x = element_line(colour = "white", size = 0),
          axis.line.y = element_line(colour = "white", size = 0),
          strip.placement = "outside", 
          strip.background.x = element_rect(colour = "white", fill=NULL), 
          strip.text = element_text(size = 18),
          #text = element_text(family = "Open sans"),
          legend.position = "none") +
    guides(colour = guide_legend(nrow = 2, byrow = TRUE))
  
  png(here('outputs', 'res', 'plots', paste0("cea2fig1_cost-daly_",format(Sys.time(), "%Y%m%d_%H-%M"),".png")), bg = 'transparent', width = 16, height = 7.75, units = 'in', res = 1000)
  (cea_bra | cea_ind | cea_zaf) / 
      legend +
    plot_layout(heights = c(2, 0.2))
  dev.off()

# Cea2Fig2: Cost v DALY (no high cost screening)
  tempforlegend3 <- ggplot(filter(costdaly, !intv %in% c("BAU","SCRhi"))) +
    facet_wrap(~iso, scales = "free", labeller = as_labeller(c("BRA" = "Brazil", "IND" = "India", "ZAF" = "South Africa"))) +
    geom_hline(yintercept = 0, size = 1) +
    geom_vline(xintercept = 0, size = 1) +
    geom_point(aes(x = DALY_avert.med, y = Inc_costs.med, colour = factor(intv,levels = intv_ord3,labels=intv_name3)), alpha=0.5, size=2.5) + 
    geom_abline(aes(intercept = 0, slope = cet_high), colour = "grey40", data = thresh, size = 1, lty=5) + 
    geom_abline(aes(intercept = 0, slope = cet_low ), colour = "grey40", data = thresh, size = 1, lty=3) + 
    geom_errorbar(aes(x = DALY_avert.med, ymin = Inc_costs.low, ymax = Inc_costs.upp, colour = factor(intv,levels = intv_ord3, labels = intv_name3)), 
                  width = 0, size = 1) +
    geom_errorbar(aes(y = Inc_costs.med, xmin = DALY_avert.low, xmax = DALY_avert.upp, , colour = factor(intv,levels = intv_ord3, labels = intv_name3)), 
                  width = 0, size = 1) +
    scale_colour_manual(values = intv_col3) +
    scale_y_continuous(labels = scales::label_number(scale = 1e-9, suffix = 'B')) +
    scale_x_continuous(labels = scales::label_number(scale = 1e-6, suffix = 'M')) +
    labs(x = "DALYs averted", y = "Incremental budget (USD)", colour = "Intervention") +
    theme_classic(base_size = 18) +
    theme(plot.title = element_text(hjust = 0.5),
          axis.line.x = element_line(colour = "white", size = 0),
          axis.line.y = element_line(colour = "white", size = 0),
          strip.placement = "outside", 
          strip.background.x = element_rect(colour = "white", fill=NULL), 
          strip.text = element_text(size = 18),
          legend.position = "bottom") +
    guides(colour = guide_legend(nrow = 2, byrow = TRUE))
  
  legend3 <- g_legend(tempforlegend3)
  
  cea_zaf <- ggplot(filter(costdaly, !intv %in% c("BAU","SCRhi"), iso == "ZAF")) +
    geom_hline(yintercept = 0, size = 1) +
    geom_vline(xintercept = 0, size = 1) +
    geom_point(aes(x = DALY_avert.med, y = Inc_costs.med, colour = factor(intv,levels = intv_ord3,labels=intv_name3)), alpha=0.5, size=2.5) + 
    geom_abline(data=filter(thresh, iso == "ZAF"), aes(intercept = 0, slope = cet_high), colour = "grey40", size = 1, lty=5) + 
    geom_errorbar(aes(x = DALY_avert.med, ymin = Inc_costs.low, ymax = Inc_costs.upp, colour = factor(intv,levels = intv_ord3, labels = intv_name3)), 
                  width = 0, size = 1) +
    geom_errorbar(aes(y = Inc_costs.med, xmin = DALY_avert.low, xmax = DALY_avert.upp, , colour = factor(intv,levels = intv_ord3, labels = intv_name3)), 
                  width = 0, size = 1) +
    scale_colour_manual(values = intv_col3) +
    scale_y_continuous(labels = scales::label_number(scale = 1e-9, suffix = 'B')) +
    scale_x_continuous(labels = scales::label_number(scale = 1e-6, suffix = 'M')) +
    labs(title= "South Africa", x = "", y = "", colour = "Intervention") +
    theme_classic(base_size = 18) +
    theme(plot.title = element_text(hjust = 0.5),
          axis.line.x = element_line(colour = "white", size = 0),
          axis.line.y = element_line(colour = "white", size = 0),
          strip.placement = "outside", 
          strip.background.x = element_rect(colour = "white", fill=NULL), 
          strip.text = element_text(size = 18),
          legend.position = "none") +
    guides(colour = guide_legend(nrow = 2, byrow = TRUE))
  
  negscal <- layer_scales(cea_zaf)$y$range$range[1]/layer_scales(cea_zaf)$y$range$range[2]
  
  cea_bra_temp <- ggplot(filter(costdaly, !intv %in% c("BAU","SCRhi"), iso == "BRA")) +
    geom_hline(yintercept = 0, size = 1) +
    geom_vline(xintercept = 0, size = 1) +
    geom_point(aes(x = DALY_avert.med, y = Inc_costs.med, colour = factor(intv,levels = intv_ord3,labels=intv_name3)), alpha=0.5, size=2.5) + 
    geom_abline(data=filter(thresh, iso == "BRA"), aes(intercept = 0, slope = cet_high), colour = "grey40", size = 1, lty=5) + 
    geom_errorbar(aes(x = DALY_avert.med, ymin = Inc_costs.low, ymax = Inc_costs.upp, colour = factor(intv,levels = intv_ord3, labels = intv_name3)), 
                  width = 0, size = 1) +
    geom_errorbar(aes(y = Inc_costs.med, xmin = DALY_avert.low, xmax = DALY_avert.upp, , colour = factor(intv,levels = intv_ord3, labels = intv_name3)), 
                  width = 0, size = 1) +
    scale_colour_manual(values = intv_col3) +
    scale_y_continuous(labels = scales::label_number(scale = 1e-9, suffix = 'B')) +
    scale_x_continuous(labels = scales::label_number(scale = 1e-6, suffix = 'M')) +
    theme_classic(base_size = 18) +
    theme(plot.title = element_text(hjust = 0.5),
          axis.line.x = element_line(colour = "white", size = 0),
          axis.line.y = element_line(colour = "white", size = 0),
          strip.placement = "outside", 
          strip.background.x = element_rect(colour = "white", fill=NULL), 
          strip.text = element_text(size = 18),
          legend.position = "none") +
    guides(colour = guide_legend(nrow = 2, byrow = TRUE))
  
  bra_min <- negscal*layer_scales(cea_bra_temp)$y$range$range[2]
  bra_max <- layer_scales(cea_bra_temp)$y$range$range[2]
  
  cea_bra <- ggplot(filter(costdaly, !intv %in% c("BAU","SCRhi"), iso == "BRA")) +
    geom_hline(yintercept = 0, size = 1) +
    geom_vline(xintercept = 0, size = 1) +
    geom_point(aes(x = DALY_avert.med, y = Inc_costs.med, colour = factor(intv,levels = intv_ord3,labels=intv_name3)), alpha=0.5, size=2.5) + 
    geom_abline(data=filter(thresh, iso == "BRA"), aes(intercept = 0, slope = cet_high), colour = "grey40", size = 1, lty=5) + 
    geom_errorbar(aes(x = DALY_avert.med, ymin = Inc_costs.low, ymax = Inc_costs.upp, colour = factor(intv,levels = intv_ord3, labels = intv_name3)), 
                  width = 0, size = 1) +
    geom_errorbar(aes(y = Inc_costs.med, xmin = DALY_avert.low, xmax = DALY_avert.upp, , colour = factor(intv,levels = intv_ord3, labels = intv_name3)), 
                  width = 0, size = 1) +
    scale_colour_manual(values = intv_col3) +
    scale_y_continuous(labels = scales::label_number(scale = 1e-9, suffix = 'B'),lim=c(bra_min,bra_max)) +
    scale_x_continuous(labels = scales::label_number(scale = 1e-6, suffix = 'M')) +
    labs(title="Brazil", x = "", y = "Incremental budget (USD)", colour = "Intervention") +
    theme_classic(base_size = 18) +
    theme(plot.title = element_text(hjust = 0.5),
          axis.line.x = element_line(colour = "white", size = 0),
          axis.line.y = element_line(colour = "white", size = 0),
          strip.placement = "outside", 
          strip.background.x = element_rect(colour = "white", fill=NULL), 
          strip.text = element_text(size = 18),
          legend.position = "none") +
    guides(colour = guide_legend(nrow = 2, byrow = TRUE))
  
  cea_ind_temp <- ggplot(filter(costdaly, !intv %in% c("BAU","SCRhi"), iso == "IND")) +
    geom_hline(yintercept = 0, size = 1) +
    geom_vline(xintercept = 0, size = 1) +
    geom_point(aes(x = DALY_avert.med, y = Inc_costs.med, colour = factor(intv,levels = intv_ord3,labels=intv_name3)), alpha=0.5, size=2.5) + 
    geom_abline(data=filter(thresh, iso == "IND"), aes(intercept = 0, slope = cet_high), colour = "grey40", size = 1, lty=5) + 
    geom_errorbar(aes(x = DALY_avert.med, ymin = Inc_costs.low, ymax = Inc_costs.upp, colour = factor(intv,levels = intv_ord3, labels = intv_name3)), 
                  width = 0, size = 1) +
    geom_errorbar(aes(y = Inc_costs.med, xmin = DALY_avert.low, xmax = DALY_avert.upp, , colour = factor(intv,levels = intv_ord3, labels = intv_name3)), 
                  width = 0, size = 1) +
    scale_colour_manual(values = intv_col3) +
    scale_y_continuous(labels = scales::label_number(scale = 1e-9, suffix = 'B')) +
    scale_x_continuous(labels = scales::label_number(scale = 1e-6, suffix = 'M')) +
    theme_classic(base_size = 18) +
    theme(plot.title = element_text(hjust = 0.5),
          axis.line.x = element_line(colour = "white", size = 0),
          axis.line.y = element_line(colour = "white", size = 0),
          strip.placement = "outside", 
          strip.background.x = element_rect(colour = "white", fill=NULL), 
          strip.text = element_text(size = 18),
          legend.position = "none") +
    guides(colour = guide_legend(nrow = 2, byrow = TRUE))
  
  ind_min <- negscal*layer_scales(cea_ind_temp)$y$range$range[2]
  ind_max <- layer_scales(cea_ind_temp)$y$range$range[2]
  
  cea_ind <- ggplot(filter(costdaly, !intv %in% c("BAU","SCRhi"), iso == "IND")) +
    geom_hline(yintercept = 0, size = 1) +
    geom_vline(xintercept = 0, size = 1) +
    geom_point(aes(x = DALY_avert.med, y = Inc_costs.med, colour = factor(intv,levels = intv_ord3,labels=intv_name3)), alpha=0.5, size=2.5) + 
    geom_abline(data=filter(thresh, iso == "IND"), aes(intercept = 0, slope = cet_high), colour = "grey40", size = 1, lty=5) + 
    geom_errorbar(aes(x = DALY_avert.med, ymin = Inc_costs.low, ymax = Inc_costs.upp, colour = factor(intv,levels = intv_ord3, labels = intv_name3)), 
                  width = 0, size = 1) +
    geom_errorbar(aes(y = Inc_costs.med, xmin = DALY_avert.low, xmax = DALY_avert.upp, , colour = factor(intv,levels = intv_ord3, labels = intv_name3)), 
                  width = 0, size = 1) +
    scale_colour_manual(values = intv_col3) +
    scale_y_continuous(labels = scales::label_number(scale = 1e-9, suffix = 'B'),lim=c(ind_min,ind_max)) +
    scale_x_continuous(labels = scales::label_number(scale = 1e-6, suffix = 'M')) +
    labs(title="India", x = "DALYs averted", y = "", colour = "Intervention") +
    theme_classic(base_size = 18) +
    theme(plot.title = element_text(hjust = 0.5),
          axis.line.x = element_line(colour = "white", size = 0),
          axis.line.y = element_line(colour = "white", size = 0),
          strip.placement = "outside", 
          strip.background.x = element_rect(colour = "white", fill=NULL), 
          strip.text = element_text(size = 18),
          legend.position = "none") +
    guides(colour = guide_legend(nrow = 2, byrow = TRUE))
  
  png(here('outputs', 'res', 'plots', paste0("cea2fig2_cost-daly_",format(Sys.time(), "%Y%m%d_%H-%M"),".png")), bg = 'transparent', width = 16, height = 7.75, units = 'in', res = 1000)
  (cea_bra | cea_ind | cea_zaf) / 
    legend3 +
    plot_layout(heights = c(2, 0.2))
  dev.off()

# Cea2Fig3: Cost v DALY (no high cost screening, with labels)
  cea_zaf <- ggplot(filter(costdaly, !intv %in% c("BAU","SCRhi"), iso == "ZAF")) +
    geom_hline(yintercept = 0, size = 1) +
    geom_vline(xintercept = 0, size = 1) +
    geom_point(aes(x = DALY_avert.med, y = Inc_costs.med, colour = factor(intv,levels = intv_ord3,labels=intv_name3)), alpha=0.5, size=2.5) + 
    geom_text(aes(x = DALY_avert.med, y = Inc_costs.med, label=factor(intv,levels = intv_ord,labels=intv_name2)), 
              vjust = -0.9, hjust = -.15) +
    geom_abline(data=filter(thresh, iso == "ZAF"), aes(intercept = 0, slope = cet_high), colour = "grey40", size = 1, lty=5) + 
    geom_errorbar(aes(x = DALY_avert.med, ymin = Inc_costs.low, ymax = Inc_costs.upp, colour = factor(intv,levels = intv_ord3, labels = intv_name3)), 
                  width = 0, size = 1) +
    geom_errorbar(aes(y = Inc_costs.med, xmin = DALY_avert.low, xmax = DALY_avert.upp, , colour = factor(intv,levels = intv_ord3, labels = intv_name3)), 
                  width = 0, size = 1) +
    scale_colour_manual(values = intv_col2) +
    scale_y_continuous(labels = scales::label_number(scale = 1e-9, suffix = 'B')) +
    scale_x_continuous(labels = scales::label_number(scale = 1e-6, suffix = 'M')) +
    labs(title= "South Africa", x = "DALYs averted", y = "", colour = "Intervention") +
    theme_classic(base_size = 18) +
    theme(plot.title = element_text(hjust = 0.5),
          axis.line.x = element_line(colour = "white", size = 0),
          axis.line.y = element_line(colour = "white", size = 0),
          strip.placement = "outside", 
          strip.background.x = element_rect(colour = "white", fill=NULL), 
          strip.text = element_text(size = 18),
          legend.position = "none") +
    guides(colour = guide_legend(nrow = 2, byrow = TRUE))
  
  negscal <- layer_scales(cea_zaf)$y$range$range[1]/layer_scales(cea_zaf)$y$range$range[2]
  
  cea_bra_temp <- ggplot(filter(costdaly, !intv %in% c("BAU","SCRhi"), iso == "BRA")) +
    geom_hline(yintercept = 0, size = 1) +
    geom_vline(xintercept = 0, size = 1) +
    geom_point(aes(x = DALY_avert.med, y = Inc_costs.med, colour = factor(intv,levels = intv_ord3,labels=intv_name3)), alpha=0.5, size=2.5) + 
    geom_text(aes(x = DALY_avert.med, y = Inc_costs.med, label=factor(intv,levels = intv_ord,labels=intv_name2)), 
              vjust = -0.9, hjust = -.15) +
    geom_abline(data=filter(thresh, iso == "BRA"), aes(intercept = 0, slope = cet_high), colour = "grey40", size = 1, lty=5) + 
    geom_errorbar(aes(x = DALY_avert.med, ymin = Inc_costs.low, ymax = Inc_costs.upp, colour = factor(intv,levels = intv_ord3, labels = intv_name3)), 
                  width = 0, size = 1) +
    geom_errorbar(aes(y = Inc_costs.med, xmin = DALY_avert.low, xmax = DALY_avert.upp, , colour = factor(intv,levels = intv_ord3, labels = intv_name3)), 
                  width = 0, size = 1) +
    scale_colour_manual(values = intv_col2) +
    scale_y_continuous(labels = scales::label_number(scale = 1e-9, suffix = 'B')) +
    scale_x_continuous(labels = scales::label_number(scale = 1e-6, suffix = 'M')) +
    theme_classic(base_size = 18) +
    theme(plot.title = element_text(hjust = 0.5),
          axis.line.x = element_line(colour = "white", size = 0),
          axis.line.y = element_line(colour = "white", size = 0),
          strip.placement = "outside", 
          strip.background.x = element_rect(colour = "white", fill=NULL), 
          strip.text = element_text(size = 18),
          legend.position = "none") +
    guides(colour = guide_legend(nrow = 2, byrow = TRUE))
  
  bra_min <- negscal*layer_scales(cea_bra_temp)$y$range$range[2]
  bra_max <- layer_scales(cea_bra_temp)$y$range$range[2]
  
  cea_bra <- ggplot(filter(costdaly, !intv %in% c("BAU","SCRhi"), iso == "BRA")) +
    geom_hline(yintercept = 0, size = 1) +
    geom_vline(xintercept = 0, size = 1) +
    geom_point(aes(x = DALY_avert.med, y = Inc_costs.med, colour = factor(intv,levels = intv_ord3,labels=intv_name3)), alpha=0.5, size=2.5) + 
    geom_text(aes(x = DALY_avert.med, y = Inc_costs.med, label=factor(intv,levels = intv_ord,labels=intv_name2)), 
              vjust = -0.9, hjust = -.15) +
    geom_abline(data=filter(thresh, iso == "BRA"), aes(intercept = 0, slope = cet_high), colour = "grey40", size = 1, lty=5) + 
    geom_errorbar(aes(x = DALY_avert.med, ymin = Inc_costs.low, ymax = Inc_costs.upp, colour = factor(intv,levels = intv_ord3, labels = intv_name3)), 
                  width = 0, size = 1) +
    geom_errorbar(aes(y = Inc_costs.med, xmin = DALY_avert.low, xmax = DALY_avert.upp, , colour = factor(intv,levels = intv_ord3, labels = intv_name3)), 
                  width = 0, size = 1) +
    scale_colour_manual(values = intv_col2) +
    scale_y_continuous(labels = scales::label_number(scale = 1e-9, suffix = 'B'),lim=c(bra_min,bra_max)) +
    scale_x_continuous(labels = scales::label_number(scale = 1e-6, suffix = 'M')) +
    labs(title="Brazil", x = "", y = "Incremental budget (USD)", colour = "Intervention") +
    theme_classic(base_size = 18) +
    theme(plot.title = element_text(hjust = 0.5),
          axis.line.x = element_line(colour = "white", size = 0),
          axis.line.y = element_line(colour = "white", size = 0),
          strip.placement = "outside", 
          strip.background.x = element_rect(colour = "white", fill=NULL), 
          strip.text = element_text(size = 18),
          legend.position = "none") +
    guides(colour = guide_legend(nrow = 2, byrow = TRUE))
  
  cea_ind_temp <- ggplot(filter(costdaly, !intv %in% c("BAU","SCRhi"), iso == "IND")) +
    geom_hline(yintercept = 0, size = 1) +
    geom_vline(xintercept = 0, size = 1) +
    geom_point(aes(x = DALY_avert.med, y = Inc_costs.med, colour = factor(intv,levels = intv_ord3,labels=intv_name3)), alpha=0.5, size=2.5) + 
    geom_text(aes(x = DALY_avert.med, y = Inc_costs.med, label=factor(intv,levels = intv_ord,labels=intv_name2)), 
              vjust = -0.9, hjust = -.15) +
    geom_abline(data=filter(thresh, iso == "IND"), aes(intercept = 0, slope = cet_high), colour = "grey40", size = 1, lty=5) + 
    geom_errorbar(aes(x = DALY_avert.med, ymin = Inc_costs.low, ymax = Inc_costs.upp, colour = factor(intv,levels = intv_ord3, labels = intv_name3)), 
                  width = 0, size = 1) +
    geom_errorbar(aes(y = Inc_costs.med, xmin = DALY_avert.low, xmax = DALY_avert.upp, , colour = factor(intv,levels = intv_ord3, labels = intv_name3)), 
                  width = 0, size = 1) +
    scale_colour_manual(values = intv_col2) +
    scale_y_continuous(labels = scales::label_number(scale = 1e-9, suffix = 'B')) +
    scale_x_continuous(labels = scales::label_number(scale = 1e-6, suffix = 'M')) +
    theme_classic(base_size = 18) +
    theme(plot.title = element_text(hjust = 0.5),
          axis.line.x = element_line(colour = "white", size = 0),
          axis.line.y = element_line(colour = "white", size = 0),
          strip.placement = "outside", 
          strip.background.x = element_rect(colour = "white", fill=NULL), 
          strip.text = element_text(size = 18),
          legend.position = "none") +
    guides(colour = guide_legend(nrow = 2, byrow = TRUE))
  
  ind_min <- negscal*layer_scales(cea_ind_temp)$y$range$range[2]
  ind_max <- layer_scales(cea_ind_temp)$y$range$range[2]
  
  cea_ind <- ggplot(filter(costdaly, !intv %in% c("BAU","SCRhi"), iso == "IND")) +
    geom_hline(yintercept = 0, size = 1) +
    geom_vline(xintercept = 0, size = 1) +
    geom_point(aes(x = DALY_avert.med, y = Inc_costs.med, colour = factor(intv,levels = intv_ord3,labels=intv_name3)), alpha=0.5, size=2.5) + 
    geom_text(aes(x = DALY_avert.med, y = Inc_costs.med, label=factor(intv,levels = intv_ord,labels=intv_name2)), 
              vjust = -0.9, hjust = -.15) +
    geom_abline(data=filter(thresh, iso == "IND"), aes(intercept = 0, slope = cet_high), colour = "grey40", size = 1, lty=5) + 
    geom_errorbar(aes(x = DALY_avert.med, ymin = Inc_costs.low, ymax = Inc_costs.upp, colour = factor(intv,levels = intv_ord3, labels = intv_name3)), 
                  width = 0, size = 1) +
    geom_errorbar(aes(y = Inc_costs.med, xmin = DALY_avert.low, xmax = DALY_avert.upp, , colour = factor(intv,levels = intv_ord3, labels = intv_name3)), 
                  width = 0, size = 1) +
    scale_colour_manual(values = intv_col2) +
    scale_y_continuous(labels = scales::label_number(scale = 1e-9, suffix = 'B'),lim=c(ind_min,ind_max)) +
    scale_x_continuous(labels = scales::label_number(scale = 1e-6, suffix = 'M')) +
    labs(title="India", x = "", y = "", colour = "Intervention") +
    theme_classic(base_size = 18) +
    theme(plot.title = element_text(hjust = 0.5),
          axis.line.x = element_line(colour = "white", size = 0),
          axis.line.y = element_line(colour = "white", size = 0),
          strip.placement = "outside", 
          strip.background.x = element_rect(colour = "white", fill=NULL), 
          strip.text = element_text(size = 18),
          legend.position = "none") +
    guides(colour = guide_legend(nrow = 2, byrow = TRUE))
  
  png(here('outputs', 'res', 'plots', paste0("cea2fig3_cost-daly_",format(Sys.time(), "%Y%m%d_%H-%M"),".png")), bg = 'transparent', width = 16, height = 7.75, units = 'in', res = 1000)
  (cea_bra | cea_ind | cea_zaf) / 
    legend +
    plot_layout(heights = c(2, 0.2))
  dev.off()
  
  # Cea2Tab1: DALYs averted
  dalys <- cea %>%
    filter(var == "DALY") %>%
    select(iso, intv, med, low, upp) %>% 
    pivot_wider(names_from = iso, values_from = c(med, low, upp)) %>% 
    mutate(across(-intv, ~ round(.x * 1e-6, digits = 1)))%>% #,
    rowwise() %>%
    mutate(across(matches("^med_"), ~ trimws(paste0(.x, "M\n", "(",
                                                    trimws(get(sub("med_", "low_", cur_column()))), "-",
                                                    trimws(get(sub("med_", "upp_", cur_column()))), "M)")))) %>%
    select(intv, matches("^med_")) %>%
    rename_with(~ sub("^med_", "", .), matches("^med_"))
  
  dalyt2 <- dalys %>%
    filter(!(str_detect(intv, 'SCRhi'))) %>%
    select(intv, BRA, IND, ZAF) %>%
    arrange(factor(intv, levels = intv_ord4)) %>%
    mutate(intvn = intv_name4[match(intv, intv_ord4)]) %>%
    mutate(BRA = ifelse(BRA == 'NAM\n(NA-NAM)', '-', BRA)) %>%
    mutate(IND = ifelse(IND == 'NAM\n(NA-NAM)', '-', IND)) %>%
    mutate(ZAF = ifelse(ZAF == 'NAM\n(NA-NAM)', '-', ZAF)) %>%
    relocate(5) %>%
    select(intvn, BRA, IND, ZAF) %>% 
    ungroup()
  colnames(dalyt2) <- c("Intervention", "Brazil", "India", "South Africa")
  
  dalyt2 %>% 
    gt() %>% 
    tab_header(title = md("Disability-adjusted life years<br>(number)")) %>%
    cols_align(align = "center") %>%
    cols_width(everything() ~ px(200)) %>%
    cols_width(Intervention ~ px(150)) %>%
    opt_table_font(font = 'Open sans') %>%
    gtsave(filename = here('outputs', 'res', 'plots', paste0("cea2tab1_daly_",format(Sys.time(), "%Y%m%d_%H-%M"),".png")))
  
  # Cea2Tab2: DALYs averted
  dalys <- cea %>%
    filter(var == "DALY_avert") %>%
    select(iso, intv, med, low, upp) %>% 
    pivot_wider(names_from = iso, values_from = c(med, low, upp)) %>% 
    mutate(across(-intv, ~ round(.x * 1e-6, digits = 1)))%>% #,
    rowwise() %>%
    mutate(across(matches("^med_"), ~ trimws(paste0(.x, "M\n", "(",
                                                    trimws(get(sub("med_", "low_", cur_column()))), "-",
                                                    trimws(get(sub("med_", "upp_", cur_column()))), "M)")))) %>%
    select(intv, matches("^med_")) %>%
    rename_with(~ sub("^med_", "", .), matches("^med_"))
  
  dalyt2 <- dalys %>%
    filter(!(str_detect(intv, 'BAU'))) %>%
    select(intv, BRA, IND, ZAF) %>%
    arrange(factor(intv, levels = intv_ord)) %>%
    mutate(intvn = intv_name[match(intv, intv_ord)]) %>%
    mutate(BRA = ifelse(BRA == 'NAM\n(NA-NAM)', '-', BRA)) %>%
    mutate(IND = ifelse(IND == 'NAM\n(NA-NAM)', '-', IND)) %>%
    mutate(ZAF = ifelse(ZAF == 'NAM\n(NA-NAM)', '-', ZAF)) %>%
    relocate(5) %>%
    select(intvn, BRA, IND, ZAF) %>% 
    ungroup() %>% 
    filter(!row_number() %in% c(4, 5)) %>% 
    mutate(intvn = ifelse(row_number() == 3, "Comm Scr", intvn))
  colnames(dalyt2) <- c("Intervention", "Brazil", "India", "South Africa")
  
  dalyt2 %>% 
    gt() %>% 
    tab_header(title = md("Disability-adjusted life years averted<br>(number relative to BAU)")) %>%
    cols_align(align = "center") %>%
    cols_width(everything() ~ px(200)) %>%
    cols_width(Intervention ~ px(150)) %>%
    opt_table_font(font = 'Open sans') %>%
    gtsave(filename = here('outputs', 'res', 'plots', paste0("cea2tab2_dalyavert_",format(Sys.time(), "%Y%m%d_%H-%M"),".png")))

# Cea3Tab1: ICERs
  nb <- cea %>%
    filter(var == "ICERs") %>%
    select(iso, intv, med, low, upp) %>% 
    pivot_wider(names_from = iso, values_from = c(med, low, upp)) %>% 
    mutate(across(-intv, ~ round(.x, digits = 0)),
           across(-intv, ~ format(.x, big.mark = ",", scientific = FALSE))) %>% 
    rowwise() %>%
    mutate(across(matches("^med_"), ~ trimws(paste0(.x, " \n(", 
                                                    trimws(get(sub("med_", "low_", cur_column()))), "-",
                                                    trimws(get(sub("med_", "upp_", cur_column()))), ")")))) %>%
    select(intv, matches("^med_")) %>%
    rename_with(~ sub("^med_", "", .), matches("^med_")) 
    
  nb2 <- nb %>%
   arrange(factor(intv, levels = intv_ord)) %>%
    mutate(intvn = intv_name[match(intv, intv_ord)]) %>%
    mutate(BRA = ifelse(BRA == "NA \n(NA-NA)", '-', BRA)) %>%
    mutate(IND = ifelse(IND == "NA \n(NA-NA)", '-', IND)) %>%
    mutate(ZAF = ifelse(ZAF == "NA \n(NA-NA)", '-', ZAF)) %>%
    relocate(5) %>%
    select(intvn, BRA, IND, ZAF)
  colnames(nb2) <- c("Intervention", "Brazil", "India", "South Africa")
  nb2 <- nb2[c(1:10),]
  
  nb2 %>% 
    gt() %>% 
    tab_header(title = md("ICERs")) %>%
    cols_align(align = "center") %>%
    cols_width(everything() ~ px(200)) %>%
    cols_width(Intervention ~ px(150)) %>%
    opt_table_font(font = 'Open sans') %>%
    gtsave(filename = here('outputs', 'res', 'plots', paste0("cea3tab1_icer_",format(Sys.time(), "%Y%m%d_%H-%M"),".png")))

# Cea4Tab1: Budget impact
  budg <- cea %>%
    filter(var == "c_Int") %>%
    select(iso, intv, med, low, upp) %>% 
    pivot_wider(names_from = iso, values_from = c(med, low, upp)) %>% 
    mutate_at(vars(-intv),
              ~ . * 1e-9, TRUE ~ as.numeric(.)) %>% 
    mutate(across(-intv, ~ round(.x, digits = 1)),
           across(-intv, ~ format(.x, big.mark = ",", scientific = FALSE))) %>% 
    rowwise() %>%
    mutate(across(matches("^med_"), ~ trimws(paste0(.x, "B\n(", 
                                                    trimws(get(sub("med_", "low_", cur_column()))), "-",
                                                    trimws(get(sub("med_", "upp_", cur_column()))), "B)")))) %>%
    select(intv, matches("^med_")) %>%
    rename_with(~ sub("^med_", "", .), matches("^med_"))
  
  budg2 <- budg %>%
    filter(!(str_detect(intv, 'BAU'))) %>%
    select(intv, BRA, IND, ZAF) %>%
    arrange(factor(intv, levels = intv_ord)) %>%
    mutate(intvn = intv_name[match(intv, intv_ord)]) %>%
    mutate(BRA = ifelse(BRA == 'NAB\n(NA-NAB)', '-', BRA)) %>%
    mutate(IND = ifelse(IND == 'NAB\n(NA-NAB)', '-', IND)) %>%
    mutate(ZAF = ifelse(ZAF == 'NAB\n(NA-NAB)', '-', ZAF)) %>%
    relocate(5) %>%
    select(intvn, BRA, IND, ZAF)
  colnames(budg2) <- c("Intervention", "Brazil", "India", "South Africa")
  
  budg2 %>% 
    gt() %>% 
    tab_header(title = md("Budget impact<br>(USD)")) %>%
    cols_align(align = "center") %>%
    cols_width(everything() ~ px(250)) %>%
    cols_width(Intervention ~ px(150)) %>%
    opt_table_font(font = 'Open sans') %>%
    gtsave(filename = here('outputs', 'res', 'plots', paste0("cea4tab1_budget_",format(Sys.time(), "%Y%m%d_%H-%M"),".png")))

# Extract CETs to compare results
cet <- read.xlsx(here("outputs", "res", "summ", "ceacalc.xlsx"), sheet = "VALUES") %>%
  as.data.frame() %>%
  `[`(13:14, 4:6) %>%
  `colnames<-`(c("BRA", "IND", "ZAF")) %>%
  mutate(across(everything(), as.numeric)) %>%
  `rownames<-`(c("cet_low", "cet_high"))
  
# Currency conversions
curr <- data.frame(BRA = 4.99, IND = 82.79, ZAF = 18.45)
  
# Summarise runs using means
cea_mean <- cea_mean %>% 
  mutate(CE_low = case_when(is.na(ICERs) & intv != "BAU" ~ 0,
                            iso == "BRA" ~ as.numeric(ICERs < cet["cet_low", "BRA"]),
                            iso == "IND" ~ as.numeric(ICERs < cet["cet_low", "IND"]),
                            iso == "ZAF" ~ as.numeric(ICERs < cet["cet_low", "ZAF"]),
                            TRUE ~ NA_real_)) %>% 
  mutate(CE_high = case_when(is.na(ICERs) & intv != "BAU" ~ 0,
                             iso == "BRA" ~ as.numeric(ICERs < cet["cet_high", "BRA"]),
                             iso == "IND" ~ as.numeric(ICERs < cet["cet_high", "IND"]),
                             iso == "ZAF" ~ as.numeric(ICERs < cet["cet_high", "ZAF"]),
                             TRUE ~ NA_real_)) %>% 
  mutate(c_Int_local = case_when(iso == "BRA" ~ c_Int * curr[, "BRA"],
                                 iso == "IND" ~ c_Int * curr[, "IND"],
                                 iso == "ZAF" ~ c_Int * curr[, "ZAF"],
                                 TRUE ~ NA_real_)) %>% 
  select(-ICERs) %>% 
  pivot_longer(cols = -c(iso, intv, run), names_to = "var", values_to = "val") %>% 
  group_by(iso, intv, var) %>% 
  summarise(mean = mean(val, na.rm = TRUE), .groups = "drop") %>% 
  pivot_wider(names_from = var, values_from = mean) %>% 
  mutate(ICER = case_when(Inc_costs == 0 & DALY_avert == 0 ~ NA_real_, 
                          Inc_costs >= 0 & DALY_avert <= 0 ~ Inf,
                          Inc_costs <= 0 & DALY_avert >= 0 ~ 0,
                          TRUE ~ Inc_costs / DALY_avert)) %>% 
  select(iso, intv, c_Int_local, c_Int, Inc_costs, DALY, DALY_avert, ICER, CE_low, CE_high)

# Clean-up
intv_ords4 <- c("BAU", "VAX", "TPT", "SCRhi", "SCRlo", "DGN", "DST", "SDS", "SDR", "PRI", "NTN")

cea_s4 <- cea_mean %>% 
  mutate(intv = factor(intv, levels = intv_ords4)) %>%
  arrange(iso, intv) %>% 
  mutate(c_Int_local = case_when(iso == "BRA" ~ formatC(c_Int_local / 1e9, format = "f", digits = 2, big.mark = ","),
                                 iso == "IND" ~ formatC(c_Int_local / 1e12, format = "f", digits = 2, big.mark = ","),
                                 iso == "ZAF" ~ formatC(c_Int_local / 1e9, format = "f", digits = 2, big.mark = ","),
                                 TRUE ~ NA_character_),
         c_Int = formatC(c_Int / 1e9, format = "f", digits = 2, big.mark = ","),
         Inc_costs = case_when(iso == "BRA" ~ formatC(Inc_costs / 1e6, format = "f", digits = 2, big.mark = ","),
                               iso == "IND" ~ formatC(Inc_costs / 1e9, format = "f", digits = 2, big.mark = ","),
                               iso == "ZAF" ~ formatC(Inc_costs / 1e6, format = "f", digits = 2, big.mark = ","),
                               TRUE ~ NA_character_),
         DALY = formatC(DALY / 1e6, format = "f", digits = 2, big.mark = ","),
         DALY_avert = formatC(DALY_avert / 1e6, format = "f", digits = 2, big.mark = ","),
         ICER = formatC(ICER, format = "f", digits = 0, big.mark = ","),
         CE_low = paste0(formatC(CE_low * 1e2, format = "f", digits = 0, big.mark = ","), "%"),
         CE_high = paste0(formatC(CE_high * 1e2, format = "f", digits = 0, big.mark = ","), "%")) %>% 
  mutate(Inc_costs = case_when(intv == 'BAU' ~ '-', TRUE ~ Inc_costs),
         DALY_avert = case_when(intv == 'BAU' ~ '-', TRUE ~ DALY_avert),
         ICER = case_when(ICER == ' NA' ~ '-', 
                          ICER == '0' ~ 'Dominates BAU',
                          ICER == 'Inf' ~ 'Dominated by BAU',
                          TRUE ~ ICER),
         CE_low = case_when(CE_low == 'NaN%' ~ '-', TRUE ~ CE_low),
         CE_high = case_when(CE_high == 'NaN%' ~ '-', TRUE ~ CE_high))
