# ----------------------------------------------------
# RISK plots (Running the CORE NIH2 model structure with the RISK dimension in tbmod)
# Elena Venero Garcia 
# ----------------------------------------------------

######################### Clean/Set data to work with ##########################

# Conditional function for UID in automated calibration
get_group_vars <- function(data, base_group_vars) {
  if ("uid" %in% names(data)) {
    return(c("uid", base_group_vars))
  } else {
    return(base_group_vars)
  }
}

# 1. Stocks ------------------------------------------
# Group the "from" "to" age groups columns in one and include only .999 years
TB_trends_49 <- output$stocks
TB_trends_49 <- TB_trends_49[grepl("\\.999$", TB_trends_49$year), ]
View(TB_trends_theory)
TB_trends_49 <- TB_trends_49[, `Age Group` := fcase(
  age_from == 0 & age_thru == 14, "[0,14]",
  age_from == 0 & age_thru == 99, "[0,99]",
  age_from == 15 & age_thru == 49, "[15,49]",
  age_from == 50 & age_thru == 99, "[50,99]")]

#TB_trends_theory <- TB_trends_49[, `Age Group` := fcase(
#  age_from == 0 & age_thru == 14, "[0,14]",
#  age_from == 0 & age_thru == 99, "[0,99]",
#  age_from == 15 & age_thru == 99, "[15,99]")]


# Variables to group by
group_to_group_ages <- get_group_vars(TB_trends_49, c("country", "year", "RISK", "TB", "Age Group"))

# Unify Adults group ([15,49] + [50,99] -> [15,99])
TB_trends_99 <- TB_trends_49 %>%
  mutate(`Age Group` = case_when(
    `Age Group` %in% c("[15,49]", "[50,99]") ~ "[15,99]",
    TRUE ~ `Age Group`
  )) %>%
  group_by(across(all_of(group_to_group_ages))) %>%
  summarize(value = sum(value), .groups = "drop")

TB_trends_99 <- setDT(TB_trends_99)

# Variables to group by
group_general <- get_group_vars(TB_trends_99, c("country", "year", "Age Group"))
group_risk <- get_group_vars(TB_trends_99, c("country", "year", "RISK", "Age Group"))

#** For prevalence, proportion subclinical, infection prevalence and ratio of non infectious to invetious TB**
# Filter out counts and death (usual age groups)
TB_trends_filtered <- TB_trends_99[!str_detect(TB, "count|dead") &  !str_detect(RISK, "dead") ]
# Filter out counts and death (ARI age groups)
TB_trends_filtered_49 <- TB_trends_49[!str_detect(TB, "count|dead") &  !str_detect(RISK, "dead") ]

#** For mortality **
#Select deaths 
TB_mort_filtered <- TB_trends_99[str_detect(TB, "dead")]

#** Population **
# Population (age groups {0,14] and [15,99])
population <-  TB_trends_filtered %>%
  group_by(across(all_of(group_general))) %>%
  summarise(Population = sum(value)) %>%
  mutate(year = floor(year))
population <- setDT(population)
View(population_49)

# Population used for ARI by age groups [0,14], [15,49] and [50+]
TB_trends_filtered_49 <- TB_trends_49[!str_detect(TB, "count|dead") &  !str_detect(RISK, "dead") ]
population_49 <-  TB_trends_filtered_49 %>%
  group_by(across(all_of(group_general))) %>%
  summarise(Population = sum(value)) %>%
  mutate(year = floor(year))
population_49 <- setDT(population_49)

# Population by RISK state (including unifected/community/)
population_risk <-  TB_trends_filtered %>%
  group_by(across(all_of(group_risk))) %>%
  summarise(Population = sum(value)) %>%
  mutate(year = floor(year))
population_risk <- setDT(population_risk)

# 2. Counts ---------------------------------------
# Group the "from" "to" age groups columns in one 
TB_counts_49 <- output$count.999
TB_counts_49 <- TB_counts_49[grepl("\\.999$", TB_counts_49$year), ]

TB_counts_49 <- TB_counts_49[, `Age Group` := fcase(
  age_from == 0 & age_thru == 14, "[0,14]",
  age_from == 0 & age_thru == 99, "[0,99]",
  age_from == 15 & age_thru == 49, "[15,49]",
  age_from == 50 & age_thru == 99, "[50,99]")]


# Unify Adults group ([15,49] + [50,99] -> [15,99])
TB_counts_99 <- TB_counts_49 %>%
  mutate(`Age Group` = case_when(
    `Age Group` %in% c("[15,49]", "[50,99]") ~ "[15,99]",
    TRUE ~ `Age Group`
  )) %>%
  group_by(across(all_of(group_to_group_ages))) %>%
  summarize(value = sum(value), .groups = "drop")

TB_counts_99 <- setDT(TB_counts_99)

################## Calculate RISK specific variables ####################

# 1. Calculate Plot 1: TB-RISK  ------------------------------
  #**ZAF: Clinical incidence per 100,000 (HIV+)**
  Inc_Dc_risk <- TB_counts_99[TB == "Inc_Dc_count"] 
  Inc_Dc_HIV <- Inc_Dc_risk[RISK == "HIVnt" | RISK == "HIVart"]
  Inc_Dc_HIV <-  Inc_Dc_HIV %>%
    group_by(across(all_of(group_general))) %>%
    summarise(total_inc = sum(value)) %>%
    mutate(year = floor(year))
  
  Dc_inc_adjust <- 0.86
  epTB_prop <- (1-0.077)
  Inc_Dc_HIV$adjusted_inc_red <- Inc_Dc_HIV$total_inc*Dc_inc_adjust
  Inc_Dc_HIV$adjusted_inc <- Inc_Dc_HIV$adjusted_inc_red/epTB_prop
  Inc_Dc_HIV <- setDT(Inc_Dc_HIV)

  Dc_inc_HIVpos <- merge(Inc_Dc_HIV, population)
  TB_risk1 <- Dc_inc_HIVpos[, total_value := (adjusted_inc/Population)*100000]


# 2. Calculate Plot 2: TB-RISK  --------------------------------
  #**ZAF: Notifications per 100,000 (HIV+)**
  Not_risk <- TB_counts_99[str_detect(TB, "Not")]
  Not_HIV <- Not_risk[RISK == "HIVnt" | RISK == "HIVart"]
  Not_HIV <-  Not_HIV %>%
    group_by(across(all_of(group_general))) %>%
    summarise(total_not = sum(value)) %>%
    mutate(year = floor(year))
  Not_HIV <- setDT(Not_HIV)
  Not_HIVpos <- merge(Not_HIV, population)
  TB_risk2 <- Not_HIVpos[, total_value := (total_not/Population)*100000]


# 3. Calculate Plot 3: TB-RISK  --------------------------------
  #**TB mortality per 100,000 (HIV+)**
  mort_tb_HIV <- TB_mort_filtered[RISK == "HIVnt" | RISK == "HIVart"]
  mort_tb_HIV <-  mort_tb_HIV %>%
    group_by(across(all_of(group_general))) %>%
    summarise(total_deaths = sum(value)) %>%
    mutate(year = floor(year))
  mort_tb_HIV <- setDT(mort_tb_HIV)
  tb_mort_HIVpos <- merge(mort_tb_HIV, population)
  TB_risk3 <- tb_mort_HIVpos[, total_value := (total_deaths/Population)*100000]
  
# 4. Calculate Plot 4: TB-RISK  --------------------------------
  #**TB mortality per 100,000 (HIV-)**
  mort_tb_noHIV <- TB_mort_filtered[RISK == "HIV0"]
  mort_tb_noHIV <-  mort_tb_noHIV %>%
    group_by(across(all_of(group_general))) %>%
    summarise(total_deaths = sum(value)) %>%
    mutate(year = floor(year))
  mort_tb_noHIV <- setDT(mort_tb_noHIV)
  tb_mort_HIVneg <- merge(mort_tb_noHIV, population)
  TB_risk4 <- tb_mort_HIVneg[, total_value := (total_deaths/Population)*100000] 
  
# 5. Calculate Plot 5: TB-RISK  --------------------------------
  #**TB notifications per 100,000 (HIV-)**
  Not_HIV0 <- Not_risk[RISK == "HIV0"]
  Not_HIV0 <-  Not_HIV0 %>%
    group_by(across(all_of(group_general))) %>%
    summarise(total_not = sum(value)) %>%
    mutate(year = floor(year))
  Not_HIV0 <- setDT(Not_HIV0)
  Not_HIVneg <- merge(Not_HIV0, population)
  TB_risk5 <- Not_HIVneg[, total_value := (total_not/Population)*100000]

# 6. Calculate Plot 6: TB-RISK  ------------------------------
  #**ZAF: Clinical incidence per 100,000 (HIV-)**
  Inc_Dc_HIV0 <- Inc_Dc_risk[RISK == "HIV0"]
  Inc_Dc_HIV0 <-  Inc_Dc_HIV0 %>%
    group_by(across(all_of(group_general))) %>%
    summarise(total_inc = sum(value)) %>%
    mutate(year = floor(year))
  
  Dc_inc_adjust <- 0.86
  Inc_Dc_HIV0$adjusted_inc_red <- Inc_Dc_HIV0$total_inc*Dc_inc_adjust
  Inc_Dc_HIV0$adjusted_inc <- Inc_Dc_HIV0$adjusted_inc_red/epTB_prop
  Inc_Dc_HIV0 <- setDT(Inc_Dc_HIV0)
  
  Dc_inc_HIVneg <- merge(Inc_Dc_HIV0, population)
  TB_risk6 <- Dc_inc_HIVneg[, total_value := (adjusted_inc/Population)*100000]
  
# 7. Calculate Plot 1: RISK dimension --------------------------
  #**HIV prevalence**
  prev_HIV <- TB_trends_filtered[RISK == "HIVnt" | RISK == "HIVart"]
  prev_HIV <-  prev_HIV %>%
    group_by(across(all_of(group_general))) %>%
    summarise(total_HIVpos = sum(value)) %>%
    mutate(year = floor(year))
  prev_HIV <- setDT(prev_HIV)
  HIVpos_prev <- merge(prev_HIV, population)
  risk1 <- HIVpos_prev[, total_value := (total_HIVpos/Population)*100]
View()
# 8. Calculate Plot 2: RISK dimension --------------------------  
  #**ART coverage**
  art_n <- TB_trends_filtered[ RISK == "HIVart"]
  art_n <-  art_n %>%
    group_by(across(all_of(group_general))) %>%
    summarise(total_HIVart = sum(value)) %>%
    mutate(year = floor(year))
  art_n <- setDT(art_n)
  art_pct <- merge(art_n, prev_HIV)
  risk2 <- art_pct[, total_value := (total_HIVart/total_HIVpos)*100]

# 9. Calculate Plot 3: RISK dimension --------------------------
  #**HIV mortality**
  TB_trends_99_noCount <- TB_trends_99[!str_detect(TB, "count")]
  mort_HIV <- TB_trends_99_noCount[RISK == "HIVdead"]
  mort_HIV <-  mort_HIV %>%
    group_by(across(all_of(group_general))) %>%
    summarise(total_HIVdeath = sum(value)) %>%
    mutate(year = floor(year))
  mort_HIV <- setDT(mort_HIV)
  HIVpos_mort <- merge(mort_HIV, population)
  risk3 <- HIVpos_mort[, total_value := (total_HIVdeath/Population)*100000]

######################## Plot risk specific graphs #############################
  
# Details for plotting
title_size = 13
axis_title_size = 11
x_axis_text_size = 9
y_axis_text_size = 9
legend_title_size = 10
legend_text_size = 10
title_align = 0.5
title_face = "bold"
line_thickness = 0.6
colour_line = "#7570B3"
point_size = 2
alpha = 0.2
patches_height = 1.6*16
height = 16
aspect_ratio = 1
error_bar_width = 0.5
date_line_thickness = 0.4
date_line_colour = "gray35"
prior_colour = "deepskyblue2"
  
breaks = seq(1999, 2050, 1)
limits = c(1999, 2050.5)
from_to = seq(1999,2050)


# Functions to plot ----------------------------------------

# 0 to 99 only 
plot_calibration_all <- function(metric_dataset, target_name, plot_title, yaxis_title) {
  if ("uid" %in% names(metric_dataset)) {
    # Plot if UID is present
    plot_calibration <- ggplot(subset(metric_dataset, `Age Group` == "[0,99]" & year %in% from_to), aes(x = year)) +
      geom_line(aes(y = total_value, group = "factor(uid)", color = "factor(uid)"), linewidth = line_thickness, na.rm=TRUE) +
      ylim(c(0, NA)) +
      geom_errorbar(data = subset(targets, name == target_name), aes(ymin = lo, ymax = hi), size = line_thickness, width = error_bar_width) +
      geom_point(data = subset(targets, name == target_name), 
                 aes(y = value), size = point_size, colour = "black") +
      labs(title = plot_title) +
      ylab(yaxis_title) + xlab("Year") +
      theme(text = element_text(family = "sans"),
            axis.line = element_line(),
            axis.line.x.bottom = element_line(linewidth = line_thickness),
            axis.line.y.left = element_line(linewidth = line_thickness),
            axis.ticks = element_line(linewidth = line_thickness),
            axis.title.x.bottom = element_text(size= axis_title_size, vjust = -2, margin = margin(b = 20)),
            axis.title.y.left = element_text(size= axis_title_size, vjust= 3, margin = margin(l = 15)),
            axis.text.x.bottom = element_text(size= x_axis_text_size, vjust = 0.3, hjust = 0, angle = 90),
            plot.title = element_text(size = title_size, vjust = 6, hjust = title_align, margin = margin(t = 20), face = title_face))
  
    } else {
    # Plot "b" if UID is not present
    plot_calibration <- ggplot(subset(metric_dataset, `Age Group` == "[0,99]" & year %in% from_to), aes(x = year)) +
      geom_line(aes(y = total_value), linewidth = line_thickness, na.rm=TRUE, colour = colour_line) +
      ylim(c(0, NA)) +
      geom_errorbar(data = subset(targets, name == target_name), aes(ymin = lo, ymax = hi), size = line_thickness, width = error_bar_width) +
      geom_point(data = subset(targets, name == target_name), 
                 aes(y = value), size = point_size, colour = "black") +
      labs(title = plot_title) +
      ylab(yaxis_title) + xlab("Year") +
      theme(text = element_text(family = "sans"),
            axis.line = element_line(),
            axis.line.x.bottom = element_line(linewidth = line_thickness),
            axis.line.y.left = element_line(linewidth = line_thickness),
            axis.ticks = element_line(linewidth = line_thickness),
            axis.title.x.bottom = element_text(size= axis_title_size, vjust = -2, margin = margin(b = 20)),
            axis.title.y.left = element_text(size= axis_title_size, vjust= 3, margin = margin(l = 15)),
            axis.text.x.bottom = element_text(size= x_axis_text_size, vjust = 0.3, hjust = 0, angle = 90),
            plot.title = element_text(size = title_size, vjust = 6, hjust = title_align, margin = margin(t = 20), face = title_face)) +
      theme(legend.position = "none")
  }
  return(plot_calibration)
}

# By age group (children and adults)
plot_calibration_age <- function(metric_dataset, target_name_child, target_name_adult, plot_title, yaxis_title) {
  if ("uid" %in% names(metric_dataset)) {
  
  # Plot if UID is present
  plot1_calibration <- ggplot(subset(metric_dataset, `Age Group` == "[0,14]"  & year %in% from_to), aes(x = year)) +
    geom_line(aes(y = total_value, group = "factor(uid)", color = "factor(uid)"), linewidth = line_thickness, na.rm=TRUE) +
    ylim(c(0, NA)) +
    geom_errorbar(data = subset(targets, name == target_name_child), aes(ymin = lo, ymax = hi), size = line_thickness, width = error_bar_width) +
    geom_point(data = subset(targets, name == target_name_child), 
               aes(y = value), size = point_size, colour = "black") +
    labs(title = paste(plot_title, "in children", sep= " ")) +
    ylab(yaxis_title) + xlab("Year") +
    theme(text = element_text(family = "sans"),
          axis.line = element_line(),
          axis.line.x.bottom = element_line(linewidth = line_thickness),
          axis.line.y.left = element_line(linewidth = line_thickness),
          axis.ticks = element_line(linewidth = line_thickness),
          axis.title.x.bottom = element_text(size= axis_title_size, vjust = -2, margin = margin(b = 20)),
          axis.title.y.left = element_text(size= axis_title_size, vjust= 3, margin = margin(l = 15)),
          axis.text.x.bottom = element_text(size= x_axis_text_size, vjust = 0.3, hjust = 0, angle = 90),
          plot.title = element_text(size = title_size, vjust = 6, hjust = title_align, margin = margin(t = 20), face = title_face)) 
  
  plot2_calibration <- ggplot(subset(metric_dataset, `Age Group` == "[15,99]"  & year %in% from_to), aes(x = year)) +
    geom_line(aes(y = total_value, group = "factor(uid)", color = "factor(uid)"), linewidth = line_thickness, na.rm=TRUE) +
    ylim(c(0, NA)) +
    geom_errorbar(data = subset(targets, name == target_name_adult), aes(ymin = lo, ymax = hi), size = line_thickness, width = error_bar_width) +
    geom_point(data = subset(targets, name == target_name_adult), 
               aes(y = value), size = point_size, colour = "black") +
    labs(title = paste(plot_title, "in adults", sep= " ")) +
    ylab(yaxis_title) + xlab("Year") +
    theme(text = element_text(family = "sans"),
          axis.line = element_line(),
          axis.line.x.bottom = element_line(linewidth = line_thickness),
          axis.line.y.left = element_line(linewidth = line_thickness),
          axis.ticks = element_line(linewidth = line_thickness),
          axis.title.x.bottom = element_text(size= axis_title_size, vjust = -2, margin = margin(b = 20)),
          axis.title.y.left = element_text(size= axis_title_size, vjust= 3, margin = margin(l = 15)),
          axis.text.x.bottom = element_text(size= x_axis_text_size, vjust = 0.3, hjust = 0, angle = 90),
          plot.title = element_text(size = title_size, vjust = 6, hjust = title_align, margin = margin(t = 20), face = title_face))
  plot_calibration <-  list(plot1_calibration, plot2_calibration)
  
  } else {
    # Plot "b" if UID is not present
    plot_calibration <- ggplot(subset(metric_dataset, `Age Group` != "[0,99]" & year %in% from_to), aes(x = year)) +
      geom_line(aes(y = total_value, group = `Age Group`, colour = `Age Group`), linewidth = line_thickness, na.rm=TRUE) +
      ylim(c(0, NA)) +
      geom_errorbar(data = subset(targets, name == target_name_child), aes(ymin = lo, ymax = hi), size = line_thickness, width = error_bar_width, colour =  "#FC8D62") +
      geom_point(data = subset(targets, name == target_name_child), 
                 aes(y = value), size = point_size, colour = "#FC8D62") +
      geom_errorbar(data = subset(targets, name == target_name_adult), aes(ymin = lo, ymax = hi), size = line_thickness, width = error_bar_width, colour =  "#66C2A5") +
      geom_point(data = subset(targets, name == target_name_adult), 
                 aes(y = value), size = point_size, colour = "#66C2A5") +
      labs(title = paste(plot_title, "by age", sep= " ")) +
      ylab(yaxis_title) + xlab("Year") +
      scale_color_manual(name = "Age", values = c( "#D95F02" , "#1B9E77"))+
      theme(text = element_text(family = "sans"),
            axis.line = element_line(),
            axis.line.x.bottom = element_line(linewidth = line_thickness),
            axis.line.y.left = element_line(linewidth = line_thickness),
            axis.ticks = element_line(linewidth = line_thickness),
            axis.title.x.bottom = element_text(size= axis_title_size, vjust = -2, margin = margin(b = 20)),
            axis.title.y.left = element_text(size= axis_title_size, vjust= 3, margin = margin(l = 15)),
            axis.text.x.bottom = element_text(size= x_axis_text_size, vjust = 0.3, hjust = 0, angle = 90),
            plot.title = element_text(size = title_size, vjust = 6, hjust = title_align, margin = margin(t = 20), face = title_face)) +
      theme(legend.text = element_text(size = legend_text_size),
            legend.title = element_text(size = legend_title_size))
  }
  return(plot_calibration)
}


# No calibration
plot_guide_targets <- function(metric_dataset, target_name, plot_title, yaxis_title) {
  if ("uid" %in% names(metric_dataset)) {
    # Plot if UID is present
    plot_calibration <- "No plot"
  } else {
    # Plot "b" if UID is not present
    plot_calibration <- ggplot(subset(metric_dataset, `Age Group` == "[0,99]" & year %in% from_to), aes(x = year)) +
      geom_line(aes(y = total_value), linewidth = line_thickness, na.rm=TRUE, colour = "#E6AB02") +
      ylim(c(0, NA)) +
      geom_errorbar(data = subset(targets, name == target_name), aes(ymin = lo, ymax = hi), size = line_thickness, width = error_bar_width, colour = "#FFD92F") +
      geom_point(data = subset(targets, name == target_name), 
                 aes(y = value), size = point_size, colour = "#FFD92F") +
      labs(title = plot_title) +
      ylab(yaxis_title) + xlab("Year") +
      theme(text = element_text(family = "sans"),
            axis.line = element_line(),
            axis.line.x.bottom = element_line(linewidth = line_thickness),
            axis.line.y.left = element_line(linewidth = line_thickness),
            axis.ticks = element_line(linewidth = line_thickness),
            axis.title.x.bottom = element_text(size= axis_title_size, vjust = -2, margin = margin(b = 20)),
            axis.title.y.left = element_text(size= axis_title_size, vjust= 3, margin = margin(l = 15)),
            axis.text.x.bottom = element_text(size= x_axis_text_size, vjust = 0.3, hjust = 0, angle = 90),
            plot.title = element_text(size = title_size, vjust = 6, hjust = title_align, margin = margin(t = 20), face = title_face)) +
      theme(legend.position = "none")
  }
  return(plot_calibration)
}



# 1. Graph 1: TB - risk -------------------------------------------------------
plot_TB_risk1 <- plot_calibration_all(TB_risk1, "TB_inchiv_0_99", "TB incidence in HIV+ (0-99)", "Incidence per 100,000")

# 2. Graph 2: TB - risk -------------------------------------------------------
plot_TB_risk2 <- plot_calibration_all(TB_risk2, "TB_nothiv_0_99", "TB notifications in HIV+ (0-99)", "Notifications per 100,000")

# 3. Graph 3: TB - risk -------------------------------------------------------
plot_TB_risk3 <- plot_calibration_all(TB_risk3, "TB_morthiv_0_99", "TB mortality in HIV+ (0-99)", "Mortality per 100,000")

# 4. Graph 4: TB - risk -------------------------------------------------------
plot_TB_risk4 <- plot_guide_targets(TB_risk4, "TB_morthivneg_0_99", "TB mortality in HIV- (0-99)", "Mortality per 100,000")

# 5. Graph 5: TB - risk -------------------------------------------------------
plot_TB_risk5 <- plot_guide_targets(TB_risk5, "TB_nothivneg_0_99", "TB notifications in HIV- (0-99)", "Notifications per 100,000")

# 6. Graph 6: TB - risk -------------------------------------------------------
plot_TB_risk6 <- plot_guide_targets(TB_risk6, "TB_inchivneg_0_99", "TB incidence in HIV- (0-99)", "Incidence per 100,000")

# 5. Graph 1: Risk -------------------------------------------------------
plot_risk1 <- plot_calibration_age(risk1, "HIV_prev_0_14", "HIV_prev_15_99", "HIV+ prevalence", "Prevalence (%)")

# 6. Graph 2: Risk -------------------------------------------------------
plot_risk2 <- plot_calibration_age(risk2, "HIV_artcov_0_14", "HIV_artcov_15_99", "ART coverage", "Coverage (%)")

# 7. Graph 3: Risk -------------------------------------------------------
plot_risk3 <- plot_calibration_all(risk3, "HIV_mort_0_99", "HIV mortality", "Deaths per 100,000")
