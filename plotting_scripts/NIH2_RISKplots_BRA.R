# ----------------------------------------------------
# Running the CORE NIH2 model structure with the RISK dimension in tbmod
# Elena Venero Garcia (taking things from Rebecca Clark's first script from TBMod course
# and from Alvaro Schwalb's script making model plots with targets)
# 
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

TB_trends_49 <- TB_trends_49[, `Age Group` := fcase(
  age_from == 0 & age_thru == 14, "[0,14]",
  age_from == 0 & age_thru == 99, "[0,99]",
  age_from == 15 & age_thru == 49, "[15,49]",
  age_from == 50 & age_thru == 99, "[50,99]")]


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
# Filter out counts and death (usual age groups) and counts from RISK
TB_trends_filtered <- TB_trends_99[!str_detect(TB, "count|dead") &  !str_detect(RISK, "count|dead") ]
#Include only count states from RISK dimension
TB_trends_filtered_risk <- TB_trends_99[!str_detect(TB, "count|dead") &  str_detect(RISK, "count|dead") ]

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

population_prison <- subset(population_risk, RISK == "prison")

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
# Filter out counts from RISK (just in case, without RISK counts)
TB_counts_filtered_risk <- TB_counts_99[!str_detect(RISK, "count") ]

                                          
# 1. Plot 1: TB-RISK dimension ------------------------------
  #**BRA: Clinical incidence per 100,000 (prison)**
  Inc_Dc_1 <- TB_counts_filtered_risk[TB == "Inc_Dc_count"] 
  Inc_Dc_prison <- Inc_Dc_1[RISK == "prison"]
  Inc_Dc_prison <-  Inc_Dc_prison %>%
    group_by(across(all_of(group_general))) %>%
    summarise(total_inc = sum(value)) %>%
    mutate(year = floor(year))
  
  Dc_inc_adjust <- 0.86
  epTB_prop <- (1-0.095)
  Inc_Dc_prison$adjusted_inc_red <- Inc_Dc_prison$total_inc*Dc_inc_adjust
  Inc_Dc_prison$adjusted_inc <- Inc_Dc_prison$adjusted_inc_red/epTB_prop
  Inc_Dc_prison <- setDT(Inc_Dc_prison)
  
  #pop_pris_15_99 <- subset(population_prison, `Age Group` == "[15,99]")
  Dc_inc_prison <- merge(Inc_Dc_prison, population_prison)
  TB_risk1 <- Dc_inc_prison[, total_value := (adjusted_inc/Population)*100000]

# 2. Plot 2: TB-RISK dimension --------------------------------------------
  #**BRA: TB notifications per 100,000 (prison)**
  Not_1 <- TB_counts_filtered_risk[str_detect(TB, "Not")]
  Not_pris <- Not_1[RISK == "prison"]
  Not_pris <-  Not_pris %>%
    group_by(across(all_of(group_general))) %>%
    summarise(total_not = sum(value)) %>%
    mutate(year = floor(year))
  Not_pris <- setDT(Not_pris)
  Not_prison <- merge(Not_pris, population_prison)
  TB_risk2 <- Not_prison[, total_value := (total_not/Population)*100000]

# 3. Plot 1: RISK dimension --------------------------
  #**BRA: prison admissions rate**
  pris_admi <- TB_trends_filtered_risk[RISK == "priscount"]
  pris_admi <-  pris_admi %>%
    group_by(across(all_of(group_general))) %>%
    summarise(total_admisions = sum(value)) %>%
    mutate(year = floor(year))
  pris_admi <- setDT(pris_admi)
  admisions_prison <- merge(pris_admi, population)
  risk1 <- admisions_prison[, total_value := (total_admisions/Population)*100000]

# 4. Plot 2: RISK dimension --------------------------
  #**BRA: Incarceration prevalence**
  pris_prev <- TB_trends_filtered[RISK == "prison"]
  pris_prev <-  pris_prev %>%
    group_by(across(all_of(group_general))) %>%
    summarise(total_prev = sum(value)) %>%
    mutate(year = floor(year))
  pris_prev <- setDT(pris_prev)
  prev_prison <- merge(pris_prev, population)
  risk2 <- prev_prison[, total_value := (total_prev/Population)*100000]

# 5. Plot 3: RISK dimension --------------------------
  #**BRA: Prevalence by RISK**
  prev_risk <- TB_trends_filtered[TB == "Ds_s_N" | TB == "Ds_r_N" | TB == "Ds_s_P" | TB == "Ds_r_P" |
                                  TB == "Dc_s_N" | TB == "Dc_r_N" | TB == "Dc_s_P" | TB == "Dc_r_P" ]
  prev_risk <-  prev_risk %>%
    group_by(across(all_of(group_risk))) %>%
    summarise(total_DsDc = sum(value)) %>%
    mutate(year = floor(year))
  prev_risk <- setDT(prev_risk)
  tb_prev_risk <- merge(prev_risk, population_risk)
  risk3 <- tb_prev_risk[, total_value := (total_DsDc/Population)*100000]
  
# 6. Plot 4: RISK dimension --------------------------------------------
  #**BRA: TB incidence per 100,000 (prison)**
  Inc_all <-  Inc_Dc_1 %>%
    group_by(across(all_of(group_risk))) %>%
    summarise(total_inc = sum(value)) %>%
    mutate(year = floor(year))
  Inc_all <- setDT(Inc_all)
  Inc_risk <- merge(Inc_all, population_risk)
  risk4 <- Inc_risk[, total_value := (total_inc/Population)*100000]

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
colour_line = "#CE2931"
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

# prison (15 to 99 only)
plot_calibration_prison <- function(metric_dataset, target_name, plot_title, yaxis_title) {
  if ("uid" %in% names(metric_dataset)) {
    # Plot if UID is present
    plot_calibration <- ggplot(subset(metric_dataset, `Age Group` == "[15,99]" & year %in% from_to), aes(x = year)) +
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
    plot_calibration <- ggplot(subset(metric_dataset, `Age Group` != "[0,99]" & year %in% from_to), aes(x = year)) +
      geom_line(aes(y = total_value, group = `Age Group`, colour = `Age Group`), linewidth = line_thickness, na.rm=TRUE) +
      ylim(c(0, NA)) +
      geom_errorbar(data = subset(targets, name == target_name), aes(ymin = lo, ymax = hi), size = line_thickness, width = error_bar_width) +
      geom_point(data = subset(targets, name == target_name), 
                 aes(y = value), size = point_size, colour = "black") +
      labs(title = plot_title) +
      ylab(yaxis_title) + xlab("Year") +
      scale_color_manual(name = "Age", values = c( "#D95F02", "#1B9E77"))+
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


# 1. Graph 1: TB - risk -------------------------------------------------------
plot_TB_risk1 <- plot_calibration_prison(TB_risk1, "TB_incpris_15_99", "TB incidence in prison", "Incidence per 100,000")
  
# 2. Graph 2: TB - risk -------------------------------------------------------
plot_TB_risk2 <- plot_calibration_prison(TB_risk2, "TB_notpris_15_99", "TB notifications in prison", "Notifications per 100,000")

# 3. Graph 1: Risk -------------------------------------------------------
plot_risk1 <- plot_calibration_prison(risk1, "Pris_admis", "Admissions in prison", "Notifications per 100,000")

# 4. Graph 2: Risk -------------------------------------------------------
plot_risk2 <- plot_calibration_prison(risk2, "Pris_prev", "Incarceration prevalence", "Prevalence per 100,000")

# 5. Graph : Risk -------------------------------------------------------
plot_risk_graphs <- function(dataset, title, yaxis_title){
  if ("uid" %in% names(dataset)) {
    # Plot if UID is present
    plot_risk3 <- "Nothing plotted"
  } else {
    # Plot "b" if UID is not present
    plot_risks <- ggplot(subset(dataset, year %in% from_to), aes(x = year)) +
      geom_line(aes(y = total_value, group = `Age Group`, colour = `Age Group`), linewidth = line_thickness, na.rm=TRUE) +
      ylim(c(0, NA)) +
      labs(title = title) +
      ylab(yaxis_title) + xlab("Year") +
      scale_color_manual(name = "Age", values = c( "#D95F02", "#7570B3", "#1B9E77"))+
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
            legend.title = element_text(size = legend_title_size))+
      facet_wrap(~RISK)
    
  }
  return(plot_risks)
}

plot_risk3 <- plot_risk_graphs(risk3, "TB prevalence by risk state", "Prevalence per 100,000")
plot_risk4 <- plot_risk_graphs(risk4, "TB incidence by risk state", "Incidence per 100,000")
