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

# 1. Plot 1: TB-RISK  ------------------------------
  #**IND: Population Attributable Fraction (PAF)**
  Dscount_BMI <- TB_trends_99[TB == "Ds_s_count" | TB == "Ds_r_count"]
  Dscount_BMI_0_99 <- subset(Dscount_BMI, `Age Group`=="[0,99]")
  
  tb_inc <- Dscount_BMI_0_99 %>%
    group_by(across(all_of(group_risk))) %>%
    summarise(incidence_count=sum(value)) %>%
    mutate(year = floor(year))

  # Merge incidence_count back to total_population dataframe
  population_risk_0_99 <- subset(population_risk, `Age Group`=="[0,99]")
  population_risk_inc <- merge(population_risk_0_99, tb_inc, by = group_risk, all.x = TRUE)

  # Filter rows where TB is "Ds" or "Dc"
  prevTB_BMI <- TB_trends_99[TB == "Ds_s_N" | TB == "Ds_r_N" | TB == "Ds_s_P" | TB == "Ds_r_P" |
                                TB == "Dc_s_N" | TB == "Dc_r_N" | TB == "Dc_s_P" | TB == "Dc_r_P" ]
  
  # Group by year, RISK, and calculate the prevalence count
  prevalence_count <- prevTB_BMI %>%
    group_by(across(all_of(group_risk))) %>%
    summarise(prevalence_count = sum(value, na.rm = TRUE)) %>%
    mutate(year = floor(year))

  # Merge prevalence_count into total_population dataframe
  population_inc_prev <- left_join(population_risk_inc, prevalence_count, by = group_risk)
  
  # Calculate prevalence as (prevalence_count / total_population) * 100,000
  population_inc_prev$prevalence <- (population_inc_prev$prevalence_count / population_inc_prev$Population) * 100000
  
  # Calculate incidence as (incidence_count / total_population) * 100,000
  population_inc_prev$incidence <- (population_inc_prev$incidence_count / population_inc_prev$Population) * 100000

  # Aggregate counts for each year
  risk_groups <- population_inc_prev %>%
    group_by(across(all_of(group_general))) %>%
    summarise(
      incidence_thin_count = sum(incidence_count[RISK %in% c("mild", "moderate")]),
      prevalence_thin_count = sum(prevalence_count[RISK %in% c("mild", "moderate")]),
      incidence_normal_count = sum(incidence_count[RISK %in% c("normal", "over")]),
      prevalence_overall_count = sum(prevalence_count),
      thin_population = sum(Population[RISK %in% c("mild", "moderate")]),
      not_thin_population = sum(Population[RISK %in% c("normal", "over")]),
      overall_population = sum(Population)
    )
  
  risk_groups <- risk_groups %>%
    mutate(
      incidence_thin = (incidence_thin_count / thin_population) * 100000,
      incidence_normal = (incidence_normal_count / not_thin_population) * 100000,
      prevalence_thin = (prevalence_thin_count ) * 100000,
      prevalence_overall = (prevalence_overall_count ) * 100000,#
      PAF = ((incidence_thin - incidence_normal) / incidence_thin) * (prevalence_thin / prevalence_overall)
    )

# 2. Plot 1: RISK  --------------------------------
  #**IND: Proportion of the population in each risk state**
  population_in_risk <- population_risk %>% 
    rename(pop_risk = Population)
  BMI_prop <- population_in_risk %>%
    left_join(population, by = group_general)
  risk1 <- BMI_prop[, prop := (pop_risk/Population)*100]
  View(risk1)
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

# 0 to 99 only 
#plot_calibration_0_99 <- function(metric_dataset, target_name, yvalue, plot_title, yaxis_title) {
#  if ("uid" %in% names(metric_dataset)) {
#    # Plot if UID is present
#    plot_calibration <- ggplot(subset(metric_dataset, `Age Group` == "[0,99]" & year %in% from_to), aes(x = year)) +
#      geom_line(aes(y = yvalue, group = "factor(uid)", color = "factor(uid)"), linewidth = line_thickness, na.rm=TRUE) +
#      ylim(c(0, NA)) +
#      geom_errorbar(data = subset(targets, name == target_name), aes(ymin = lo, ymax = hi), size = line_thickness, width = error_bar_width) +
#      geom_point(data = subset(targets, name == target_name), 
#                 aes(y = value), size = point_size, colour = "black") +
#      labs(title = plot_title) +
#      ylab(yaxis_title) + xlab("Year") +
#      theme(text = element_text(family = "sans"),
#            axis.line = element_line(),
#            axis.line.x.bottom = element_line(linewidth = line_thickness),
#            axis.line.y.left = element_line(linewidth = line_thickness),
#            axis.ticks = element_line(linewidth = line_thickness),
#            axis.title.x.bottom = element_text(size= axis_title_size, vjust = -2, margin = margin(b = 20)),
#            axis.title.y.left = element_text(size= axis_title_size, vjust= 3, margin = margin(l = 15)),
#            axis.text.x.bottom = element_text(size= x_axis_text_size, vjust = 0.3, hjust = 0, angle = 90),
#            plot.title = element_text(size = title_size, vjust = 6, hjust = title_align, margin = margin(t = 20), face = title_face))
    
#  } else {
#    # Plot "b" if UID is not present
#    plot_calibration <- ggplot(subset(metric_dataset, `Age Group` == "[0,99]" & year %in% from_to), aes(x = year)) +
#      geom_line(aes(y = yvalue), linewidth = line_thickness, na.rm=TRUE, colour = colour_line) +
#      ylim(c(0, NA)) +
#      geom_errorbar(data = subset(targets, name == target_name), aes(ymin = lo, ymax = hi), size = line_thickness, width = error_bar_width) +
#      geom_point(data = subset(targets, name == target_name), 
#                 aes(y = value), size = point_size, colour = "black") +
#      labs(title = plot_title) +
#      ylab(yaxis_title) + xlab("Year") +
#      theme(text = element_text(family = "sans"),
#            axis.line = element_line(),
#            axis.line.x.bottom = element_line(linewidth = line_thickness),
#            axis.line.y.left = element_line(linewidth = line_thickness),
#            axis.ticks = element_line(linewidth = line_thickness),
#            axis.title.x.bottom = element_text(size= axis_title_size, vjust = -2, margin = margin(b = 20)),
#            axis.title.y.left = element_text(size= axis_title_size, vjust= 3, margin = margin(l = 15)),
#            axis.text.x.bottom = element_text(size= x_axis_text_size, vjust = 0.3, hjust = 0, angle = 90),
#            plot.title = element_text(size = title_size, vjust = 6, hjust = title_align, margin = margin(t = 20), face = title_face)) +
#      theme(legend.position = "none")
#  }
#  return(plot_calibration)
#}

  
# 1. Graph 1: TB - risk -------------------------------------------------------
#plot_TB_risk1 <- plot_calibration_0_99(risk_groups, "", PAF, "Population Attributable Fraction (0-99)", "PAF") 
 

  if ("uid" %in% names(risk_groups)) {
    # Plot if UID is present
    plot_TB_risk1 <- ggplot(subset(risk_groups & year %in% from_to), aes(x = year)) +
      geom_line(aes(y = PAF, group = "factor(uid)", color = "factor(uid)"), linewidth = line_thickness, na.rm=TRUE) +
      ylim(c(0, NA)) +
      geom_errorbar(data = subset(targets, name == "PAF"), aes(ymin = lo, ymax = hi), size = line_thickness, width = error_bar_width) +
      geom_point(data = subset(targets, name == "PAF"), 
                 aes(y = value), size = point_size, colour = "black") +
      labs(title = "Population Attributable Fraction (0-99)") +
      ylab("PAF") + xlab("Year") +
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
    plot_TB_risk1 <- ggplot(subset(risk_groups, `Age Group` == "[0,99]" & year %in% from_to), aes(x = year)) +
      geom_line(aes(y = PAF), linewidth = line_thickness, na.rm=TRUE, colour = colour_line) +
      ylim(c(0, NA)) +
      geom_errorbar(data = subset(targets, name == "PAF"), aes(ymin = lo, ymax = hi), size = line_thickness, width = error_bar_width) +
      geom_point(data = subset(targets, name == "PAF"), 
                 aes(y = value), size = point_size, colour = "black") +
      labs(title = "Population Attributable Fraction (0-99)") +
      ylab("PAF") + xlab("Year") +
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


# 2. Graph 1: Risk -------------------------------------------------------
#plot_risk1 <- plot_calibration_0_99(risk1, "", prop, "Proportion of population n\in each BMI state (0-99)", "Proportion of population (%)") 

if ("uid" %in% names(risk1)) {
  # Plot if UID is present
  plot_risk1 <- ggplot(subset(risk1 & year %in% from_to), aes(x = year)) +
    geom_line(aes(y = prop, group = "factor(uid)", color = "factor(uid)"), linewidth = line_thickness, na.rm=TRUE) +
    ylim(c(0, NA)) +
    labs(title = "Proportion of population \n in each BMI state (0-99)") +
    ylab("Proportion of population (%)") + xlab("Year") +
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
  plot_risk1 <- ggplot(subset(risk1, `Age Group` == "[0,99]" & year %in% from_to), aes(x = year)) +
    geom_line(aes(y = prop, group=RISK, colour = RISK), linewidth = line_thickness, na.rm=TRUE) +
    ylim(c(0, NA)) +
    labs(title = "Proportion of population \n in each BMI state (0-99)") +
    ylab("Proportion of population (%)") + xlab("Year") +
    scale_color_manual(name = "Age", values = c( "#E7298A", "#66A61E", "#E6AB02", "#666666"))+
    theme(text = element_text(family = "sans"),
          axis.line = element_line(),
          axis.line.x.bottom = element_line(linewidth = line_thickness),
          axis.line.y.left = element_line(linewidth = line_thickness),
          axis.ticks = element_line(linewidth = line_thickness),
          axis.title.x.bottom = element_text(size= axis_title_size, vjust = -2, margin = margin(b = 20)),
          axis.title.y.left = element_text(size= axis_title_size, vjust= 3, margin = margin(l = 15)),
          axis.text.x.bottom = element_text(size= x_axis_text_size, vjust = 0.3, hjust = 0, angle = 90),
          plot.title = element_text(size = title_size, vjust = 6, hjust = title_align, margin = margin(t = 20), face = title_face)) +
    theme(text = element_text(family = "sans"),
          axis.line = element_line(),
          axis.line.x.bottom = element_line(linewidth = line_thickness),
          axis.line.y.left = element_line(linewidth = line_thickness),
          axis.ticks = element_line(linewidth = line_thickness),
          axis.title.x.bottom = element_text(size= axis_title_size, vjust = -2, margin = margin(b = 20)),
          axis.title.y.left = element_text(size= axis_title_size, vjust= 3, margin = margin(l = 15)),
          axis.text.x.bottom = element_text(size= x_axis_text_size, vjust = 0.3, hjust = 0, angle = 90),
          plot.title = element_text(size = title_size, vjust = 6, hjust = title_align, margin = margin(t = 20), face = title_face))
}


