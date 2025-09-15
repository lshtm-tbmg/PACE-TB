# ----------------------------------------------------
# Code with the calculations and plots that are common between countries
# Main plots with calibration targets include: 
#   - Pulmonary TB prevalence per 100,000 (population 15-99)
#   - Clinical incdence per 100,000 (population 0-99)
#   - TB mortality per 100,000 (population 0-99)
#   - TB notifications per 100,000 (population 0-99)
#   - % MDR notified by previous treatment history (population 0-99)
#   - % subclinical in prevalent infectious TB (population 0-99 IND, population 15-99)
# Analytical plots:
#   - Adults and children (incidence, notifications and mortality (and prevalence for children only))
#   - Infection prevalence
#   - Annual Risk of infection
#   - Ratio non-infectious vs infectious TB
#   - Population by age_group and risk
#   - Treatment success by age_group
# Elena Venero Garcia (taking things from Rebecca Clark's first script from TBMod course
# and from Alvaro Schwalb's script making model plots with targets)

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
# Group the "from" "to" age_groups columns in one and include only .999 years
TB_trends_49 <- output$stocks
TB_trends_49 <- TB_trends_49[grepl("\\.999$", TB_trends_49$year), ]

TB_trends_49 <- TB_trends_49[, age_group := fcase(
  age_from == 0 & age_thru == 14, "[0,14]",
  age_from == 0 & age_thru == 99, "[0,99]",
  age_from == 15 & age_thru == 49, "[15,49]",
  age_from == 50 & age_thru == 99, "[50,99]")]


# Variables to group by
group_to_group_ages <- get_group_vars(TB_trends_49, c("country", "year", "RISK", "TB", "age_group"))


# Unify Adults group ([15,49] + [50,99] -> [15,99])
TB_trends_99 <- TB_trends_49 %>%
  mutate(age_group = case_when(
    age_group %in% c("[15,49]", "[50,99]") ~ "[15,99]",
    TRUE ~ age_group
  )) %>%
  group_by(across(all_of(group_to_group_ages))) %>%
  summarize(value = sum(value), .groups = "drop")

TB_trends_99 <- setDT(TB_trends_99)

# Variables to group by
group_general <- get_group_vars(TB_trends_99, c("country", "year", "age_group"))
group_risk <- get_group_vars(TB_trends_99, c("country", "year", "RISK", "age_group"))

#** For prevalence, proportion subclinical, infection prevalence and ratio of non infectious to invetious TB**
# Filter out counts and death (usual age_groups)
TB_trends_filtered <- TB_trends_99[!str_detect(TB, "count|dead") &  !str_detect(RISK, "dead|count") ]

# Filter out counts and death (ARI age_groups)
TB_trends_filtered_49 <- TB_trends_49[!str_detect(TB, "count|dead") &  !str_detect(RISK, "dead|count") ]

#** For mortality **
#Select deaths 
TB_mort_filtered <- TB_trends_99[str_detect(TB, "dead")]

#** Population **
# Population (age_groups {0,14] and [15,99])
population <-  TB_trends_filtered %>%
  group_by(across(all_of(group_general))) %>%
  summarise(Population = sum(value)) %>%
  mutate(year = floor(year))
population <- setDT(population)


# Population used for ARI by age_groups [0,14], [15,49] and [50+]
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
# Group the "from" "to" age_groups columns in one 
TB_counts_49 <- output$count.999
TB_counts_49 <- TB_counts_49[grepl("\\.999$", TB_counts_49$year), ]

TB_counts_49 <- TB_counts_49[, age_group := fcase(
  age_from == 0 & age_thru == 14, "[0,14]",
  age_from == 0 & age_thru == 99, "[0,99]",
  age_from == 15 & age_thru == 49, "[15,49]",
  age_from == 50 & age_thru == 99, "[50,99]")]


# Unify Adults group ([15,49] + [50,99] -> [15,99])
TB_counts_99 <- TB_counts_49 %>%
  mutate(age_group = case_when(
    age_group %in% c("[15,49]", "[50,99]") ~ "[15,99]",
    TRUE ~ age_group
  )) %>%
  group_by(across(all_of(group_to_group_ages))) %>%
  summarize(value = sum(value), .groups = "drop")

TB_counts_99 <- setDT(TB_counts_99)


#Code to get proportion for initialization -------------------------------

#I_num <- TB_trends_filtered[TB == "I_s_N" ]
#I_num <-  I_num %>%
#  group_by(across(all_of(group_general))) %>%
#  summarise(total = sum(value)) %>%
#  mutate(year = floor(year))
#I_num <- setDT(I_num)
#I_prop <- merge(I_num, population)
#I_prop <- I_prop[, proportion := (total/Population)]
#I_prop[year==2000]

#Dn_num <- TB_trends_filtered[TB == "Dn_s_N"]
#Dn_num <-  Dn_num %>%
#  group_by(across(all_of(group_general))) %>%
#  summarise(total = sum(value)) %>%
#  mutate(year = floor(year))
#Dn_num <- setDT(Dn_num)
#Dn_prop <- merge(Dn_num, population)
#Dn_prop <- Dn_prop[, proportion := (total/Population)]
#Dn_prop[year==2000]

#Ds_num <- TB_trends_filtered[TB == "Ds_s_N"]
#Ds_num <-  Ds_num %>%
#  group_by(across(all_of(group_general))) %>%
#  summarise(total = sum(value)) %>%
#  mutate(year = floor(year))
#Ds_num <- setDT(Ds_num)
#Ds_prop <- merge(Ds_num, population)
#Ds_prop <- Ds_prop[, proportion := (total/Population)]
#Ds_prop[year==2000]

#Dc_num <- TB_trends_filtered[TB == "Dc_s_N"]
#Dc_num <-  Dc_num %>%
#  group_by(across(all_of(group_general))) %>%
#  summarise(total = sum(value)) %>%
#  mutate(year = floor(year))
#Dc_num <- setDT(Dc_num)
#Dc_prop <- merge(Dc_num, population)
#Dc_prop <- Dc_prop[, proportion := (total/Population)]
#Dc_prop[year==2000]

################## Calculate variables included in the plot ###################

# 1. TB Prevalence (Population) -------------------------------------
prev_tb <- TB_trends_filtered[TB == "Ds_s_N" | TB == "Ds_r_N" | TB == "Ds_s_P" | TB == "Ds_r_P" |
                                TB == "Dc_s_N" | TB == "Dc_r_N" | TB == "Dc_s_P" | TB == "Dc_r_P" ]

prev_tb <-  prev_tb %>%
  group_by(across(all_of(group_general))) %>%
  summarise(total_DsDc = sum(value)) %>%
  mutate(year = floor(year))

if (ISO=="IND") {
  prop_T <- (1-0.12)
} else if (ISO=="ZAF"){
  prop_T <- (1-0.043)
} else {
  prop_T <- 1
}

prev_tb$adjusted_prev <- prev_tb$total_DsDc/prop_T
prev_tb <- setDT(prev_tb)
tb_prev_pop <- merge(prev_tb, population)
tb_prev_pop <- tb_prev_pop[, total_value := ((adjusted_prev/Population))*100000]

# 2. % subclinical in prevalent infectious TB (Population) --------
pct_Ds <- TB_trends_filtered[TB == "Ds_s_N" | TB == "Ds_r_N" | TB == "Ds_s_P" | TB == "Ds_r_P" ]
pct_Ds <-  pct_Ds %>%
  group_by(across(all_of(group_general))) %>%
  summarise(total_Ds = sum(value)) %>%
  mutate(year = floor(year))
pct_Ds <- setDT(pct_Ds)
Ds_pct_pop <- merge(pct_Ds, prev_tb)
Ds_pct_pop <- Ds_pct_pop[, total_value := (total_Ds/total_DsDc)*100]


# 3. TB mortality per 100,000 (Population) -------------------------
total_mort_tb <-  TB_mort_filtered %>%
  group_by(across(all_of(group_general))) %>%
  summarise(total_deaths = sum(value)) %>%
  mutate(year = floor(year))
total_mort_tb <- setDT(total_mort_tb)
tb_mort_pop <- merge(total_mort_tb, population)
tb_mort_pop <- tb_mort_pop[, total_value := (total_deaths/Population)*100000]

# 4. Clinical incidence per 100,000 (Population) ------------------
Inc_Dc <- TB_counts_99[str_detect(TB, "Inc_Dc")]
Inc_Dc <-  Inc_Dc %>%
  group_by(across(all_of(group_general))) %>%
  summarise(total_inc = sum(value)) %>%
  mutate(year = floor(year))
Dc_inc_adjust <- 0.86

if (ISO=="IND") {
  epTB_prop <- (1-0.181)
} else if (ISO=="ZAF"){
  epTB_prop <- (1-0.077)
} else {
  epTB_prop <- (1-0.095)
}

Inc_Dc$adjusted_inc_red <- Inc_Dc$total_inc*Dc_inc_adjust
Inc_Dc$adjusted_inc <- Inc_Dc$adjusted_inc_red/epTB_prop

Inc_Dc <- setDT(Inc_Dc)
Dc_inc_pop <- merge(Inc_Dc, population)

Dc_inc_pop <- Dc_inc_pop[, total_value := (adjusted_inc/Population)*100000]

# 5. TB notifications per 100,000 (Population) ------------------
Not_all <- TB_counts_99[ str_detect(TB, "Not")]
Not_all <-  Not_all %>%
  group_by(across(all_of(group_general))) %>%
  summarise(total_not = sum(value)) %>%
  mutate(year = floor(year))
Not_all <- setDT(Not_all)
Not_pop <- merge(Not_all, population)
Not_pop <- Not_pop[, total_value := (total_not/Population)*100000]

# 6. % MDR new notification (Population) -----------------------
MDR_new <- TB_counts_99[ str_detect(TB, "MDR_N")]
MDR_new <-  MDR_new %>%
  group_by(across(all_of(group_general))) %>%
  summarise(total_MDR_new = sum(value)) %>%
  mutate(year = floor(year))
MDR_new <- setDT(MDR_new)

not_N <- TB_counts_99[TB == "Not_N_count"]
not_N <-  not_N %>%
  group_by(across(all_of(group_general))) %>%
  summarise(total_not = sum(value)) %>%
  mutate(year = floor(year))
not_N <- setDT(not_N)

new_MDR <- merge(MDR_new, not_N)
new_MDR <- new_MDR[, MDR_new_pct := (total_MDR_new/total_not)*100]

# 7. % MDR previously notified (Population) -------------------
MDR_prev <- TB_counts_99[ str_detect(TB, "MDR_P")]
MDR_prev <-  MDR_prev %>%
  group_by(across(all_of(group_general))) %>%
  summarise(total_MDR_prev = sum(value)) %>%
  mutate(year = floor(year))
MDR_prev <- setDT(MDR_prev)

not_P <- TB_counts_99[TB == "Not_P_count"]
not_P <-  not_P %>%
  group_by(across(all_of(group_general))) %>%
  summarise(total_not = sum(value)) %>%
  mutate(year = floor(year))
not_P <- setDT(not_P)

prev_MDR <- merge(MDR_prev, not_P)
prev_MDR <- prev_MDR[, MDR_prev_pct := (total_MDR_prev/total_not)*100]

# 8. Treatment success (ST Population) --------------------------
success_all_ST <- TB_counts_99[TB == "ST_succ_count"]
success_all_ST <-  success_all_ST %>%
  group_by(across(all_of(group_general))) %>%
  summarise(total_succ = sum(value)) %>%
  mutate(year = floor(year))
success_all_ST <- setDT(success_all_ST)

unsuccess_all_ST <- TB_counts_99[TB == "ST_unsucc_count"]
unsuccess_all_ST <-  unsuccess_all_ST %>%
  group_by(across(all_of(group_general))) %>%
  summarise(total_unsucc = sum(value)) %>%
  mutate(year = floor(year))
unsuccess_all_ST <- setDT(unsuccess_all_ST)

population_ST <- merge(success_all_ST, unsuccess_all_ST)
population_ST <- population_ST[, total_ST := (total_succ + total_unsucc)]

succ_all_ST <- merge(success_all_ST, population_ST)
succ_all_ST <- succ_all_ST[, total_value := (total_succ/total_ST)*100]

# 9. Treatment success (ST Population) --------------------------
success_all_RT <- TB_counts_99[TB == "RT_succ_count"]
success_all_RT <-  success_all_RT %>%
  group_by(across(all_of(group_general))) %>%
  summarise(total_succ = sum(value)) %>%
  mutate(year = floor(year))
success_all_RT <- setDT(success_all_RT)

unsuccess_all_RT <- TB_counts_99[TB == "RT_unsucc_count"]
unsuccess_all_RT <-  unsuccess_all_RT %>%
  group_by(across(all_of(group_general))) %>%
  summarise(total_unsucc = sum(value)) %>%
  mutate(year = floor(year))
unsuccess_all_RT <- setDT(unsuccess_all_RT)

population_RT <- merge(success_all_RT, unsuccess_all_RT)
population_RT <- population_RT[, total_RT := (total_succ + total_unsucc)]

succ_all_RT <- merge(success_all_RT, population_RT)
succ_all_RT <- succ_all_RT[, total_value := (total_succ/total_RT)*100]

# 10. Annual Risk of infection (DS) ------------------
    
    # 10a)and 10b) Lambda for total population and age bracket
    lambda_age_s <-  TB_counts_49[TB == "I_s_count" ]
    lambda_age_s <-  lambda_age_s %>%
      group_by(across(all_of(group_general))) %>%
      summarise(total_lambda = sum(value)) %>%
      mutate(year = floor(year))
    lambda_age_s <- setDT(lambda_age_s)
    ARI_age_s <- merge(lambda_age_s, population_49)
    ARI_age_s <- ARI_age_s[, total_value := (total_lambda/Population)*100]
    
    # 10c) Lambda by RISK
    lambda_risk_s <-  TB_counts_99[TB == "I_s_count" ]
    lambda_risk_s <-  lambda_risk_s %>%
      group_by(across(all_of(group_risk))) %>%
      summarise(total_lambda = sum(value)) %>%
      mutate(year = floor(year))
    lambda_risk_s <- setDT(lambda_risk_s)
    ARI_risk_s <- merge(lambda_risk_s, population_risk)
    ARI_risk_s <- ARI_risk_s[, total_value := (total_lambda/Population)*100]

# 11. Annual Risk of infection (DS) ------------------
    
    # 11a)and 11b) Lambda for total population and age bracket
    lambda_age_r <-  TB_counts_49[TB == "I_r_count" ]
    lambda_age_r <-  lambda_age_r %>%
      group_by(across(all_of(group_general))) %>%
      summarise(total_lambda = sum(value)) %>%
      mutate(year = floor(year))
    lambda_age_r <- setDT(lambda_age_r)
    ARI_age_r <- merge(lambda_age_r, population_49)
    ARI_age_r <- ARI_age_r[, total_value := (total_lambda/Population)*100]
    
    # 11c) Lambda by RISK
    lambda_risk_r <-  TB_counts_99[TB == "I_r_count" ]
    lambda_risk_r <-  lambda_risk_r %>%
      group_by(across(all_of(group_risk))) %>%
      summarise(total_lambda = sum(value)) %>%
      mutate(year = floor(year))
    lambda_risk_r <- setDT(lambda_risk_r)
    ARI_risk_r <- merge(lambda_risk_r, population_risk)
    ARI_risk_r <- ARI_risk_s[, total_value := (total_lambda/Population)*100]
    

# 12. Infection prevalence (I+Dn+Ds+Dc) (Population) -------------------------------------
prev_inf <- TB_trends_filtered[TB == "I_s_N" | TB == "I_r_N" | TB == "I_s_P" | TB == "I_r_P" |
                              TB == "Dn_s_N" | TB == "Dn_r_N" | TB == "Dn_s_P" | TB == "Dn_r_P" |
                              TB == "Ds_s_N" | TB == "Ds_r_N" | TB == "Ds_s_P" | TB == "Ds_r_P" |
                              TB == "Dc_s_N" | TB == "Dc_r_N" | TB == "Dc_s_P" | TB == "Dc_r_P" ]
prev_inf <-  prev_inf %>%
  group_by(across(all_of(group_general))) %>%
  summarise(total_inf = sum(value)) %>%
  mutate(year = floor(year))
prev_inf <- setDT(prev_inf)
inf_prev_pop <- merge(prev_inf, population)
inf_prev_pop <- inf_prev_pop[, total_value := (total_inf/Population)*100000]

# 13. Ratio non-infectious VS infectious TB (Population)
prev_Dn <- TB_trends_filtered[TB == "Dn_s_N" | TB == "Dn_r_N" | TB == "Dn_s_P" | TB == "Dn_r_P"]
prev_Dn <-  prev_Dn %>%
  group_by(across(all_of(group_general))) %>%
  summarise(total_Dn = sum(value)) %>%
  mutate(year = floor(year))
prev_Dn <- setDT(prev_Dn)
ratio_Dn_vs_DsDc <- merge(prev_Dn, prev_tb)
ratio_Dn_vs_DsDc <- ratio_Dn_vs_DsDc[, total_value := (total_Dn/total_DsDc)] 

# 14. Proportion of on-treatment mortality in TB mortality
treat_mort_tb <- TB_mort_filtered[str_detect(TB, "ST|RT")]
treat_mort_tb <-  treat_mort_tb %>%
  group_by(across(all_of(group_general))) %>%
  summarise(treat_deaths = sum(value)) %>%
  mutate(year = floor(year))
treat_mort_tb <- setDT(treat_mort_tb)
tb_mort_prop <- merge(treat_mort_tb, total_mort_tb)
tb_mort_prop <- tb_mort_prop[, total_value := (treat_deaths/total_deaths)*100]

# 15. Proportion of mortality by age group
total_mort_tb_no0to99 <- subset(total_mort_tb, age_group != "[0,99]")
total_mort_tb_0to99 <- subset(total_mort_tb, age_group == "[0,99]")
total_mort_tb_0to99 <- total_mort_tb_0to99 %>% select(-age_group)
result <- merge(total_mort_tb_no0to99, total_mort_tb_0to99, by = c("country", "year"))

tb_mort_age_prop <- result[, total_value := (total_deaths.x/total_deaths.y)*100]

###################### Plots with calibration target ##########################

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

# Define the function to plot metrics conditionally based on UID

# Calibrating to only one age_group of the variable
plot_calibration_no_legend <- function(metric_dataset, age_variable, target_name, plot_title, yaxis_title, limit_specified) {
  final_title <- paste(plot_title, age_variable, sep = " ")
  if ("uid" %in% names(metric_dataset)) {
    # Plot if UID is present
    plot_calibration <- ggplot(subset(metric_dataset, age_group == age_variable & year %in% from_to), aes(x = year)) +
      geom_line(aes(y = total_value, group = "factor(uid)", color = "factor(uid)"), linewidth = line_thickness, na.rm=TRUE) +
      ylim(c(0, limit_specified)) +
      geom_errorbar(data = subset(targets, name == target_name), aes(ymin = lo, ymax = hi), size = line_thickness, width = error_bar_width) +
      geom_point(data = subset(targets, name == target_name), 
                 aes(y = value), size = point_size, colour = "black") +
      labs(title = final_title) +
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
    plot_calibration <- ggplot(subset(metric_dataset, age_group == age_variable & year %in% from_to), aes(x = year)) +
      geom_line(aes(y = total_value), linewidth = line_thickness, na.rm=TRUE, colour = colour_line) +
      ylim(c(0, limit_specified)) +
      geom_errorbar(data = subset(targets, name == target_name), aes(ymin = lo, ymax = hi), size = line_thickness, width = error_bar_width) +
      geom_point(data = subset(targets, name == target_name), 
                 aes(y = value), size = point_size, colour = "black") +
      labs(title = final_title) +
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

#Calibrating to >1 age_group of the variable (notifications and incidence)
plot_cali_many_targets <- function(metric_dataset, target_name, plot_title, yaxis_title) {
  if ("uid" %in% names(metric_dataset)) {
    # Plot if UID is present
    plot_calibration_1 <- ggplot(subset(metric_dataset, age_group == "[0,99]" & year %in% from_to), aes(x = year)) +
      geom_line(aes(y = total_value, group = "factor(uid)", color = "factor(uid)"), linewidth = line_thickness, na.rm=TRUE) +
      ylim(c(0, NA)) +
      geom_errorbar(data = subset(targets, name == paste(target_name,"_0_99", sep="")), aes(ymin = lo, ymax = hi), size = line_thickness, width = error_bar_width) +
      geom_point(data = subset(targets, name == paste(target_name,"_0_99", sep="")), 
                 aes(y = value), size = point_size, colour = "black") +
      labs(title = paste(plot_title, "[0,99]", sep=" ")) +
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
    
    plot_calibration_2 <- ggplot(subset(metric_dataset, age_group == "[0,14]" & year %in% from_to), aes(x = year)) +
      geom_line(aes(y = total_value, group = "factor(uid)", color = "factor(uid)"), linewidth = line_thickness, na.rm=TRUE) +
      ylim(c(0, NA)) +
      geom_errorbar(data = subset(targets, name == paste(target_name,"_0_14", sep="")), aes(ymin = lo, ymax = hi), size = line_thickness, width = error_bar_width) +
      geom_point(data = subset(targets, name == paste(target_name,"_0_14", sep="")), 
                 aes(y = value), size = point_size, colour = "black") +
      labs(title = paste(plot_title, "[0,14]", sep=" ")) +
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
    
    plot_calibration_3 <- ggplot(subset(metric_dataset, age_group == "[15,99]" & year %in% from_to), aes(x = year)) +
      geom_line(aes(y = total_value, group = "factor(uid)", color = "factor(uid)"), linewidth = line_thickness, na.rm=TRUE) +
      ylim(c(0, NA)) +
      geom_errorbar(data = subset(targets, name == paste(target_name,"_15_99", sep="")), aes(ymin = lo, ymax = hi), size = line_thickness, width = error_bar_width) +
      geom_point(data = subset(targets, name == paste(target_name,"_15_99", sep="")), 
                 aes(y = value), size = point_size, colour = "black") +
      labs(title = paste(plot_title, "[15,99]", sep=" ")) +
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
    
    plot_calibration_targetS <- list()
    plot_calibration_targetS <-  list(plot_calibration_1, plot_calibration_2, plot_calibration_3)
    
  } else {
    # Plot "b" if UID is not present
    plot_calibration_targetS <- ggplot(subset(metric_dataset, year %in% from_to), aes(x = year)) +
      geom_line(aes(y = total_value, group=age_group, colour = age_group), linewidth = line_thickness, na.rm=TRUE) +
      ylim(c(0, NA)) +
      geom_errorbar(data = subset(targets, name == paste(target_name,"_0_99", sep="")), aes(ymin = lo, ymax = hi), size = line_thickness, width = error_bar_width, colour = "#8DA0CB") +
      geom_point(data = subset(targets, name == paste(target_name,"_0_99", sep="")), 
                 aes(y = value), size = point_size, colour = "#8DA0CB") +
      geom_errorbar(data = subset(targets, name == paste(target_name,"_0_14", sep="")), aes(ymin = lo, ymax = hi), size = line_thickness, width = error_bar_width, colour = "#FC8D62") +
      geom_point(data = subset(targets, name == paste(target_name,"_0_14", sep="")), 
                 aes(y = value), size = point_size, colour = "#FC8D62") +
      geom_errorbar(data = subset(targets, name == paste(target_name,"_15_99", sep="")), aes(ymin = lo, ymax = hi), size = line_thickness, width = error_bar_width, colour = "#66C2A5") +
      geom_point(data = subset(targets, name == paste(target_name,"_15_99", sep="")), 
                 aes(y = value), size = point_size, colour = "#66C2A5") +
      labs(title = plot_title) +
      ylab(yaxis_title) + xlab("Year") +
      scale_color_manual(name = "Age", values = c( "#D95F02", "#7570B3" , "#1B9E77"))+
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
            plot.title = element_text(size = title_size, vjust = 6, hjust = title_align, margin = margin(t = 20), face = title_face)) +
      theme(legend.text = element_text(size = legend_text_size),
            legend.title = element_text(size = legend_title_size))
  }
  return(plot_calibration_targetS)
}

# 1. TB prevalence (Population) -------------------------------------------------
if (ISO == "BRA") {
  plot_TBprev_pop <- plot_calibration_no_legend(tb_prev_pop, "[15,99]", "", "TB prevalence in the population", "Prevalence per 100,000", NA)
} else {
  plot_TBprev_pop <- plot_calibration_no_legend(tb_prev_pop, "[15,99]", "TB_prev_15_99", "TB prevalence in the population", "Prevalence per 100,000", NA)
}

# 2. % subclinical in prevalent infectious TB (Population) -------------------
Ds_pct_target <- subset(targets, name == "Ds_pct")
Ds_pct_target <- Ds_pct_target %>%
  mutate(age_group = case_when(
    age_from == 0 & age_thru == 14 ~ '[0,14]',
    age_from == 15 & age_thru == 99 ~ '[15,99]',
    age_from == 0 & age_thru == 99 ~ '[0,99]',
    TRUE ~ NA_character_  # This handles any unexpected cases
  ))
Dspct_filtered_age <- Ds_pct_pop[Ds_pct_pop$age_group %in% Ds_pct_target$age_group]
age_used <- Dspct_filtered_age$age_group[1]
age_text <- as.character(age_used)

if (ISO == "BRA") {
  plot_Ds_pct <- plot_calibration_no_legend(Ds_pct_pop, "[15,99]", "", "% Subclinical TB in prevalent \ninfectious TB in the population", "Proportion of the prevalence (%)", 100)
} else {
  plot_Ds_pct <- plot_calibration_no_legend(Ds_pct_pop, age_text, "Ds_pct", "% Subclinical TB in prevalent \ninfectious TB in the population", "Proportion of the prevalence (%)", 100)
}


# 3. TB mortality (Population) ----------------------------------------------
plot_mort_pop <- plot_calibration_no_legend(tb_mort_pop, "[0,99]", "TB_mort_0_99", "TB mortality in the population", "Mortality per 100,000", NA)

# 4. Clinical incidence (Population) ----------------------------------------
plot_Dc_inc_pop <- plot_cali_many_targets(Dc_inc_pop, "TB_inc", "Clinical incidence in \nthe population", "Incidence per 100,000")

# 5. TB notifications (Population) --------------------------------------
plot_Not_pop <- plot_cali_many_targets(Not_pop, "TB_not", "TB notifications in \nthe population", "Notifications per 100,000")

# 6. % MDR (Population) (6 and 7 with uid) ------------------------------------------------
plot_MDR <- list()

if ("uid" %in% names(new_MDR)) {
  # Plot if UID is present
  plot1_calibration <- ggplot(subset(new_MDR, age_group == "[0,99]"  & year %in% from_to), aes(x = year)) +
    geom_line(aes(y = MDR_new_pct, group = "factor(uid)", color = "factor(uid)"), linewidth = line_thickness, na.rm=TRUE) +
    ylim(c(0, NA)) +
    geom_errorbar(data = subset(targets, name == "MDR_new_0_99"), aes(ymin = lo, ymax = hi), size = line_thickness, width = error_bar_width) +
    geom_point(data = subset(targets, name == "MDR_new_0_99"), 
               aes(y = value), size = point_size, colour = "black") +
    labs(title = "% new MDR in the population (0-99)") +
    ylab("Proportion of notifications (%)") + xlab("Year") +
    theme(text = element_text(family = "sans"),
          axis.line = element_line(),
          axis.line.x.bottom = element_line(linewidth = line_thickness),
          axis.line.y.left = element_line(linewidth = line_thickness),
          axis.ticks = element_line(linewidth = line_thickness),
          axis.title.x.bottom = element_text(size= axis_title_size, vjust = -2, margin = margin(b = 20)),
          axis.title.y.left = element_text(size= axis_title_size, vjust= 3, margin = margin(l = 15)),
          axis.text.x.bottom = element_text(size= x_axis_text_size, vjust = 0.3, hjust = 0, angle = 90),
          plot.title = element_text(size = title_size, vjust = 6, hjust = title_align, margin = margin(t = 20), face = title_face)) 
  plot2_calibration <- ggplot(subset(prev_MDR, age_group == "[0,99]"  & year %in% from_to), aes(x = year)) +
    geom_line(aes(y = MDR_prev_pct, group = "factor(uid)", color = "factor(uid)"), linewidth = line_thickness, na.rm=TRUE) +
    ylim(c(0, NA)) +
    geom_errorbar(data = subset(targets, name == "MDR_prev_0_99"), aes(ymin = lo, ymax = hi), size = line_thickness, width = error_bar_width) +
    geom_point(data = subset(targets, name == "MDR_prev_0_99"), 
               aes(y = value), size = point_size, colour = "black") +
    labs(title = "% previous MDR among \nnotifications (0-99)") +
    ylab("Proportion of notifications (%)") + xlab("Year") +
    theme(text = element_text(family = "sans"),
          axis.line = element_line(),
          axis.line.x.bottom = element_line(linewidth = line_thickness),
          axis.line.y.left = element_line(linewidth = line_thickness),
          axis.ticks = element_line(linewidth = line_thickness),
          axis.title.x.bottom = element_text(size= axis_title_size, vjust = -2, margin = margin(b = 20)),
          axis.title.y.left = element_text(size= axis_title_size, vjust= 3, margin = margin(l = 15)),
          axis.text.x.bottom = element_text(size= x_axis_text_size, vjust = 0.3, hjust = 0, angle = 90),
          plot.title = element_text(size = title_size, vjust = 6, hjust = title_align, margin = margin(t = 20), face = title_face))
  plot_MDR <-  list(plot1_calibration, plot2_calibration)
} else {
  MDR_merged <- merge(new_MDR, prev_MDR, by = c("year", "age_group", "country") )
  MDR_pct <- MDR_merged %>%
    pivot_longer(cols = c(MDR_new_pct, MDR_prev_pct), names_to = "History", values_to = "pct")
  # Plot "b" if UID is not present
  plot_MDR_pct <- ggplot(subset(MDR_pct, age_group == "[0,99]" & year %in% from_to), aes(x = year)) +
    geom_line(aes(y = pct, group = History, colour = History), linewidth = line_thickness, na.rm=TRUE) +
    ylim(c(0, NA)) +
    geom_errorbar(data = subset(targets, name == "MDR_new_0_99"), aes(ymin = lo, ymax = hi), size = line_thickness, width = error_bar_width, colour = "#A6D854" ) +
    geom_point(data = subset(targets, name == "MDR_new_0_99"), 
               aes(y = value), size = point_size, colour = "#A6D854" ) +
    geom_errorbar(data = subset(targets, name == "MDR_prev_0_99"), aes(ymin = lo, ymax = hi), size = line_thickness, width = error_bar_width, colour = "#E78AC3") +
    geom_point(data = subset(targets, name == "MDR_prev_0_99"), 
               aes(y = value), size = point_size, colour = "#E78AC3") +
    labs(title = "% MDR among \nnotifications (0-99)") +
    ylab("Proportion of notifications (%)") + xlab("Year") +
    scale_color_manual(name = " Treatment \nhistory", values = c("#66A61E", "#E7298A" ),
                       labels = c("New", "Previous")) +
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
  plot_MDR <-  list(plot_MDR_pct)
}


# Plots 7, 8 and 9 are RISK specific 

############################# Diagnostics plots ##################################
# Plot both adults and children if there is no uid but only children if there is uid
plot_calibration_children <- function(metric_dataset, target_name, plot_title, yaxis_title) {
  if ("uid" %in% names(metric_dataset)) {
    # Plot if UID is present
    plot_calibration <- ggplot(subset(metric_dataset, age_group == "[0,14]" & year %in% from_to), aes(x = year)) +
      geom_line(aes(y = total_value, group = "factor(uid)", color = "factor(uid)"), linewidth = line_thickness, na.rm=TRUE) +
      ylim(c(0, NA)) +
      geom_errorbar(data = subset(targets, name == target_name), aes(ymin = lo, ymax = hi), size = line_thickness, width = error_bar_width) +
      geom_point(data = subset(targets, name == target_name), 
                 aes(y = value), size = point_size, colour = "black") +
      labs(title = paste(plot_title, "in children", sep=" ")) +
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
    plot_calibration <- ggplot(subset(metric_dataset, age_group != "[0,99]" & year %in% from_to), aes(x = year)) +
      geom_line(aes(y = total_value, group = age_group, colour = age_group), linewidth = line_thickness, na.rm=TRUE) +
      ylim(c(0, NA)) +
      geom_errorbar(data = subset(targets, name == target_name), aes(ymin = lo, ymax = hi), size = line_thickness, width = error_bar_width, colour =  "#FC8D62") +
      geom_point(data = subset(targets, name == target_name), 
                 aes(y = value), size = point_size, colour = "#FC8D62" ) +
      labs(title = paste(plot_title, "by age", sep=" ")) +
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
      theme(legend.position = "none")
  }
  return(plot_calibration)
}


# 10. Prevalence -------------------------------------------------
plot_TBprev_pop_age <- plot_calibration_no_legend(tb_prev_pop, "[0,14]", "", "TB prevalence in children", "Prevalence per 100,000", NA)

# 12. TB mortality ----------------------------------------------
if ("uid" %in% names(tb_mort_pop)) {
  # Plot if UID is present
  plot_mort_pop_age <- "No plot"
} else {
  plot_mort_pop_age <- ggplot(subset(tb_mort_pop, age_group !="[0,99]" & year %in% from_to), aes(x = year)) +
    geom_line(aes(y = total_value, group = age_group, colour = age_group), linewidth = line_thickness, na.rm=TRUE) +
    ylim(c(0, NA)) +
    labs(title = "TB mortality by age group") +
    ylab("Mortality per 100,000") + xlab("Year") +
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
# 14. Treatment success -------------------------------------
plot_treat_succ <- function(metric_dataset, treatment) {
if ("uid" %in% names(metric_dataset)) {
  # Plot if UID is present
  plot_Tsuccess_age <- "No plot (too many lines)"
} else {
  plot_Tsuccess_age <- ggplot(subset(metric_dataset, year %in% from_to), aes(x = year)) +
    geom_line(aes(y = total_value, group = age_group, colour = age_group), linewidth = line_thickness, na.rm=TRUE) +
    ylim(c(0, NA)) +
    labs(title = paste(treatment," treatment success by \nage group", sep="")) +
    ylab("Proportion of treatment (%)") + xlab("Year") +
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
          legend.title = element_text(size = legend_title_size))
}
  return(plot_Tsuccess_age)
}

plot_Tsuccess_age_ST <- plot_treat_succ(succ_all_ST, "ST")

plot_Tsuccess_age_RT <- plot_treat_succ(succ_all_RT, "RT")

# 15. Population --------------------------------------
if ("uid" %in% names(population)) {
  # Plot if UID is present
  plot_population <- "No plot"
  
} else {
  #Merge population datasets
  population <- population %>% mutate(Variable = "Age")
  population <- population %>% rename(Group = age_group)

  population_risk_0_99 <- subset(population_risk, age_group == "[0,99]")
  population_risk_0_99 <- population_risk_0_99 %>% select(-age_group)
  population_risk_0_99 <- population_risk_0_99 %>% mutate(Variable = "RISK state")
  population_risk_0_99 <- population_risk_0_99 %>% rename(Group = RISK)
  
  
  pop_all_risk_merged <- rbind(population, population_risk_0_99)
  
  # IND has more RISK compartments so add one colour to the plot if ISO = IND
  colors_ind <- c("#D95F02", "#7570B3", "#1B9E77", "#E7298A", "#66A61E", "#FFD92F", "#8B0000") # 7th color added
  colors_other <- c("#D95F02", "#7570B3", "#1B9E77", "#E7298A", "#66A61E", "#FFD92F")
  colors_population <- if (ISO == "IND") colors_ind else colors_other
  
  #Plot
  plot_population <- ggplot(subset(pop_all_risk_merged, year %in% from_to), aes(x = year)) +
    geom_line(aes(y = Population, group = Group, colour = Group, linetype=Variable), linewidth = line_thickness, na.rm=TRUE) +
    ylim(c(0, NA)) +
    labs(title = "Population by age and risk") +
    ylab("Population (number in 1,000)") + xlab("Year") +
    scale_color_manual(name = "Group", values = colors_population) +
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

# 16. Infection prevalence
if ("uid" %in% names(inf_prev_pop)) {
  # Plot if UID is present
  plot_inf_prev <- "No plot"
  
} else {
  plot_inf_prev <- ggplot(subset(inf_prev_pop, age_group == "[0,99]" & year %in% from_to), aes(x = year)) +
    geom_line(aes(y = total_value), linewidth = line_thickness, na.rm=TRUE, colour = "#7570B3") +
    ylim(c(0, NA)) +
    labs(title = "Infection prevalence (0-99)") +
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
}

# 17. Ratio of non-infectious VS infectious TB
if ("uid" %in% names(ratio_Dn_vs_DsDc)) {
  # Plot if UID is present
  plot_ratio <- "No plot"
  
} else {
  plot_ratio <- ggplot(subset(ratio_Dn_vs_DsDc, age_group == "[0,99]" & year %in% from_to), aes(x = year)) +
    geom_line(aes(y = total_value), linewidth = line_thickness, na.rm=TRUE, colour = "#7570B3") +
    ylim(c(0, NA)) +
    labs(title = "Non-infectious VS \ninfectious TB (0-99)") +
    ylab("Ratio") + xlab("Year") +
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

# 18. ARI population -------------------------------
plot_ARI_func <- function(dataset_age, dataset_risk, strain) {
  if ("uid" %in% names(dataset_age)) {
    # Plot if UID is present
    plot_ARI <- "No plot"
    
  } else {
    # Prisonsis the only RISK that has increased lambda so ARI by risk only looked in prisons (merged datasets considering this)
    if (ISO == "BRA") {
      dataset_age <- dataset_age %>% mutate(Variable = "Age")
      dataset_age <- dataset_age %>% rename(Group = age_group)
      
      dataset_risk <- subset(dataset_risk, age_group == "[0,99]")
      dataset_risk <- dataset_risk %>% select(-age_group)
      dataset_risk <- dataset_risk %>% mutate(Variable = "RISK state")
      dataset_risk <- dataset_risk %>% rename(Group = RISK) 
      
      ARI_age_risk_merged <- rbind(dataset_age, dataset_risk)
    } else {
      dataset_age <- dataset_age %>% mutate(Variable = "Age")
      dataset_age <- dataset_age %>% rename(Group = age_group)
      
      ARI_age_risk_merged <- dataset_age
    }
    
    # IND has more RISK compartments so add one colour to the plot if ISO = IND
    colors_ind2 <- c("#D95F02", "#7570B3" , "#666666", "#A6761D", "#E7298A", "#66A61E" , "#FFD92F", "#8B0000") # 7th color added
    colors_other2 <- c("#D95F02", "#7570B3" , "#666666", "#A6761D", "#E7298A", "#66A61E" , "#FFD92F")
    colors_ARI <- if (ISO == "IND") colors_ind2 else colors_other2
    
    #Plot
    plot_ARI <- ggplot(subset(ARI_age_risk_merged, year %in% from_to), aes(x = year)) +
      geom_line(aes(y = total_value, group = Group, colour = Group, linetype=Variable), linewidth = line_thickness, na.rm=TRUE) +
      ylim(c(0, NA)) +
      labs(title = paste("Annual Risk of Infection \n(",strain,")", sep = "")) +
      ylab("Proportion of population/year (%)") + xlab("Year")  +
      scale_color_manual(name = "Group", values = colors_ARI) +
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
}
  
plot_ARI_s <- plot_ARI_func(ARI_age_s, ARI_risk_s, "DS")
plot_ARI_r <- plot_ARI_func(ARI_age_r, ARI_risk_r, "DR")

# 14. Plot proportion of on treatment mortality

if (ISO =="BRA"){
  if ("uid" %in% names(tb_mort_age_prop)) {
    # Plot if UID is present
    plot_mort_prop <- "No target"
  } else {
    # Plot "b" if UID is not present
    plot_mort_prop <- ggplot(subset(tb_mort_prop, age_group == "[0,99]" & year %in% from_to), aes(x = year)) +
      geom_line(aes(y = total_value), linewidth = line_thickness, na.rm=TRUE, colour =  "#E6AB02") +
      ylim(c(0, 100)) +
      geom_errorbar(data = subset(targets, name == "treat_mort"), aes(ymin = lo, ymax = hi), size = line_thickness, width = error_bar_width, colour="#FFD92F") +
      geom_point(data = subset(targets, name == "treat_mort"), 
                 aes(y = value), size = point_size, colour = "#FFD92F") +
      labs(title = "Percentage of total mortality among \nindividuals on treatment") +
      ylab("Percentage (%)") + xlab("Year") +
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
  }else {
  plot_mort_prop <- plot_cali_many_targets(tb_mort_prop, "", "Percentage of total mortality among \nindividuals on treatment", "Percentage (%)")
  
  }


# 15. Plot proportion TB mortality by age group

plot_mort_age_prop <- plot_calibration_children(tb_mort_age_prop, "", "Percentage of TB mortality \nby age group", "Percentage (%)")
