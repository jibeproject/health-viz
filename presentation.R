## Move to markdown

library(tidyverse)
library(readr)
library(ggplot2)
library(esquisse)
library(dplyr)
library(stringr)
library(gt)

## Data baseline year for reference and scenario

directory <- "C:/Users/mbzd2/OneDrive - RMIT University/JIBE/JIBE-WP6/healthmicrosim/"

exposures_base_2021 <- read_csv(paste0(directory,"manchester/simulationResults/ForUrbanTransition/reference/health/03_exposure_and_rr/pp_exposure_2021.csv"))
  
exposures_intervention <- read_csv(paste0(directory,"manchester/simulationResults/ForUrbanTransition/cycleIntervention/health/03_exposure_and_rr/pp_exposure_2021.csv"))

person_base <- read_csv(paste0(directory, "manchester/simulationResults/ForUrbanTransition/reference/health/04_death_and_disease/pp_2029.csv"))

health_base <- read_csv(paste0(directory, "manchester/simulationResults/ForUrbanTransition/reference/health/04_death_and_disease/pp_healthDiseaseTracker_2029.csv")) # %>%
  # left_join(person_base)


  
health_intervention <- read_csv(paste0(directory,"manchester/simulationResults/ForUrbanTransition/cycleIntervention/health/04_death_and_disease/pp_healthDiseaseTracker_2029.csv"))

############################ PM2.5, NO2 and mmets ##################################

#Marina to do

exposure_ref <- read_csv("manchester/simulationResults/ForUrbanTransition/reference/health/03_exposure_and_rr/pp_exposure_2025.csv")

exposure_cint <- read_csv("manchester/simulationResults/ForUrbanTransition/cycleIntervention/health/03_exposure_and_rr/pp_exposure_2025.csv")

dd_ref <- read_csv("manchester/simulationResults/ForUrbanTransition/reference/sp_2021_2050/dd_2025_ref.csv")

dd_cint <- read_csv("manchester/simulationResults/ForUrbanTransition/cycleIntervention/dd_2025_cint.csv")

zone <- read_csv("manchester/synPop/sp_2021/zoneSystem.csv")

################### Data Preparation ###########################################

exposure_ref <- exposure_ref %>% 
  select(id,
         hhid,
         age,
         gender,
         mmetHr_cycle,
         mmetHr_walk,
         mmetHr_otherSport,
         exposure_normalised_pm25,
         exposure_normalised_no2) %>%
  mutate(scenario = "Baseline") %>% 
  left_join(dd_ref %>% 
              select(ddid = id, hhID, zone_id = zone),
            by = c("hhid" = "hhID")) %>% 
  left_join(zone %>% 
              select(location = ladnm,imd = imd10, oaID),
            by = c("zone_id" = "oaID"))

exposure_cint <- exposure_cint %>% 
  select(id,
         hhid,
         age,
         gender,
         mmetHr_cycle,
         mmetHr_walk,
         mmetHr_otherSport,
         exposure_normalised_pm25,
         exposure_normalised_no2) %>%
  mutate(scenario = "Cycling Intervention") %>% 
  left_join(dd_cint %>% 
              select(ddid = id, hhID, zone_id = zone),
            by = c("hhid" = "hhID")) %>% 
  left_join(zone %>% 
              select(location = ladnm,imd = imd10, oaID),
            by = c("zone_id" = "oaID"))

exposure <- bind_rows(exposure_ref, exposure_cint)

rm(exposure_ref, exposure_cint)

################### Adding Zone and Demographics ########################

exposure <- exposure %>% 
  filter(age >= 18) %>% 
  mutate(gender = factor(gender,
                         levels = c(1,2),
                         labels = c("Male","Female")),
         scenario = factor(scenario, 
                           levels = c("Baseline","Cycling Intervention")),
         age_group = factor(case_when(
           age >= 18 & age <= 25 ~ "18-25",
           age >= 26 & age <= 65 ~ "26-65",
           age >= 66 ~ "65+",
           TRUE ~ "Other"), 
           levels = c("18-25", "26-65", "65+", "Other")),
         imd = factor(imd,
                      levels = c(1,2,3,4,5,6,7,8,9,10),
                      labels = c("Most Deprived", 2,3,4,5,6,7,8,9,"Least Deprived")),
         total_mmetHr = mmetHr_cycle+mmetHr_walk+mmetHr_otherSport)

########################## Data Visualizations #################################

summary_stats_pm25 <- exposure %>%
  group_by(scenario) %>%
  summarize(
    min = min(exposure_normalised_pm25),
    IQR_low = quantile(exposure_normalised_pm25, 0.25),
    median = median(exposure_normalised_pm25),
    IQR_high = quantile(exposure_normalised_pm25, 0.75),
    max = max(exposure_normalised_pm25))

# Overall

pm25_scenarios <- ggplot(exposure, aes(x = scenario, y = exposure_normalised_pm25, fill = scenario)) +
  geom_boxplot() +
  labs(title = "Weekly Individual PM2.5 Exposure Levels",
       y = "PM2.5 (µg/m³)",
       fill = "Scenario") +
  theme_minimal(base_size = 16) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.x = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text = element_text(face = "bold"),
    legend.text = element_text(face = "bold"),
    legend.title = element_text(face = "bold"))

# Age

pm25_age <- ggplot(exposure, aes(x = age_group, y = exposure_normalised_pm25, fill = scenario)) +
  geom_boxplot() +
  labs(
    title = "Weekly Individual PM2.5 Exposure Levels by Age",
    y = "PM2.5 (µg/m³)",
    fill = "Scenario") +
  theme_minimal(base_size = 16) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(), 
    axis.title.x = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold"), 
    axis.text = element_text(face = "bold"),
    legend.text = element_text(face = "bold"),
    legend.title = element_text(face = "bold"))

# Gender

pm25_gender <- ggplot(exposure, aes(x = gender, y = exposure_normalised_pm25, fill = scenario)) +
  geom_boxplot() +
  labs(title = "Weekly Individual PM2.5 Exposure Levels by Gender",
       y = "PM2.5 (µg/m³)",
       fill = "Scenario") +
  theme_minimal(base_size = 16) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.x = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text = element_text(face = "bold"),
    legend.text = element_text(face = "bold"),
    legend.title = element_text(face = "bold"))

pm25_imd <- ggplot(exposure, aes(x = imd, y = exposure_normalised_pm25, fill = scenario)) +
  geom_boxplot() +
  labs(
    title = "Weekly PM2.5 Exposure Levels by Index of Multiple Deprivation (IMD)",
    y = "PM2.5 (µg/m³)",
    fill = "Scenario") +
  theme_minimal(base_size = 16) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(), 
    axis.title.x = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold"), 
    axis.text = element_text(face = "bold"),
    legend.text = element_text(face = "bold"),
    legend.title = element_text(face = "bold"))

# No2

no2_scenarios <- ggplot(exposure, aes(x = scenario, y = exposure_normalised_no2, fill = scenario)) +
  geom_boxplot() +
  labs(title = "Weekly Individual NO2 Exposure Levels",
       y = "NO2 (µg/m³)",
       fill = "Scenario") +
  theme_minimal(base_size = 16) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.x = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text = element_text(face = "bold"),
    legend.text = element_text(face = "bold"),
    legend.title = element_text(face = "bold"))

# Age

no2_age <- ggplot(exposure, aes(x = age_group, y = exposure_normalised_no2, fill = scenario)) +
  geom_boxplot() +
  labs(
    title = "Weekly Individual NO2 Exposure Levels by Age",
    y = "NO2 (µg/m³)",
    fill = "Scenario") +
  theme_minimal(base_size = 16) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(), 
    axis.title.x = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold"), 
    axis.text = element_text(face = "bold"),
    legend.text = element_text(face = "bold"),
    legend.title = element_text(face = "bold"))

# Gender

no2_gender <- ggplot(exposure, aes(x = gender, y = exposure_normalised_no2, fill = scenario)) +
  geom_boxplot() +
  labs(title = "Weekly Individual NO2 Exposure Levels by Gender",
       y = "NO2 (µg/m³)",
       fill = "Scenario") +
  theme_minimal(base_size = 16) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.x = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text = element_text(face = "bold"),
    legend.text = element_text(face = "bold"),
    legend.title = element_text(face = "bold"))

#IMD

no2_imd <- ggplot(exposure, aes(x = imd, y = exposure_normalised_no2, fill = scenario)) +
  geom_boxplot() +
  labs(
    title = "Weekly Inidividual NO2 Exposure Levels by Index of Multiple Deprivation (IMD)",
    y = "NO2 (µg/m³)",
    fill = "Scenario") +
  theme_minimal(base_size = 16) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(), 
    axis.title.x = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold"), 
    axis.text = element_text(face = "bold"),
    legend.text = element_text(face = "bold"),
    legend.title = element_text(face = "bold"))

# mmetHR

mmet_scenarios <- ggplot(exposure, aes(x = scenario, y = total_mmetHr, fill = scenario)) +
  geom_boxplot() +
  labs(title = "Total Marginal MET Hours per Week",
       y = "mMET-hours/week",
       fill = "Scenario") +
  theme_minimal(base_size = 16) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.x = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text = element_text(face = "bold"),
    legend.text = element_text(face = "bold"),
    legend.title = element_text(face = "bold"))

# Age

mmet_age <- ggplot(exposure, aes(x = age_group, y = total_mmetHr, fill = scenario)) +
  geom_boxplot() +
  labs(
    title = "Total Marginal MET Hours per Week by Age",
    y = "mMET-hours/week",
    fill = "Scenario") +
  theme_minimal(base_size = 16) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(), 
    axis.title.x = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold"), 
    axis.text = element_text(face = "bold"),
    legend.text = element_text(face = "bold"),
    legend.title = element_text(face = "bold"))

# Gender

mmet_gender <- ggplot(exposure, aes(x = gender, y = total_mmetHr, fill = scenario)) +
  geom_boxplot() +
  labs(title = "Total Marginal MET Hours per Week by Gender",
       y = "mMET-hours/week",
       fill = "Scenario") +
  theme_minimal(base_size = 16) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.x = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text = element_text(face = "bold"),
    legend.text = element_text(face = "bold"),
    legend.title = element_text(face = "bold"))

#IMD

mmet_imd <- ggplot(exposure, aes(x = imd, y = total_mmetHr, fill = scenario)) +
  geom_boxplot() +
  labs(
    title = "Total Marginal MET Hours per Week by Index of Multiple Deprivation (IMD)",
    y = "mMET-hours/week",
    fill = "Scenario") +
  theme_minimal(base_size = 16) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(), 
    axis.title.x = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold"), 
    axis.text = element_text(face = "bold"),
    legend.text = element_text(face = "bold"),
    legend.title = element_text(face = "bold"))

# PM2.5 Table
pm25_table <- exposure %>%
  group_by(scenario) %>%
  summarize(
    Mean = round(mean(exposure_normalised_pm25), 2),
    `5th` = round(quantile(exposure_normalised_pm25, 0.05), 2),
    `25th` = round(quantile(exposure_normalised_pm25, 0.25), 2),
    `35th` = round(quantile(exposure_normalised_pm25, 0.35), 2),
    `50th` = round(quantile(exposure_normalised_pm25, 0.5), 2),
    `95th` = round(quantile(exposure_normalised_pm25, 0.95), 2),
    Median = round(median(exposure_normalised_pm25), 2),
    .groups = 'drop'
  ) %>%
  mutate(
    `Change (%)` = round((Median - Median[scenario == "Baseline"]) / Median[scenario == "Baseline"] * 100, 2),
    Type = "PM2.5"
  )

# NO2 Table
no2_table <- exposure %>%
  group_by(scenario) %>%
  summarize(
    Mean = round(mean(exposure_normalised_no2), 2),
    `5th` = round(quantile(exposure_normalised_no2, 0.05), 2),
    `25th` = round(quantile(exposure_normalised_no2, 0.25), 2),
    `35th` = round(quantile(exposure_normalised_no2, 0.35), 2),
    `50th` = round(quantile(exposure_normalised_no2, 0.5), 2),
    `95th` = round(quantile(exposure_normalised_no2, 0.95), 2),
    Median = round(median(exposure_normalised_no2), 2),
    .groups = 'drop'
  ) %>%
  mutate(
    `Change (%)` = round((Median - Median[scenario == "Baseline"]) / Median[scenario == "Baseline"] * 100, 2),
    Type = "NO2"
  )

# Physical Activity (MMET) Table
mmet_table <- exposure %>%
  group_by(scenario) %>%
  summarize(
    Mean = round(mean(total_mmetHr), 2),
    `5th` = round(quantile(total_mmetHr, 0.05), 2),
    `25th` = round(quantile(total_mmetHr, 0.25), 2),
    `35th` = round(quantile(total_mmetHr, 0.35), 2),
    `50th` = round(quantile(total_mmetHr, 0.5), 2),
    `95th` = round(quantile(total_mmetHr, 0.95), 2),
    Median = round(median(total_mmetHr), 2),
    .groups = 'drop'
  ) %>%
  mutate(
    `Change (%)` = round((Median - Median[scenario == "Baseline"]) / Median[scenario == "Baseline"] * 100, 2),
    Type = "Physical Activity"
  )

combined_table <- bind_rows(pm25_table, no2_table, mmet_table) %>% 
  arrange(Type, scenario) %>% 
  gt() %>% 
  tab_row_group(
    label = md("**PM2.5**"), 
    rows = Type == "PM2.5"
  ) %>% 
  tab_row_group(
    label = md("**NO2**"), 
    rows = Type == "NO2"
  ) %>% 
  tab_row_group(
    label = md("**Physical Activity**"), 
    rows = Type == "Physical Activity"
  ) %>% 
  cols_hide(columns = "Type") %>% 
  cols_label(
    scenario = "", 
    Mean = "Mean", 
    `5th` = "5th Percentile", 
    `25th` = "25th Percentile", 
    `35th` = "35th Percentile", 
    `50th` = "50th Percentile", 
    `95th` = "95th Percentile", 
    Median = "Median", 
    `Change (%)` = "Change (%)"
  ) %>% 
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels() 
  ) %>% 
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_row_groups()) %>% 
  tab_header(
    title = "Summary Statistics of Individual Exposures",
    subtitle = "The change is calculated as percentage difference with the baseline scenario") %>% 
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_title())

combined_table

############################ Health ##################################################

## Diseases ###
## The idea here is to create line graps with y-axis as year and y-axis diseases. Exclude healthy.
## The data is one row per id. When condition changes the id in fact keeps the original condition and we need reflect this. 

### Sample data 

health_base_sample <-  head(health_base, 1000)

view(health_base_sample)

## Saved to try esquisser
# write.csv(health_base_sample, "manchester/simulationResults/ForUrbanTransition/samples/health_ref.csv", row.names = FALSE)

## Data with totals per conditions for all modelled years
data_long <- health_base_sample %>%
  pivot_longer(cols = starts_with("20"), names_to = "year", values_to = "condition") %>%
  mutate(year = as.numeric(sub("20", "", year))) %>%
  group_by(year, condition) %>%
  summarise(count = n(), .groups = 'drop') %>%
  pivot_wider(names_from = condition, values_from = count, values_fill = 0) %>%
  bind_rows(data_long %>%
      summarise(across(where(is.numeric), sum)) %>%
      mutate(Category = "Total")) %>% 
  filter (year<29) # later replace with simulation years (2050?)

datal_long_years <- data_long %>%
  slice(-n()) %>%
  select(!c("null", "Total", "Category", "healthy")) # Remove healthy for graph but keep for life years table 

# Reshape data from wide to long format
data_plot_years <- datal_long_years %>%
  pivot_longer(
    cols = -year,              # Exclude 'year' column from pivoting
    names_to = "disease",      # Name of the new column for disease names
    values_to = "value"        # Name of the new column for values
  )

# Plot with ggplot2
ggplot(data_plot_years, aes(x = year, y = value, colour = disease)) +
  geom_line() +
  theme_minimal() +
  labs(
    x = "Year",
    y = "Value",
    title = "Trends of Different Health Conditions Over Time",
    colour = "Disease"
  )


## TRY to keep condition per id

# Function to add rows for status changes

library(dplyr)
library(tidyr)

add_status_change_rows <- function(df) {
  # Step 1: Reshape to long format and identify status changes
  df_long <- df %>%
    pivot_longer(cols = starts_with("20"), names_to = "year", values_to = "status") %>%
    arrange(id, year) %>%
    group_by(id) %>%
    mutate(status_change = status != lag(status, default = first(status))) %>%
    ungroup()
  
  # Step 2: Create new rows for each unique status period per ID
  # For each period, get the start and end year based on changes in status
  new_rows <- df_long %>%
    group_by(id) %>%
    mutate(
      start_year = if_else(status_change, lag(year, default = first(year)), NA_character_),
      end_year = if_else(status_change, year, NA_character_)
    ) %>%
    fill(start_year, .direction = "down") %>%  # Fill down the start year
    group_by(id, start_year) %>%
    summarize(
      status = first(status),
      start_year = as.integer(first(start_year)),
      end_year = as.integer(first(end_year)) - 1,  # End before next status change
      .groups = 'drop'
    ) %>%
    rowwise() %>%
    do({
      data.frame(
        id = .$id,
        year = .$start_year:.$end_year,
        status = .$status
      )
    }) %>%
    ungroup() %>%
    pivot_wider(names_from = year, values_from = status, values_fill = "null")
  
  # Step 3: Bind new rows with the original data and ensure no duplicates
  bind_rows(df, new_rows) %>%
    distinct() %>%
    arrange(id, across(starts_with("20")))
}

# Apply the function
result_df <- add_status_change_rows(health_base_sample)
print(result_df)
