## Move to markdown


library(tidyverse)
library(readr)
library(ggplot2)
library(esquisse)
library(dplyr)


directory <- "C:/Users/mbzd2/OneDrive - RMIT University/JIBE/JIBE-WP6/healthmicrosim/"

exposures_base_2021 <- read_csv(paste0(directory,"manchester/simulationResults/ForUrbanTransition/reference/health/03_exposure_and_rr/pp_exposure_2021.csv"))
  
exposures_intervention <- read_csv(paste0(directory,"manchester/simulationResults/ForUrbanTransition/cycleIntervention/health/03_exposure_and_rr/pp_exposure_2021.csv"))

person_base <- read_csv(paste0(directory, "manchester/simulationResults/ForUrbanTransition/reference/health/04_death_and_disease/pp_2029.csv"))

health_base <- read_csv(paste0(directory, "manchester/simulationResults/ForUrbanTransition/reference/health/04_death_and_disease/pp_healthDiseaseTracker_2029.csv")) # %>%
  # left_join(person_base)


  
health_intervention <- read_csv("manchester/simulationResults/ForUrbanTransition/cycleIntervention/health/04_death_and_disease/pp_healthDiseaseTracker_2029.csv")



### Sample data 

health_base_sample <-  head(health_base, 1000)

view(health_base_sample)

# Make sure to replace 'your_data' with the name of your actual data frame
write.csv(health_base_sample, "manchester/simulationResults/ForUrbanTransition/samples/health_ref.csv", row.names = FALSE)


## Data with totals per conditions for all modelled years
data_long <- health_base_sample %>%
  pivot_longer(cols = starts_with("20"), names_to = "year", values_to = "condition") %>%
  mutate(year = as.numeric(sub("20", "", year))) %>%
  group_by(year, condition) %>%
  summarise(count = n(), .groups = 'drop') %>%
  pivot_wider(names_from = condition, values_from = count, values_fill = 0) %>%
  bind_rows(
    data_long %>%
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

## Data with total per condition per year

## TO DO

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
