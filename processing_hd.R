# Load libraries
library(tidyverse)
library(arrow)

# Set seed for reproducibility
set.seed(2024)

# Read cycleIntervention health output as an example
# hd <- read_csv("C:/Users/ajnab/RMIT University/JIBE working group - General/manchester/simulationResults/ForUrbanTransition/cycleIntervention/health/04_death_and_disease/pp_healthDiseaseTracker_2029.csv")

## Belen folder
directory <- "C:/Users/mbzd2/OneDrive - RMIT University/JIBE/JIBE-WP6/healthmicrosim/"

health_base <- read_csv(paste0(directory, "manchester/simulationResults/ForUrbanTransition/reference/health/04_death_and_disease/pp_healthDiseaseTracker_2039.csv")) # %>%
# left_join(person_base)
# health_intervention <- read_csv(paste0(directory,"manchester/simulationResults/ForUrbanTransition/cycleIntervention/health/04_death_and_disease/pp_healthDiseaseTracker_2029.csv"))


# Same for quicker run
# shd <- hd |> slice_sample(n = 100)

####### Referecen data #######

# Loop through each row
for (i in 1:nrow(health_base)){
  # Create a long df for each row
  ur <- pivot_longer(health_base[i,], cols = -id)
  # Get a table for each disease state with the first year when it was appeared
  fv <- ur |> group_by(value) |> summarise(fy = first(name)) |> arrange(fy)
  # Loop through the first value dataset with first year for the disease appearance
  for (j in 1:nrow(fv)){
    # Ignore healty and null states
    if (!fv$value[j] %in% c("healthy", "null"))
    {
      # Do not carry forward the healthy state to any other state
      if (fv$value[j - 1] != "healthy")
        # Join previous state with the current one with a | as a separator
        fv$value[j] <- paste(fv$value[j], fv$value[j - 1], sep = "|")
    }
  }
  # Update the long format for each ID with the new states
  for (k in 1:nrow(fv)){
    if (!fv$value[k] %in% c("healthy", "null"))
        ur[(ur$name >= as.numeric(fv$fy[k]) & ur$name < (as.numeric(fv$fy[k + 1]))),]$value <- fv$value[k]
  }
  
  # Update the states
  health_base <- rows_update(health_base, pivot_wider(ur, id_cols=id))
}



####### Scenario data #######

# Loop through each row
for (i in 1:nrow(health_intervention)){
  # Create a long df for each row
  ur <- pivot_longer(health_intervention[i,], cols = -id)
  # Get a table for each disease state with the first year when it was appeared
  fv <- ur |> group_by(value) |> summarise(fy = first(name)) |> arrange(fy)
  # Loop through the first value dataset with first year for the disease appearance
  for (j in 1:nrow(fv)){
    # Ignore healty and null states
    if (!fv$value[j] %in% c("healthy", "null"))
    {
      # Do not carry forward the healthy state to any other state
      if (fv$value[j - 1] != "healthy")
        # Join previous state with the current one with a | as a separator
        fv$value[j] <- paste(fv$value[j], fv$value[j - 1], sep = "|")
    }
  }
  # Update the long format for each ID with the new states
  for (k in 1:nrow(fv)){
    if (!fv$value[k] %in% c("healthy", "null"))
      ur[(ur$name >= as.numeric(fv$fy[k]) & ur$name < (as.numeric(fv$fy[k + 1]))),]$value <- fv$value[k]
  }
  
  # Update the states
  health_intervention <- rows_update(health_intervention, pivot_wider(ur, id_cols=id))
}


### Optimised version

library(data.table)
library(dplyr)

# Convert health_base to data.table for fast processing
health_base_dt <- as.data.table(health_base)

# Melt (pivot_longer) the entire dataset once to create the long format
ur <- melt(health_base_dt, id.vars = "id", variable.name = "year", value.name = "state")

# For each id, determine the first year each disease state appears, excluding "healthy" and "null"
fv <- ur[!(state %in% c("healthy", "null")), .(first_year = min(as.numeric(year))), by = .(id, state)]
setorder(fv, id, first_year)

# Concatenate previous states cumulatively within each id
fv[, cumulative_state := Reduce(function(x, y) paste(y, x, sep = "|"), state, accumulate = TRUE), by = id]

# Merge cumulative states back to the main dataset
ur <- merge(ur, fv[, .(id, state, cumulative_state, first_year)], by = c("id", "state"), all.x = TRUE)

# Create a final state column preserving "healthy" where applicable and setting "dead" to "null"
ur[, final_state := ifelse(
  state == "healthy" | is.na(cumulative_state), 
  state,  # Keep "healthy" or original if cumulative_state is NA
  cumulative_state  # Otherwise use cumulative state
), by = id]

# Change any state that includes "dead" to "null"
ur[, final_state := ifelse(grepl("dead", final_state), "null", final_state)]

# Use dplyr::fill to carry forward cumulative states within each id
ur <- ur %>%
  group_by(id) %>%
  arrange(id, year) %>%
  mutate(final_state = ifelse(final_state == "null", NA, final_state)) %>%  # Treat "null" as NA
  fill(final_state, .direction = "down") %>%
  replace_na(list(final_state = "healthy")) %>%  # Replace any remaining NA with "healthy"
  ungroup()

# Reshape back to wide format, retaining the modified states
health_base_updated <- dcast(setDT(ur), id ~ year, value.var = "final_state")

# Ensure the original "healthy" and "null" states are preserved where they belong
health_base_updated[is.na(health_base_updated)] <- health_base[is.na(health_base_updated)]

# Assign the result back to the original health_base variable if desired
health_base_plot <- health_base_updated

