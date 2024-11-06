# Load libraries
library(tidyverse)

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




### Optimised version

library(data.table)
library(dplyr)

# Convert health_base to data.table for fast processing
health_base_dt <- as.data.table(health_base)

# Melt (pivot_longer) the entire dataset once to create the long format
ur <- melt(health_base_dt, id.vars = "id", variable.name = "year", value.name = "state")

# For each id, determine the first year each disease state appears
fv <- ur[!(state %in% c("healthy", "null")), .(first_year = min(as.numeric(year))), by = .(id, state)]
setorder(fv, id, first_year)

# Concatenate previous states in a cumulative fashion within each id
fv[, cumulative_state := Reduce(function(x, y) paste(y, x, sep = "|"), state, accumulate = TRUE), by = id]

# Join the cumulative state information back to the main long format (ur)
ur <- merge(ur, fv[, .(id, state, cumulative_state, first_year)], by = c("id", "state"), all.x = TRUE)

# Fill cumulative states forward based on the first year appearance within each id
ur[, cumulative_state := ifelse(
  as.numeric(year) >= first_year, cumulative_state, NA_character_
), by = id]

# Use dplyr::fill to carry forward cumulative states (filling down within each id)
ur <- ur %>%
  group_by(id) %>%
  arrange(id, year) %>%
  mutate(final_state = cumulative_state) %>%
  fill(final_state, .direction = "down") %>%
  ungroup()

# Reshape back to wide format, retaining the modified states
health_base_updated <- dcast(setDT(ur), id ~ year, value.var = "final_state")

# Replace NA values with original "healthy" and "null" states if needed
health_base_updated[is.na(health_base_updated)] <- health_base[is.na(health_base_updated)]

# Assign the result back to the original health_base variable if desired
health_base <- health_base_updated
