# Load libraries
library(tidyverse)
library(arrow)

# Set seed for reproducibility
set.seed(2024)

# Read cycleIntervention health output as an example
hd <- read_csv("C:/Users/ajnab/RMIT University/JIBE working group - General/manchester/simulationResults/ForUrbanTransition/cycleIntervention/health/04_death_and_disease/pp_healthDiseaseTracker_2029.csv")

# Same for quicker run
shd <- hd |> slice_sample(n = 100)

# Loop through each row
for (i in 1:nrow(shd)){
  # Create a long df for each row
  ur <- pivot_longer(shd[i,], cols = -id)
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
  shd <- rows_update(shd, pivot_wider(ur, id_cols=id))
}
