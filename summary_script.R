# Separate each row by separator |
# health_base_fr <- health_base |> as.data.frame() |> filter(str_detect(everything(-id), '|'))


library(tidyverse)
require(tictoc)
# Set seed
set.seed(1024)

health_base <- read_csv("C:/Users/Ali/RMIT University/JIBE working group - simulationResults/ForUrbanTransition/reference/health/04_death_and_disease/pp_healthDiseaseTracker_2039_fixBug_processed.csv")

health_cyc <- read_csv("C:/Users/Ali/RMIT University/JIBE working group - simulationResults/ForUrbanTransition/cycleIntervention/health/04_death_and_disease/pp_healthDiseaseTracker_2039_new_processed.csv")

# Set sample size
sample_size <- 100000

# Keep until 2039
health_base <- health_base |> dplyr::select(1:21) |> slice_sample(n = sample_size)

# Keep until 2039
health_cyc <- health_cyc |> dplyr::select(1:21) |> slice_sample(n = sample_size)

get_expanded_rows <- function(health_base_fr){
  health_base_fr |> 
  pivot_longer(cols = -c(id)) |> 
  mutate(unpacked = str_split(value, "\\|")) |> 
  unnest() |> 
  mutate(value = str_trim(unpacked))  |> 
  dplyr::select(-unpacked)
}


tic()
# Expand each row separated by | character
health_base_summary <- get_expanded_rows(health_base)

toc()

# Expand each row separated by | character
health_cyc_summary <- get_expanded_rows(health_cyc)

# States
states_base_sum <- health_base_summary |> 
  group_by(name, value)|> 
  summarise(nv = dplyr::n(), 
            freq = round(100 * nv / sample_size, 1), scenario = "reference")


ggplot(states_base_sum) +
  aes(x = name, y = nv, fill = value) +
  geom_col() +
  scale_fill_hue(direction = 1) +
  theme_minimal() +
  facet_wrap(vars(value))


states_base_freq <- states_base_sum |>  
  filter(freq > 0)

ggplot(states_base_freq) +
  aes(x = name, y = freq, fill = value) +
  geom_col() +
  scale_fill_hue(direction = 1) +
  theme_minimal() +
  facet_wrap(vars(value))



# States
states_cyc_sum <- health_cyc_summary |> 
  group_by(name, value)|> 
  summarise(nv = dplyr::n(), 
            freq = round(100 * nv / sample_size, 1), scenario = "cycling intervention")


ggplot(states_cyc_sum) +
  aes(x = name, y = nv, fill = value) +
  geom_col() +
  scale_fill_hue(direction = 1) +
  theme_minimal() +
  facet_wrap(vars(value))


states_cyc_freq <- states_cyc_sum |>  
  filter(freq > 0)

ggplot(states_cyc_freq) +
  aes(x = name, y = freq, fill = value) +
  geom_col() +
  scale_fill_hue(direction = 1) +
  theme_minimal() +
  facet_wrap(vars(value))

combine_summary <- bind_rows(states_base_sum, states_cyc_sum) |> mutate(scenario = factor(scenario, levels = c("reference", "cycling intervention")))

plotly::ggplotly(combine_summary %>%
  filter(freq >= 2L & freq <= 85L) %>%
  ggplot() +
  aes(x = name, y = freq, fill = scenario) +
  geom_col(position = "dodge2") +
  scale_fill_hue(direction = 1) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90L)) +
  facet_wrap(vars(value)))

tbl <- combine_summary |> filter(value %in% c("healty", "null")) |> group_by(scenario, name) |> summarise(count = sum(nv))

ggplot(tbl) +
  aes(x = name, y = count, fill = scenario) +
  geom_col(position = "dodge2") +
  scale_fill_hue(direction = 1) +
  labs(
    x = "Simulation year",
    y = "Count",
    title = "Cumulative alive people over time"
  ) +
  theme_minimal()
