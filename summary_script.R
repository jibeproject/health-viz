# Separate each row by separator |
# health_base_fr <- health_base |> as.data.frame() |> filter(str_detect(everything(-id), '|'))


library(tidyverse)
require(tictoc)
# Set seed
set.seed(1024)

# health_base <- read_csv("C:/Users/Ali/RMIT University/JIBE working group - simulationResults/ForUrbanTransition/reference/health/04_death_and_disease/pp_healthDiseaseTracker_2039_fixBug_processed.csv")
# 
# health_cyc <- read_csv("C:/Users/Ali/RMIT University/JIBE working group - simulationResults/ForUrbanTransition/cycleIntervention/health/04_death_and_disease/pp_healthDiseaseTracker_2039_new_fixBug_processed.csv")


directory <- "C:/Users/mbzd2/OneDrive - RMIT University/JIBE/JIBE-WP6/healthmicrosim/"

health_base <- read_csv(paste0(directory, "manchester/simulationResults/ForUrbanTransition/reference/health/04_death_and_disease/pp_healthDiseaseTracker_2039_fixBug_processed.csv"))

health_cyc <- read_csv(paste0(directory, "manchester/simulationResults/ForUrbanTransition/cycleIntervention/health/04_death_and_disease/pp_healthDiseaseTracker_2039_new_fixBug_processed.csv"))

# Set sample size
sample_size <- 1000000

# Keep until 2039
health_base <- health_base |> dplyr::select(1:21) #|> slice_sample(n = sample_size)

health_base_dead <- health_base |> pivot_longer(cols = -id) |> filter(str_detect(value, "dead")) |> mutate(value = "dead") |> pivot_wider(id_cols=id)

health_base <- rows_update(health_base, health_base_dead)

# Keep until 2039
health_cyc <- health_cyc |> dplyr::select(1:21) #|> slice_sample(n = sample_size)

health_cyc_dead <- health_cyc |> pivot_longer(cols = -id) |> filter(str_detect(value, "dead")) |> mutate(value = "dead") |> pivot_wider(id_cols=id)

health_cyc <- rows_update(health_cyc, health_cyc_dead)


get_expanded_rows <- function(health_base_fr){
  health_base_fr |> 
  pivot_longer(cols = -c(id)) |> 
  mutate(unpacked = str_split(value, "\\|")) |> 
  unnest() |> 
  mutate(value = str_trim(unpacked))  |> 
  dplyr::select(-unpacked)
}


# Expand each row separated by | character
health_base_summary <- get_expanded_rows(health_base)

# Expand each row separated by | character
health_cyc_summary <- get_expanded_rows(health_cyc)

# States
states_base_sum <- health_base_summary |> filter(!is.na(value) & !value %in% c("dead", "null")) |> 
  group_by(name, value)|> 
  summarise(nv = dplyr::n(), scenario = "reference") |> mutate(sumnv = sum(nv), freq = round(100 * nv / sumnv, 1))


ggplot(states_base_sum) +
  aes(x = name, y = nv, fill = value) +
  geom_col() +
  scale_fill_hue(direction = 1) +
  theme_minimal() +
  facet_wrap(vars(value), scales = "free_y") +
  labs(x = "Year", y = "NV", fill = "Value") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 4))





states_base_freq <- states_base_sum |>  
  filter(freq > 0)

ggplot(states_base_freq) +
  aes(x = name, y = freq, fill = value) +
  geom_col() +
  scale_fill_hue(direction = 1) +
  theme_minimal() +
  facet_wrap(vars(value))



# States
states_cyc_sum <- health_cyc_summary |> filter(!is.na(value) & !value %in% c("dead", "null")) |> 
  group_by(name, value)|> 
  summarise(nv = dplyr::n(), scenario = "cycling intervention") |> mutate(sumnv = sum(nv), freq = round(100 * nv / sumnv, 1)) 


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

# plotly::ggplotly(
#   
#   combine_summary %>%
#     filter(freq >= 2) %>%
#     ggplot() +
#     aes(x = name, y = nv, fill = scenario) +
#     geom_bar(stat = "summary", fun = "sum", position = "dodge2") +
#     scale_fill_hue(direction = 1) +
#     coord_flip() +
#     theme_minimal() +
#     geom_text(aes(label = nv),
#               position = position_dodge(width = .9), size = 2) +
#     facet_wrap(vars(value)) +  
#     labs(x = "diseases", y = "values")
#   
#   )


## Alternative


combine_summary %>%
  filter(value %in% c("breast_cancer", "coronary_heart_disease", "lung_cancer")) %>%  # keep some only for ilustration
  ggplot() +
  aes(y = name, x = nv, fill = scenario) +  # Map `nv` to x and `name` to y
  geom_bar(stat = "summary", fun = "sum", position = "dodge2") +
  scale_fill_hue(direction = 1) +
  theme_minimal() +
  geom_text(aes(label = nv),
            position = position_dodge(width = 0.9), size = 2, hjust = -0.2) +  # Adjust text position if needed
  facet_wrap(vars(value), scales = "free_x") +  # Allow x-axis to vary by facet
  labs(x = "Values", y = "Diseases")


difference_diseases <- combine_summary %>% 
  group_by(value, scenario) %>%
  summarise(total=sum(nv)) %>%
  ungroup() %>%
  pivot_wider(names_from = scenario,
              values_from = total) %>%
  mutate(difference=(reference - `cycling intervention`)/reference)




write.csv(difference_diseases, paste0(directory, "manchester/simulationResults/ForUrbanTransition/samples/diseases.csv"))

tbl <- combine_summary |> filter(!value %in% c("dead", "null")) |> group_by(scenario, name) |> summarise(count = sum(nv))

reference_lifeyears <- tbl %>% filter(scenario == "reference")

ref_lf <- sum(reference_lifeyears$count)

ref_sc <- sum(cycle_lifeyears$count)

cycle_lifeyears <- tbl %>% filter(scenario == "cycling intervention")


# Don't present on below

g <- ggplot(tbl) +
  aes(x = name, y = count, fill = scenario) +
  geom_col(position = "dodge2") +
  scale_fill_hue(direction = 1) +
  labs(
    x = "Simulation year",
    y = "Count",
    title = "Cumulative alive people over time"
  ) +
  theme_minimal() +
  geom_text(aes(label = count),
    position = position_dodge(width = .9), angle=90)

plotly::ggplotly(g)
