rm(list=ls(all=TRUE))
# Load Libraries ----------------------------------------------------------

library("tidyverse")
source("R/99_functions.R")

# Load Data ---------------------------------------------------------------

timeseries_country <- read_csv("data/01_timeseries_country.csv")
country_data <- read_csv("data/02_country_data.csv")
world_map <- read_csv("data/02_world_map_data.csv")

# Augment timeseries ------------------------------------------------------

timeseries_augment <- timeseries_country %>%
  left_join(country_data, by = c("Country/Region" = "country")) %>%
  mutate(Confirmed_per_100k_citizen = Confirmed / Population * 100000,
         Deaths_per_100k_citizen = Deaths / Population * 100000,
         Recovered_per_100k_citizen = Recovered / Population * 100000)


# Add Lat and Long to country level data ----------------------------------


map_data_augment <- 
  country_data %>%
  full_join(world_map, by = "country") 

# Add the latest date data (cases, and deaths)
map_data_augment <- timeseries_augment %>%
  get_latest_date_data() %>%
  full_join(world_map, by = c("Country/Region" = "country"))
  

# Calculate new cases and rolling cases -----------------------------------

# Calculating daily cases & deaths from cumsums. 
# Further calculation of 14 day means.
timeseries_augment <- timeseries_augment %>% 
  group_by(`Country/Region`) %>% 
  arrange(Date) %>% 
  mutate(New_confirmed = Confirmed - lag(Confirmed, n = 1),
         New_deaths = Deaths - lag(Deaths, n = 1),
         New_recovered = Recovered - lag(Recovered, n = 1),
         Case_fatality = Deaths/Confirmed,
         
         Rolling_mean_confirmed = (lead(Confirmed, n = 7) 
                                   - lag(Confirmed, n = 7))/14,
         Rolling_mean_deaths = (lead(Deaths, n = 7) 
                                - lag(Deaths, n = 7))/14,
         Rolling_case_fatality = Rolling_mean_deaths
         /Rolling_mean_confirmed)

# Finding waves
# Criteria for wave
# Deaths is at least 10 % higher than 1 weeks previously
increase_factor = 1.1 
no_of_days = 7

# Adding a "wave" factor to the data
timeseries_augment <- timeseries_augment %>% 
  mutate(Wave_status = case_when(
    Rolling_mean_deaths < 1 ~ "Non_Wave",
    
    lead(x = Rolling_mean_deaths, n = no_of_days) / Rolling_mean_deaths 
    >= increase_factor ~ "Wave",
    
    lead(x = Rolling_mean_deaths, n = no_of_days) / Rolling_mean_deaths 
    < increase_factor ~ "Non_Wave"),
    
    Wave_status = fct_recode(Wave_status)
  )

# Write Data --------------------------------------------------------------
timeseries_augment %>%
  write_csv("data/03_augmented_timeseries.csv")

timeseries_augment %>%
  write_csv("Covid-19 app/shiny_data/03_augmented_timeseries.csv")

map_data_augment %>%
  write_csv("data/03_augmented_map_data.csv")

map_data_augment %>%
  write_csv("Covid-19 app/shiny_data/03_augmented_map_data.csv")

