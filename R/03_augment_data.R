# This script could possibly also be used to generate many other agumented variables
# Active cases, incidence rate, case fatality, and groupings of the age/gdp
# could be considered

rm(list=ls(all=TRUE))
# Load Libraries ----------------------------------------------------------

library(tidyverse)
library(maps)
library(mapproj)
source("R/99_functions.R")


# Load Data ---------------------------------------------------------------

timeseries_country <- read_csv("data/01_timeseries_country.csv")
country_data <- read_csv("data/02_country_data.csv")
world_map <- read_csv("data/02_world_map_data.csv")

# Augment timeseries ------------------------------------------------------

timeseries_augment <- timeseries_country %>%
  left_join(country_data, by = c("Country/Region" = "country")) %>%
  mutate("Cases_per_100k_citizen" = Confirmed/Population * 100000,
         "Deaths_per_100k_citizen" = Deaths/Population * 100000,
         "Recovered_per_100k_citizen" = Recovered/Population * 100000)


# Add Lat and Long to country level data ----------------------------------


map_data_augment <- 
  country_data %>%
  full_join(world_map, by = "country") 

map_data_augment <- timeseries_augment %>%
  get_latest_date_data() %>%
  full_join(world_map, by = c("Country/Region" = "country"))
  
  


# Write Data --------------------------------------------------------------
timeseries_augment %>%
  write_csv("data/03_augmented_timeseries.csv")

timeseries_augment %>%
  write_csv("Final_shiny/shiny_data/03_augmented_timeseries.csv")

map_data_augment %>%
  write_csv("data/03_augmented_map_data.csv")

map_data_augment %>%
  write_csv("Final_shiny/shiny_data/03_augmented_map_data.csv")

