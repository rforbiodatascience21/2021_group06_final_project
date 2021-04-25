# This script could possibly also be used to generate many other agumented variables
# Active cases, incidence rate, case fatality, and groupings of the age/gdp
# could be considered

rm(list=ls(all=TRUE))
# Load Libraries ----------------------------------------------------------

library("tidyverse")
library("maps")
library("mapproj")

#test 

# Load Data ---------------------------------------------------------------

timeseries_country <- read_csv("data/01_timeseries_country.csv")
country_data <- read_csv("data/02_country_data.csv")

world_map <- map_data("world") # data in R giving country Lat and Long. 

# Augment timeseries ------------------------------------------------------

country_data <- 
  country_data %>% 
  mutate(country=ifelse(country=="US","United States",country)) %>% 
  mutate(country=ifelse(country=="Congo (Kinshasa)","Democratic Republic of the Congo",country))

timeseries_country <- 
  timeseries_country %>% 
  rename(
    "country" = "Country/Region",   # rename the column name
  ) %>%
  mutate(country=ifelse(country=="US","United States",country)) 

timeseries_augment <- timeseries_country %>%
  left_join(country_data, by = c("country")) %>%
  mutate("Cases_per_100k_citizen" = Cases/Population * 100000)


# Add Lat and Long to country level data ----------------------------------

world_map <- 
  world_map %>% 
  rename(
    "country" = "region",   # rename the column name
  ) %>%
  select(-"subregion") %>% 
  mutate(country=ifelse(country=="USA","United States",country))%>% 
  mutate(country=ifelse(country=="UK","United Kingdom",country))

map_data_augment <- 
  country_data%>%
  full_join(world_map, by = c("country")) 


# Write Data --------------------------------------------------------------
timeseries_augment %>%
  write_csv("data/03_augmented_timeseries.csv")

map_data_augment %>%
  write_csv("data/03_augmented_map_data.csv")

map_data_augment %>%
  write_csv("Final_shiny/shiny_data/03_augmented_map_data.csv")

