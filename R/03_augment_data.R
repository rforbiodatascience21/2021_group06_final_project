# This script could possibly also be used to generate many other agumented variables
# Active cases, incidence rate, case fatality, and groupings of the age/gdp
# could be considered

rm(list=ls(all=TRUE))
# Load Libraries ----------------------------------------------------------

library(tidyverse)


# Load Data ---------------------------------------------------------------

timeseries_country <- read_csv("data/01_timeseries_country.csv")
country_data <- read_csv("data/02_country_data.csv")



# Augment timeseries ------------------------------------------------------

timeseries_augment <- timeseries_country %>%
  left_join(country_data, by = c("Country/Region" = "country"))




# Write Data --------------------------------------------------------------
timeseries_augment %>%
  write_csv("data/03_augmented_timeseries.csv")

