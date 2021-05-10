
# Load Libraries ----------------------------------------------------------

library("shiny")
library("tidyverse")


# Load Data ---------------------------------------------------------------

augmented_map_data <-
  read_csv("shiny_data/03_augmented_map_data.csv")

timeseries <-
  read_csv("shiny_data/03_augmented_timeseries.csv",
           col_types = cols(
             "Rolling_mean_confirmed" = col_double(),
             "Rolling_mean_deaths" = col_double(),
             "Rolling_case_fatality" = col_double(),
             "Wave_status" = col_character()))


# Wrangle Data ------------------------------------------------------------

# Clean up country names to be compatible with map.where()
timeseries <- timeseries %>%
  mutate(Country = recode(Country, 
                          "US" = "USA",
                          "United Kingdom" = "UK",
                          "Burma" = "Myanmar"))

  



