
# Load Libraries ----------------------------------------------------------

library("shiny")
library("tidyverse")


# Load Data ---------------------------------------------------------------

augmented_map_data <-
  read_csv("shiny_data/03_augmented_map_data.csv")

timeseries <-
  read_csv("shiny_data/03_augmented_timeseries.csv")


# Wrangle Data ------------------------------------------------------------

# Clean up country names to be compatible with map.where()
timeseries <- timeseries %>%
  mutate("Country/Region" = str_replace(`Country/Region`,
                                        pattern = "United Kingdom",
                                        replacement = "UK")) %>%
  mutate("Country/Region" = str_replace(`Country/Region`,
                                        pattern = "US",
                                        replacement = "USA")) %>%
  mutate("Country/Region" = str_replace(`Country/Region`,
                                        pattern = "Burma",
                                        replacement = "Myanmar"))
  




