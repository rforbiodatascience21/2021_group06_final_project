# global file for shiny app 

library("shiny")
library("tidyverse")
library("maps")
library("mapproj")

#test
# need to load all the final/cleaned data we are using for the graphics.

augmented_map_data <-
  read_csv("shiny_data/03_augmented_map_data.csv")

timeseries <-
  read_csv("shiny_data/03_augmented_timeseries.csv")

# Clean up country names to be compatiable with map.where()
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
  




