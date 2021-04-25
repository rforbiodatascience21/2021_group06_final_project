# global file for shiny app 

library("shiny")
library("tidyverse")
library("maps")
library("mapproj")

# need to load all the final/cleaned data we are using for the graphics.

timeseries_augment <- 
  read_csv("data/03_augmented_timeseries.csv")

