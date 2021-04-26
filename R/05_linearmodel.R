rm(list=ls(all=TRUE))

# Load Libraries ----------------------------------------------------------
library(tidyverse)
source("R/99_functions.R")

# Load Data ---------------------------------------------------------------

timeseries_data <- read_csv("data/03_augmented_timeseries.csv")

# Wrangle data ------------------------------------------------------------

# Subset to latest date

latest_date_data <- get_latest_date_data(timeseries_data)

# Select explanatory variable (pop over 65)

model_data <- latest_date_data %>% 
  select(IncomeGroup, 
         Deaths_per_100k_citizen, 
         `Pop%_above65`
         ) %>% 
  group_by(IncomeGroup) %>% 
  nest() %>% 
  ungroup() %>%  
  mutate(mdl = purrr::map(conf.int = TRUE,data,
                   ~lm(Deaths_per_100k_citizen ~ `Pop%_above65`, 
                       data = .x,
                       family = gaussian())))







