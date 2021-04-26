rm(list=ls(all=TRUE))


# Load Libraries ----------------------------------------------------------
library(tidyverse)
library(broom)
library(patchwork)
library(ggrepel)
library(cowplot)
source("R/99_functions.R")
library(stringr)
library(purrr)


# Load Data ---------------------------------------------------------------
timeseries_data <- read_csv("data/03_augmented_timeseries.csv")



# Wrangle data ------------------------------------------------------------

# Subset to latest date

latest_date_data <- get_latest_date_data(timeseries_data)

# Select explanatory variable (pop over 65)

latest_date_data %>% 
  select(Region, 
         Deaths_per_100k_citizen, 
         `Pop%_above65`
         ) %>% 
  group_by(Region) %>%
  nest %>% 
  ungroup

model_data$data[[1]]


model_data %>% 
  map(data, ~lm(Deaths_per_100k_citizen ~ `Pop%_above65`,
                              data = .x,
                              family = gaussian()))

model_data  %>%
  mutate(mdl = map(conf.int = TRUE,data,
                   ~lm(Deaths_per_100k_citizen ~ `Pop%_above65`, 
                       data = .x,
                       family = gaussian())))

country_model <- function(df) {
  lm(Deaths_per_100k_citizen ~ `Pop%_above65`, data = df)
}

models <- map(model_data$data, country_model)

model_data_lm <- model_data %>% 
  mutate(model = map(data, country_model))
model_data






