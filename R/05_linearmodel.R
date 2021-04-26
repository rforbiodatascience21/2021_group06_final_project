rm(list=ls(all=TRUE))


# Load Libraries ----------------------------------------------------------
library(tidyverse)
library(broom)
library(patchwork)
library(ggrepel)
library(cowplot)
source("R/99_functions.R")


# Load Data ---------------------------------------------------------------
timeseries_data <- read_csv("data/03_augmented_timeseries.csv")



# Wrangle data ------------------------------------------------------------

# Subset to latest date

latest_date_data <- get_latest_date_data(timeseries_data)

# Select explanatory variable (pop over 65)

model_data <- latest_date_data %>% 
  select(Region, 
         Deaths_per_100k_citizen, 
         `Pop%_above65`
         ) %>% 
  group_by(Region) %>% 
  nest() %>% 
  ungroup()

model_data_lm <- model_data %>% 
  mutate(mdl = map((conf.int = TRUE,data,~glm(Deaths_per_100k_citizen ~ `Pop%_above65`, 
                                              data = .x, family = binomial(link = "logit"))), tidied = map(mdl, tidy)) %>% 
           unnest(tidied)


gravier_sample_mdl <- gravier_sample %>% 
  mutate(mdl = map((conf.int = TRUE,data,~glm(outcome ~ log2_expr_level, data = .x, family = binomial(link = "logit"))), tidied = map(mdl, tidy)) %>% 
           unnest(tidied)








