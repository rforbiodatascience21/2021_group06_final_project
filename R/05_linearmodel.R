rm(list=ls(all=TRUE)) 

# Load Libraries ----------------------------------------------------------
library(tidyverse)
source("R/99_functions.R")
library(stringr)
library(purrr)
library(broom)

# Load Data ---------------------------------------------------------------

timeseries_data <- read_csv("data/03_augmented_timeseries.csv")

# Wrangle data ------------------------------------------------------------

# Subset to latest date

latest_date_data <- get_latest_date_data(timeseries_data) %>%
  drop_na()

# Select explanatory variable (pop over 65)

model_data <- latest_date_data %>% 
  select(IncomeGroup, 
         Deaths_per_100k_citizen, 
         `Pop%_above65`
         ) %>% 
  group_by(IncomeGroup) %>% 
  nest() %>% 
  ungroup() %>%  
  mutate(mdl = purrr::map(data,
                   ~glm(Deaths_per_100k_citizen ~ `Pop%_above65`, 
                       data = .x,
                       family = gaussian()
                       ))) 


model_data <-
  model_data %>% 
  mutate(mdl_tidy = purrr::map(mdl, tidy, conf.int = TRUE)) %>% 
  unnest(mdl_tidy)

model_data

ggplot(latest_date_data, aes(`Pop%_above65`, Deaths_per_100k_citizen)) +
  geom_point(aes(color = factor(IncomeGroup))) +
  geom_smooth(method ="lm",aes(color = IncomeGroup),se=F) +
  coord_cartesian() 

ggplot(latest_date_data, aes(Age_median, Deaths_per_100k_citizen)) +
  geom_point(aes(color = factor(IncomeGroup))) +
  geom_smooth(method ="lm",aes(color = IncomeGroup),se=F) +
  facet_wrap(IncomeGroup ~ .,scale="free_x")

# working on this....
model_data %>%
  ggplot(aes(x=estimate,
             y=IncomeGroup))+
  geom_vline(xintercept = 0, linetype="dashed")+
  geom_point()+
  geom_errorbarh(aes(xmin=conf.low,xmax=conf.high))


  


