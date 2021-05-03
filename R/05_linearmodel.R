rm(list=ls(all=TRUE)) 

# Load Libraries ----------------------------------------------------------
library("tidyverse")
source("R/99_functions.R")
library("stringr")
library("purrr")
library("broom")

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
         `Pop%_above65`) %>% 
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
  coord_cartesian()+
  theme_minimal()+
  labs(y="Deaths per 100k", x = "Median Age", color = "Income Group")

ggplot(latest_date_data, aes(Age_median, Deaths_per_100k_citizen)) +
  geom_point(aes(color = factor(IncomeGroup))) +
  geom_smooth(method ="lm",aes(color = IncomeGroup),se=F) +
  facet_wrap(IncomeGroup ~ .,scale="free_x")+
  theme_minimal()+
  labs(y="Deaths per 100k", x = "Median Age", color = "Income Group")

model_data %>%
  filter(term!="(Intercept)") %>%
  ggplot(aes(x=estimate,
             y=IncomeGroup))+
  geom_vline(xintercept = 0, linetype="dashed")+
  geom_point()+
  geom_errorbarh(aes(xmin=conf.low,xmax=conf.high))
  
library(ggplot2)
  td <- tidy(fit, conf.int = TRUE)
  ggplot(td, aes(estimate, term, color = term)) +
    geom_point() +
    geom_errorbarh(aes(xmin = conf.low, xmax = conf.high)) +
    geom_vline()

## second GLM model:

model_data2 <- 
  latest_date_data %>% 
  select(Region, 
         Confirmed_per_100k_citizen, 
         Gdp,
         Inequality,
         Urban_pop_perct,
         Pop_density) %>% 
  group_by(Region) %>% 
  nest() %>% 
  ungroup() %>%  
  mutate(mdl = purrr::map(data,
                          ~glm(Confirmed_per_100k_citizen ~ Gdp+Inequality+Urban_pop_perct+Pop_density, 
                               data = .x,
                               family = gaussian()
                          ))) 

model_data2 <-
  model_data2 %>% 
  mutate(mdl_tidy = purrr::map(mdl, tidy, conf.int = TRUE)) %>% 
  unnest(mdl_tidy)

# all the data 
model_data2 %>%
  filter(term!="(Intercept)") %>%
  ggplot(aes(x = estimate,y = Region, color = term)) +
  geom_point() +
  geom_errorbarh(aes(xmin = conf.low,
                     xmax = conf.high)) +
  geom_vline(xintercept = 0, linetype="dashed")

# Only the sig ones. 
model_data2 %>%
  filter(term != "(Intercept)",
         p.value < 0.05) %>%
  ggplot(aes(x = estimate, y = Region, color = term)) +
  geom_point() +
  geom_errorbarh(aes(xmin = conf.low,
                     xmax = conf.high)) +
  geom_vline(xintercept = 0, 
             linetype="dashed")+
  theme_minimal()






  


