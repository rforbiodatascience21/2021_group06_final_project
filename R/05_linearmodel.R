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
         `Pop%_above65`,
         Urban_pop_perct) %>% 
  group_by(IncomeGroup) %>% 
  nest() %>% 
  ungroup() %>%  
  mutate(mdl = purrr::map(data,
                   ~lm(Deaths_per_100k_citizen ~ `Pop%_above65`+Urban_pop_perct, 
                       data = .x
                       ))) 


model_data <-
  model_data %>% 
  mutate(mdl_tidy = purrr::map(mdl, tidy, conf.int = TRUE)) %>% 
  unnest(mdl_tidy)

model_data  %>%
  filter(term != "(Intercept)",
         p.value < 0.05)

# Make a label dataframe for the facets: 
Sig.DF <- data.frame(IncomeGroup = c("High income", 
                                "Upper middle income", 
                                "Lower middle income", 
                                "Low income"), 
                     label = c('p<0.05','p<0.05','p<0.05', 'p>0.05'),
                     x = c(5,5,5,2.5))

latest_date_data %>%
  mutate(IncomeGroup = fct_relevel(IncomeGroup, 
                                   c("High income", 
                                     "Upper middle income", 
                                     "Lower middle income", 
                                     "Low income"))) %>%
ggplot( aes(`Pop%_above65`, Deaths_per_100k_citizen)) +
  geom_point(aes(color = factor(IncomeGroup))) +
  geom_smooth(method ="lm",aes(color = IncomeGroup),se=F) +
  facet_wrap(IncomeGroup ~ .,scale="free_x")+
  geom_text(y = 225, aes(x=x, label = label), data = Sig.DF)+
  theme_minimal()+
  labs(y="Deaths per 100k", x = "Population % > 65 yrs", color = "Income Group") 

# Make a label dataframe for the facets: 
Sig.DF2 <- data.frame(IncomeGroup = c("High income", 
                                     "Upper middle income", 
                                     "Lower middle income", 
                                     "Low income"), 
                     label = c('p<0.05','p<0.05','p<0.05', 'p>0.05'))

latest_date_data %>%
  mutate(IncomeGroup = fct_relevel(IncomeGroup, 
                                   c("High income", 
                                     "Upper middle income", 
                                     "Lower middle income", 
                                     "Low income"))) %>%
latest_date_data %>%
  mutate(IncomeGroup = fct_relevel(IncomeGroup, 
                                   c("High income", 
                                     "Upper middle income", 
                                     "Lower middle income", 
                                     "Low income"))) %>%
ggplot(aes(`Urban_pop_perct`, Deaths_per_100k_citizen)) +
  geom_point(aes(color = factor(IncomeGroup))) +
  geom_smooth(method ="lm",aes(color = IncomeGroup),se=F) +
  facet_wrap(IncomeGroup ~ .,scale="free_x")+
  geom_text(x = 4, y = 225, aes(label = label), data = Sig.DF)+
  theme_minimal()+
  labs(y="Deaths per 100k", x = "Population % living in Urban ", color = "Income Group")

model_data %>%
  filter(term!="(Intercept)") %>%
  ggplot(aes(x=estimate,
             y=IncomeGroup))+
  geom_vline(xintercept = 0, linetype="dashed")+
  geom_point()+
  geom_errorbarh(aes(xmin=conf.low,xmax=conf.high))
  

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






  


