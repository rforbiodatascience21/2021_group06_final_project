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
  theme_minimal()+
  labs(y = "Deaths per 100k",
       x = "Population % > 65 yrs",
       color = "Income Group",
       title = "Linear regression relationship between deaths per 100k and Pop. % > 65yrs")


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
  theme_minimal()+
  labs(y="Deaths per 100k", x = "Population % living in Urban ", color = "Income Group")+ 
  ggtitle("Linear regression relationship between deaths per 100k and pop. % living in urban") 

model_data %>%
  filter(term!="(Intercept)") %>%
  ggplot(aes(x = estimate, y = IncomeGroup, color = term)) +
  geom_vline(xintercept = 0, linetype="dashed")+
  geom_point()+
  geom_errorbarh(aes(xmin=conf.low,xmax=conf.high))+
  theme_minimal()+
  labs(y="", x = " Slope Estimate", color = "Indep. Variable")+
  scale_color_manual(labels = c("Pop. % > 65", "Urban Pop. %"), values = c("deepskyblue1", "tomato2"))+ 
  ggtitle("Slope estimates for deaths per 100k grouped by income level")

model_data %>%
  filter(term != "(Intercept)",
         p.value < 0.05) %>%
  ggplot(aes(x = estimate, y = IncomeGroup, color = term)) +
  geom_point() +
  geom_errorbarh(aes(xmin = conf.low,
                     xmax = conf.high)) +
  geom_vline(xintercept = 0, 
             linetype="dashed")+
  theme_minimal()+
  labs(y="", x = "Slope Estimate", color = "Indep. Variable")+
  scale_color_manual(labels = c("Pop. % > 65", "Urban Pop. %"), values = c("deepskyblue1", "tomato2"))+ 
  ggtitle("Significant slope estimates for deaths per 100k grouped by income level")

  
## second GLM model:

model_data2 <- 
  latest_date_data %>% 
  group_by(Region) %>% 
  nest() %>% 
  ungroup() %>%  
  mutate(mdl = purrr::map(data,
                          ~lm(Confirmed_per_100k_citizen ~ Gdp+Pop_density, 
                               data = .x
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
  geom_vline(xintercept = 0, linetype="dashed")+
  labs(y="", x = "Slope Estimate", color = "Indep. Variable")+
  scale_color_manual(labels = c("Gdp", "Pop Density"), values = c("deepskyblue1", "tomato2"))+ 
  ggtitle("Slope estimates for cases per 100k grouped by region")


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
  theme_minimal()+
  labs(y="", x = "Slope Estimate", color = "Indep. Variable")+
  scale_color_manual(labels = c("Gdp", "Pop Density"), values = c("deepskyblue1", "tomato2"))+ 
  ggtitle("Significant slope estimates for cases per 100k grouped by region")

latest_date_data %>%
ggplot( aes(Gdp, Confirmed_per_100k_citizen)) +
  geom_point(aes(color = factor(Region))) +
  geom_smooth(method ="lm",aes(color = Region),se=F) +
  facet_wrap(Region ~ .,scale="free_x")+
  theme_minimal()

latest_date_data %>%
  ggplot( aes(Pop_density, Confirmed_per_100k_citizen)) +
  geom_point(aes(color = factor(Region))) +
  geom_smooth(method ="lm",aes(color = Region),se=F) +
  facet_wrap(Region ~ .,scale="free_x")+
  theme_minimal()



  


