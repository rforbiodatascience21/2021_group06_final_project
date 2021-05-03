rm(list=ls(all=TRUE)) 

# Load Libraries ----------------------------------------------------------
library("tidyverse")
source("R/99_functions.R")
library("stringr")
library("purrr")
library("broom")
library("patchwork")

# Load Data ---------------------------------------------------------------

timeseries_data <- read_csv("data/03_augmented_timeseries.csv")

# Wrangle data ------------------------------------------------------------

# Subset to latest date

latest_date_data <- get_latest_date_data(timeseries_data) %>%
  drop_na() %>%
  mutate(IncomeGroup = fct_relevel(IncomeGroup, 
                                   c("High income", 
                                     "Upper middle income", 
                                     "Lower middle income", 
                                     "Low income"))) 

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

## Plot results ------------------------------------------------------------
  
# scatter plots
lm1_p1 <- 
  latest_date_data%>%
  ggplot( aes(`Pop%_above65`, Deaths_per_100k_citizen)) +
  geom_point(aes(color = factor(IncomeGroup))) +
  geom_smooth(method ="lm",aes(color = IncomeGroup),se=F) +
  facet_wrap(IncomeGroup ~ .,scale="free_x")+
  theme_minimal()+
  labs(y = "Deaths per 100k",
       x = "Population % > 65 yrs",
       color = "Income Group") 

lm1_p2 <- 
  latest_date_data %>%
  ggplot(aes(`Urban_pop_perct`, Deaths_per_100k_citizen)) +
  geom_point(aes(color = factor(IncomeGroup))) +
  geom_smooth(method ="lm",aes(color = IncomeGroup),se=F) +
  facet_wrap(IncomeGroup ~ .,scale="free_x")+
  theme_minimal()+
  labs(y="", x = "Population % living in urban ", color = "Income Group")

lm1_Final_line_plot <- 
      lm1_p1 + lm1_p2 + 
      plot_annotation(title = 'Simple Linear regression model results for deaths per 100k',
                      tag_levels = 'A',
                      theme = theme(plot.title = element_text(hjust = 0.5)))+
      plot_layout(guides = "collect") &
      theme(legend.position = 'bottom')

lm1_Final_line_plot

# Slope Estimate plots

Est_plot_lm1 <- 
model_data %>%
  filter(term!="(Intercept)") %>%
  ggplot(aes(x = estimate, y = IncomeGroup, color = term)) +
  geom_vline(xintercept = 0, linetype="dashed")+
  geom_point()+
  geom_errorbarh(aes(xmin=conf.low,xmax=conf.high))+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(y="", x = " Slope Estimate", color = "Indep. Variable",
       caption = "* indicates significant p-value < 0.05")+
  scale_color_manual(labels = c("Pop. % > 65", "Urban Pop. %"), values = c("deepskyblue1", "tomato2"))+ 
  ggtitle("Slope estimates for deaths per 100k grouped by income level") + 
  annotate("text", 
           x = c(6.3, 6.6, 1, 5), 
           y = c(3.2, 2.2, 2.2, 1.2), 
           label = "*"
           )

Est_plot_lm1

Est_plot_lm1_sig <-
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
  theme(plot.title = element_text(hjust = 0.5))+
  labs(y="", x = "Slope Estimate", color = "Indep. Variable")+
  scale_color_manual(labels = c("Pop. % > 65", "Urban Pop. %"), values = c("deepskyblue1", "tomato2"))+ 
  ggtitle("Significant slope estimates for deaths per 100k grouped by income level")

Est_plot_lm1_sig
  
## second GLM model -----------------------------------------------------------

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

## Plot results ------------------------------------------------------------

# scatter plots
lm2_p1 <- 
  latest_date_data %>%
  ggplot( aes(Gdp, Confirmed_per_100k_citizen)) +
  geom_point(aes(color = factor(Region))) +
  geom_smooth(method ="lm",aes(color = Region),se=F) +
  facet_wrap(Region ~ .,scale="free_x")+
  theme_minimal()+
  labs(y = "Cases per 100k",
       x = "GDP",
       color = "Region") 

lm2_p2 <- 
  latest_date_data %>%
  ggplot( aes(Pop_density, Confirmed_per_100k_citizen)) +
  geom_point(aes(color = factor(Region))) +
  geom_smooth(method ="lm",aes(color = Region),se=F) +
  facet_wrap(Region ~ .,scale="free_x")+
  theme_minimal()+
  labs(y = "Cases per 100k",
       x = "Population Density",
       color = "Region") 

lm2_Final_line_plot <- 
  lm2_p1 / lm2_p2 + 
  plot_annotation(title = 'Simple Linear regression model results for cases per 100k',
                  tag_levels = 'A',
                  theme = theme(plot.title = element_text(hjust = 0.5)))+
  plot_layout(guides = "collect") &
  theme(legend.position = 'bottom')

lm2_Final_line_plot

# Slope Estimate plots

# all the data 
Est_plot_lm2 <- 
  model_data2 %>%
  filter(term!="(Intercept)") %>%
  ggplot(aes(x = estimate,y = Region, color = term)) +
  geom_point() +
  geom_errorbarh(aes(xmin = conf.low,
                     xmax = conf.high)) +
  geom_vline(xintercept = 0, linetype="dashed")+
  theme_minimal()+
  labs(y="", x = "Slope Estimate", color = "Indep. Variable")+
  scale_color_manual(labels = c("Gdp", "Pop Density"), values = c("deepskyblue1", "tomato2"))+ 
  ggtitle("Slope estimates for cases per 100k grouped by region")+
  annotate("text", 
           x = c(1, 2.95, -7, 1), 
           y = c(5, 4.2, 1.2, 1), 
           label = "*"
  )

Est_plot_lm2

# Only the sig ones. 
Est_plot_lm2_sig <- 
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

Est_plot_lm2_sig


