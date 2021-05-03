# This script is for Joen to do EDA

rm(list=ls(all=TRUE))
# Load Libraries ----------------------------------------------------------

library("tidyverse")
library("tidyquant")

augmented_timeseries <- read_csv("data/03_augmented_timeseries.csv")

augmented_timeseries <- augmented_timeseries %>% 
  group_by(`Country/Region`) %>% 
  arrange(Date) %>% 
  mutate(New_confirmed = Confirmed - lag(Confirmed, n = 1),
         New_deaths = Deaths - lag(Deaths, n = 1),
         New_recovered = Recovered - lag(Recovered, n = 1),
         Case_fatality = Deaths/Confirmed)



augmented_timeseries <- augmented_timeseries %>% 
  tq_mutate(
    select     = New_confirmed,
    mutate_fun = rollapply, 
    width      = 14,
    FUN        = mean,
    na.rm      = TRUE,
    col_rename = "Rolling_mean_confirmed") %>% 
  tq_mutate(
    select     = New_deaths,
    mutate_fun = rollapply, 
    width      = 14,
    FUN        = mean,
    na.rm      = TRUE,
    col_rename = "Rolling_mean_deaths") %>% 
  mutate(Rolling_case_fatality = Rolling_mean_deaths/Rolling_mean_confirmed) %>% 
  select(Date,Confirmed,New_confirmed,Rolling_mean_confirmed,Deaths,New_deaths,Rolling_mean_deaths,everything())


#finding waves
#Criteria for wave
#Deaths is at least 100 % higher than 4 weeks previously
increase_factor = 1.1 
no_of_days = 7
  
augmented_timeseries <- augmented_timeseries %>% 
  mutate(Wave_status = case_when(
    Rolling_mean_deaths < 1 ~ "Non_Wave",
    lead(Rolling_mean_deaths, n = no_of_days)/Rolling_mean_deaths >= increase_factor ~ "Wave",
    lead(Rolling_mean_deaths, n = no_of_days)/Rolling_mean_deaths < increase_factor ~ "Non_Wave"
    ),
    Wave_status = fct_recode(Wave_status)
    )

augmented_timeseries %>% 
  group_by(Date) %>%
  filter(Wave_status == "Wave") %>% 
  count() %>% 
  ggplot(aes(x = Date, y = n)) +
  geom_point()
  
### Plotting
### Select a country
augmented_timeseries2 <- augmented_timeseries %>% 
  filter(`Country/Region` == "India")

augmented_timeseries2 %>% 
  drop_na() %>% 
  ggplot(aes(x = Date, y = Rolling_mean_deaths, color = Wave_status))+
  geom_point()


augmented_timeseries2 %>% 
  ggplot(aes(x = Date))+
  geom_line(aes(y = New_confirmed),color = "blue")+
  geom_line(aes(y = New_deaths),color = "red")+
  geom_line(aes(y = New_recovered),color = "green")

augmented_timeseries2 %>% 
  ggplot(aes(x = Date,
             y = Case_fatality))+
  geom_point()


augmented_timeseries2 %>% 
  ggplot(aes(x = Date))+
  geom_point(aes(y = Case_fatality*100), color = "black") +
  geom_point(aes(y = Rolling_case_fatality*100), color = "red") +
  geom_point(aes(y = log10(New_confirmed)), color = "Green")

