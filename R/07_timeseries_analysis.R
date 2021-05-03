# This script is for Joen to do EDA

rm(list=ls(all=TRUE))
# Load Libraries ----------------------------------------------------------

library("tidyverse")
library("tidyquant")

augmented_timeseries <- read_csv("data/03_augmented_timeseries.csv")

#Calculating daily cases & deaths from cumsums. further calculation of 14 day means.
augmented_timeseries <- augmented_timeseries %>% 
  group_by(`Country/Region`) %>% 
  arrange(Date) %>% 
  mutate(New_confirmed = Confirmed - lag(Confirmed, n = 1),
         New_deaths = Deaths - lag(Deaths, n = 1),
         New_recovered = Recovered - lag(Recovered, n = 1),
         Case_fatality = Deaths/Confirmed,
         Rolling_mean_confirmed = (lead(Confirmed, n = 7) - lag(Confirmed, n = 7))/14,
         Rolling_mean_deaths = (lead(Deaths, n = 7) - lag(Deaths, n = 7))/14,
         Rolling_case_fatality = Rolling_mean_deaths/Rolling_mean_confirmed)


#finding waves
#Criteria for wave
#Deaths is at least 10 % higher than 1 weeks previously
increase_factor = 1.1 
no_of_days = 7
  
#adding a "wave" factor to the data
augmented_timeseries <- augmented_timeseries %>% 
  mutate(Wave_status = case_when(
    Rolling_mean_deaths < 1 ~ "Non_Wave",
    lead(Rolling_mean_deaths, n = no_of_days)/Rolling_mean_deaths >= increase_factor ~ "Wave",
    lead(Rolling_mean_deaths, n = no_of_days)/Rolling_mean_deaths < increase_factor ~ "Non_Wave"
    ),
    Wave_status = fct_recode(Wave_status)
    )


### Plotting
# plotting the mean (14-day mean) number of countries that actively have a wave
augmented_timeseries %>% 
  group_by(Date) %>%
  filter(Wave_status == "Wave") %>% 
  count(name = "no_waves")%>%
  ungroup() %>% 
  mutate(cumsum_no_waves = cumsum(no_waves),
         mean_waves = (lead(cumsum_no_waves, n = 7)-lag(cumsum_no_waves, n = 7))/14)%>% 
  ggplot(aes(x = Date, y = mean_waves)) +
  geom_point()+
  scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle=45, hjust = 1))

# plotting the mean (14-day mean) number of countries that actively have a wave by region
augmented_timeseries %>% 
  group_by(Date,Region) %>%
  filter(Wave_status == "Wave") %>% 
  count(name = "no_waves")%>% 
  group_by(Region) %>% 
  arrange(Date) %>% 
  mutate(cumsum_no_waves = cumsum(no_waves),
         mean_waves = (lead(cumsum_no_waves, n = 7)-lag(cumsum_no_waves, n = 7))/14)%>% 
  ggplot(aes(x = Date, y = mean_waves)) +
  geom_point()+
  scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle=45, hjust = 1))+
  facet_wrap(~Region)

augmented_timeseries2 <- augmented_timeseries %>% 
  filter(`Country/Region` == "Denmark")

augmented_timeseries2 %>% 
  drop_na() %>% 
  ggplot(aes(x = Date, y = Rolling_mean_deaths, color = Wave_status))+
  geom_point()+
  scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle=45, hjust = 1))

augmented_timeseries2 %>% 
  ggplot(aes(x = Date))+
  geom_line(aes(y = New_confirmed),color = "blue")+
  geom_line(aes(y = New_deaths),color = "red")+
  geom_line(aes(y = New_recovered),color = "green")+
  scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y")+
  scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle=45, hjust = 1))

augmented_timeseries2 %>% 
  ggplot(aes(x = Date,
             y = Case_fatality))+
  geom_point()+
  scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle=45, hjust = 1))


augmented_timeseries2 %>% 
  ggplot(aes(x = Date))+
  geom_point(aes(y = Case_fatality*100), color = "black") +
  geom_point(aes(y = Rolling_case_fatality*100), color = "red") +
  geom_point(aes(y = log10(New_confirmed)), color = "Green")+
  scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle=45, hjust = 1))


