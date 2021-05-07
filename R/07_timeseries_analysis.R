# This script is for Joen to do EDA

rm(list=ls(all=TRUE))
# Load Libraries ----------------------------------------------------------

library("tidyverse")

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
  theme(axis.text.x = element_text(angle=45, hjust = 1))+
  labs(title = "How many countries were in a wave at any given date",
       subtitle = "Number of countries with an increase of 10% in confirmed cases over a 7 day period",
       x="Date",
       y="Number of countries")

# plotting the mean (14-day mean) number of countries that actively have a wave by region
augmented_timeseries %>% 
  drop_na(Region) %>% 
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
  facet_wrap(~Region)+
  labs(title = "How many countries in a region were in a wave at any given date",
       subtitle = "Number of countries with an increase of 10% in confirmed cases over a 7 day period",
       x="Date",
       y="Number of countries")

### select a country
selected_country <-  "Germany"

augmented_timeseries_single_country <- augmented_timeseries %>% 
  filter(`Country/Region` == selected_country)

augmented_timeseries_single_country %>% 
  drop_na() %>% 
  ggplot(aes(x = Date, y = Rolling_mean_deaths, color = Wave_status))+
  geom_point()+
  scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle=45, hjust = 1))+
  labs(title = str_c("Identifying waves in ",selected_country),
       subtitle = str_c("Waves are identified as ",
                        (increase_factor-1)*100,
                        "% growth over a period of ",
                        no_of_days,
                        " days"),
       x="Date",y="Daily number of confirmed deaths")

augmented_timeseries_single_country %>% 
  ggplot(aes(x = Date,
             y = Case_fatality*100))+
  geom_point()+
  scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle=45, hjust = 1))+
  labs(title = "Does the case-fatality change over time?",
       subtitle = str_c("Cummulative case fatility in ",
                        selected_country,
                        " over time")
       ,x="Date",
       y="Case fatality (%)")

### ISSUE, want to have dual axis, one for case fatality and one for new_case
augmented_timeseries_single_country %>% 
  ggplot(aes(x = Date))+
  geom_point(aes(y = Case_fatality*100, color = "Cummulative Case Fatility")) +
  geom_point(aes(y = Rolling_case_fatality*100, color = "Rolling Case Fatility")) +
  geom_point(aes(y = log10(New_confirmed),color = "Log10 of daily cases"))+
  scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y") +
  scale_y_continuous(
    name = "Case Fatality (%)",
    sec.axis = sec_axis(~.*1, name="Log10 of daily confirmed cases")
  )+ 
  theme_minimal()+
  theme(axis.text.x = element_text(angle=45, hjust = 1),legend.position = "bottom")+
  labs(title = "Case fatality ratio spike right after a drop of confirmed cases",
       subtitle = str_c("Number of new confirmed cases and Case fatility rates over time in ",
                        selected_country),
       x = "Date")

