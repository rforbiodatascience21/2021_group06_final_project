# In this script we are investigating the criteria for a Covid-19 wave.
# Further we take a look into case fatality ratios and investigate the best
# ways to calculate them

rm(list=ls(all=TRUE))
# Load Libraries ----------------------------------------------------------

library("tidyverse")
library("lubridate")
source("R/03_augment_data.R")

# Load Data ---------------------------------------------------------------

augmented_timeseries <- read_csv("data/03_augmented_timeseries.csv",
                                 col_types = cols(
                                   "Rolling_mean_confirmed" = col_double(),
                                   "Rolling_mean_deaths" = col_double(),
                                   "Rolling_case_fatality" = col_double(),
                                   "Wave_status" = col_character()))


# Wrangle Data ------------------------------------------------------------


#Creating a single country dataset, for illustrative purposes.
selected_country <-  "Denmark"

augmented_timeseries_single_country <- augmented_timeseries %>% 
  filter(`Country/Region` == selected_country)


# Plot data ---------------------------------------------------------------

# plotting the number of death colored by whether a country fulfill the wave
# criteria

country_wave_plot <- augmented_timeseries_single_country %>% 
  drop_na() %>% 
  ggplot(mapping = aes(x = Date,
                       y = Rolling_mean_deaths,
                       color = Wave_status))+
  geom_point()+
  scale_x_date(date_breaks = "1 month", 
               date_labels =  "%b %Y") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle=45, hjust = 1))+
  labs(title = str_c("Identifying waves in ", 
                     selected_country),
       subtitle = str_c("Waves are identified as ",
                        (increase_factor-1)*100,
                        "% growth over a period of ",
                        no_of_days,
                        " days"),
       x = "Date",
       y = "Daily number of confirmed deaths")

# plotting the mean (14-day mean) number of countries that actively have a wave
global_wave_trend_plot <- augmented_timeseries %>% 
  group_by(Date) %>%
  summarise(no_waves = sum(Wave_status == "Wave", na.rm = T),
            no_countries = n()) %>% 
  mutate(global_wave_percentage = no_waves/no_countries*100) %>% 
  ungroup() %>% 
  ggplot(mapping = aes(x = Date)) +
  geom_point(aes(y = global_wave_percentage,
                 color = "Percentage of contries in wave (%)"),
             alpha = 0.5)+
  scale_x_date(date_breaks = "1 month",
               date_labels =  "%b %Y") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle=45, 
                                   hjust = 1))+
  labs(title = "How many large a faction was in a wave at any given date",
       subtitle = "Percent of countries with an increase of 10% in confirmed cases over a 7 day period",
       x = "Date",
       y = "Number of countries with in a wave")

# plotting the mean (14-day mean) number of countries that actively have a wave by region
region_wave_trend_plot <- augmented_timeseries %>% 
  drop_na(Region) %>% 
  group_by(Date, Region) %>%
  summarise(no_waves = sum(Wave_status == "Wave", na.rm = T),
            no_countries = n()) %>% 
  mutate(region_wave_percentage = no_waves/no_countries*100) %>% 
  ggplot(mapping = aes(x = Date,
                       y = region_wave_percentage)) +
  geom_line(size = 1)+
  scale_x_date(date_breaks = "1 month", 
               date_labels =  "%b %Y") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle=45, hjust = 1))+
  facet_wrap(~Region)+
  labs(title = "How many countries in a region were in a wave at any given date",
       subtitle = "Percentage of countries in a reion with an increase of 10% in confirmed cases over a 7 day period",
       x="Date",
       y="Percentage of countries in region (%)")



country_case_fatality_plot <- augmented_timeseries_single_country %>% 
  ggplot(mapping = aes(x = Date,
                       y = Case_fatality * 100))+
  geom_point()+
  scale_x_date(date_breaks = "1 month", 
               date_labels =  "%b %Y") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle=45, hjust = 1))+
  labs(title = "Does the case-fatality change over time?",
       subtitle = str_c("Cummulative case fatility in ",
                        selected_country,
                        " over time"),
       x = "Date",
       y = "Case fatality (%)")

### ISSUE, want to have dual axis, one for case fatality and one for new_case
country_rolling_case_fatility_plot <- augmented_timeseries_single_country %>% 
  ggplot(aes(x = Date))+
  geom_point(aes(y = Case_fatality*100,
                 color = "Cummulative Case Fatility")) +
  geom_point(aes(y = Rolling_case_fatality*100,
                 color = "Rolling Case Fatility")) +
  geom_point(aes(y = log10(New_confirmed),
                 color = "Log10 of daily cases"))+
  scale_x_date(date_breaks = "1 month", 
               date_labels =  "%b %Y") +
  scale_y_continuous(
    name = "Case Fatality (%)",
    sec.axis = sec_axis(~.*1, name="Log10 of daily confirmed cases")
  )+ 
  theme_minimal()+
  theme(axis.text.x = element_text(angle=45, 
                                   hjust = 1),
        legend.position = "bottom")+
  labs(title = "Case fatality ratio spike right after a drop of confirmed cases",
       subtitle = str_c("Number of new confirmed cases and Case fatility rates over time in ",
                        selected_country),
       x = "Date")

country_shifted_case_fatility_plot <- augmented_timeseries_single_country %>% 
  mutate(lead7_case_fatality =
           lead(Rolling_mean_deaths, n= 7)/Rolling_mean_confirmed,
         lead14_case_fatality = 
           lead(Rolling_mean_deaths, n= 14)/Rolling_mean_confirmed,
         lead21_case_fatality = 
           lead(Rolling_mean_deaths, n= 21)/Rolling_mean_confirmed,
         lead28_case_fatality = 
           lead(Rolling_mean_deaths, n= 28)/Rolling_mean_confirmed
         ) %>% 
  ggplot(aes(x = Date))+
  geom_line(aes(y = Rolling_case_fatality*100,
                color = "Rolling Case Fatility"), alpha = 0.5) +
  geom_line(aes(y = lead7_case_fatality*100,
                color = "7 day lag Rolling Case Fatility")) +
  geom_line(aes(y = lead14_case_fatality*100,
                color = "14 day lag Rolling Case Fatility"),
            size = 1) +
  geom_line(aes(y = lead21_case_fatality*100,
                color = "21 day lag Rolling Case Fatility"),
            size = 1) +
  geom_line(aes(y = lead28_case_fatality*100,
                color = "28 day lag Rolling Case Fatility")) +
  scale_x_date(date_breaks = "1 month", 
               date_labels =  "%b %Y", 
               limits = c(as_date("2020-04-01"),as_date("2021-04-01"))) +
  scale_y_continuous(
    limits = c(0,10),
    name = "Case Fatality (%)")+ 
  theme_minimal()+
  theme(axis.text.x = element_text(angle=45, hjust = 1),
        legend.position = "bottom")+
  labs(title = "Case fatality ratio should be calculated with a 14-21 day delay",
       subtitle = str_c("Case fatality ratio in ",
                        selected_country,
                        " accounting for 7, 14 ,21 & days delay of death"),
       x = "Date")


# Write plots -------------------------------------------------------------

ggsave("results/07_global_wave_trend.png",
       plot = global_wave_trend_plot,
       height = 6,
       width = 12)
ggsave("results/07_region_wave_trend.png",
       plot = region_wave_trend_plot,
       height = 6,
       width = 12)
ggsave("results/07_country_wave_timeline.png",
       plot = country_wave_plot,
       height = 6,
       width = 12)
ggsave("results/07_country_case_fatality.png",
       plot = country_case_fatality_plot,
       height = 6,
       width = 12)
ggsave("results/07_country_rolling_case_fatality.png",
       plot = country_rolling_case_fatility_plot,
       height = 6,
       width = 12)
ggsave("results/07_country_shifted_case_fatality.png",
       plot = country_shifted_case_fatility_plot,
       height = 6,
       width = 12)


