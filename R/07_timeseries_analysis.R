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
selected_country <- "Denmark"

augmented_timeseries_single_country <- augmented_timeseries %>% 
  filter(`Country/Region` == selected_country)


# Plot data ---------------------------------------------------------------

# plotting the number of death colored by whether a country fulfill the wave
# criteria

country_wave_plot <- augmented_timeseries_single_country %>% 
  drop_na(Wave_status) %>% 
  ggplot(mapping = aes(x = Date,
                       y = Rolling_mean_deaths,
                       color = Wave_status))+
  geom_point()+
  scale_x_date(date_breaks = "1 month", 
               date_labels =  "%b %Y") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  labs(title = str_c("Identifying waves in ", 
                     selected_country),
       subtitle = str_c("Waves are identified as ",
                        (increase_factor-1) * 100,
                        "% growth over a period of ",
                        no_of_days,
                        " days"),
       x = "Date",
       y = "Daily number of confirmed deaths")

# plotting the mean (14-day mean) number of countries that actively have a wave

global_wave_trend_plot <- augmented_timeseries %>% 
  drop_na(Wave_status) %>%
  count(Wave_status, Date, 
        name = "Counts") %>%
  pivot_wider(id_cols = Date,
              names_from = Wave_status,
              values_from = Counts) %>% 
  mutate(global_wave_percentage = Wave / (Non_Wave + Wave)) %>%
  ggplot(mapping = aes(x = Date,
                       y = global_wave_percentage)) +
  geom_point(alpha = 0.5)+
  scale_x_date(date_breaks = "1 month",
               date_labels =  "%b %Y") +
  scale_y_continuous(labels = scales::percent_format())+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, 
                                   hjust = 1))+
  labs(title = "How many large a faction was in a wave at any given date",
       subtitle = "Percent of countries with an increase of 10% in confirmed cases over a 7 day period",
       x = "Date",
       y = "Percentage of countries with in a wave")

# plotting the mean (14-day mean) number of countries that actively have a wave by region
region_wave_trend_plot <- augmented_timeseries %>% 
  drop_na(Region, Wave_status) %>% 
  count(Wave_status, Date, Region) %>%
  pivot_wider(id_cols = c(Date, Region),
              names_from = Wave_status,
              values_from = n) %>%
  mutate(region_wave_percentage = Wave / (Non_Wave + Wave)) %>%
  ggplot(mapping = aes(x = Date,
                       y = region_wave_percentage)) +
  geom_line(size = 1)+
  scale_x_date(date_breaks = "2 month", 
               date_labels =  "%b %Y") +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank())+
  facet_wrap(~Region)+
  labs(title = "How many countries in a region were in a wave at any given date",
       subtitle = "Percentage of countries in a reion with an increase of 10% in confirmed cases over a 7 day period",
       y="Percentage of countries in region")



country_case_fatality_plot <- augmented_timeseries_single_country %>% 
  ggplot(mapping = aes(x = Date,
                       y = Case_fatality))+
  geom_point()+
  scale_x_date(date_breaks = "1 month", 
               date_labels =  "%b %Y") +
  scale_y_continuous(labels = scales::percent_format())+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank())+
  labs(title = "Does the case-fatality change over time?",
       subtitle = str_c("Cummulative case fatility in ",
                        selected_country,
                        " over time"),
       y = "Case fatality")


country_rolling_case_fatility_plot <- augmented_timeseries_single_country %>% 
  ggplot(mapping = aes(x = Date))+
  geom_point(mapping = aes(y = Case_fatality * 100,
                           color = "Cummulative Case Fatility")) +
  geom_point(mapping = aes(y = Rolling_case_fatality * 100,
                           color = "Rolling Case Fatility")) +
  scale_x_date(date_breaks = "1 month", 
               date_labels =  "%b %Y") +
  scale_y_continuous(
    name = "Case Fatality", labels = scales::percent_format(scale = 1)
  )+ 
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, 
                                   hjust = 1),
        legend.position = "bottom",
        axis.title.x = element_blank(),
        legend.title = element_blank())+
  labs(title = "Case fatality ratio spike right after a drop of confirmed cases",
       subtitle = str_c("Number of new confirmed cases and Case fatility rates over time in ",
                        selected_country))


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
