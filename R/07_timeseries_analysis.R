rm(list = ls(all = TRUE))

# Load Libraries ----------------------------------------------------------

library("tidyverse")
library("lubridate")

# Load Data ---------------------------------------------------------------

timeseries_data <- read_csv("data/03_augmented_timeseries.csv",
                            col_types = cols(
                              "Rolling_mean_confirmed" = col_double(),
                              "Rolling_mean_deaths"    = col_double(),
                              "Rolling_case_fatality"  = col_double(),
                              "Wave_status"            = col_character()))

# Wrangle Data ------------------------------------------------------------


# Creating a single country dataset, for illustrative purposes.
selected_country <- "Denmark"

timeseries_data_single_country <- timeseries_data %>% 
  filter(Country == selected_country)


# Plot data ---------------------------------------------------------------

# plotting the number of death colored by whether a country fulfill the wave
country_wave_plot <- timeseries_data_single_country %>% 
  drop_na(Wave_status) %>% 
  ggplot(mapping = aes(x = Date,
                       y = Rolling_mean_deaths,
                       color = Wave_status)) +
  geom_point() +
  scale_x_date(date_breaks = "1 month", 
               date_labels =  "%b %Y") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, 
                                   hjust = 1),
        axis.title.x = element_blank()) +
  labs(title = str_c("Identifying waves in ", selected_country),
       subtitle = "Waves are identified as 10 % increase in deaths over a period of 7 days",
       x = "Date",
       y = "Daily number of confirmed deaths",
       color = "Wave Status")

# Plotting the mean (14-day mean) number of countries that actively have a wave
global_wave_trend_plot <- timeseries_data %>% 
  drop_na(Wave_status) %>%
  group_by(Date) %>% 
  summarise(Global_wave_percentage = 
              sum(Wave_status == "Wave", na.rm = T) / n()) %>%
  ggplot(mapping = aes(x = Date,
                       y = Global_wave_percentage)) +
  geom_point(alpha = 0.5) +
  scale_x_date(date_breaks = "1 month",
               date_labels =  "%b %Y") +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, 
                                   hjust = 1),
        axis.title.x = element_blank()) +
  labs(title = "Do Covid-waves occur at the same time accross the world?",
       subtitle = "Percentage of world countries with a 10% increase in deaths over a 7 day period",
       x = "Date",
       y = "Percentage of countries in a wave")

# Plotting the mean (14-day mean) number of countries
# that actively have a wave by region
region_wave_trend_plot <- timeseries_data %>% 
  drop_na(Region, Wave_status) %>% 
  group_by(Region, Date) %>% 
  summarise(Region_wave_percentage = 
              sum(Wave_status == "Wave", na.rm = T)/n()) %>%
  ggplot(mapping = aes(x = Date,
                       y = Region_wave_percentage)) +
  geom_line(size = 1) +
  scale_x_date(date_breaks = "2 month", 
               date_labels =  "%b %Y") +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, 
                                   hjust = 1),
        axis.title.x = element_blank()) +
  facet_wrap(~Region) +
  labs(title = "Do Covid-waves occur at the same time in a region?",
       subtitle = "Percentage of countries in a region with a 10% increase in deaths cases over a 7 day period",
       y = "Percentage of countries in region in a wave")


# Plotting case fatality timeseries for selected country
country_case_fatality_plot <- timeseries_data_single_country %>% 
  ggplot(mapping = aes(x = Date,
                       y = Case_fatality)) +
  geom_point() +
  scale_x_date(date_breaks = "1 month", 
               date_labels =  "%b %Y") +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, 
                                   hjust = 1),
        axis.title.x = element_blank()) +
  labs(title = "Does the case-fatality change over time?",
       subtitle = str_c("Cumulative case fatility in ",
                        selected_country,
                        " over time"),
       y = "Case fatality")

# Plotting both cumulative and rolling case fatality over time
country_rolling_case_fatility_plot <- timeseries_data_single_country %>% 
  ggplot(mapping = aes(x = Date)) +
  geom_point(mapping = aes(y = Case_fatality,
                           color = "Cumulative Case Fatility")) +
  geom_point(mapping = aes(y = Rolling_case_fatality,
                           color = "Rolling Case Fatility")) +
  scale_x_date(date_breaks = "1 month", 
               date_labels =  "%b %Y") +
  scale_y_continuous(name = "Case fatality", 
                     labels = scales::percent_format()) + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, 
                                   hjust = 1),
        legend.position = "bottom",
        axis.title.x = element_blank(),
        legend.title = element_blank()) +
  labs(title = "Case fatality spikes in brief periods",
       subtitle = str_c("Cumulative case fatality and 14-day rolling mean case-fatality in",
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

