
rm(list=ls(all=TRUE))

# Load Libraries ----------------------------------------------------------

library(tidyverse)



# Load data ---------------------------------------------------------------

confirmed_global <- read_csv("data/_raw/time_series_covid19_confirmed_global.csv")
deaths_global <- read_csv("data/_raw/time_series_covid19_deaths_global.csv")
recovered_global <- read_csv("data/_raw/time_series_covid19_recovered_global.csv")



# Wrangle data ------------------------------------------------------------

# Pivot data
confirmed_global <- confirmed_global %>%
  pivot_longer(cols = matches("\\d+/\\d+/\\d+"),
               names_to = "Date",
               values_to = "Confirmed")

deaths_global <- deaths_global %>%
  pivot_longer(cols = matches("\\d+/\\d+/\\d+"),
               names_to = "Date",
               values_to = "Deaths")

recovered_global <- recovered_global %>%
  pivot_longer(cols = matches("\\d+/\\d+/\\d+"),
               names_to = "Date",
               values_to = "Recovered")

# Generate counts only on country basis (sum Province/state)
confirmed_global_country <- confirmed_global %>%
  group_by(`Country/Region`, Date) %>%
  summarise(Confirmed = sum(Confirmed))

deaths_global_country <- deaths_global %>%
  group_by(`Country/Region`, Date) %>%
  summarise(Deaths = sum(Deaths))

recovered_global_country <- recovered_global %>%
  group_by(`Country/Region`, Date) %>%
  summarise(Recovered = sum(Recovered))

# Join time series for countries
combined_timeseries_country <- confirmed_global_country %>%
  left_join(deaths_global_country, by = c("Country/Region", "Date")) %>%
  left_join(recovered_global_country, by = c("Country/Region", "Date"))

# Join time series on province level
combined_timeseries_province <- confirmed_global %>%
  left_join(deaths_global, by = c("Country/Region", "Date", "Province/State")) %>%
  left_join(recovered_global, by = c("Country/Region", "Date", "Province/State")) %>%
  select(-ends_with(c('.x', '.y')))

# Mutate date format
combined_timeseries_country <- combined_timeseries_country %>%
  mutate(Date = lubridate::mdy(Date))

combined_timeseries_province <- combined_timeseries_province %>%
  mutate(Date = lubridate::mdy(Date))

# Pivot longer to make a "Status variable"
combined_timeseries_country <- combined_timeseries_country %>%
  pivot_longer(cols = c(Confirmed, Deaths, Recovered),
               names_to = "Status",
               values_to = "Cases")

combined_timeseries_province <- combined_timeseries_province %>%
  pivot_longer(cols = c(Confirmed, Deaths, Recovered),
               names_to = "Status",
               values_to = "Cases")

# Write data --------------------------------------------------------------
combined_timeseries_country %>%
  write_csv("data/01_timeseries_country.csv")

combined_timeseries_province %>%
  write_csv("data/01_timeseries_province.csv")
