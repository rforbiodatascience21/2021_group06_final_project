
rm(list=ls(all=TRUE))

# Load Libraries ----------------------------------------------------------

library("tidyverse")
source("R/99_functions.R")

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
  left_join(deaths_global_country, 
            by = c("Country/Region", "Date")) %>%
  left_join(recovered_global_country, 
            by = c("Country/Region", "Date"))


# Mutate date format
combined_timeseries_country <- combined_timeseries_country %>%
  mutate(Date = lubridate::mdy(Date))




# Write data --------------------------------------------------------------
combined_timeseries_country %>%
  write_csv("data/01_timeseries_country.csv")

# Alternate  way ------------------------------------------------------------


covid_data <- tribble(
  ~variable_name, ~file_path,
  "Confirmed",    "data/_raw/time_series_covid19_confirmed_global.csv",
  "Deaths",       "data/_raw/time_series_covid19_deaths_global.csv",
  "Recovered",    "data/_raw/time_series_covid19_recovered_global.csv"
)

#load data
covid_data <- covid_data %>% 
  mutate(raw_data = purrr::map(file_path, ~read_csv(.)))

#define a function for pivoting the data to a long format
pivot_data <- function(df, var_name){
  df %>%
    pivot_longer(cols = matches("\\d+/\\d+/\\d+"),
                 names_to = "Date",
                 values_to = var_name)
}

#pivot the data and join the pivoted dataframes
covid_data <- covid_data %>% 
  mutate(pivoted_data = purrr::map2(raw_data, variable_name, pivot_data)) %>% 
  pluck("pivoted_data") %>% 
  purrr::reduce(left_join, by = c("Province/State","Country/Region","Lat","Long","Date"))

#Sum up variable on country level and reformat date.
covid_data <- covid_data %>% 
  group_by(`Country/Region`, Date) %>%
  summarise(Confirmed = sum(Confirmed),
            Deaths = sum(Deaths),
            Recovered = sum(Recovered)) %>% 
  mutate(Date = lubridate::mdy(Date))
