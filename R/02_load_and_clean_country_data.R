rm(list=ls(all=TRUE))


# Load Libraries ----------------------------------------------------------

library(tidyverse)

# Load Data ---------------------------------------------------------------

population <- read_csv("data/_raw/population_total.csv")
pop_density <- read_csv("data/_raw/population_density_per_square_km.csv")
age <- read_csv("data/_raw/median_age_years.csv")
gdp <- read_csv("data/_raw/gdp_per_capita.csv")
sex_ratio <- read_csv("data/_raw/sex_ratio_all_age_groups.csv")
inequality <- read_csv("data/_raw/gini.csv")



# Wrangle Data ------------------------------------------------------------

# Extract the features that we are interested in
population <- population %>%
  select(country, "2020") %>%
  rename("Population" = "2020")

pop_density <- pop_density %>%
  select(country, "2020") %>%
  rename("Pop_density" = "2020")

age <- age %>%
  select(country, "2020") %>%
  rename("Age_median" = "2020") 

gdp <- gdp %>%
  select(country, "2020") %>%
  rename("Gdp" = "2020")

sex_ratio <- sex_ratio %>%
  select(country, "2020") %>%
  rename("Sex_ratio" = "2020")

inequality <- inequality %>%
  select(country, "2020") %>%
  rename("Inequality" = "2020")

# Join the data
combined_tibble <- population %>%
  full_join(pop_density, by = "country") %>%
  full_join(age, by = "country") %>%
  full_join(gdp, by = "country") %>%
  full_join(sex_ratio, by = "country") %>%
  full_join(inequality, by = "country")


# Write Data --------------------------------------------------------------
combined_tibble %>%
  write_csv("data/02_country_data.csv")
