rm(list=ls(all=TRUE))


# Load Libraries ----------------------------------------------------------

library("tidyverse")
library("maps")
library("mapproj")
source("R/99_functions.R")

# Load Data ---------------------------------------------------------------

# data from gapminder
population <- read_csv("data/_raw/population_total.csv")
pop_density <- read_csv("data/_raw/population_density_per_square_km.csv")
age <- read_csv("data/_raw/median_age_years.csv")
gdp <- read_csv("data/_raw/gdp_per_capita.csv")
sex_ratio <- read_csv("data/_raw/sex_ratio_all_age_groups.csv")
inequality <- read_csv("data/_raw/gini.csv")

# Data from the world bank
Income_grp <- read_csv("data/_raw/Income_grp.csv")
Population_above65 <- read_csv("data/_raw/Population_65.csv")
Urban_pop_per <- read_csv("data/_raw/urban_pop_perct.csv")

world_map <- map_data("world") # data in R giving country Lat and Long. 


# Wrangle Data ------------------------------------------------------------

# Extract the features that we are interested in
  
population <- population %>%
  select(country, "2020") %>%
  rename(Population = "2020")

pop_density <- pop_density %>%
  select(country, "2020") %>%
  rename(Pop_density = "2020")

age <- age %>%
  select(country, "2020") %>%
  rename(Age_median = "2020") 

gdp <- gdp %>%
  select(country, "2020") %>%
  rename(Gdp = "2020")

sex_ratio <- sex_ratio %>%
  select(country, "2020") %>%
  rename(Sex_ratio = "2020")

inequality <- inequality %>%
  select(country, "2020") %>%
  rename(Inequality = "2020")

Income_grp <- Income_grp %>%
  select(Region, IncomeGroup, TableName) %>%
  rename(country = TableName) %>%
  mutate(Region = recode(Region,
      "South Asia" = "Asia & Pacific",
      "North America" = "Americas & Caribbean",
      "East Asia & Pacific" = "Asia & Pacific",
      "Latin America & Caribbean" = "Americas & Caribbean"))
    
Population_above65 <- 
  Population_above65 %>%
  select(`Country Name`, "2019") %>% #no data fr 2020, so most recent yr then
  rename(country = `Country Name`,
         `Pop%_above65` = "2019")

Urban_pop_per <-
  Urban_pop_per %>%
  select(`Country Name`,"2019") %>% #no data for 2020, so most recent yr then
  rename(country = `Country Name`,
         Urban_pop_perct = "2019")

# Join the data
combined_tibble <- population %>%
  full_join(pop_density, by = "country") %>%
  full_join(age, by = "country") %>%
  full_join(gdp, by = "country") %>%
  full_join(sex_ratio, by = "country") %>%
  full_join(inequality, by = "country") %>%
  
  # since this is from a diff data source...
  # only want countries with all the data, so left join
  left_join(Population_above65, by = "country") %>% 
  left_join(Income_grp, by = "country")  %>% 
  left_join(Urban_pop_per, by = "country")

# Fix discrepancies between country name in this data and timeseries

## Replacement
combined_tibble <- combined_tibble %>%
  mutate(country = recode(country,
      "Myanmar" = "Burma",
      "Cape Verde" = "Cabo Verde",
      "Congo, Rep."= "Congo (Brazzaville)",
      "Congo, Dem. Rep." = "Congo (Kinshasa)",
      "Czech Republic" = "Czechia",
      "South Korea" = "Korea, South",
      "Kyrgyz Republic" = "Kyrgyzstan",
      "Lao" = "Laos",
      "Micronesia, Fed. Sts." = "Micronesia",
      "St. Kitts and Nevis" = "Saint Kitts and Nevis",
      "St. Lucia" = "Saint Lucia",
      "St. Vincent and the Grenadines" = "Saint Vincent and the Grenadines",
      "Slovak Republic" = "Slovakia",
      "United States" = "US",
      "Palestine" = "West Bank and Gaza"))



# World map data ----------------------------------------------------------

world_map <- 
  world_map %>% 
  rename(country = region) %>%
  select(-subregion) %>% 
  mutate(country = recode(country, 
     "USA" = "US",
     "UK" = "United Kingdom",
     "Democratic Republic of the Congo" = "Congo (Kinshasa)",
     "Myanmar" = "Burma",
     "Cape Verde" = "Cabo Verde"))


# Write Data --------------------------------------------------------------
combined_tibble %>%
  write_csv("data/02_country_data.csv")

world_map %>%
  write_csv("data/02_world_map_data.csv")

