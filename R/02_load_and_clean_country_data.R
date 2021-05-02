rm(list=ls(all=TRUE))


# Load Libraries ----------------------------------------------------------

library("tidyverse")
library("maps")
library("mapproj")
source("R/99_functions.R")

# Load Data ---------------------------------------------------------------

# data from gapminder?
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

Income_grp <- Income_grp %>%
  select(Region,IncomeGroup,TableName) %>%
  rename(country = TableName) %>%
  mutate(Region = str_replace(Region, 
                               pattern = "South Asia", 
                               replacement = "Asia & Pacific"))%>%
  mutate(Region = str_replace(Region, 
                              pattern = "North America", 
                              replacement = "Americas & Caribbean")) %>%
  mutate(Region = str_replace(Region, 
                              pattern = "East Asia & Pacific", 
                              replacement = "Asia & Pacific"))%>%
  mutate(Region = str_replace(Region, 
                              pattern = "Latin America & Caribbean", 
                              replacement = "Americas & Caribbean"))
  
Population_above65 <- 
  Population_above65 %>%
  select("Country Name","2019") %>% #no data for 2020, so most recent yr then
  rename(country = "Country Name",
         "Pop%_above65" = "2019")

Urban_pop_per <-
  Urban_pop_per %>%
  select("Country Name","2019") %>% #no data for 2020, so most recent yr then
  rename(country = "Country Name",
         "Urban_pop_perct" = "2019")

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

combined_tibble <- combined_tibble %>%
  mutate(country = str_replace(country, 
                               pattern = "Myanmar", 
                               replacement = "Burma")) %>%
  
  mutate(country = str_replace(country,
                               pattern = "Cape Verde",
                               replacement = "Cabo Verde")) %>%
  
  mutate(country = str_replace(country, 
                               pattern = "Congo, Rep.", 
                               replacement = "Congo (Brazzaville)")) %>%
  
  mutate(country = str_replace(country, 
                               pattern = "Congo, Dem. Rep.",
                               replacement = "Congo (Kinshasa)")) %>%
  
  mutate(country = str_replace(country, 
                               pattern = "Czech Republic",
                               replacement = "Czechia")) %>%
  
  mutate(country = str_replace(country,
                               pattern = "South Korea",
                               replacement = "Korea, South")) %>%
  
  mutate(country = str_replace(country, 
                               pattern = "Kyrgyz Republic",
                               replacement = "Kyrgyzstan")) %>%
  
  mutate(country = str_replace(country,
                               pattern = "Lao", 
                               replacement = "Laos")) %>%
  
  mutate(country = str_replace(country, 
                               pattern = "Micronesia, Fed. Sts.",
                               replacement = "Micronesia")) %>%
  
  mutate(country = str_replace(country, 
                               pattern = "St. Kitts and Nevis",
                               replacement = "Saint Kitts and Nevis")) %>%
  
  mutate(country = str_replace(country,
                               pattern = "St. Lucia",
                               replacement = "Saint Lucia")) %>%
  
  mutate(country = str_replace(country,
                               pattern = "St. Vincent and the Grenadines",
                               replacement = "Saint Vincent and the Grenadines")) %>%
  
  mutate(country = str_replace(country,
                               pattern = "Slovak Republic", 
                               replacement = "Slovakia")) %>%
  
  mutate(country = str_replace(country, 
                               pattern = "United States",
                               replacement = "US")) %>%
  
  mutate(country = str_replace(country, 
                               pattern = "Palestine", 
                               replacement = "West Bank and Gaza"))
  

# World map data ----------------------------------------------------------

world_map <- 
  world_map %>% 
  rename("country" = "region") %>%
  select(-"subregion") %>% 
  mutate(country = str_replace(country, 
                               pattern = "USA", 
                               replacement = "US")) %>%
  mutate(country = str_replace(country, 
                               pattern = "UK", 
                               replacement = "United Kingdom")) %>%
  mutate(country = str_replace(country, 
                               pattern = "Democratic Republic of the Congo", 
                               replacement = "Congo (Kinshasa)")) %>%
  mutate(country = str_replace(country, 
                               pattern = "Myanmar", 
                               replacement = "Burma")) %>%
  mutate(country = str_replace(country,
                               pattern = "Cape Verde",
                               replacement = "Cabo Verde"))


# Write Data --------------------------------------------------------------
combined_tibble %>%
  write_csv("data/02_country_data.csv")

world_map %>%
  write_csv("data/02_world_map_data.csv")

