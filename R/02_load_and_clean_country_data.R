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
income_grp <- read_csv("data/_raw/Income_grp.csv")
population_above65 <- read_csv("data/_raw/Population_65.csv")
urban_pop_per <- read_csv("data/_raw/urban_pop_perct.csv")

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

income_grp <- income_grp %>%
  select(Region, IncomeGroup, TableName) %>%
  rename(country = TableName) %>%
  mutate(Region = recode(Region,
      "South Asia" = "Asia & Pacific",
      "North America" = "Americas & Caribbean",
      "East Asia & Pacific" = "Asia & Pacific",
      "Latin America & Caribbean" = "Americas & Caribbean"))
    
population_above65 <- 
  population_above65 %>%
  select(`Country Name`, "2019") %>% #no data fr 2020, so most recent yr then
  rename(country = `Country Name`,
         `Pop%_above65` = "2019")

urban_pop_per <-
  urban_pop_per %>%
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
  left_join(population_above65, by = "country") %>% 
  left_join(income_grp, by = "country")  %>% 
  left_join(urban_pop_per, by = "country")



# Alternate DRY version of data load and join -----------------------------

world_map <- map_data("world") # data in R giving country Lat and Long. 

#Loading the gapminder data
gapminder_data <- tribble(
  ~Variable_name, ~File_path,
  "Population",  "data/_raw/population_total.csv",
  "Pop_density", "data/_raw/population_density_per_square_km.csv",
  "Age_median",  "data/_raw/median_age_years.csv",
  "Gdp",         "data/_raw/gdp_per_capita.csv",
  "Sex_ratio",   "data/_raw/sex_ratio_all_age_groups.csv",
  "Inequality",  "data/_raw/gini.csv",
)

gapminder_data <- gapminder_data %>% 
  mutate(Raw_data = purrr::map(File_path,~read_csv(.)))

#Loading the non-gapminder data
income_grp <- read_csv("data/_raw/Income_grp.csv")
population_above65 <- read_csv("data/_raw/Population_65.csv")
urban_pop_per <- read_csv("data/_raw/urban_pop_perct.csv")

#Wraggling the non-gapminder data
income_grp <- income_grp %>%
  select(Region, IncomeGroup, TableName) %>%
  rename(country = TableName) %>%
  mutate(Region = recode(Region,
                         "South Asia" = "Asia & Pacific",
                         "North America" = "Americas & Caribbean",
                         "East Asia & Pacific" = "Asia & Pacific",
                         "Latin America & Caribbean" = "Americas & Caribbean"))

population_above65 <- 
  population_above65 %>%
  select(`Country Name`, "2019") %>% #no data fr 2020, so most recent yr then
  rename(country = `Country Name`,
         `Pop%_above65` = "2019")

urban_pop_per <-
  urban_pop_per %>%
  select(`Country Name`,"2019") %>% #no data for 2020, so most recent yr then
  rename(country = `Country Name`,
         Urban_pop_perct = "2019")

#Wraggling the gapminder data

#defining a function to extract data from year 2020
get_year_2020_data <- function(tbl, var_name) {
  tbl %>% 
    select(country, "2020") %>% 
    rename(!!var_name := "2020")
}

#using the get_year_2020_data function to extraxt year 2020 data from all datasets
gapminder_data <- gapminder_data %>% 
  mutate(`2020_data` = purrr::map2(.x = Raw_data, 
                                   .y = Variable_name, 
                                   ~get_year_2020_data(.x, .y)))

#Joining the gapminder data with the non-gapminder data
all_data <- gapminder_data %>% 
  pluck("2020_data") %>% 
  append(list(income_grp, population_above65, urban_pop_per))
all_data <- all_data %>%
  reduce(left_join, by = "country")


# Replacing inconsistent country names -------------------------------------
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




# Rename world data -------------------------------------------------------


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

