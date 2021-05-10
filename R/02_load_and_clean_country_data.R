rm(list = ls(all = TRUE))


# Load Libraries ----------------------------------------------------------

library("tidyverse")
library("maps")
library("mapproj")
source("R/99_functions.R")

# Load Data ---------------------------------------------------------------

world_map <- map_data("world") # data in R giving country Lat and Long. 

# Loading the gapminder data
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
  mutate(Raw_data = purrr::map(File_path, ~read_csv(.)))

# Loading the non-gapminder data
income_grp         <- read_csv("data/_raw/Income_grp.csv")
population_above65 <- read_csv("data/_raw/Population_65.csv",skip=3)
urban_pop_per      <- read_csv("data/_raw/urban_pop_perct.csv",skip=3)


# Wrangle Data ------------------------------------------------------------

# Wrangling the non-gapminders data
income_grp <- income_grp %>%
  select(Region, IncomeGroup, TableName) %>%
  rename(Country = TableName) %>%
  mutate(Region = recode(Region,
                         "South Asia" = "Asia & Pacific",
                         "North America" = "Americas & Caribbean",
                         "East Asia & Pacific" = "Asia & Pacific",
                         "Latin America & Caribbean" = "Americas & Caribbean"))

population_above65 <- 
  population_above65 %>%
  select(`Country Name`, "2019") %>% #no data fr 2020, so most recent yr then
  rename(Country = `Country Name`,
         `Pop%_above65` = "2019")

urban_pop_per <-
  urban_pop_per %>%
  select(`Country Name`,"2019") %>% #no data for 2020, so most recent yr then
  rename(Country = `Country Name`,
         Urban_pop_perct = "2019")

# Wrangling the gapminder data
# Defining a function to extract data from year 2020
get_year_2020_data <- function(tbl, var_name) {
  tbl %>% 
    select(country, "2020") %>% 
    rename(!!var_name := "2020",
           Country = country)
}

# Using the get_year_2020_data function to extraxt year 2020 data
# from all datasets
gapminder_data <- gapminder_data %>% 
  mutate(`2020_data` = map2(.x = Raw_data, 
                            .y = Variable_name, 
                            ~get_year_2020_data(.x, .y)))

# Joining the gapminder data with the non-gapminder data
combined_tibble <- gapminder_data %>% 
  pluck("2020_data") %>%
  append(list(income_grp, population_above65, urban_pop_per)) %>%
  reduce(left_join, by = "Country")

# Replacing inconsistent country names -------------------------------------

# Fix discrepancies between country name in this data and timeseries
combined_tibble <- combined_tibble %>%
  mutate(Country = recode(Country,
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

# Rename world data
world_map <- 
  world_map %>% 
  rename(Country = region) %>%
  select(-subregion) %>% 
  mutate(Country = recode(Country, 
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

