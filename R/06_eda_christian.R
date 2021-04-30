rm(list=ls(all=TRUE))


# Load Libraries ----------------------------------------------------------
library(tidyverse)

source("R/99_functions.R")


# Load Data ---------------------------------------------------------------
timeseries_data <- read_csv("data/03_augmented_timeseries.csv")

# Subset to latest date

latest_date_data <- get_latest_date_data(timeseries_data)


# Plots -------------------------------------------------------------------

strat_region_plot <- latest_date_data %>%
  group_by(Region) %>%
  arrange(desc(Cases_per_100k_citizen)) %>%
  slice_head(n = 10) %>%
  ungroup() %>%
  mutate(`Country/Region` = as_factor(`Country/Region`)) %>%
  mutate(`Country/Region` = fct_reorder(`Country/Region`,
                                        Cases_per_100k_citizen)) %>%
  
  ggplot(mapping = aes(x = Cases_per_100k_citizen,
                       y = `Country/Region`))+
  facet_wrap(~ Region, scales = "free")+
  geom_bar(stat="identity")+
  labs(x = 'Cases per 100k citizens',
       y = 'Country')


deaths_income <- latest_date_data %>%
  mutate(IncomeGroup = as_factor(IncomeGroup)) %>%
  mutate(IncomeGroup = fct_reorder(IncomeGroup,
                                    desc(Deaths_per_100k_citizen))) %>%
  ggplot(mapping = aes(x = IncomeGroup,
                       y = Deaths_per_100k_citizen,
                       fill = IncomeGroup))+
  geom_boxplot()+
  labs(x = "Income Group",
       y = "Deaths per 100k citizens")+
  theme_minimal()+
  theme(legend.position = "none")

# another way to possibly visualize this data...? --> note from Anna

cases_by_income_region_plot<- latest_date_data  %>%
  mutate(IncomeGroup = fct_relevel(IncomeGroup, c("High income", "Upper middle income", "Lower middle income", "Low income"))) %>%
  ggplot(aes(x=IncomeGroup,
             y=Cases_per_100k_citizen,
             size = `Population`,
             color=Region)) +
  geom_point(alpha=0.5,
             position = position_jitter(w = 0.2, h = .2)) +
  coord_flip()+
  scale_x_discrete(limits=rev)+
  labs(x = " ",
       y = "Cases per 100k citizens")+
  theme_minimal()+
  guides(size=FALSE)

# Write plots -------------------------------------------------------------

ggsave("results/06_highest_cases_per_region.png",
       plot = strat_region_plot)
ggsave("results/06_deaths_by_income.png",
       plot = deaths_income)
ggsave("results/06_cases_by_income_and_region.png")

