rm(list=ls(all=TRUE))

# Load Libraries ----------------------------------------------------------
library("tidyverse")
source("R/99_functions.R")


# Load Data ---------------------------------------------------------------
timeseries_data <- read_csv("data/03_augmented_timeseries.csv",
                            col_types = cols(
                              "Rolling_mean_confirmed" = col_double(),
                              "Rolling_mean_deaths" = col_double(),
                              "Rolling_case_fatality" = col_double(),
                              "Wave_status" = col_character()))

# Subset to latest date

latest_date_data <- get_latest_date_data(timeseries_data)


# Plots -------------------------------------------------------------------

strat_region_plot <- latest_date_data %>%
  drop_na(Region) %>%
  group_by(Region) %>%
  arrange(desc(Confirmed_per_100k_citizen)) %>%
  slice_head(n = 10) %>%
  ungroup() %>%
  mutate(`Country/Region` = as_factor(`Country/Region`)) %>%
  mutate(`Country/Region` = fct_reorder(`Country/Region`,
                                        Confirmed_per_100k_citizen)) %>%
  
  ggplot(mapping = aes(x = Confirmed_per_100k_citizen,
                       y = `Country/Region`))+
  facet_wrap(~ Region, scales = "free")+
  geom_bar(stat="identity")+
  labs(x = 'Cases per 100k citizens',
       y = '',
       title = 'Top 10 Countries with Highest Amount of Cases for Each Region')+
  theme(axis.title.y = element_blank())+
  theme_minimal()


deaths_income <- latest_date_data %>%
  drop_na(IncomeGroup) %>%
  mutate(IncomeGroup = as_factor(IncomeGroup)) %>%
  mutate(IncomeGroup = fct_reorder(IncomeGroup,
                                   desc(Deaths_per_100k_citizen))) %>%
  ggplot(mapping = aes(x = IncomeGroup,
                       y = Deaths_per_100k_citizen,
                       fill = IncomeGroup))+
  geom_boxplot()+
  labs(x = " ",
       y = "Deaths per 100k citizens",
       title = "Relationship Between Income Group and Deaths")+
  theme_minimal()+
  theme(legend.position = "none")

cases_by_income_region_plot<- latest_date_data  %>%
  drop_na(IncomeGroup) %>%
  mutate(IncomeGroup = fct_relevel(IncomeGroup, c("Low income",
                                                  "Lower middle income",
                                                  "Upper middle income", 
                                                  "High income"))) %>%
  ggplot(aes(x = Confirmed_per_100k_citizen,
             y = IncomeGroup,
             size = `Population`,
             color = Region)) +
  geom_point(alpha = 0.5,
             position = position_jitter(w = 0.2, h = .2)) +
  labs(x = "Cases per 100k citizens",
       title = "Relationship Between Income Group and Cases",
       y = " ")+
  theme_minimal()+
  guides(size=FALSE)

# figure for the slides ---------------------------------------------------

eda_slide_plot <- (cases_by_income_region_plot / deaths_income)+ 
  plot_annotation(tag_levels = "A",
                  theme = theme(plot.title = element_text(hjust = 0.5)))&
  theme(legend.position = "bottom")

# Write plots -------------------------------------------------------------

ggsave("results/06_highest_cases_per_region.png",
       plot = strat_region_plot,
       height = 6,
       width = 8.5)
ggsave("results/06_deaths_by_income.png",
       plot = deaths_income,
       height = 6,
       width = 8.5)
ggsave("results/06_cases_by_income_and_region.png",
       plot = cases_by_income_region_plot,
       height = 6,
       width = 8.5)
ggsave("results/06_eda_slide_plot.png",
       plot = eda_slide_plot,
       height = 8.5,
       width = 8.5)

