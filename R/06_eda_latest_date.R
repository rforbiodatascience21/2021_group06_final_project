rm(list = ls(all = TRUE))

# Load Libraries ----------------------------------------------------------
library("tidyverse")
source("R/99_functions.R")


# Load Data ---------------------------------------------------------------
timeseries_data <- read_csv("data/03_augmented_timeseries.csv",
                            col_types = cols(
                              "Rolling_mean_confirmed" = col_double(),
                              "Rolling_mean_deaths"    = col_double(),
                              "Rolling_case_fatality"  = col_double(),
                              "Wave_status"            = col_character()))
# Subset to latest date
latest_date_data <- get_latest_date_data(timeseries_data)


# Plots -------------------------------------------------------------------

# Highest number of cases per region plot
strat_region_plot <- latest_date_data %>%
  drop_na(Region) %>%
  group_by(Region) %>%
  arrange(desc(Confirmed_per_100k_citizen)) %>%
  slice_head(n = 10) %>%
  ungroup()  %>%
  mutate(Country = fct_reorder(Country,
                               Confirmed_per_100k_citizen)) %>%
  
  ggplot(mapping = aes(x = Confirmed_per_100k_citizen,
                       y = Country)) +
  facet_wrap(~ Region, 
             scales = "free_y") +
  geom_col() +
  labs(x = "Cases per 100k citizens",
       title = "Top 10 Countries with Highest Amount of Cases for Each Region")+
  theme_minimal() +
  theme(axis.title.y = element_blank())

# Deaths stratified on income plot
deaths_income <- latest_date_data %>%
  drop_na(IncomeGroup) %>%
  mutate(IncomeGroup = fct_relevel(IncomeGroup, c("Low income",
                                                  "Lower middle income",
                                                  "Upper middle income", 
                                                  "High income"))) %>%
  ggplot(mapping = aes(x = Deaths_per_100k_citizen,
                       y = IncomeGroup)) +
  geom_boxplot(fill = "#708090",
               alpha = 0.5) +
  labs(x = "Deaths per 100k citizens",
       title = "Relationship Between Income Group and Deaths") +
  theme_minimal() +
  theme(legend.position = "none", 
        axis.title.y = element_blank())

# More detailed deaths and income plot
deaths_by_income_region_plot<- latest_date_data  %>%
  drop_na(IncomeGroup) %>%
  mutate(IncomeGroup = fct_relevel(IncomeGroup, c("Low income",
                                                  "Lower middle income",
                                                  "Upper middle income", 
                                                  "High income"))) %>%
  ggplot(aes(x = Deaths_per_100k_citizen,
             y = IncomeGroup,
             size = `Population`,
             color = Region)) +
  geom_point(alpha = 0.5,
             position = position_jitter(w = 0.2, h = 0.2)) +
  labs(x = "Deaths per 100k citizens",
       title = "Detailed View of Income Groups and Number of Deaths",
       size = "Population (millions)") +
  theme_minimal() +
  scale_size(range = c(0.1, 10),
                            breaks = 1000000 * c(250, 500, 750, 1000, 1250),
                            labels = c("250", "500", "750", "1000", "1250")) +
  theme(axis.title.y = element_blank())

# figure for the slides ---------------------------------------------------

eda_slide_plot <- (deaths_income + 
                     theme(plot.title = element_blank(),
                           axis.title.x = element_text())  +
                   deaths_by_income_region_plot + 
                     theme(plot.title = element_blank(),
                           axis.title.x = element_text(),
                           axis.text.y = element_blank())) +
  plot_annotation(tag_levels = "A",
                  title = "Relationship between Income Group and Deaths",
                  theme = theme(plot.title = element_text(hjust = 0.5)))
  

# Write plots -------------------------------------------------------------

ggsave("results/06_highest_cases_per_region.png",
       plot = strat_region_plot,
       height = 6,
       width = 8.5)
ggsave("results/06_deaths_by_income.png",
       plot = deaths_income,
       height = 6,
       width = 8.5)
ggsave("results/06_death_by_income_and_region.png",
       plot = deaths_by_income_region_plot,
       height = 6,
       width = 8.5)
ggsave("results/06_eda_slide_plot.png",
       plot = eda_slide_plot,
       height = 5,
       width = 10)

