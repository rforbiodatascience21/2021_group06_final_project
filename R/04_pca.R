rm(list=ls(all=TRUE))


# Load Libraries ----------------------------------------------------------
library(tidyverse)
library(broom)
library(patchwork)
library(ggrepel)
library(cowplot)
source("R/99_functions.R")


# Load Data ---------------------------------------------------------------
timeseries_data <- read_csv("data/03_augmented_timeseries.csv")



# Wrangle data ------------------------------------------------------------

# Subset to latest date

latest_date_data <- get_latest_date_data(timeseries_data)


# PCA ---------------------------------------------------------------------


# Does it matter if there are 3 entries for each country?
# Include more of the new variables
pca_fit <- latest_date_data %>%
  select(Population, Pop_density, Age_median, Gdp, Sex_ratio, Inequality) %>%
  prcomp(scale = TRUE)

# Investigate what the outer data points are?
confirmed_plot <- pca_fit %>%
  augment(latest_date_data) %>%
  
  ggplot(aes(x = .fittedPC1, y = .fittedPC2, 
             color = Cases_per_100k_citizen)) + 
  geom_point(size = 1.5,
             alpha = 0.7) +
  labs(x = "PC 1",
       y = "PC 2",
       color = "Cases per 100k citizens",
       subtitle = "Confirmed cases per 100k citizens")+
  theme_minimal()+
  theme(legend.position = "bottom")+
  scale_color_gradient(low = "#00BFC4",
                       high = "#F8766D")

deaths_plot <- pca_fit %>%
  augment(latest_date_data) %>%
  
  ggplot(aes(x = .fittedPC1, y = .fittedPC2, 
             color = Deaths_per_100k_citizen)) + 
  geom_point(size = 1.5,
             alpha = 0.5) +
  labs(x = "PC 1",
       y = "PC 2",
       color = "Deaths per 100k citizens",
       subtitle = "Deaths per 100k citizens")+
  theme_minimal()+
  theme(legend.position = "bottom")+
  scale_color_gradient(low = "#00BFC4",
                       high = "#F8766D")

cases_death_pca_plot<- confirmed_plot + deaths_plot


arrow_style <- arrow(
  angle = 20, ends = "first", type = "closed", length = grid::unit(8, "pt")
)

PC_directions_plot <- pca_fit %>%
  tidy(matrix = "rotation") %>%
  pivot_wider(names_from = "PC", names_prefix = "PC", values_from = "value") %>%
  
  ggplot(aes(PC1, PC2,label = column)) +
  geom_segment(xend = 0, yend = 0, arrow = arrow_style)+
  geom_text_repel(color = "#904C2F")+
  labs(title = "Principal component directions in feature space")+
  theme_minimal()

variance_explained_plot <- pca_fit %>%
  tidy(matrix = "eigenvalues") %>%
  ggplot(aes(PC, percent)) +
  geom_col(fill = "#56B4E9", alpha = 0.8, ) +
  scale_x_continuous(breaks = 1:6) +
  scale_y_continuous(
    labels = scales::percent_format(),
    expand = expansion(mult = c(0, 0.01)))+
  labs(x = "Principal Component", 
       y = "Percent", 
       title = "Variance explained by each principal component")+
  theme_minimal()


# Save results ------------------------------------------------------------

ggsave("results/04_pca_projections.png", cases_death_pca_plot)
ggsave("results/04_pca_variance_explained.png", variance_explained_plot)
ggsave("results/04_pca_directions.png", PC_directions_plot)
