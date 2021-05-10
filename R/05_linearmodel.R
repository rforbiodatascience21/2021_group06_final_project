rm(list=ls(all=TRUE)) 

# Load Libraries ----------------------------------------------------------
library("tidyverse")
library("broom")
library("patchwork")
library("ggrepel")
source("R/99_functions.R")
# Load Data ---------------------------------------------------------------

timeseries_data <- read_csv("data/03_augmented_timeseries.csv",
                            col_types = cols(
                              "Rolling_mean_confirmed" = col_double(),
                              "Rolling_mean_deaths" = col_double(),
                              "Rolling_case_fatality" = col_double(),
                              "Wave_status" = col_character()))

# Wrangle data ------------------------------------------------------------

# Subset to latest date

latest_date_data <- get_latest_date_data(timeseries_data) %>%
  drop_na(`Pop%_above65`, Urban_pop_perct, IncomeGroup, Gdp, Pop_density) %>%
  mutate(IncomeGroup = fct_relevel(IncomeGroup, 
                                   c("High income", 
                                     "Upper middle income", 
                                     "Lower middle income", 
                                     "Low income"))) 

model_data <- latest_date_data %>% 
  group_by(IncomeGroup) %>% 
  nest() %>% 
  ungroup() %>%  
  mutate(mdl = purrr::map(data,
                   ~lm(Deaths_per_100k_citizen ~ `Pop%_above65`+ Urban_pop_perct, 
                       data = .x)),
         mdl_tidy = purrr::map(mdl, tidy, conf.int = TRUE)) %>%
  unnest(mdl_tidy)


## Plot results ------------------------------------------------------------
  
# scatter plots
lm1_p1 <- 
  latest_date_data%>%
  ggplot(mapping = aes(x = `Pop%_above65`,
                       y = Deaths_per_100k_citizen,
                       color = IncomeGroup)) +
  geom_point() +
  geom_smooth(method = "lm", se=F) +
  facet_wrap(IncomeGroup ~ .)+
  theme_minimal()+
  labs(y = "Deaths per 100k",
       x = "Population % > 65 yrs",
       color = "Income Group") 

lm1_p2 <- 
  latest_date_data %>%
  ggplot(mapping = aes(x = `Urban_pop_perct`,
                       y = Deaths_per_100k_citizen,
                       color = IncomeGroup)) +
  geom_point() +
  geom_smooth(method ="lm", se=F) +
  facet_wrap(IncomeGroup ~ .)+
  theme_minimal()+
  labs(x = "Population % living in urban ", 
       color = "Income Group")+
  theme(axis.title.y = element_blank())

lm1_Final_line_plot <- 
      lm1_p1 + lm1_p2 + 
      plot_annotation(title = "Simple Linear regression model results for deaths per 100k",
                      tag_levels = "A",
                      theme = theme(plot.title = element_text(hjust = 0.5)))+
      plot_layout(guides = "collect") &
      theme(legend.position = "bottom")

lm1_Final_line_plot

# Slope Estimate plots

Est_plot_lm1 <- 
model_data %>%
  filter(term!="(Intercept)") %>%
  mutate(significant = if_else(condition = p.value < 0.05,
                               true = "*",
                               false = "")) %>%
  
  ggplot(mapping = aes(x = estimate,
                       y = IncomeGroup,
                       color = term, 
                       xmin = conf.low, 
                       xmax = conf.high,
                       label = significant)) +
  geom_vline(xintercept = 0,
             linetype = "dashed")+
  geom_point()+
  geom_errorbarh()+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), 
        axis.title.y = element_blank())+
  labs(x = " Slope Estimate",
       color = "Indep. Variable",
       caption = "* indicates significant p-value < 0.05",
       title = "Slope estimates for deaths per 100k with conf. intervals")+
  scale_color_manual(labels = c("Pop. % > 65", "Urban Pop. %"), 
                     values = c("deepskyblue1", "tomato2"))+ 
  geom_text(size = 6,
            nudge_x = 0.5, 
            nudge_y = 0.1,
            show.legend = FALSE)

Est_plot_lm1

Est_plot_lm1_sig <-
model_data %>%
  filter(term != "(Intercept)",
         p.value < 0.05) %>%
  
  ggplot(mapping = aes(x = estimate, 
                       y = IncomeGroup,
                       color = term,
                       xmin = conf.low,
                       xmax = conf.high)) +
  geom_point() +
  geom_errorbarh() +
  geom_vline(xintercept = 0, 
             linetype="dashed")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_blank())+
  labs(x = "Slope Estimate",
       color = "Indep. Variable",
       title = "Significant slope estimates for deaths per 100k")+
  scale_color_manual(labels = c("Pop. % > 65", "Urban Pop. %"),
                     values = c("deepskyblue1", "tomato2"))
 

Est_plot_lm1_sig
  
## second GLM model -----------------------------------------------------------

model_data2 <- 
  latest_date_data %>% 
  group_by(Region) %>% 
  nest() %>% 
  ungroup() %>%  
  mutate(mdl = purrr::map(data,
                          ~lm(Confirmed_per_100k_citizen ~ Gdp + Pop_density, 
                               data = .x)),
         mdl_tidy = purrr::map(mdl, tidy, conf.int = TRUE)) %>%
  unnest(mdl_tidy)

## Plot results ------------------------------------------------------------

# scatter plots
lm2_p1 <- 
  latest_date_data %>%
  ggplot(mapping = aes(x = Gdp, 
                       y = Confirmed_per_100k_citizen,
                       color = Region)) +
  geom_point() +
  geom_smooth(method ="lm", se = F) +
  facet_wrap(Region ~ .)+
  theme_minimal()+
  labs(y = "Cases per 100k",
       x = "GDP",
       color = "Region") 

lm2_p2 <- 
  latest_date_data %>%
  ggplot(mapping = aes(x = Pop_density, 
                       y = Confirmed_per_100k_citizen,
                       color = Region)) +
  geom_point() +
  geom_smooth(method ="lm", se=F) +
  facet_wrap(Region ~ .)+
  theme_minimal()+
  labs(y = "Cases per 100k",
       x = "Population Density",
       color = "Region") 

lm2_Final_line_plot <- 
  lm2_p1 / lm2_p2 + 
  plot_annotation(title = "Simple Linear regression model results for cases per 100k",
                  tag_levels = 'A',
                  theme = theme(plot.title = element_text(hjust = 0.5)))+
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

lm2_Final_line_plot

# Slope Estimate plots

# all the data 
Est_plot_lm2 <- 
  model_data2 %>%
  filter(term!="(Intercept)") %>%
  mutate(significant = if_else(condition = p.value < 0.05,
                              true = "*",
                              false = "")) %>%
  ggplot(mapping = aes(x = estimate,
                      y = Region,
                      color = term,
                      xmin = conf.low,
                      xmax = conf.high,
                      label = significant)) +
  geom_point() +
  geom_errorbarh() +
  geom_vline(xintercept = 0, 
             linetype="dashed")+
  theme_minimal()+
  theme(axis.title.y = element_blank())+
  labs(x = "Slope Estimate", 
       color = "Indep. Variable",
       title = "Slope estimates for cases per 100k with conf. intervals",
       caption = "* indicates significance with p < 0.05")+
  scale_color_manual(labels = c("Gdp", "Pop Density"),
                     values = c("deepskyblue1", "tomato2"))+ 
  geom_text(size = 6,
            nudge_x = 0.5, 
            nudge_y = 0.1,
            show.legend = FALSE)
  
Est_plot_lm2

# Only the sig ones. 
Est_plot_lm2_sig <- 
model_data2 %>%
  filter(term != "(Intercept)",
         p.value < 0.05) %>%
  ggplot(mapping = aes(x = estimate,
                       y = Region,
                       color = term,
                       xmin = conf.low,
                       xmax = conf.high)) +
  geom_point() +
  geom_errorbarh() +
  geom_vline(xintercept = 0, 
             linetype="dashed")+
  theme_minimal()+
  theme(axis.title.y = element_blank()) +
  labs(x = "Slope Estimate", 
       color = "Indep. Variable",
       title = "Significant slope estimates for cases per 100k")+
  scale_color_manual(labels = c("Gdp", "Pop Density"), 
                     values = c("deepskyblue1", "tomato2"))

Est_plot_lm2_sig

# Write plots -------------------------------------------------------------

ggsave("results/05_lm1_line_plot.png",
       plot = lm1_Final_line_plot,
       height = 6,
       width = 10)
ggsave("results/05_lm1_Est_plot1.png",
       plot = Est_plot_lm1,
       height = 6,
       width = 8.5)
ggsave("results/05_lm1_Est_plot_sig.png",
       plot = Est_plot_lm1_sig,
       height = 6,
       width = 8.5)
ggsave("results/05_lm2_line_plot.png",
       plot = lm2_Final_line_plot,
       height = 6,
       width = 10)
ggsave("results/05_lm2_Est_plot1.png",
       plot = Est_plot_lm2,
       height = 6,
       width = 8.5)
ggsave("results/05_lm2_Est_plot_sig.png",
       plot = Est_plot_lm2_sig,
       height = 6,
       width = 8.5)

