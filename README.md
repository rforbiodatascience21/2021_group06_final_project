# Exam Project Group 6 (Christian, Anna, Hanna, Joen)

## Description

The Covid-19 pandemic has been with us for over a year now. 
Here we investigate how the spread of covid in indivdual countries is correlated
with multiple demographic and economical factors, that we deemed interesting to investigate.


## Data
All raw data can be found in data/_raw.

Raw data consists of multiple timeseries datasets taken from John Hopkins 
university. Each timeseries dataset describes a specific measurement 
(confirmed case, death and recovered). These datasets contains variables describing the
country and location. Furthermore it then consists of a variable for each day,
which indicates the amount of cases, deaths or recoveries.

Raw data containing specific statistics about each country, such as population,
density and age has been downloaded from gapminder.com/data and data.worldbank.org/. This is used to augment
our timeseries data for various plots and models.


### Run Project
This entire project and its analysis can be run to generate all results, by running
00_doit.R.

### Dependencies
For making this project, the following packages and versions were used:

* R version 4.0.3
* tidyverse 1.3.1
* maps 3.3.0
* mapproj 1.2.7
* broom 0.7.6
* cowplot 1.1.1
* patchwork 1.1.1
* ggrepel 0.9.1
* wesanderson 0.3.6
* shinythemes 1.2.0
* shiny 1.6.0
* knitr 1.32
* rmarkdown 2.7
* rlang 0.4.10

