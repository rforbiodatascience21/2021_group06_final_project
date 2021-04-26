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


