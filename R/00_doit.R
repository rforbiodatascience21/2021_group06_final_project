# Run all scripts ---------------------------------------------------------
source(file = "R/01_load_and_clean_timeseries.R")
source(file = "R/02_load_and_clean_country_data.R")
source(file = "R/03_augment_data.R")
source(file = "R/04_pca.R")
source(file = "R/05_linearmodel.R")
source(file = "R/06_eda_latest_date.R")
source(file = "R/07_timeseries_analysis")
rmarkdown::render('doc/final_presentation.Rmd')
