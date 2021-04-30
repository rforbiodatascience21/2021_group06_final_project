
get_latest_date_data <- function(tibble){ 
  tibble %>%
  filter(Date == max(Date))
}

