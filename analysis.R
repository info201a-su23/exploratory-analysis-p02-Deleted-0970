library(tidyverse)
library(lubridate)

# Read data into file
global_temp <- read_csv("data/GlobalTemperatures.csv")
country_temp <- read_csv("data/GlobalLandTemperaturesByCountry.csv")
city_temp <- read_csv("data/GlobalLandTemperaturesByCity.csv")

# Aggregate data into annual data:
annual_global_temp <- annual_temp_helper(global_temp)
annual_country_temp <- country_temp %>%
  mutate(dt = floor_date(dt, 'year')) %>%
  group_by(Country, dt) %>%
  country_temp_helper()

# Get dimensions
get_dim <- function(){
  # Number of rows:
  rows_global <- nrow(global_temp)
  rows_country <- nrow(country_temp)
  rows_city <- nrow(city_temp)
  # Number of columns:
  cols_global <- ncol(global_temp)
  cols_country <- ncol(country_temp)
  cols_city <- ncol(city_temp)
  
  dim_table <- data.frame(
    `Dimensions` = c("Rows", "Columns"),
    `global_temp` = c(rows_global, cols_global),
    `country_temp` = c(rows_country, cols_country),
    `city_temp` = c(rows_city, cols_city)
    )
  return(dim_table)
}

# Get column names:
get_colnames <- function(){
  colnames_global <- colnames(global_temp)
  colnames_country <- colnames(country_temp)
  colnames_city <- colnames(city_temp)
  
  col_name_list <- list(
    global_temp = colnames_global,
    country_temp = colnames_country,
    city_temp = colnames_city
  )
  return(col_name_list)
}

# Get the data types (does not return anything):
get_col_str <- function(){
  coltypes_global <- str(global_temp)
  coltypes_country <- str(country_temp)
  coltypes_city <- str(city_temp)
  
  col_str_list <- list(
    global_temp = coltypes_global,
    country_temp = coltypes_country,
    city_temp = coltypes_city
  )
}

# Helper functions:
annual_temp_helper <- function(temp_data){
  temp <- temp_data %>%
    group_by(dt = floor_date(dt, 'year')) %>%
    summarise(
      LandAverageTemperature = mean(LandAverageTemperature, na.rm = TRUE),
      LandAverageTemperatureUncertainty = mean(LandAverageTemperatureUncertainty,
                                               na.rm = TRUE),
      LandMaxTemperature = max(LandMaxTemperature, na.rm = TRUE),
      LandMaxTemperatureUncertainty = mean(LandMaxTemperatureUncertainty, 
                                           na.rm = TRUE),
      LandMinTemperature = min(LandMinTemperature, na.rm = TRUE),
      LandMinTemperatureUncertainty = mean(LandMinTemperatureUncertainty, 
                                           na.rm = TRUE),
      LandAndOceanAverageTemperature = mean(LandAndOceanAverageTemperature,
                                            na.rm = TRUE),
      LandAndOceanAverageTemperatureUncertainty = mean(
        LandAndOceanAverageTemperatureUncertainty, na.rm = TRUE)
    ) %>%
    mutate(dt = format(dt, "%Y")) %>%
    mutate_all(~ifelse(is.nan(.), NA, .)) %>%
    mutate_all(~ifelse(is.infinite(.), NA, .))
  return(temp)
}

country_temp_helper <- function(temp_data){
  temp <- temp_data %>%
    summarise(
      AverageTemperature = mean(AverageTemperature, na.rm = TRUE),
      AverageTemperatureUncertainty = mean(AverageTemperatureUncertainty,
                                               na.rm = TRUE)) %>%
    mutate(dt = format(dt, "%Y")) %>%
    mutate_all(~ifelse(is.nan(.), NA, .)) %>%
    mutate_all(~ifelse(is.infinite(.), NA, .))
  return(temp)
}

reframe_by_global_event_type <- function(climate_data){
  climate_data <- climate_data %>%
    reframe(dt,
            event_type,
            LandAverageTemperature,
            LandAverageTemperatureUncertainty,
            LandMaxTemperature,
            LandMaxTemperatureUncertainty,
            LandMinTemperature,
            LandMinTemperatureUncertainty,
            LandAndOceanAverageTemperature,
            LandAndOceanAverageTemperatureUncertainty
    )
  return(climate_data)
}

# Questions to answer:
# 1: How much have global land temperatures changed since 1750?
global_temp_change <- function(start_year = 1750, end_year = 2015){
  temp_change <- annual_global_temp %>%
    filter(dt == start_year | dt == end_year) %>%
    mutate_all(~ifelse(is.na(.), 0, .)) %>%
    summarize(
      dt = paste(start_year, "-", end_year),
      LandAverageTemperature = diff(
        LandAverageTemperature, lag = 1),
      LandAverageTemperatureUncertainty = diff(
        LandAverageTemperatureUncertainty, lag = 1),
      LandMaxTemperature = diff(LandMaxTemperature, lag = 1),
      LandMaxTemperatureUncertainty = diff(
        LandMaxTemperatureUncertainty, lag = 1),
      LandMinTemperature = diff(LandMinTemperature, lag = 1),
      LandMinTemperatureUncertainty = diff(
        LandMinTemperatureUncertainty, lag = 1),
      LandAndOceanAverageTemperature = diff(
        LandAndOceanAverageTemperature, lag = 1),
      LandAndOceanAverageTemperatureUncertainty = diff(
        LandAndOceanAverageTemperatureUncertainty, lag = 1),
    ) %>%
    mutate(event_type = "chg_avg_temp") %>%
    reframe_by_global_event_type()
  return(temp_change)
}

# 2: What are the min and max values in the global data-set?
# 2.1: When the hottest average year globally since 1750 and how hot was it?
global_max_avg_temp <- function(start_year = 1750, end_year = 2015){
  hottest_year <- annual_global_temp %>%
    filter(dt >= start_year & dt <= end_year) %>%
    filter(LandAverageTemperature == max(
      LandAverageTemperature, 
      na.rm = TRUE)) %>%
    mutate(event_type = "max_avg_temp") %>%
    reframe_by_global_event_type()
  return(hottest_year)
}

# 2.2: When the coolest average year globally since 1750 and how hot was it?
global_min_avg_temp <- function(start_year = 1750, end_year = 2015){
  coldest_year <- annual_global_temp %>%
    filter(dt >= start_year & dt <= end_year) %>%
    filter(LandAverageTemperature == min(
      LandAverageTemperature, 
      na.rm = TRUE)) %>%
    mutate(event_type = "min_avg_temp") %>%
    reframe_by_global_event_type()
  return(coldest_year)
}

# 2.3: When is the median average year globally since 1750 and how hot was it?
global_med_avg_temp <- function(start_year = 1750, end_year = 2015){
  median_year <- annual_global_temp %>%
    filter(dt >= start_year & dt <= end_year) %>%
    arrange(desc(LandAverageTemperature)) 
  
  midpoint_index <- ceiling(nrow(median_year) / 2)
  
  median_year <- median_year %>%
    slice(midpoint_index) %>%
    mutate(event_type = "med_avg_temp") %>%
    reframe_by_global_event_type()
  return(median_year)
}

# 2.4: Create a table of data from questions 1 and 2
global_annual_summary <- function(start_year = 1750, end_year = 2015){
  max <- global_max_avg_temp(start_year, end_year)
  min <- global_min_avg_temp(start_year, end_year)
  med <- global_med_avg_temp(start_year, end_year)
  chg <- global_temp_change(start_year, end_year)
  
  summary_tbl <- max %>% 
    full_join(med) %>%
    full_join(min) %>%
    full_join(chg) %>%
    arrange(dt)
  return(summary_tbl)
}

# 3: What are the min and max values in the country data-set?
# 3.1: What is the hottest average day since 1750? 

# 3.2: When was it, where was it, and how hot was it?

# 3.3: What is the coldest average day since 1750?

# 3.4: When was it, where was it, and how cool was it?

# 3.5: Create a table of this data

