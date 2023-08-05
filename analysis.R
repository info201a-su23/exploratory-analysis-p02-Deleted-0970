library(tidyverse)

# Read data into file
global_temp <- read_csv("data/GlobalTemperatures.csv")
country_temp <- read_csv("data/GlobalLandTemperaturesByCountry.csv")
annual_city_temp <- read_csv("CityAnnualTemps.csv")

# Get dimensions
get_dim <- function() {
  # Number of rows:
  rows_global <- nrow(global_temp)
  rows_country <- nrow(country_temp)
  rows_city <- nrow(annual_city_temp)
  # Number of columns:
  cols_global <- ncol(global_temp)
  cols_country <- ncol(country_temp)
  cols_city <- ncol(annual_city_temp)

  dim_table <- data.frame(
    `Dimensions` = c("Rows", "Columns"),
    `global_temp` = c(rows_global, cols_global),
    `country_temp` = c(rows_country, cols_country),
    `annual_city_temp` = c(rows_city, cols_city)
  )
  return(dim_table)
}

# Get column names:
get_colnames <- function() {
  colnames_global <- colnames(global_temp)
  colnames_country <- colnames(country_temp)
  colnames_city <- colnames(annual_city_temp)

  col_name_list <- list(
    global_temp = colnames_global,
    country_temp = colnames_country,
    annual_city_temp = colnames_city
  )
  return(col_name_list)
}

# Get the data types (does not return anything):
get_col_str <- function() {
  coltypes_global <- str(global_temp)
  coltypes_country <- str(country_temp)
  coltypes_city <- str(annual_city_temp)

  col_str_list <- list(
    global_temp = coltypes_global,
    country_temp = coltypes_country,
    annual_city_temp = coltypes_city
  )
}

# Helper functions:
global_temp_helper <- function(temp_data) {
  temp <- temp_data %>%
    group_by(dt = (floor_date(dt, 'year'))) %>%
    summarise(
      LandAverageTemperature = mean(LandAverageTemperature, na.rm = TRUE),
      LandAverageTemperatureUncertainty = mean(
        LandAverageTemperatureUncertainty, na.rm = TRUE),
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

country_temp_helper <- function(temp_data) {
  temp <- temp_data %>%
    mutate(dt = (floor_date(dt, 'year'))) %>%
    group_by(Country, dt) %>%
    summarise(
      MaxAverageTemperature = max(AverageTemperature, na.rm = TRUE),
      MinAverageTemperature = min(AverageTemperature, na.rm = TRUE),
      AverageTemperature = mean(AverageTemperature, na.rm = TRUE),
      AverageTemperatureUncertainty = mean(AverageTemperatureUncertainty,
                                           na.rm = TRUE)
    ) %>%
    mutate(dt = format(dt, "%Y")) %>%
    mutate_all(~ifelse(is.nan(.), NA, .)) %>%
    mutate_all(~ifelse(is.infinite(.), NA, .))
  return(temp)
}

reframe_by_global_event_type <- function(climate_data) {
  climate_data <- climate_data %>%
    reframe(
      dt,
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

reframe_by_country_event_type <- function(climate_data) {
  climate_data <- climate_data %>%
    reframe(
      dt,
      Country,
      event_type,
      AverageTemperature,
      MaxAverageTemperature,
      MinAverageTemperature,
      AverageTemperatureUncertainty
    )
  return(climate_data)
}

reframe_by_city_event_type <- function(climate_data) {
  climate_data <- climate_data %>%
    reframe(
      dt,
      Country,
      City,
      event_type,
      AverageTemperature,
      MaxAverageTemperature,
      MinAverageTemperature,
      AverageTemperatureUncertainty,
      lat,
      lng
    )
  return(climate_data)
}

# Aggregate data into annual data using helper functions:
annual_global_temp <- global_temp_helper(global_temp)
annual_country_temp <- country_temp_helper(country_temp)

# Questions to answer:
# 1: How much have global land temperatures changed since 1750?
global_temp_change <- function(start_year = 1750, end_year = 2015) {
  temp_change <- annual_global_temp %>%
    filter(dt == start_year | dt == end_year) %>%
    mutate_all(~ifelse(is.na(.), 0, .)) %>%
    summarize(
      dt = paste(start_year, "-", end_year),
      LandAverageTemperature = diff(
        LandAverageTemperature, lag = 1
      ),
      LandAverageTemperatureUncertainty = diff(
        LandAverageTemperatureUncertainty, lag = 1
      ),
      LandMaxTemperature = diff(LandMaxTemperature, lag = 1
      ),
      LandMaxTemperatureUncertainty = diff(
        LandMaxTemperatureUncertainty, lag = 1
      ),
      LandMinTemperature = diff(LandMinTemperature, lag = 1),
      LandMinTemperatureUncertainty = diff(
        LandMinTemperatureUncertainty, lag = 1
      ),
      LandAndOceanAverageTemperature = diff(
        LandAndOceanAverageTemperature, lag = 1
      ),
      LandAndOceanAverageTemperatureUncertainty = diff(
        LandAndOceanAverageTemperatureUncertainty, lag = 1
      )
    ) %>%
    mutate(event_type = "chg_avg_temp") %>%
    reframe_by_global_event_type()
  return(temp_change)
}

# 2: What are the min and max values in the global data-set?
# 2.1: When the hottest average year globally since 1750 and how hot was it?
global_max_avg_temp <- function(start_year = 1750, end_year = 2015) {
  hottest_year <- annual_global_temp %>%
    filter(dt >= start_year & dt <= end_year) %>%
    filter(
      LandAverageTemperature == max(LandAverageTemperature, na.rm = TRUE)
    ) %>%
    mutate(event_type = "max_avg_temp") %>%
    reframe_by_global_event_type()
  return(hottest_year)
}

# 2.2: When the coolest average year globally since 1750 and how hot was it?
global_min_avg_temp <- function(start_year = 1750, end_year = 2015) {
  coldest_year <- annual_global_temp %>%
    filter(dt >= start_year & dt <= end_year) %>%
    filter(LandAverageTemperature == min(
      LandAverageTemperature,
      na.rm = TRUE
    )) %>%
    mutate(event_type = "min_avg_temp") %>%
    reframe_by_global_event_type()
  return(coldest_year)
}

# 2.3: When is the median average year globally since 1750 and how hot was it?
global_med_avg_temp <- function(start_year = 1750, end_year = 2015) {
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

# 2.4: What is the average global temperature from 1750 to 2015?
global_avg_temp <- function(start_year = 1750, end_year = 2015) {
  avg_temp <- annual_global_temp %>%
    filter(dt >= start_year & dt <= end_year) %>%
    summarise(
      dt = paste(start_year, "-", end_year),
      LandAverageTemperature = mean(
        LandAverageTemperature, na.rm = TRUE
      ),
      LandAverageTemperatureUncertainty = mean(
        LandAverageTemperatureUncertainty, na.rm = TRUE
      ),
      LandMaxTemperature = mean(
        LandMaxTemperature, na.rm = TRUE
      ),
      LandMaxTemperatureUncertainty = mean(
        LandMaxTemperatureUncertainty, na.rm = TRUE
      ),
      LandMinTemperature = mean(
        LandMinTemperature, na.rm = TRUE
      ),
      LandMinTemperatureUncertainty = mean(
        LandMinTemperatureUncertainty, na.rm = TRUE
      ),
      LandAndOceanAverageTemperature = mean(
        LandAndOceanAverageTemperature, na.rm = TRUE
      ),
      LandAndOceanAverageTemperatureUncertainty = mean(
        LandAndOceanAverageTemperatureUncertainty, na.rm = TRUE
      )
    ) %>%
    mutate(event_type = "avg_temp") %>%
    reframe_by_global_event_type()
  return(avg_temp)
}

# 2.5: Create a table of data from questions 1 and 2
global_annual_summary <- function(start_year = 1750, end_year = 2015) {
  max <- global_max_avg_temp(start_year, end_year)
  min <- global_min_avg_temp(start_year, end_year)
  med <- global_med_avg_temp(start_year, end_year)
  chg <- global_temp_change(start_year, end_year)
  avg <- global_avg_temp(start_year, end_year)

  summary_tbl <- chg %>%
    full_join(avg) %>%
    full_join(med) %>%
    full_join(min) %>%
    full_join(max) %>%
    arrange(dt)
  return(summary_tbl)
}

# 3: How much have land temperatures changed since 1850 by country?
country_temp_change <- function(start_year = 1850, end_year = 2013) {
  temp_change <- annual_country_temp %>%
    filter(dt %in% c(start_year, end_year)) %>%
    arrange(dt) %>%
    group_by(Country) %>%
    mutate(
      dt = paste(start_year, "-", end_year),
      event_type = "chg_temp",
      AverageTemperature = ifelse(
        all(!is.na(AverageTemperature)),
        diff(AverageTemperature, lag = 1), NaN),
      MaxAverageTemperature = ifelse(
        all(!is.na(MaxAverageTemperature)),
        diff(AverageTemperature, lag = 1), NaN),
      MinAverageTemperature = ifelse(
        all(!is.na(MinAverageTemperature)),
        diff(AverageTemperature, lag = 1), NaN),
      AverageTemperatureUncertainty = ifelse(
        all(!is.na(AverageTemperatureUncertainty)),
        diff(AverageTemperatureUncertainty, lag = 1), NaN
      )
    ) %>%
    reframe_by_country_event_type()
  return(temp_change)
}

# 4: What are the min and max values in the country data-set?
# 4.1: What is the hottest day since 1850 by country? 
country_max_avg_temp <- function(start_year = 1850, end_year = 2013) {
  temp_max <- annual_country_temp %>%
    filter(dt %in% c(start_year: end_year)) %>%
    arrange(dt) %>%
    group_by(Country) %>%
    filter(MaxAverageTemperature == max(
      MaxAverageTemperature, na.rm = TRUE
    )) %>%
    mutate(event_type = "max_temp") %>%
    reframe_by_country_event_type()
  return(temp_max)
}

# 4.2: What is the coldest temperature each year per country since 1850?
country_min_avg_temp <- function(start_year = 1850, end_year = 2013) {
  temp_min <- annual_country_temp %>%
    filter(dt %in% c(start_year: end_year)) %>%
    arrange(dt) %>%
    group_by(Country) %>%
    filter(MinAverageTemperature == min(
      MinAverageTemperature, na.rm = TRUE
    )) %>%
    mutate(event_type = "min_temp") %>%
    reframe_by_country_event_type()
  return(temp_min)
}

# 4.3: What is the mean temperature for each country since 1850?
country_avg_temp <- function(start_year = 1850, end_year = 2013) {
  temp_change <- annual_country_temp %>%
    filter(dt %in% c(start_year: end_year)) %>%
    arrange(dt) %>%
    group_by(Country) %>%
    summarize(
      dt = paste(start_year, "-", end_year),
      event_type = "avg_temp",
      AverageTemperature = mean(AverageTemperature, na.rm = TRUE),
      AverageTemperatureUncertainty = mean(AverageTemperatureUncertainty,
                                           na.rm = TRUE),
      MaxAverageTemperature = mean(MaxAverageTemperature, na.rm = TRUE),
      MinAverageTemperature = mean(MinAverageTemperature, na.rm = TRUE)
    ) %>%
    reframe_by_country_event_type()
  return(temp_change)
}

# 4.4: Create a table of this data
country_annual_summary <- function(start_year = 1850, end_year = 2013) {
  max <- country_max_avg_temp(start_year, end_year)
  min <- country_min_avg_temp(start_year, end_year)
  avg <- country_avg_temp(start_year, end_year)
  chg <- country_temp_change(start_year, end_year)

  summary_tbl <- max %>% 
    full_join(min) %>%
    full_join(avg) %>%
    full_join(chg) %>%
    arrange(desc(dt))
  return(summary_tbl)
}

# 5: How much have land temperatures changed since 1850 by city?
city_temp_change <- function(start_year = 1850, end_year = 2013) {
  temp_change <- annual_city_temp %>%
    filter(dt %in% c(start_year, end_year)) %>%
    arrange(dt) %>%
    group_by(Country, City) %>%
    mutate(
      dt = paste(start_year, "-", end_year),
      event_type = "chg_temp",
      AverageTemperature = ifelse(
        all(!is.na(AverageTemperature)),
        diff(AverageTemperature, lag = 1), NaN
      ),
      MaxAverageTemperature = ifelse(
        all(!is.na(MaxAverageTemperature)),
        diff(AverageTemperature, lag = 1), NaN
      ),
      MinAverageTemperature = ifelse(
        all(!is.na(MinAverageTemperature)),
        diff(AverageTemperature, lag = 1), NaN
      ),
      AverageTemperatureUncertainty = ifelse(
        all(!is.na(AverageTemperatureUncertainty)),
        diff(AverageTemperatureUncertainty, lag = 1), NaN
      )
    ) %>%
    distinct(Country, City, .keep_all = TRUE) %>%
    reframe_by_city_event_type()
  return(temp_change)
}

# 6: What are the min and max values in the city data-set?
# 6.1: What is the hottest day since 1850 by city?
city_max_avg_temp <- function(start_year = 1850, end_year = 2013) {
  temp_max <- annual_city_temp %>%
    filter(dt %in% c(start_year: end_year)) %>%
    arrange(dt) %>%
    group_by(Country, City) %>%
    filter(MaxAverageTemperature == max(
      MaxAverageTemperature, na.rm = TRUE
    )) %>%
    mutate(event_type = "max_temp") %>%
    reframe_by_city_event_type()
  return(temp_max)
}

# 6.2: What is the coldest average year per city since 1850?
city_min_avg_temp <- function(start_year = 1850, end_year = 2013) {
  temp_min <- annual_city_temp %>%
    filter(dt %in% c(start_year: end_year)) %>%
    arrange(dt) %>%
    group_by(Country, City) %>%
    filter(MinAverageTemperature == min(
      MinAverageTemperature, na.rm = TRUE
    )) %>%
    mutate(event_type = "min_temp") %>%
    reframe_by_city_event_type()
  return(temp_min)
}

# 6.3: What is the mean temperature for each city since 1850?
city_avg_temp <- function(start_year = 1850, end_year = 2013) {
  temp_change <- annual_city_temp %>%
    filter(dt %in% c(start_year: end_year)) %>%
    arrange(dt) %>%
    group_by(Country, City) %>%
    summarize(
      dt = paste(start_year, "-", end_year),
      event_type = "avg_temp",
      AverageTemperature = mean(AverageTemperature, na.rm = TRUE),
      AverageTemperatureUncertainty = mean(AverageTemperatureUncertainty,
                                           na.rm = TRUE),
      MaxAverageTemperature = mean(MaxAverageTemperature, na.rm = TRUE),
      MinAverageTemperature = mean(MinAverageTemperature, na.rm = TRUE),
      lat,
      lng
    ) %>%
    distinct(Country, City, .keep_all = TRUE) %>%
    reframe_by_city_event_type()
  return(temp_change)
}

# 6.4: Create a table of this data
city_annual_summary <- function(start_year = 1850, end_year = 2013) {
  max <- city_max_avg_temp(start_year, end_year)
  min <- city_min_avg_temp(start_year, end_year)
  avg <- city_avg_temp(start_year, end_year)
  chg <- city_temp_change(start_year, end_year)

  summary_tbl <- max %>% 
    full_join(min) %>%
    full_join(avg) %>%
    full_join(chg) %>%
    arrange(desc(dt)) %>%
    mutate(
      AverageTemperature = ifelse(
        is.nan(AverageTemperature), NA, AverageTemperature
      ),
      MaxAverageTemperature = ifelse(
        is.nan(MaxAverageTemperature), NA, MaxAverageTemperature
      ),
      MinAverageTemperature = ifelse(
        is.nan(MinAverageTemperature), NA, MinAverageTemperature
      ),
      AverageTemperatureUncertainty = ifelse(
        is.nan(AverageTemperatureUncertainty), NA,
        AverageTemperatureUncertainty
      ),
    )
  return(summary_tbl)
}
