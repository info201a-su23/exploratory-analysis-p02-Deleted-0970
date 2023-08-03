library(tidyverse)

# Read data into file
global_temp <- read_csv("data/GlobalTemperatures.csv")
country_temp <- read_csv("data/GlobalLandTemperaturesByCountry.csv")
city_temp <- read_csv("data/GlobalLandTemperaturesByCity.csv")

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

# Questions to answer:
# 1: How much have global land temperatures changed since 1750?
global_temp_change <- function(){
  temps_1750 <- global_temp %>%
    filter(substr(dt, 1, 4) == "1750") %>%
    mutate(LandAverageTemperature = replace_na(LandAverageTemperature, 0))
  temps_2015 <- global_temp %>%
    filter(substr(dt, 1, 4) == "2015") %>%
    mutate(LandAverageTemperature = replace_na(LandAverageTemperature, 0))
  
  temp_change <- mean(
    temps_2015$LandAverageTemperature
    ) - mean(
      temps_1750$LandAverageTemperature
      )
  return(temp_change)
}

# 2: What are the min and max values in the global data-set?
# 2.1: What is the hottest day globally since 1750? 

# 2.2: When was it and how hot was it?

# 2.3: What is the coldest day globally since 1750?

# 2.4: When was it and how cool was it?

# 2.5: Create a table of this data

# 3: What are the min and max values in the country data-set?
# 3.1: What is the hottest day since 1750? 

# 3.2: When was it, where was it, and how hot was it?

# 3.3: What is the coldest day since 1750?

# 3.4: When was it, where was it, and how cool was it?

# 3.5: Create a table of this data

