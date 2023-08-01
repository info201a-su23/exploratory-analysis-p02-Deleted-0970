library(tidyverse)

# Read data into file
global_temp <- read_csv("data/GlobalTemperatures.csv")
country_temp <- read_csv("data/GlobalLandTemperaturesByCountry.csv")
city_temp <- read_csv("data/GlobalLandTemperaturesByCity.csv")

# Get dimensions
# Number of rows:
rows_global <- nrow(global_temp)
rows_country <- nrow(country_temp)
rows_city <- nrow(city_temp)
# Number of columns:
cols_global <- ncol(global_temp)
cols_country <- ncol(country_temp)
cols_city <- ncol(city_temp)

# Get column names:
colnames_global <- colnames(global_temp)
colnames_country <- colnames(country_temp)
colnames_city <- colnames(city_temp)

# Get the data types:
coltypes_global <- str(global_temp)
coltypes_country <- str(country_temp)
coltypes_city <- str(city_temp)

# Questions to answer:
