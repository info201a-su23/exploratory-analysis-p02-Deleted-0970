library(dplyr)
library(lubridate)
library(readr)

# Aggregate the huge city data csv file into a smaller csv file.
city_temp <- read_csv("data/GlobalLandTemperaturesByCity.csv")

city_temp_helper <- function(temp_data){
  temp <- temp_data %>%
    mutate(dt = format(dt, "%Y")) %>%
    group_by(Country, City, dt) %>%
    summarise(
      City,
      Country,
      MaxAverageTemperature = max(AverageTemperature, na.rm = TRUE),
      MinAverageTemperature = min(AverageTemperature, na.rm = TRUE),
      AverageTemperature = mean(AverageTemperature, na.rm = TRUE),
      AverageTemperatureUncertainty = mean(AverageTemperatureUncertainty,
                                           na.rm = TRUE),
      Latitude,
      Longitude) %>%
    distinct(Country, City, dt, .keep_all = TRUE) %>%
    mutate(MaxAverageTemperature = ifelse(
      is.infinite(MaxAverageTemperature), NA, MaxAverageTemperature),
      MinAverageTemperature = ifelse(
        is.infinite(MinAverageTemperature), NA, MinAverageTemperature)
    )
  return(temp)
}

convert_coords <- function(coord_data) {
  coord_data %>%
    mutate(lat = as.numeric(sub("([0-9.]+)[NS]", "\\1", Latitude))) %>%
    mutate(lat = ifelse(
      substr(Latitude, nchar(Latitude), nchar(Latitude)) == "S", -lat, lat)) %>%
    select(-Latitude) %>%
    mutate(lng = as.numeric(sub("([0-9.]+)[EW]", "\\1", Longitude))) %>%
    mutate(lng = ifelse(
      substr(Longitude, nchar(Longitude), nchar(Longitude)) == "W",
      -lng, lng)) %>%
    select(-Longitude)
}

annual_city_temp <- city_temp_helper(city_temp) %>%
  convert_coords()

write_csv(annual_city_temp, file = "CityAnnualTemps.csv")
