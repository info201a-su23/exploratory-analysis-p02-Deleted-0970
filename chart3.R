source("analysis.R")
library(tidyverse)
library(lubridate)

# Suppress the warning temporarily
suppressWarnings({
  # Calculate city temperature change
  city_temp_change_summary <- city_annual_summary(start_year = 1850, end_year = 2013)
})

# Function to calculate city temperature differences
calculate_city_temp_differences <- function(temp_summary) {
  temp_diff_data <- temp_summary %>%
    group_by(Country, City) %>%
    mutate(
      HeatDifference = AverageTemperature - lag(AverageTemperature, default = first(AverageTemperature)),
      HeatDifference = ifelse(is.na(HeatDifference), 0, HeatDifference)
    ) %>%
    ungroup()
  return(temp_diff_data)
}

# Calculate city temperature differences using the new function
city_temp_diff_data <- calculate_city_temp_differences(city_temp_change_summary)

# Create the Bubble chart
ggplot(city_temp_diff_data, aes(x = Longitude, y = Latitude, size = HeatDifference)) +
  geom_point(alpha = 0.7) +
  scale_size_continuous(range = c(1, 5)) +  # Adjust the range of bubble sizes
  labs(title = "City Temperature Changes",
       x = "Longitude",
       y = "Latitude",
       size = "Temperature Difference",
       color = "Year") +
  theme_minimal()

