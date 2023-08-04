source("analysis.R")
library(tidyverse)
library(lubridate)

# Calculate city temperature change
city_temp_change_summary <- city_annual_summary()

# Function to calculate city temperature differences
city_temp_differences <- function(temp_summary) {
  temp_diff_data <- temp_summary %>%
    filter(event_type == "chg_temp")
  return(temp_diff_data)
}

# Calculate city temperature differences using the new function
city_temp_diff_data <- city_temp_differences(city_temp_change_summary)

# Create the Bubble chart
ggplot(city_temp_diff_data, aes(x = lng, y = lat,
                                size = AverageTemperatureUncertainty)) +
  geom_point(alpha = 0.5, aes(colour = AverageTemperature)) +
  scale_size_continuous(range = c(1, 5)) +  # Adjust the range of bubble sizes
  labs(title = "City Temperature Changes",
       subtitle = "Change in city temperatures from 1850-2013",
       x = "Longitude",
       y = "Latitude",
       size = "Temperature Uncertainty",
       color = "Temperature Difference") +
  theme_minimal()

