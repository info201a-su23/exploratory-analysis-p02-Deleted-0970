source("analysis.R")
library(ggplot2)

# Function to calculate the difference in heat between years for each city
city_temp_difference <- function(start_year = 1850, end_year = 2013){
  temp_diff <- city_annual_summary(start_year, end_year) %>%
    group_by(Country, City) %>%
    arrange(dt) %>%
    mutate(HeatDifference = diff(AverageTemperature, lag = 1)) %>%
    ungroup()
  return(temp_diff)
}

# Calculate the difference in heat between years for each city
heat_diff_data <- city_temp_difference()

# Create the Bubble chart
ggplot(heat_diff_data, aes(x = Longitude, y = Latitude, size = HeatDifference, color = dt)) +
  geom_point(alpha = 0.7) +
  scale_size_continuous(range = c(5, 15)) +  # Adjust the size range of bubbles as needed
  labs(title = "Difference in Heat by Year",
       x = "Longitude",
       y = "Latitude",
       size = "Heat Difference",
       color = "Year") +
  theme_minimal()
