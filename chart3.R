source("analysis.R")
library(tidyverse)
library(plotly)

# Calculate city temperature change
city_temp_change_summary <- city_temp_change()

# Function to filter city temperature by change
city_temp_differences <- function(temp_summary) {
  temp_diff_data <- temp_summary %>%
    reframe(
      Country,
      City,
      dt,
      TempChange = AverageTemperature,
      TempChangeMax = MaxAverageTemperature,
      TempChangeMin = MinAverageTemperature,
      Uncertainty = AverageTemperatureUncertainty,
      lat,
      lng
    )
  return(temp_diff_data)
}

# Calculate city temperature differences using the new function
city_temp_diff_data <- city_temp_differences(city_temp_change_summary)

# world map from ggplot2
world_map <- map_data('world')

# blank theme to remove labels
blank_theme <- theme_bw() +
  theme(
    axis.line = element_blank(), # remove axis lines
    axis.text = element_blank(), # remove axis labels
    axis.ticks = element_blank(), # remove axis ticks
    axis.title = element_blank(), # remove axis titles
    plot.background = element_blank(), # remove gray background
    panel.grid.major = element_blank(), # remove major grid lines
    panel.grid.minor = element_blank(), # remove minor grid lines
    panel.border = element_blank(), # remove border around plot
  )

# Create the Bubble chart
city_plot <- ggplot() +
  geom_polygon(data = world_map, 
               aes(
                 x = long, 
                 y = lat, 
                 group = group
               ), 
               fill = "lightgray") +
  geom_point(data = city_temp_diff_data, alpha = 0.5, mapping = aes(
    x = lng, y = lat,
    colour = TempChange,
    size = Uncertainty,
    text = paste0("Country: ", Country, "<br>",
                 "City: ", City)
    )) +
  scale_size_continuous(range = c(0.1, 1)) +  # Adjust the range of bubble sizes
  labs(title = "City Temperature Changes",
       subtitle = "Change in city temperatures from 1850-2013", 
       x = "Longitude",
       y = "Latitude",
       size = "Temperature Uncertainty", # TODO: This is not showing up on chart
       color = "Temperature Difference") +
  theme_minimal() + blank_theme + coord_fixed()

city_plot <- ggplotly(city_plot) %>%
  layout(
    title = list(text = paste0(
      'City Temperature Changes',
      '<br>',
      '<sup>',
      'Change in city temperatures from 1850-2013',
      '</sup>'
    )))
