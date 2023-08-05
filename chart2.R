source("analysis.R") # access analysis.R methods and data
library(tidyverse)
library(plotly)

# Organize Year data
# global_temp <- annual_global_temp %>% 
#   mutate(dt = as.numeric(dt)) %>%
#   arrange(dt) %>%
#   rename(Year = dt, AvgTemp = LandAverageTemperature)

# Create Scatter plot of average global temp over time
# temp_plot <- ggplot(data = global_temp, aes(x = Year, y = AvgTemp)) +
#   geom_point(size = 0.75) + 
#   labs(title = "Average Global Temperature Over Time",
#        x = "Year",
#       y = "Average Temperature"
#        ) +
#   geom_smooth(method = "loess", formula = y ~ x) +
#   theme_light()

selected_continents <- c(
  "Europe", 
  "Africa", 
  "Asia", 
  "Antarctica", 
  "Oceania", 
  "North America", 
  "South America"
  )

continent_lat <- data.frame(
  Continent = c(
    "Europe",
    "Africa",
    "Asia",
    "Antarctica",
    "Oceania",
    "North America",
    "South America"
  ),
  lat = c(
    54.5260,
    8.7832,
    34.0479,
    -82.8628,
    -25.2744,
    37.0902,
    -14.2350
  )
)

continent_temp <- annual_country_temp %>%
  filter(Country %in% selected_continents) %>%
  filter(dt == max(dt)) %>%
  rename(Continent = Country) %>%
  left_join(continent_lat, by = "Continent")

temp_bar <- ggplot(
  data = continent_temp, 
  aes(x = Continent, y = AverageTemperature, fill = lat)
  ) +
  geom_bar(stat = "identity", width = 0.7) +  
  #stat = "identity" to plot the actual values rather than count
  scale_fill_gradient(low = "blue", high = "red") +
  labs(title = "Average Temperature by Continent (C)",
       x = "Continent",
       y = "Average Temperature",
       fill = "Latitude") +
  theme_light() +
  theme(axis.text.x = element_text(angle = 25, hjust = 1))
ggplotly(temp_bar)

  


