source("analysis.R") # access analysis.R methods and data
library(tidyverse)
# library(plotly) # for plotting

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

continent_temp <- annual_country_temp %>%
  filter(Country %in% selected_continents) %>%
  filter(dt == max(dt))

temp_bar <- ggplot(
  data = continent_temp, 
  aes(x = Country, y = AverageTemperature, fill = AverageTemperature)
  ) +
  geom_bar(stat = "identity") +  
  #stat = "identity" to plot the actual values rather than count
  scale_fill_gradient(low = "blue", high = "red") +
  labs(title = "Average Temperature by Continent (C)",
       x = "Continent",
       y = "Average Temperature",
       fill = "Average Temperature") +
  theme_light() + coord_fixed()

ggplotly(temp_bar)

  


