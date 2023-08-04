source("analysis.R") # access analysis.R methods and data
library(ggplot2) # plot creation
library(dplyr) # data wrangling



# Calculate the first derivative (velocity of temperature change)
# vel_global_temp <- annual_global_temp %>%
#  mutate(dt = as.numeric(dt)) %>%
#  arrange(dt) %>%
#  rename(Year = dt) %>%
#  mutate(
#    velocity = (
#      LandAverageTemperature - lag(LandAverageTemperature)) / (Year - lag(Year))
#    )

# Create the scatterplot
# vel_plot <- ggplot(data = vel_global_temp, aes(x = Year, y = velocity)) +
#   geom_point() +
#   labs(title = "Velocity of Temperature Change Over Time",
#       x = "Year",
#       y = "Velocity"
#       ) +
#  geom_smooth(method = "loess", formula = y ~ x) +
#  scale_y_log10() + theme_light()
  
# plotly to make it interative :D
#ggplotly(vel_plot)

# Organize Year data
global_temp <- annual_global_temp %>% 
  mutate(dt = as.numeric(dt)) %>%
  arrange(dt) %>%
  rename(Year = dt, AvgTemp = LandAverageTemperature)

# Create Scatter plot
temp_plot <- ggplot(data = global_temp, aes(x = Year, y = AvgTemp)) +
  geom_point(size = 0.75) + 
  labs(title = "Average Global Temperature Over Time",
       x = "Year",
       y = "Average Temperature"
       ) +
  geom_smooth(method = "loess", formula = y ~ x) +
  theme_light()

ggplotly(temp_plot)



