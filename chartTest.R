source("analysis.R") # access analysis.R methods and data
library(ggplot2) # plot creation
library(dplyr) # data wrangling



# Calculate the first derivative (velocity of temperature change)
 vel_global_temp <- annual_global_temp %>%
  mutate(dt = as.numeric(dt)) %>%
  arrange(dt) %>%
  rename(Year = dt) %>%
  mutate(
    velocity = (
      LandAverageTemperature - lag(LandAverageTemperature)) / (Year - lag(Year))
    )

# Create the scatterplot
vel_plot <- ggplot(
   data = vel_global_temp, 
   aes(
     x = Year, 
     y = velocity, 
     size = LandAverageTemperatureUncertainty,
     alpha = 0.5)
   ) +
   geom_point() +
   labs(title = "Velocity of Temperature Change Over Time",
       x = "Year",
       y = "Velocity",
       size = "Temperature Uncertainty"
       ) +
  scale_y_log10() + theme_light() +
   guides(size = guide_legend(
     title = "Temperature Uncertainty")) +
   scale_size_continuous(range = c(1, 7)) +
   scale_alpha_continuous(range = c(0.2, 1)) +
  geom_smooth(method = "loess", formula = y ~ x, se = FALSE)

# plotly to make it interactive :D
ggplotly(vel_plot)