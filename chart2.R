source("analysis.R")
library(ggplot2)

x_values <- seq(1, 3)
y_values <- seq(1,3)

ggplot() +
  geom_point(aes(x=x_values, y = y_values))
