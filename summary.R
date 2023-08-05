library(tidyverse)
library(knitr)
source("analysis.R")

# Create summary tables here of data aggregated by year, min, max, and average.
global_summary_table <- global_annual_summary()
country_summary_table <- country_annual_summary()
# city_summary_table <- city_annual_summary()


# Calculating values for summary
temp_store <- global_temp_change(1750, 2015)
avg_global_temp_change <- round(temp_store$LandAverageTemperature, 2)

temp_store <- global_min_avg_temp(1750, 2015)
lowest_global_temp <- round(temp_store$LandAverageTemperature, 2)

temp_store <- global_max_avg_temp(1750, 2015)
highest_global_temp <- round(temp_store$LandAverageTemperature, 2)

temp_store <- global_med_avg_temp(1750, 2015)
median_global_temp <- round(temp_store$LandAverageTemperature, 2)

temp_store <- global_avg_temp(1750, 2015)
average_global_temp <- round(temp_store$LandAverageTemperature, 2)