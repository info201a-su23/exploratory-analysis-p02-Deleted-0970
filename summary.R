library(tidyverse)
library(knitr)
source("analysis.R")

# Create a summary table here
global_summary_table <- global_annual_summary()
country_summary_table <- country_annual_summary()
city_summary_table <- city_annual_summary()
