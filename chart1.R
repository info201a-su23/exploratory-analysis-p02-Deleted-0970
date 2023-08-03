library(ggplot2) # create plots
library(dplyr) # data wrangling
library(scales) 
library(maps)
library(mapproj) # allows for coord_map() to prevent stretching
library(readr) # allows for read_csv to create tibble
options(scipen = 999) # removes scientific notation

filename <- paste0()

data <- read_csv(filename, stringsAsFactors = FALSE)

