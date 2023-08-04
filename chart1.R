source("analysis.R") # access analysis.R methods and data
library(ggplot2) # create plots
library(dplyr) # data wrangling
library(scales) # for map creation
library(maps) # for map creation
library(readr) # allows for read_csv to create tibble
library(plotly) # adds interactive labels on graph


# Create a table for data set country names and world map country names
country_mapping <- data.frame(
  Country = c(
    "United States", 
    "Bonaire, Saint Eustatius And Saba", 
    "Congo", 
    "Falkland Islands (Islas Malvinas)",
    "Heard Island And Mcdonald Islands",
    "Antigua And Barbuda",
    "United Kingdom",
    "Trinidad And Tobago",
    "Saint Vincent And The Grenadines",
    "Isle Of Man",
    "Curaçao",
    "Palestina",
    "South Georgia And The South Sandwich Isla",
    "Timor Leste",
    "Turks And Caicas Islands",
    "Saint Pierre And Miquelon",
    "Sao Tome And Principe",
    "Bosnia And Herzegovina",
    "Congo (Democratic Republic Of The)",
    "Côte D'Ivoire",
    "Burma",
    "French Southern And Antarctic Lands",
    "Federated States Of Micronesia",
    "Guinea Bissau",
    "Saint Kitts And Nevis",
    "Saint Barthélemy",
    "Macedonia"
    ),
  country_corrected = c(
    "USA", 
    "Saba", 
    "Republic of Congo", 
    "Falkland Islands",
    "Heard Island",
    "Antigua",
    "UK",
    "Trinidad",
    "Saint Vincent",
    "Isle of Man",
    "Curacao",
    "Palestine",
    "South Georgia",
    "Timor-Leste",
    "Turks and Caicos Islands",
    "Saint Pierre and Miquelon",
    "Sao Tome and Principe",
    "Bosnia and Herzegovina",
    "Democratic Republic of the Congo",
    "Ivory Coast",
    "Myanmar",
    "French Southern and Antarctic Lands",
    "Micronesia",
    "Guinea-Bissau",
    "Saint Kitts",
    "Saint Barthelemy",
    "North Macedonia"
    )
)

# South Sudan is not included in the dataset for some reason
# Solution: 
# Duplicate values for Sudan and clone them into South Sudan
duplicated_rows <- annual_country_temp %>%
  filter(Country == "Sudan") %>%
  mutate(Country = "South Sudan")
annual_country_temp <- bind_rows(annual_country_temp, duplicated_rows)

# Merge the name correction table with the data to correct country names
aggregate_country_temp <- annual_country_temp %>%
  filter(dt == max(dt)) %>%
  left_join(country_mapping, by = "Country") %>%
  mutate(country_corrected = 
           ifelse(!is.na(country_corrected), country_corrected, Country))
# Remove countries not found in world map country names
aggregate_country_temp <- select(aggregate_country_temp, -Country)

# world map from ggplot2
world_map <- map_data('world')

# joining world map with country data
world_temp_map <- left_join(
  aggregate_country_temp, world_map, by = c("country_corrected" = "region"))

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

# creating map with ggplot2
temp_map <- ggplot(data = world_temp_map) +
  geom_polygon(mapping = aes(x = long,
                             y = lat,
                             group = group,
                             fill = AverageTemperature,
                             text = paste("Country: ", country_corrected, "<br>"
                                          ))) +
  scale_fill_continuous(low = "grey",
                        high = "red",
                        limits = c(-20, 40)) +
  labs(title = "Average Temperature By Country",
       fill = "Temperature (C)") + blank_theme

# plotly for interactivity :D
ggplotly(temp_map)
