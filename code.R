# Load necessary libraries
library(tidyverse)
library(ggplot2)
library(lubridate)

# Load datasets
country_metadata <- read_csv("C:/Users/Bikash Kumbhakar/OneDrive/Desktop/FL/July/17.07.24/country_metadata.csv")
View(country_metadata)
library(readr)
country_data <- read_csv("C:/Users/Bikash Kumbhakar/OneDrive/Desktop/FL/July/17.07.24/country_data (1).csv")



# Inspect datasets
str(country_data)
str(country_metadata)

# Check column names
names(country_data)
names(country_metadata)

# Merge datasets on 'location'
combined_data <- merge(country_data, country_metadata, by = 'location')

# Convert date columns to Date type, handle missing values, etc.
combined_data$date <- ymd(combined_data$date)
combined_data <- combined_data %>% filter(!is.na(date))


# Define countries of interest
countries_of_interest <- c('Ireland', 'Denmark', 'Falkland Islands', 'Germany', 'Guernsey', 'Country6', 'Country7', 'Country8', 'Country9', 'Country10')

# Filter data for Ireland and the 9 other countries
filtered_data <- combined_data %>% filter(location %in% countries_of_interest)

# Summarize data for analysis
summary_data <- filtered_data %>%
  group_by(location, year = year(date)) %>%
  summarize(total_cases = sum(total_cases, na.rm = TRUE), total_deaths = sum(total_deaths, na.rm = TRUE))


# Inspect the column names of country_metadata
names(country_metadata)


library(maps)
world_map <- map_data("world")

ggplot() +
  geom_map(data = world_map, map = world_map,
           aes(x = long, y = lat, map_id = region),
           fill = "white", color = "black") +
  geom_point(data = country_metadata, 
             aes(x = longitude, y = latitude, size = population),
             color = "red", alpha = 0.5) +
  labs(title = "World Map Chart", x = "Longitude", y = "Latitude") +
  theme_minimal()




# Install and load necessary packages
install.packages("ggplot2")
install.packages("dplyr")
install.packages("rnaturalearth")
install.packages("rnaturalearthdata")
install.packages("sf")
install.packages("viridis")  # For color scales

library(ggplot2)
library(dplyr)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(viridis)



# Load the world map data
world <- ne_countries(scale = "medium", returnclass = "sf")

# Merge your data with the world map data
map_data <- merge(world, country_data, by.x = "name", by.y = "location", all.x = TRUE)

# Create the World Map Chart
ggplot(data = map_data) +
  geom_sf(aes(fill = total_cases), color = "white") +
  scale_fill_viridis(option = "plasma", na.value = "grey50") +
  labs(title = "World Map of Total COVID-19 Cases by Country",
       fill = "Total Cases") +
  theme_minimal()


# Generate a bar chart of total COVID-19 cases by country
ggplot(summary_data, aes(x = location, y = total_cases, fill = location)) +
  geom_bar(stat = "identity") +
  labs(title = "Total COVID-19 Cases by Country", x = "Country", y = "Total Cases") +
  theme_minimal()



# Generate a scatterplot with a linear regression line
ggplot(filtered_data, aes(x = date, y = total_cases, color = location)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Scatterplot with Linear Regression Line", x = "Date", y = "Total Cases") +
  theme_minimal()



# Generate a time-series chart of COVID-19 cases
ggplot(filtered_data, aes(x = date, y = total_cases, color = location)) +
  geom_line() +
  labs(title = "Time-Series Chart of COVID-19 Cases", x = "Date", y = "Total Cases") +
  theme_minimal()



