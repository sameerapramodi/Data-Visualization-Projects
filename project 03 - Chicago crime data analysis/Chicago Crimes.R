# Install and load necessary libraries
install.packages(c("tidyverse", "lubridate", "janitor", "naniar", "viridis"))
library(tidyverse)
library(lubridate)
library(janitor)
library(naniar)
library(ggtext)
library(viridis)
library(RColorBrewer)

# 1. Load the dataset
crime_data <- read.csv("C:/Users/R E V O/Downloads/Crimes_-_2001_to_Present_20250505.csv")

# 2. Inspect the structure of the dataset
glimpse(crime_data)

# Check column names for any mismatches
str(crime_data)

# 3. Clean column names and parse date-related columns
crime_data <- crime_data %>%
  clean_names() %>%
  mutate(
    date_parsed = mdy_hms(date),  # Convert 'Date' to datetime format
    month = month(date_parsed, label = TRUE),  # Extract month
    hour = hour(date_parsed),  # Extract hour
    weekday = wday(date_parsed, label = TRUE),  # Extract weekday
    arrest = as.logical(arrest)  # Ensure arrest column is logical TRUE/FALSE
  )

# 4. Visualize missing values
gg_miss_var(crime_data) +
  labs(title = "Missing Values in Crime Data")

# 5. Crimes by hour of day
ggplot(crime_data, aes(x = hour)) +
  geom_bar(fill = "steelblue") +
  labs(title = "Crimes by Hour of Day", x = "Hour", y = "Count")

# 6. Crimes by weekday (day of the week)
ggplot(crime_data, aes(x = weekday)) +
  geom_bar(fill = "steelblue") +
  labs(title = "Crimes by Weekday", x = "Weekday", y = "Count")

# 7. Top 10 Crime Types
crime_data %>%
  count(primary_type, sort = TRUE) %>%
  top_n(10) %>%
  ggplot(aes(x = reorder(primary_type, n), y = n)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Top 10 Crime Types", x = "Crime Type", y = "Count")

# 8. Arrests by Month
crime_data %>%
  group_by(month) %>%
  summarise(arrest_count = sum(arrest, na.rm = TRUE)) %>%
  ggplot(aes(x = month, y = arrest_count)) +
  geom_col(fill = "steelblue") +
  labs(title = "Monthly Arrest Counts", x = "Month", y = "Number of Arrests")

# 9. Theft Incidents in District 10 Over the Year
crime_data %>%
  filter(primary_type == "THEFT", district == 10) %>%
  count(month) %>%
  ggplot(aes(x = month, y = n)) +
  geom_col(fill = "steelblue") +
  labs(title = "Monthly Theft Cases in District 10", x = "Month", y = "Theft Cases")


# Filter for major crime types
crime_data %>%
  filter(primary_type %in% c("THEFT", "BATTERY", "ASSAULT")) %>%
  count(primary_type, hour, weekday, district) %>%
  ggplot(aes(x = hour, y = weekday, fill = n)) +
  geom_tile(color = "white") +
  scale_fill_distiller(palette = "YlOrRd", direction = 1, name = "Crime Count") +
  facet_wrap(~ primary_type, ncol = 1) +
  labs(
    title = "Major Crime Frequency by Hour and Day of Week",
    subtitle = "Faceted by Crime Type (THEFT, BATTERY, ASSAULT)",
    x = "Hour of Day",
    y = "Day of Week"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(size = 12),
    plot.title = element_text(face = "bold")
  )

# 10. Crimes by Police District
crime_data %>%
  count(district) %>%
  ggplot(aes(x = reorder(district, n), y = n)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Crimes by Police District", x = "District", y = "Number of Crimes")

# 11. Multivariate Analysis: Crime Type, Hour, Day, and Location (Barplot)
crime_data %>%
  filter(primary_type %in% c("THEFT", "BATTERY", "ASSAULT")) %>%
  group_by(primary_type, hour, weekday) %>%
  summarise(count = n(), .groups = 'drop') %>%
  ggplot(aes(x = hour, y = count, fill = weekday)) +
  geom_col(position = "dodge") +
  facet_wrap(~ primary_type) +
  labs(
    title = "Crime Frequency by Hour and Weekday",
    x = "Hour of Day",
    y = "Number of Crimes",
    fill = "Weekday"
  ) +
  theme_minimal()

# Filter the data for Robbery and Theft crime types, evening hours, and weekends (Saturday & Sunday)
crime_data_filtered <- crime_data %>%
  filter(primary_type %in% c("ROBBERY", "THEFT"),
         hour >= 18 & hour <= 23,  # Evening hours (6 PM to 11 PM)
         weekday %in% c("Saturday", "Sunday"))  # Weekend days (Saturday and Sunday)



#Hypothesis testing
# Create the contingency matrix
arrest_data <- matrix(c(1261, 5324, 58410, 2426), 
                      nrow = 2, 
                      byrow = TRUE,
                      dimnames = list(
                        CrimeType = c("NARCOTICS", "THEFT"),
                        Arrest = c("False", "True")
                      ))

# Perform Chi-square test
chisq.test(arrest_data)
