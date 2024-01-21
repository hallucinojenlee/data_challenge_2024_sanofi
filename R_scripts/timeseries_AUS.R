library(tidyverse)
library(dplyr)
library(readxl)
library(ggplot2)
#install.packages("lubridate")
library(lubridate)
#install.packages('forecast')
library(forecast)

setwd("~/OneDrive - London School of Hygiene and Tropical Medicine/2. Term 2/Data challenge/Sanofi/Datasets")

# Read in the Australian hospitalisaiton data
master <- read_excel("Consolidated_dataset_MASTER.xlsx")
aus <- master %>% 
        filter(Country == "Australia") %>% 
        #drop_na(hospitalisation_num) %>% 
        mutate(year_week = as.factor(paste(Year, Week_num, sep = "-")))

# Read in the Covid data from 'Our World in Data' & filter for hospitalisation data
covid <- read_csv("https://covid.ourworldindata.org/data/owid-covid-data.csv")
# Manipulate the data so that it has ISO week number and year
covid_aus <- covid %>% 
  filter(location=='Australia')

covid_new_permil <- covid %>% 
  filter(location=='Australia') %>%
  select(date, new_cases_per_million, total_deaths_per_million) %>%
  mutate(year = year(date),
         Week_num = isoweek(date)) %>% 
  filter(year > 2016 & year < 2024) %>%
  filter(Week_num > 13 & Week_num < 45) %>% 
  replace(is.na(.), 0)

covid_new_permil_weekly <- covid_new_permil %>% 
  group_by(year, Week_num) %>%
  #summarise(new_cases_per_million = sum(new_cases_per_million), .groups = 'drop') %>% 
  summarise(total_deaths_per_million = sum(total_deaths_per_million), .groups = 'drop') %>%
  mutate(year_week = paste(year, Week_num, sep = "-"))

# Plot the yearly hospitalisation data on the same week axis
ggplot(aus, aes(x = Week_num, y = hospitalisation_num)) +
  geom_line(aes(group = Year, color = as.factor(Year))) +
  scale_color_manual(values = c("red", "blue", "green", "orange", "purple", "black")) +
  xlab("Week number") +
  ylab("Number of hospitalisations") +
  theme_minimal()

# Plot the Australian hospitalisation data against the Covid data
ggplot(aus, aes(x=year_week, y=hospitalisation_num)) +
  geom_line(aes(group=1), color='red') +
  geom_line(data = covid_new_permil_weekly, aes(x = year_week, y = total_deaths_per_million, group=1), color='blue') +
  xlab('')
  theme_minimal() 
  #geom_line(data = covid_new_permil_weekly, aes(x = year_week, y = new_cases_per_million)) +
  xlab("Week number") +
  ylab("Number of hospitalisations") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))  # Rotate x-axis text if needed

### Diamond plot
  
# Plot the hospitalisation on diamond plot
hos_ts <- ts(aus$hospitalisation_num, start = c(2017, 14), frequency = 30)
week_ts <- ts(aus$Week_num, start = c(2017, 14), frequency = 29)
ggseasonplot(week_ts, polar=T)
ggseasonplot(hos_ts, polar=T, continuous = T, ylab = "Hospitalisations", xlab = "Week number")

# Subset the data to only include the years 2017-2019
aus_17_19 <- aus %>% 
  filter(Year > 2016 & Year < 2020) %>% 
  arrange(Year, Week_num)
# Transform the data into a time series
aus1719_ts <- ts(aus_17_19$hospitalisation_num, start = c(2017, 14), frequency = 30)
ggplot(aus_17_19)

ts17 <- aus %>% 
  filter(Year == 2017) %>%
  ts(aus$hospitalisation_num, start = c(2017, 14), frequency = 28)

# Subset the data to onky incude the yaers 2022-2023
aus_22_23 <- aus %>% 
  filter(Year > 2021 & Year < 2024)
# Transform the data into a time series
aus2223_ts <- ts(aus_22_23$hospitalisation_num, start = c(2022, 14), end = c(2023, 43))

# Combine the two 2 time series filling 2020-2021 with NAs
aus_ts <- ts(c(aus1719_ts,rep(NA, 2 * 29),  aus2223_ts), start = c(2017, 14), frequency = 29)

# Plot the time series data
week <- as.character(14:43)
ggseasonplot(aus_ts, year.labels = TRUE) +
  scale_x_continuous(labels = aus$Week_num)
