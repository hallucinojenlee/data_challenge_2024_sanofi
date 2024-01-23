library(tidyverse)
library(dplyr)
library(readxl)
library(ggplot2)
library(plyr)

(path<-getwd())

#####################
### Create Southern Hemi dataset
#####################

master <- read_excel(paste0(path,'/Dataset/Consolidated_dataset_MASTER.xlsx'),
                     sheet='Flu')
southern <- master %>% 
  filter(Country == 'Australia' | Country == 'Brazil') %>% 
  mutate(Year_week=paste(Year,Week_num,"1",sep='_'),
         Date=as.Date(Year_week,'%Y_%W_%u'))

aus <- southern %>% 
  filter(Country == 'Australia', Month >=3, Month <= 10)

brazil <- southern %>%
  filter(Country == 'Brazil', Month >=3, Month <= 10, Year != 2020, Year != 2021) # flu seaseon only minus 2020 and 2021 (same format as Aus)

#####################
### Plotting
#####################
# Eg. 1: Timeseries
eg1 <- ggplot() +
  geom_line(data=aus, aes(x=Date, y=hospitalisation_num), color='skyblue', size=0.7) +
  geom_line(data=brazil, aes(x=Date, y=hospitalisation_num), color='red', size=0.7) +
  theme_minimal()
  
# Export as png
png_eg1 <- file.path(paste0(path,"/Output/graphs/", "SOUTHERN_eg1_timeseries.png"))
ggsave(png_eg1, plot = eg1, width = 10, height = 8, dpi = 300)

# Eg. 2: Plot on the same week axis
# Plot 1: Aus & Brazil separately
eg2 <- ggplot() +
  geom_line(data=aus,aes(x=Week_num, y=hospitalisation_num, group = Year, color = as.factor(Year))) +
  geom_line(data=brazil, aes(x=Week_num, y=hospitalisation_num, group = Year, color = as.factor(Year))) +
  xlab("Week number") +
  ylab("Number of hospitalisations") +
  theme_minimal()

# Export as png
png_eg2 <- file.path(paste0(path,"/Output/graphs/", "SOUTHERN_eg2_timeseries_week_sep.png"))
ggsave(png_eg2, plot = eg2, width = 10, height = 8, dpi = 300)

# Plot 2: Aus & Brazil together
# Create a joined dataset: 2019, 2022, 2023 only (AUS missing 2020 & 2021, BRA missing 2017 & 2018)
combined <- aus %>% 
  inner_join(brazil, by = 'Year_week') %>%
  mutate(total_hospitalisation_num = hospitalisation_num.x + hospitalisation_num.y) 

# Plot
eg3 <- ggplot(combined, aes(x = Week_num.y, y = total_hospitalisation_num)) +
  geom_line(aes(group = Year.x, color = as.factor(Year.x))) +
  xlab("Week number") +
  ylab("Number of hospitalisations") +
  theme_minimal()

# Export as png
png_eg3 <- file.path(paste0(path,"/Output/graphs/", "SOUTHERN_eg1_timeseries_week_combined.png"))
ggsave(png_eg3, plot = eg3, width = 10, height = 8, dpi = 300)
