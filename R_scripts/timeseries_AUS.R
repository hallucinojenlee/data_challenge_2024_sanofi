library(tidyverse)
library(dplyr)
library(readxl)
library(ggplot2)
library(fmsb)

#setwd("~/OneDrive - London School of Hygiene and Tropical Medicine/2. Term 2/Data challenge/Sanofi/Datasets")
(path<-getwd())

#####################
### Manipulate AUS data 
#####################

# Read in the Australian hospitalisaiton data
master <- read_excel(paste0(path,"/Dataset/Consolidated_dataset_MASTER.xlsx"),
                     sheet="Flu")
aus <- master %>% 
        filter(Country == "Australia") %>% 
        #drop_na(hospitalisation_num) %>% 
        mutate(year_week = as.factor(paste(Year, Week_num, sep = "-")))

#####################
### Manipulate COVID data 
#####################

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

#####################
### Plotting tests 
#####################

### Timeseries (ggplot)

# Plot the yearly hospitalisation data on the same week axis
eg1 <- ggplot(aus, aes(x = Week_num, y = hospitalisation_num)) +
  geom_line(aes(group = Year, color = as.factor(Year))) +
  scale_color_manual(values = c("red", "blue", "green", "orange", "purple")) +
  xlab("Week number") +
  ylab("Number of hospitalisations") +
  theme_minimal()

# Export as png
png_eg1 <- file.path(paste0(path,"/Output/graphs/", "AUS_eg1_timeseries.png"))
ggsave(png_eg1, plot = eg1, width = 10, height = 8, dpi = 300)


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

  
### Spider plot (radarchart)

# Create new df so that each row is each year and week num in columns, showing hospitalisation for each week across the years
wide <- aus %>%
  select(Year, Week_num, hospitalisation_num) %>%
  replace(is.na(.), 0) %>%
  spread(key = Week_num, value = hospitalisation_num) %>%
  ungroup() %>%
  as.data.frame()

# Rename each row to the respective years
wide2 <- wide[,-1]
rownames(wide2) <- wide[,1]

# Create a max row and a min row
max_row <- setNames(rep(500, ncol(wide2)), names(wide2))
min_row <- setNames(rep(0, ncol(wide)), names(wide))

# Bind the max and min rows to your wide data frame
wide2 <- rbind(max_row, min_row, wide2)

# Plot and save the radar chart
png_eg2 <- file.path(paste0(path,"/Output/graphs/", "AUS_eg2_radar_chart.png"))
png(png_eg2, width = 800, height = 600)

eg2 <- radarchart(wide2,
                  cglty = 1,       # Grid line type,
                  pty = 31,        # Plot type (31 = filled)
                  plwd = 2,        # Plot line width
                  cglcol = "gray", # Grid line color
                  vlcex = 0.8,     # Label size
                  )
# Add a legend
legend_labels <- rownames(wide2)[-c(1, 2)]
legend(x="topright", legend = legend_labels, col = 1:length(legend_labels), lty=1:1, cex = 0.7)

dev.off()

# Alternative option: ggradar
# install.packages("remotes")
# remotes::install_github("ricardo-bion/ggradar")
# library(ggradar)
# 
# ggradar(wide,
#         grid.min=0,
#         grid.max=500
# )