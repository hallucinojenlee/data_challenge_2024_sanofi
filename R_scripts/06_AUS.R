library(tidyverse)
library(dplyr)
library(readxl)
library(ggplot2)
#library(fmsb)

# Set working directory
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
### Manipulate COVID data (not used in the end)
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
### Plotting graphs 
#####################

## LSHTM colour palette - HEX:
# dark green: #004550
# light green: #2aac6c
# sky blue: #01abce
# yellow: #fbb800
# orange: #e95b0d
# grey: #b1b2b3

### Timeseries (ggplot)

# Plot the yearly hospitalisation data on the same week axis
eg1 <- ggplot(aus, aes(x = Week_num, y = hospitalisation_num)) +
  geom_line(aes(group = Year, color = as.factor(Year)), size=0.8) +
  scale_color_manual(values = c("#f4b18d", "#f77935", "#b1b2b3", "#18cdf1", "#088199")) +
  xlab("Week number") +
  ylab("Number of hospitalisations") +
  ggtitle("Number of Influenza hospitalisations in Australia") +
  labs(color = "Year") +
  theme_minimal() + 
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        legend.title = element_text(size=20), 
        legend.text = element_text(size=20),
        legend.position=c(0.1,0.85),
        panel.grid.minor = element_blank(), 
        text = element_text(size=20,
                          family="Arial"),
        axis.line = element_line(colour = "black"))

# Export as png
png_eg1 <- file.path(paste0(path,"/Output/graphs/06_Australia/", "AUS_eg1_timeseries.png"))
ggsave(png_eg1, plot = eg1, width = 10, height = 8, dpi = 300)



# Plot the Australian hospitalisation data against the Covid data (not used in the end)
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

  
### Spider plot (radarchart) (not used in the end)

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
png(png_eg2, width = 600, height = 600)

colours=c("#f4b18d", "#f77935", "#b1b2b3", "#18cdf1", "#088199")

eg2 <- radarchart(wide2,
                  pcol=colours,
                  cglty = 1,       # Grid line type,
                  pty = 31,        # Plot type (31 = filled)
                  plwd = 1.7,        # Plot line width
                  seg = 2,         # Number of segments
                  cglwd = 0.1,     # Grid line width
                  cglcol = "gray", # Grid line color
                  vlcex = 0.8,     # Label size,
                  )
# Add a legend
legend_labels <- rownames(wide2)[-c(1, 2)]
legend(x="topleft", legend = legend_labels, col = colours[1:length(legend_labels)], lty=1:1, cex = 0.7)

dev.off()
