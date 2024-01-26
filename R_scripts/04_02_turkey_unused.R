#### This is the country analysis for turkey ####
#Date: 21 JAN 2024
#Author: Kai (with thanks to script from Jenny^^)
library(tidyverse)
library(dplyr)
library(readxl)
library(ggplot2)
library(fmsb)


#setwd("C:/Users/three/Neu Kasten_5440/022 master degree LSHTM/LSHTM/013 Data Challenge/My Sanofi_5440/data_challenge_2024_sanofi")
(path<-getwd())


##read data
df_raw<-read_excel(paste0(path,"/Dataset/Consolidated_dataset_MASTER.xlsx"),
                   sheet="Flu",
                   col_types = c("text","text","numeric","numeric","numeric","text","text","numeric","numeric","numeric","numeric","numeric"))


df<-df_raw[df_raw$Country=="Türkiye" & df_raw$Year!=2024,]

#prepare variables 
df$year_week<-paste0( df$Year,"-" ,df$Week_num )

df$hospitalisation_rate<-(df$hospitalisation_num /df$Population) *1000000 #aligning with covid 1000000


##############################
## COVID data preparation ####
##############################
covid <- read_csv("https://covid.ourworldindata.org/data/owid-covid-data.csv")
#https://ourworldindata.org/grapher/daily-covid-cases-deaths?country=~TUR

unique(covid$location)

names(covid)
covid_turkey <- covid %>% 
  filter(location=='Turkey') %>%
  select(date, new_cases_per_million, total_deaths_per_million) %>%
  #hosp_patients, hosp_patients_per_million missing
  mutate(year = year(date),
         Week_num = isoweek(date)) %>% 
  filter(year > 2016 & year < 2024) 


covid_turkey_weeklycase <- covid_turkey %>% 
  group_by(year, Week_num) %>%
  summarise(new_cases_per_million = sum(new_cases_per_million, na.rm=TRUE),#watch out the NA
            .groups = 'drop') %>%
  mutate(year_week = paste(year, Week_num, sep = "-"))


###################
## Time series ####
###################

#1. hosp
# Plot the yearly hospitalisation data on the same week axis
timeseries_turkiye_flu_nocovid <- ggplot(df, 
        aes(x = Week_num, y = hospitalisation_num)) +
  geom_line(aes(group = Year, color = as.factor(Year))) +
 # scale_color_manual(values = c("red", "blue", "green", "orange", "purple")) +
  labs(title="Influenza hospitalizations, Türkiye",   
       x="Week number", 
       y="Number of hospitalisations")+
  theme_minimal()
print(timeseries_turkiye_flu_nocovid)

ggsave("Output/graphs/02_Turkiye_flu_hosp_nocovid.png",
       plot = timeseries_turkiye_flu_nocovid, 
       width = 10, height = 8, dpi = 300)


# 2. rate
timeseries_turkiye_flu_nocovid_rate <- ggplot(df, 
                                         aes(x = Week_num, y = hospitalisation_rate)) +
  geom_line(aes(group = Year, color = as.factor(Year))) +
  # scale_color_manual(values = c("red", "blue", "green", "orange", "purple")) +
  labs(title="Influenza Hospitalization Rate, Türkiye",   
       x="Week number", 
       y="Hospitalisation Rate (per 1,000,000)")+
  theme_minimal()


ggsave("Output/graphs/02_Turkiye_flu_rate_nocovid.png",
       plot = timeseries_turkiye_flu_nocovid_rate, 
       width = 10, height = 8, dpi = 300)


#################################
## time series against COVID ####
#################################

timeseries_turkiye_flu_covid_rate<-ggplot(
       df, 
       aes(x=year_week, 
           y=hospitalisation_rate)) +
  geom_line(aes(group=1), color='red') +
  
  geom_line(data = covid_turkey_weeklycase, 
            aes(x = year_week, 
                y = new_cases_per_million, 
                group=1), 
            color='blue') +
  xlab('')+
  theme_minimal() +
  labs(title = "Influenza and COVID-19 hospitalisation rate, Türkiye (per 1,000,000)",
       x="Year-Week number",
       y="hospitalisation rate (per 1,000,000)")+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))  # Rotate x-axis text if needed



ggsave("Output/graphs/02_Turkiye_flu_rate_covid.png",
       plot = timeseries_turkiye_flu_covid_rate, 
       width = 10, height = 8, dpi = 300)



#################################
## time series against COVID, difference of  ####
#################################

#calculate baseline
temp<-df |>
  filter(Year < 2020)|>
  group_by(Week_num) |>
  summarise_at(vars(hospitalisation_rate), list(hospitalisation_rate_mean_prepandemic = mean))

df<-merge(df, temp,
      all.x=TRUE,
      by="Week_num")
df$hospitalisation_rate_difference<- df$hospitalisation_rate - df$hospitalisation_rate_mean_prepandemic

timeseries_flu_rate_difference <- ggplot(df[df$Year >= 2020 ,], 
  aes(x = Week_num, y = hospitalisation_rate_difference)) +
  geom_line(aes(group = Year, color = as.factor(Year))) +
  labs(title="Difference in Influenza Hospitalization Rate, compared to Prepandemic, Türkiye",   
       x="Week number", 
       y="Difference in Hospitalisation Rate (per 1,000,000)")+
  theme_minimal()
print(timeseries_flu_rate_difference)
