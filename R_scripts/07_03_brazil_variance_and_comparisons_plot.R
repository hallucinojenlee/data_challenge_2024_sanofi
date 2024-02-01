library(lubridate)
library(tidyverse)
library(dplyr)
library(rio) 
library(ggplot2)

path <- 'https://github.com/hallucinojenlee/data_challenge_2024_sanofi/raw/main/Dataset/Consolidated_dataset_MASTER.xlsx'

#Flu

#reading
mast <- import_list(path) 
flu <- mast$Flu
flu <- flu %>% filter(Country == 'Brazil')
flu$Week_date <- as.numeric(flu$Week_date)
flu$Week_date <- as.Date(flu$Week_date, origin = "1899-12-30")
flu$Week_num <- isoweek(flu$Week_date)
flu$Year <- isoyear(flu$Week_date)
brazil_clean <- flu
brazil_clean[brazil_clean$Month == 12 & brazil_clean$Week_num == 1,"Month"] <- 1
brazil_clean <- brazil_clean %>% group_by(Year, Month, Week_num, Week_date, hospitalisation_rate) %>% 
  summarise(hospitalisation_num = sum(hospitalisation_num, na.rm = TRUE))


#comparison
brazil_clean$kat_year <- NA
brazil_clean[brazil_clean$Week_num %in% c(9:53) & brazil_clean$Year %in% c(2019),"kat_year"] <- '2019/2020'
brazil_clean[brazil_clean$Week_num %in% c(1:8) & brazil_clean$Year %in% c(2020),"kat_year"] <- '2019/2020'
brazil_clean[brazil_clean$Week_num %in% c(9:53) & brazil_clean$Year %in% c(2020),"kat_year"] <- '2020/2021'
brazil_clean[brazil_clean$Week_num %in% c(1:8) & brazil_clean$Year %in% c(2021),"kat_year"] <- '2020/2021'
brazil_clean[brazil_clean$Week_num %in% c(9:53) & brazil_clean$Year %in% c(2021),"kat_year"] <- '2021/2022'
brazil_clean[brazil_clean$Week_num %in% c(1:8) & brazil_clean$Year %in% c(2022),"kat_year"] <- '2021/2022'
brazil_clean[brazil_clean$Week_num %in% c(9:53) & brazil_clean$Year %in% c(2022),"kat_year"] <- '2022/2023'
brazil_clean[brazil_clean$Week_num %in% c(1:8) & brazil_clean$Year %in% c(2023),"kat_year"] <- '2022/2023'
brazil_clean[brazil_clean$Week_num %in% c(9:53) & brazil_clean$Year %in% c(2023),"kat_year"] <- '2023/2024'
brazil_clean[brazil_clean$Week_num %in% c(1:8) & brazil_clean$Year %in% c(2024),"kat_year"] <- '2023/2024'
brazil_clean <- brazil_clean[-c(1:8), ]

plot(x = brazil_clean$Week_date, brazil_clean$hospitalisation_num, type = 'l')

#plot
brazil_clean <- as.data.frame(brazil_clean)
x <- brazil_clean %>% select(Week_num, hospitalisation_num, kat_year) 
x_p <- x %>%
  pivot_wider(names_from = kat_year,
              values_from = hospitalisation_num)
x_p$Week_num <- as.numeric(x_p$Week_num)
x_p$Week_num <- factor(x_p$Week_num, levels=c(9:53,1:8))
x_p$Week_num
x_p <- x_p[!x_p$Week_num == "53", ]
everyother <- function(x) x[seq_along(x) %% 4 == 0]
ggplot() +
  geom_line(data = x_p , aes(x = Week_num, `2019/2020`, group = 1, color = '2019/2020'), size = 1) +
  geom_line(data = x_p , aes(x = Week_num, `2022/2023`, group = 1, color = '2022/2023'), size = 1) +
  geom_line(data = x_p , aes(x = Week_num, `2023/2024`, group = 1, color = '2023/2024'), size = 1) +
  scale_x_discrete(name = 'Calendar week number', breaks = everyother) + scale_color_manual('Year',values = c("#b1b2b3",  
                                                                                                              "#18cdf1", "#088199")) +
  
  scale_y_continuous(name="No of Hospitalisation",expand=c(0,0),
                     limits=c(0,1200)) +
  theme(panel.border = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.background = element_rect(fill = "transparent"),
        legend.position=c(0.1,0.9),
        text=element_text(size=14,family="Arial"),
        plot.title=element_text(hjust=0.5)) 

#RSV

#reading
mast <- import_list(path) 
rsv <- mast$RSV
rsv <- rsv %>% filter(Country == 'Brazil')
rsv$Week_date <- as.numeric(rsv$Week_date)
rsv$Week_date <- as.Date(rsv$Week_date, origin = "1899-12-30")
rsv$Week_num <- isoweek(rsv$Week_date)
rsv$Year <- isoyear(rsv$Week_date)
brazil_clean <- rsv
brazil_clean[brazil_clean$Month == 12 & brazil_clean$Week_num == 1,"Month"] <- 1
brazil_clean <- brazil_clean %>% group_by(Year, Month, Week_num, Week_date, hospitalisation_rate) %>% 
  summarise(hospitalisation_num = sum(hospitalisation_num, na.rm = TRUE))

#comparison
brazil_clean$kat_year <- NA
brazil_clean[brazil_clean$Week_num %in% c(9:53) & brazil_clean$Year %in% c(2019),"kat_year"] <- '2019/2020'
brazil_clean[brazil_clean$Week_num %in% c(1:8) & brazil_clean$Year %in% c(2020),"kat_year"] <- '2019/2020'
brazil_clean[brazil_clean$Week_num %in% c(9:53) & brazil_clean$Year %in% c(2020),"kat_year"] <- '2020/2021'
brazil_clean[brazil_clean$Week_num %in% c(1:8) & brazil_clean$Year %in% c(2021),"kat_year"] <- '2020/2021'
brazil_clean[brazil_clean$Week_num %in% c(9:53) & brazil_clean$Year %in% c(2021),"kat_year"] <- '2021/2022'
brazil_clean[brazil_clean$Week_num %in% c(1:8) & brazil_clean$Year %in% c(2022),"kat_year"] <- '2021/2022'
brazil_clean[brazil_clean$Week_num %in% c(9:53) & brazil_clean$Year %in% c(2022),"kat_year"] <- '2022/2023'
brazil_clean[brazil_clean$Week_num %in% c(1:8) & brazil_clean$Year %in% c(2023),"kat_year"] <- '2022/2023'
brazil_clean[brazil_clean$Week_num %in% c(9:53) & brazil_clean$Year %in% c(2023),"kat_year"] <- '2023/2024'
brazil_clean[brazil_clean$Week_num %in% c(1:8) & brazil_clean$Year %in% c(2024),"kat_year"] <- '2023/2024'
brazil_clean <- brazil_clean[-c(1:8), ]

plot(x = brazil_clean$Week_date, brazil_clean$hospitalisation_num, type = 'l')

#plot
brazil_clean <- as.data.frame(brazil_clean)
x <- brazil_clean %>% select(Week_num, hospitalisation_num, kat_year) 
x_p <- x %>%
  pivot_wider(names_from = kat_year,
              values_from = hospitalisation_num)
x_p$Week_num <- as.numeric(x_p$Week_num)
x_p$Week_num <- factor(x_p$Week_num, levels=c(9:53,1:8))
x_p$Week_num
x_p <- x_p[!x_p$Week_num == "53", ]
everyother <- function(x) x[seq_along(x) %% 4 == 0]
ggplot() +
  geom_line(data = x_p , aes(x = Week_num, `2019/2020`, group = 1, color = '2019/2020'), size = 1) +
  geom_line(data = x_p , aes(x = Week_num, `2022/2023`, group = 1, color = '2022/2023'), size = 1) +
  geom_line(data = x_p , aes(x = Week_num, `2023/2024`, group = 1, color = '2023/2024'), size = 1) +
  scale_x_discrete(name = 'Calendar week number', breaks = everyother) + scale_color_manual('Year', values = c("#b1b2b3",  
                                                                                                               "#18cdf1", "#088199")) +
  
  scale_y_continuous(name="No of Hospitalisation",expand=c(0,0),
                     limits=c(0,1500)) +
  theme(panel.border = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.background = element_rect(fill = "transparent"),
        legend.position=c(0.9,0.9),
        text=element_text(size=14,family="Arial"),
        plot.title=element_text(hjust=0.5)) 

# RSV-FLu time series
mast <- import_list(path) 
flu <- mast$Flu
flu <- flu %>% filter(Country == 'Brazil')
flu$Week_date <- as.numeric(flu$Week_date)
flu$Week_date <- as.Date(flu$Week_date, origin = "1899-12-30")
flu$Week_num <- isoweek(flu$Week_date)
flu$Year <- isoyear(flu$Week_date)

mast <- import_list(path) 
rsv <- mast$RSV
rsv <- rsv %>% filter(Country == 'Brazil')
rsv$Week_date <- as.numeric(rsv$Week_date)
rsv$Week_date <- as.Date(rsv$Week_date, origin = "1899-12-30")
rsv$Week_num <- isoweek(rsv$Week_date)
rsv$Year <- isoyear(rsv$Week_date)

brazil_clean <- merge(flu, rsv[ , c("Year", "Week_num", 'Month', 'hospitalisation_num')], by.x=c("Year", "Week_num", 'Month'), by.y=c("Year", "Week_num", 'Month'))

ggplot() +
  geom_line(data=brazil_clean,aes(x=Week_date,y=hospitalisation_num.x),color="#088199",size=0.7) +
  geom_line(data=brazil_clean,aes(x=Week_date,y=hospitalisation_num.y),color="#f77935",size=0.7) + scale_x_date(date_labels="%yW%W",date_breaks="2 months") +
  theme(panel.grid.major = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.background = element_rect(fill = "transparent"),
        axis.text.x = element_text(angle=90, vjust = 0.5, hjust=1),
        plot.title=element_text(hjust=0.5)) + labs(y="No of Hospitalisation", x = 'Year-Week')


#flu A and B
mast <- import_list(path) 
flu <- mast$Flu
flu <- flu %>% filter(Country == 'Brazil')
flu$Week_date <- as.numeric(flu$Week_date)
flu$Week_date <- as.Date(flu$Week_date, origin = "1899-12-30")
flu$Week_num <- isoweek(flu$Week_date)
flu$Year <- isoyear(flu$Week_date)
brazil_clean <- flu
brazil_clean <- as.data.frame(brazil_clean)  
#reading
mast <- import_list(path) 
flu <- mast$Flu
flu <- flu %>% filter(Country == 'Brazil')
flu$Week_date <- as.numeric(flu$Week_date)
flu$Week_date <- as.Date(flu$Week_date, origin = "1899-12-30")
flu$Week_num <- isoweek(flu$Week_date)
flu$Year <- isoyear(flu$Week_date)
brazil_clean <- flu
brazil_clean[brazil_clean$Month == 12 & brazil_clean$Week_num == 1,"Month"] <- 1
brazil_clean <- brazil_clean %>% group_by(Year, Month, Week_num, Week_date, hospitalisation_rate) %>% 
  summarise(hospitalisation_num = sum(hospitalisation_num, na.rm = TRUE))


#comparison
brazil_clean$kat_year <- NA
brazil_clean[brazil_clean$Week_num %in% c(9:53) & brazil_clean$Year %in% c(2019),"kat_year"] <- '2019/2020'
brazil_clean[brazil_clean$Week_num %in% c(1:8) & brazil_clean$Year %in% c(2020),"kat_year"] <- '2019/2020'
brazil_clean[brazil_clean$Week_num %in% c(9:53) & brazil_clean$Year %in% c(2020),"kat_year"] <- '2020/2021'
brazil_clean[brazil_clean$Week_num %in% c(1:8) & brazil_clean$Year %in% c(2021),"kat_year"] <- '2020/2021'
brazil_clean[brazil_clean$Week_num %in% c(9:53) & brazil_clean$Year %in% c(2021),"kat_year"] <- '2021/2022'
brazil_clean[brazil_clean$Week_num %in% c(1:8) & brazil_clean$Year %in% c(2022),"kat_year"] <- '2021/2022'
brazil_clean[brazil_clean$Week_num %in% c(9:53) & brazil_clean$Year %in% c(2022),"kat_year"] <- '2022/2023'
brazil_clean[brazil_clean$Week_num %in% c(1:8) & brazil_clean$Year %in% c(2023),"kat_year"] <- '2022/2023'
brazil_clean[brazil_clean$Week_num %in% c(9:53) & brazil_clean$Year %in% c(2023),"kat_year"] <- '2023/2024'
brazil_clean[brazil_clean$Week_num %in% c(1:8) & brazil_clean$Year %in% c(2024),"kat_year"] <- '2023/2024'
brazil_clean <- brazil_clean[-c(1:8), ]

ggplot(data = brazil_clean, aes(x=Week_num, y=Flu_B)) + geom_line(aes(colour=kat_year)) + theme_minimal()
x <- brazil_clean %>% select(Week_num, Flu_A, kat_year) 


x_p <- x %>%
  pivot_wider(names_from = kat_year,
              values_from = Flu_A)
x_p$Week_num <- as.numeric(x_p$Week_num)
x_p$Week_num <- factor(x_p$Week_num, levels=c(9:53,1:8))
x_p <- x_p[!x_p$Week_num == "53", ]
everyother <- function(x) x[seq_along(x) %% 4 == 0]
a <- ggplot() +
  geom_line(data = x_p , aes(x = Week_num, `2019/2020`, group = 1, color = '2019/2020'), size = 1) +
  geom_line(data = x_p , aes(x = Week_num, `2022/2023`, group = 1, color = '2022/2023'), size = 1) +
  geom_line(data = x_p , aes(x = Week_num, `2023/2024`, group = 1, color = '2023/2024'), size = 1) +
  scale_x_discrete(name = 'Season week number', breaks = everyother) + scale_color_manual('Year',values = c("#b1b2b3",  
                                                                                                            "#18cdf1", "#088199")) +
  
  scale_y_continuous(name="Hospitalisation",expand=c(0,0),
                     limits=c(0,300)) +
  theme(panel.border = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.background = element_rect(fill = "transparent"),
        legend.position=c(0.1,0.9),
        text=element_text(size=14,family="Arial"),
        plot.title=element_text(hjust=0.5)) 

x <- brazil_clean %>% select(Week_num, Flu_B, kat_year) 

x_p <- x %>%
  pivot_wider(names_from = kat_year,
              values_from = Flu_B)
x_p$Week_num <- as.numeric(x_p$Week_num)
x_p$Week_num <- factor(x_p$Week_num, levels=c(9:53,1:8))
x_p <- x_p[!x_p$Week_num == "53", ]
everyother <- function(x) x[seq_along(x) %% 4 == 0]
b <- ggplot() +
  geom_line(data = x_p , aes(x = Week_num, `2019/2020`, group = 1, color = '2019/2020'), size = 1) +
  geom_line(data = x_p , aes(x = Week_num, `2022/2023`, group = 1, color = '2022/2023'), size = 1) +
  geom_line(data = x_p , aes(x = Week_num, `2023/2024`, group = 1, color = '2023/2024'), size = 1) +
  scale_x_discrete(name = 'Season week number', breaks = everyother) + scale_color_manual('Year',values = c("#b1b2b3",  
                                                                                                            "#18cdf1", "#088199")) +
  
  scale_y_continuous(name="No of Hospitalisation",expand=c(0,0),
                     limits=c(0,100)) +
  theme(panel.border = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.background = element_rect(fill = "transparent"),
        legend.position=c(0.1,0.9),
        text=element_text(size=14,family="Arial"),
        plot.title=element_text(hjust=0.5)) 
