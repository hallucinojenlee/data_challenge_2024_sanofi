library(rio)
library(tidyverse)
library(lubridate)
library(gganimate)
library(gifski)

path <- 'https://github.com/hallucinojenlee/data_challenge_2024_sanofi/raw/main/Dataset/Consolidated_dataset_MASTER.xlsx'

# RSV-FLu time series

#UK
mast <- import_list(path) 
flu <- mast$Flu
flu <- flu %>% filter(Country == 'England')
flu$Week_date <- as.numeric(flu$Week_date)
flu$Week_date <- as.Date(flu$Week_date, origin = "1899-12-30")
flu$Week_num <- isoweek(flu$Week_date)
flu$Year <- isoyear(flu$Week_date)
flu$hospitalisation_rate <- as.numeric(flu$hospitalisation_rate)

mast <- import_list(path) 
rsv <- mast$RSV
rsv <- rsv %>% filter(Country == 'England')
rsv$Week_date <- as.Date(rsv$Week_date, origin = "1899-12-30")
rsv$Week_num <- isoweek(rsv$Week_date)
rsv$Year <- isoyear(rsv$Week_date)


clean <- merge(flu, rsv[ , c("Year", "Week_num", 'Month', 'hospitalisation_rate')], by.x=c("Year", "Week_num", 'Month'), by.y=c("Year", "Week_num", 'Month'))

x <- ggplot() +
  geom_line(data=clean,aes(x=Week_date,y=hospitalisation_rate.x),color="#088199",size=0.7) +
  geom_line(data=clean,aes(x=Week_date,y=hospitalisation_rate.y),color="#f77935",size=0.7) + scale_x_date(date_labels="%yW%W",date_breaks="2 months") +
  geom_point(data=clean,aes(x=Week_date,y=hospitalisation_rate.x),color="#088199",size=2) +
  geom_point(data=clean,aes(x=Week_date,y=hospitalisation_rate.y),color="#f77935",size=2) +
  geom_text(data=clean, aes(x=Week_date, y=hospitalisation_rate.x + 1.5,label=paste('Flu')), color = "#088199") +
  geom_text(data=clean, aes(x=Week_date, y=hospitalisation_rate.y + 1,label=paste('RSV')), color = "#f77935") +
  ylim(-0.2, 16) +
  theme(panel.grid.major = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.background = element_rect(fill = "transparent"),
        axis.text.x = element_text(angle=90, vjust = 0.5, hjust=1),
        plot.title=element_text(hjust=0.5)) + labs(y="Hospitalisation Rate per 100k", x = 'Year-Week') 
x
y <- x + transition_reveal(Week_date)

animate(y, fps = 100, duration = 1.5, width = 1800, height = 865, renderer = gifski_renderer(loop = FALSE, "gganim_uk.gif"),res= 200)# accuracy every fold

#US
mast <- import_list(path) 
flu <- mast$Flu
flu <- flu %>% filter(Country == 'US')
flu$Week_date <- as.numeric(flu$Week_date)
flu$Week_date <- as.Date(flu$Week_date, origin = "1899-12-30")
flu$Week_num <- isoweek(flu$Week_date)
flu$Year <- isoyear(flu$Week_date)
flu$hospitalisation_rate <- as.numeric(flu$hospitalisation_rate)

mast <- import_list(path) 
rsv <- mast$RSV
rsv <- rsv %>% filter(Country == 'US')
rsv$Week_date <- as.Date(rsv$Week_date, origin = "1899-12-30")
rsv$Week_num <- isoweek(rsv$Week_date)
rsv$Year <- isoyear(rsv$Week_date)


clean <- merge(flu, rsv[ , c("Year", "Week_num", 'Month', 'hospitalisation_rate')], by.x=c("Year", "Week_num"), by.y=c("Year", "Week_num"))
clean$hospitalisation_rate.x[is.na(clean$hospitalisation_rate.x)] <- 0
clean$hospitalisation_rate.y[is.na(clean$hospitalisation_rate.y)] <- 0

x <- ggplot() +
  geom_line(data=clean,aes(x=Week_date,y=hospitalisation_rate.x),color="#088199",size=0.7) +
  geom_line(data=clean,aes(x=Week_date,y=hospitalisation_rate.y),color="#f77935",size=0.7) + scale_x_date(date_labels="%yW%W",date_breaks="2 months") +
  geom_point(data=clean,aes(x=Week_date,y=hospitalisation_rate.x),color="#088199",size=2) +
  geom_point(data=clean,aes(x=Week_date,y=hospitalisation_rate.y),color="#f77935",size=2) +
  geom_text(data=clean, aes(x=Week_date, y=hospitalisation_rate.x + 1.5,label=paste('Flu')), color = "#088199") +
  geom_text(data=clean, aes(x=Week_date, y=hospitalisation_rate.y + 1,label=paste('RSV')), color = "#f77935")  +
  theme(panel.grid.major = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.background = element_rect(fill = "transparent"),
        axis.text.x = element_text(angle=90, vjust = 0.5, hjust=1),
        plot.title=element_text(hjust=0.5)) + labs(y="Hospitalisation Rate per 100k", x = 'Year-Week') 
x
y <- x + transition_reveal(Week_date)

animate(y, fps = 100, duration = 1.5, width = 1800, height = 865, renderer = gifski_renderer(loop = FALSE, "gganim_us.gif"),res= 200)# accuracy every fold

#Brazil
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
rsv$Week_date <- as.Date(rsv$Week_date, origin = "1899-12-30")
rsv$Week_num <- isoweek(rsv$Week_date)
rsv$Year <- isoyear(rsv$Week_date)

brazil_clean <- merge(flu, rsv[ , c("Year", "Week_num", 'Month', 'hospitalisation_num')], by.x=c("Year", "Week_num", 'Month'), by.y=c("Year", "Week_num", 'Month'))

x <- ggplot() +
  geom_line(data=brazil_clean,aes(x=Week_date,y=hospitalisation_num.x),color="#088199",size=0.7) +
  geom_line(data=brazil_clean,aes(x=Week_date,y=hospitalisation_num.y),color="#f77935",size=0.7) + scale_x_date(date_labels="%yW%W",date_breaks="2 months") +
  geom_point(data=brazil_clean,aes(x=Week_date,y=hospitalisation_num.x),color="#088199",size=2) +
  geom_point(data=brazil_clean,aes(x=Week_date,y=hospitalisation_num.y),color="#f77935",size=2) +
  geom_text(data=brazil_clean, aes(x=Week_date, y=hospitalisation_num.x + 200,label=paste('Flu')), color = "#088199") +
  geom_text(data=brazil_clean, aes(x=Week_date, y=hospitalisation_num.y + 300,label=paste('RSV')), color = "#f77935") +
  theme(panel.grid.major = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.background = element_rect(fill = "transparent"),
        axis.text.x = element_text(angle=90, vjust = 0.5, hjust=1),
        plot.title=element_text(hjust=0.5)) + labs(y="No of Hospitalisation", x = 'Year-Week') 
x
y <- x + transition_reveal(Week_date)
animate(y, fps = 100, duration = 1.5, width = 1800, height = 865, renderer = gifski_renderer(loop = FALSE, "gganim_brazil.gif"),res= 200)# accuracy every fold
