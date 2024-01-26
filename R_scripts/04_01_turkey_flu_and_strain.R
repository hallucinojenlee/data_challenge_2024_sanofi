#### This is the country analysis for turkey ####
#Date: 24 JAN 2024
#Author: Kai (with thanks to script from Gio^^)

library(RColorBrewer)
library(tidyverse)
library(ggthemes)
library(readxl)
library(scales)
library(gridExtra)

setwd("C:/Users/three/Neu Kasten_5440/022 master degree LSHTM/LSHTM/013 Data Challenge/My Sanofi_5440/data_challenge_2024_sanofi")
(path<-getwd())


##########
#loading##
##########

df_flu<-read_excel(paste0(path,"/Dataset/Consolidated_dataset_MASTER.xlsx"),
                   sheet="Flu",
                   col_types = c("text","text","numeric","numeric","numeric","text","text","numeric","numeric","numeric","numeric","numeric"))
df_flu<-df_flu[df_flu$Country=="Türkiye",]

df_rsv<-read_excel(paste0(path,"/Dataset/Consolidated_dataset_MASTER.xlsx"),
                   sheet="RSV",
                   col_types = c("text","text","numeric","numeric","numeric","text","text","numeric","numeric","numeric","numeric","numeric"))
df_rsv<-df_rsv[df_rsv$Country=="Türkiye",]

covid_raw <- read_csv("https://covid.ourworldindata.org/data/owid-covid-data.csv")
#https://ourworldindata.org/grapher/daily-covid-cases-deaths?country=~TUR

df_covid_raw <- covid_raw %>% 
  filter(location=='Turkey') %>%
  select(date, new_cases_per_million, total_deaths_per_million) %>%
  #hosp_patients, hosp_patients_per_million missing
  mutate(year = year(date),
         Week_num = isoweek(date)) %>% 
  filter(year > 2016 & year < 2024) 

#weekly
df_covid <- df_covid_raw %>% 
  group_by(year, Week_num) %>%
  summarise(new_cases_per_million = sum(new_cases_per_million, na.rm=TRUE),#watch out the NA
            .groups = 'drop') %>%
  mutate(year_week = paste(year, Week_num, sep = "-"))


#####################
## data tangling ####
#####################

##flu ##
season_week_cut<-20
df_flu$Season_week<- ifelse( df_flu$Week_num> season_week_cut, #iso week 21 become season week 1
                             df_flu$Week_num -season_week_cut,
                             df_flu$Week_num+52-season_week_cut)
df_flu$Season<-NA

for (year in 2017:2024) {
  condition_1 <- (df_flu$Year == year & df_flu$Week_num <= season_week_cut)
  condition_2 <- (
    (df_flu$Year == year & df_flu$Week_num > season_week_cut) |
      ((df_flu$Year == year + 1) & df_flu$Week_num <= season_week_cut)
  )
  
  df_flu$Season <- ifelse(condition_1, paste0(year - 1, "/", substr(as.character(year), 3, 4)), df_flu$Season)
  df_flu$Season <- ifelse(condition_2, paste0(year, "/", substr(as.character(year + 1), 3, 4)), df_flu$Season)
}

#df[,c("Season","Year","Week_num","Season_week")]

df_flu<-df_flu %>% #Added columns with season + season week
  mutate(Year=factor(Year),
         Season=factor(Season),
         Year_week=paste(Year,Week_num,"1",sep='_'),
         #Date=as.Date(Year_week,'%Y_%W_%u'),#yday 369 in year 2020 is invalid
         Year=factor(Year),
         hospitalisation_rate=as.numeric(1000000*hospitalisation_num/Population    ))


## RSV ##
#pass flu's variable to rsv df
temp<-df_flu[,c("Year","Week_num","Season","Season_week")]

df_rsv<-merge(df_rsv,temp,
        by=c("Year","Week_num"),
        all.x=TRUE)

df_rsv<-df_rsv %>% #Added columns with season + season week
  mutate(Year=factor(Year),
         Season=factor(Season),
         Year_week=paste(Year,Week_num,"1",sep='_'),
         Date=as.Date(Year_week,'%Y_%W_%u'),#yday 369 in year 2020 is invalid
         Year=factor(Year),
         hospitalisation_rate=as.numeric(1000000*hospitalisation_num/population    ))



##################################
##plot flu + RSV over 7 years ####
##################################

plot_flu_RSV <- ggplot()+

  geom_line(data=df_flu,aes(x=Date,y=hospitalisation_rate),color="skyblue",size=0.7) +
  geom_line(data=df_rsv,aes(x=Date,y=hospitalisation_rate),color="purple",size=0.7)+
  labs(title="Influenza & RSV Hospitalisation Trends in Türkiye",
       x="Year - week number") +
  scale_y_continuous(name="Hospitalisation rate (per 100k)",expand=c(0,0),
                     limits=c(0,1.2)) +
  scale_x_date(date_labels="%yW%W",date_breaks="2 months",
               limits=c(as.Date("2017-10-01"),as.Date("2023-12-31"))) +
  theme(panel.grid.major = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.background = element_rect(fill = "transparent"),
        axis.text.x = element_text(angle=90, vjust = 0.5, hjust=1),
        plot.title=element_text(hjust=0.5))

print(plot_flu_RSV)

ggsave("Output/graphs/02_Turkiye_flu_rsv_7yeartrend.png",
       plot = plot_flu_RSV, 
       width = 11, height = 5, dpi = 300)


#######################
##plot flu overlap ####
#######################



x_axis_index<-df_rsv %>%
  select(Week_num) %>%
  slice(1:52) %>%
  slice(which(row_number() %% 2==1)) %>%
  mutate(Week_num = as.factor(Week_num))



tt<-subset(df_rsv,(as.integer(Season) %in% c(2,3,4,7,8)))



plot_flu<-ggplot(data=  subset(df_flu,
                               df_flu$Season!="2020/2021"&df_flu$Season!="2021/2022") ,
                aes(x=Season_week,
                    y=hospitalisation_rate,
                    colour=Season))+
  geom_line(size=0.8)
+ 
  labs(title="Flu hospitalisation rates in England (2017-2023)") + 
  scale_color_manual(values = c("#f4b18d", "#f77935", "#b1b2b3", 
                                "#18cdf1", "#088199")) +
  scale_x_continuous(name="Calendar week number",expand=c(0,0),breaks=seq(1,52,by=2),
                     labels=xaxis_index$Week_num) +
  scale_y_continuous(name="Hospitalisation rate (per 100k)",expand=c(0,0),
                     limits=c(0,20)) 

+
  theme(panel.border = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.background = element_rect(fill = "transparent"),
        legend.position=c(0.1,0.7),
        text=element_text(size=14,family="Arial"),
        plot.title=element_text(hjust=0.5))
