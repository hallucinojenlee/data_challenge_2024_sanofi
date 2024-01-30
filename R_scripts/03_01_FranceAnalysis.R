#####DATA CLEANING AND ANALYSIS FRANCE#####
#Updated 30 Jan 2024
#Author: Gio

library(RColorBrewer)
library(tidyverse)
library(ggthemes)
library(readxl)
library(scales)
library(gridExtra)

path<-getwd()

########################
#Load data for analysis
########################
fluraw <- read_excel(paste0(path,"/Dataset/Consolidated_dataset_MASTER.xlsx"), 
                     sheet = "Flu")

season_week<-read.csv(paste0(path,"/Dataset/seasonweek.csv"))
                      
#########################
#Data processing
#########################

#Merge with season year & season week for plotting of graphs
franceflu<- fluraw %>%
  filter(Country=="France")%>%
  #Create data from year and week number
  select(Year,Month,Week_num,hospitalisation_num) %>%
  mutate(Year_week=paste(Year,Week_num,"1",sep='_'),
         Date=as.Date(Year_week,'%Y_%W_%u'),
         Year=factor(Year),
         hospitalisation_num=as.numeric(hospitalisation_num))

franceflu_c<-left_join(x=franceflu,y=season_week,by="Year_week") %>%
  mutate(Season=factor(Season),
         Year.y=factor(Year.y))

str(franceflu_c)

#Merge rsv data
francersv<- rsvraw %>%
  filter(Country=="France")%>%
  select(Year,Month,Week_num,hospitalisation_num) %>%
  mutate(Year_week=paste(Year,Week_num,"1",sep='_'),
         Date=as.Date(Year_week,'%Y_%W_%u'),
         Year=factor(Year),
         hospitalisation_num=as.numeric(hospitalisation_num))

francersv_c<-left_join(x=francersv,y=season_week,by="Year_week") %>%
  mutate(Season=factor(Season),
         Year.y=factor(Year.y))

str(francersv_c)

#Generate column for correct week number label 
xaxis_index<-ukflu_sub %>%
  select(Week_num) %>%
  slice(1:52) %>%
  mutate(WeekNum = as.factor(Week_num))

#Flu Season trends
flufr<-ggplot(data=subset(franceflu_c,as.integer(Season) %in% c(1,2,5,6)),
                               aes(x=Season_week,y=hospitalisation_num,
                                   colour=Season))+
  geom_line(size=0.8) + 
  labs(title="Flu hospitalisation in France (2017-2023)") + 
  scale_color_manual(values = c("#f77935", "#b1b2b3", 
                                "#18cdf1", "#088199")) +
  scale_x_continuous(name="Calendar week number",expand=c(0,0),breaks=seq(1,52,by=2),
                     labels=xaxis_index$Week_num) +
  scale_y_continuous(name="No of hospitalisation",expand=c(0,0)) +
  theme(panel.border = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.background = element_rect(fill = "transparent"),
        legend.position=c(0.1,0.7),
        text=element_text(size=14,family="Arial"),
        plot.title=element_text(hjust=0.5))

ggsave("/Users/giojacob/Desktop/HDS_23_24/Data Challenge/france_flu.png",
       plot=flufr,width=7,height=4)

#RSV season trends
rsvfr<-ggplot(data=subset(francersv_c,as.integer(Season) %in% c(1,2,5)),
              aes(x=Season_week,y=hospitalisation_num,
                  colour=Season))+
  geom_line(size=0.8) + 
  labs(title="RSV hospitalisation in France (2017-2023)") + 
  scale_color_manual(values = c("#f77935", "#b1b2b3", 
                                "#18cdf1", "#088199")) +
  scale_x_continuous(name="Calendar week number",expand=c(0,0),breaks=seq(1,52,by=2),
                     labels=xaxis_index$Week_num) +
  scale_y_continuous(name="No of hospitalisation",expand=c(0,0)) +
  theme(panel.border = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.background = element_rect(fill = "transparent"),
        legend.position=c(0.1,0.7),
        text=element_text(size=14,family="Arial"),
        plot.title=element_text(hjust=0.5))

rsvfr

ggsave("/Users/giojacob/Desktop/HDS_23_24/Data Challenge/france_rsv.png",
       plot=rsvfr,width=7,height=4)

#Flu and rsv time series
fr_comb<-ggplot() +
  geom_line(data=franceflu_c,aes(x=Date,y=hospitalisation_num),color="#088199",size=0.7) +
  geom_line(data=francersv_c,aes(x=Date,y=hospitalisation_num),color="#f77935",size=0.7) +
  labs(title="Influenza & RSV hospitalisation trends in France") +
  scale_y_continuous(name="No of hospitalisation",expand=c(0,0)) +
  scale_x_date(date_labels="%yW%W",date_breaks="2 months") +
               #limits=c(as.Date("2017-01-02"),as.Date("2023-12-18"))) +
  theme(panel.grid.major = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.background = element_rect(fill = "transparent"),
        axis.text.x = element_text(angle=90, vjust = 0.5, hjust=1),
        plot.title=element_text(hjust=0.5))

fr_comb
ggsave("/Users/giojacob/Desktop/HDS_23_24/Data Challenge/frcombined.png",
       plot=fr_comb,width=11,height=4)

#France summary tables
franceflu_sum<-franceflu_c %>%
  group_by(Season) %>% 
  summarise(Maximum=max(hospitalisation_num,na.rm=TRUE),
            Max_week=Year_week[which.max(hospitalisation_num)])

francersv_sum<-francersv_c %>%
  group_by(Season) %>% 
  summarise(Maximum=max(hospitalisation_num),
            Max_week=Year_week[which.max(hospitalisation_num)])
