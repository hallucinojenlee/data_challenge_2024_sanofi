#####UK DATA EXPLORATION CODE#####

#CODE FOR COUNTRY LEVEL CHART IS AT LINE 158 to 174

getwd()
setwd("/Users/giojacob/Desktop/HDS_23_24/Data Challenge")

#Load packages
install.packages("ggthemes")
install.packages("RColorBrewer")
library(RColorBrewer)
library(tidyverse)
library(ggthemes)
library(readxl)
library(scales)
library(gridExtra)

#Load datasets
fluraw <- read_excel("~/Library/CloudStorage/OneDrive-LondonSchoolofHygieneandTropicalMedicine/Sanofi/Datasets/Consolidated_dataset_MASTER.xlsx", 
                     sheet = "Flu")
rsvraw <- read_excel("~/Library/CloudStorage/OneDrive-LondonSchoolofHygieneandTropicalMedicine/Sanofi/Datasets/Consolidated_dataset_MASTER.xlsx", 
                     sheet = "RSV")
fluage_18_20 <- read_excel("~/Library/CloudStorage/OneDrive-LondonSchoolofHygieneandTropicalMedicine/Sanofi/Datasets/Consolidated_dataset_MASTER.xlsx", 
                           sheet = "England_Flu_17_20_age")
fluage_20 <- read_excel("~/Library/CloudStorage/OneDrive-LondonSchoolofHygieneandTropicalMedicine/Sanofi/Datasets/Consolidated_dataset_MASTER.xlsx", 
                        sheet = "UKFlu_20-23_age")
rsv_age <- read_excel("~/Library/CloudStorage/OneDrive-LondonSchoolofHygieneandTropicalMedicine/Sanofi/Datasets/Consolidated_dataset_MASTER.xlsx", 
                      sheet = "England_RSV_age")
covid<-read.csv("https://covid.ourworldindata.org/data/owid-covid-data.csv") %>%
  select(location, date,weekly_hosp_admissions,weekly_hosp_admissions_per_million) %>%
  filter(location=="United Kingdom")

#Filter dataset for Flu
ukflu_s<-ukflu_s %>% #Added columns with season + season week
  mutate(Year=factor(Year),
         Season=factor(Season),
         Year_week=paste(Year,Week_num,"1",sep='_'),
         Date=as.Date(Year_week,'%Y_%W_%u'),
         Year=factor(Year),
         hospitalisation_rate=as.numeric(hospitalisation_rate))

ukflu_c<- fluraw %>%
  filter(Country=="England")%>%
  select(Year,Month,Week_num,hospitalisation_rate) %>%
  mutate(Year_week=paste(Year,Week_num,"1",sep='_'),
         Date=as.Date(Year_week,'%Y_%W_%u'),
         Year=factor(Year),
         hospitalisation_rate=as.numeric(hospitalisation_rate))

ukflu_c<-left_join(x=ukflu_c,y=season_week,by="Year_week") 

ukflu_c<-ukflu_c %>% mutate(Season = factor(ukflu_c$Season),
                            Year.y = factor(Year.y))

str(ukflu_c)

#Filter out covid years
ukflu_sub<-ukflu_s %>%
  filter(as.integer(Season) %in% c(2,3,4,7,8))

#Create df of season + season week for matching with rsv data
season_week<-ukflu_s %>%
  select(Year_week,Season,Season_week)

df <-data.frame(Year_week=c("2020_15_1","2020_16_1","2020_17_1",
                            "2020_18_1","2020_19_1","2020_20_1"),
                Season=c("2019/20","2019/20","2019/20",
                         "2019/20","2019/20","2019/20"),
                Season_week=34:39)

season_week<-rbind(season_week,df)

#Load rsv data
ukrsv_c<- rsvraw %>%
  filter(Country=="England")%>%
  select(Year,Month,Week_num,hospitalisation_rate) %>%
  mutate(Year_week=paste(Year,Week_num,"1",sep='_'),
         Date=as.Date(Year_week,'%Y_%W_%u'),
         Year=factor(Year),
         hospitalisation_rate=as.numeric(hospitalisation_rate))
         
ukrsv_c<-left_join(x=ukrsv_c,y=season_week,by="Year_week") 

ukrsv_c<-ukrsv_c %>% mutate(Season = factor(ukrsv_c$Season),
         Year.y = factor(Year.y))

str(ukrsv_c)

#Load and process age data post covid
fluage_20<-fluage_20 %>%
  mutate(Year_week=paste(Year,`Week number`,"1",sep='_'),
         Date=as.Date(Year_week,'%Y_%W_%u'),
         Year=factor(Year))

fluage_20<-left_join(x=fluage_20,y=season_week,by="Year_week")

fluage_20_long<-fluage_20 %>%
  pivot_longer(cols=!c("Year","Week number","Year_week","Date",
                       "Season","Season_week"),
               names_to='age_group',
               values_to='hospitalisation_rate') %>%
  mutate(age_group=as.factor(age_group)) %>% drop_na()

str(fluage_20_long)

#Load and process age data pre covid
fluage_1820long<-fluage_18_20 %>% 
  pivot_longer(cols=!c("Year","Week"),
               names_to='age_group',
               values_to='hospitalisation_cases') %>%
  mutate(Year_week=paste(Year,Week,"1",sep='_'),
         Date=as.Date(Year_week,'%Y_%W_%u'),
         Year=factor(Year),
         age_group=factor(age_group))

fluage_1820long<-left_join(x=fluage_1820long,y=season_week,by="Year_week")

str(fluage_1820long)

#Data exploration/summary
ukflu_sum<-ukflu_s %>%
  group_by(Season) %>% 
  summarise(Maximum=max(hospitalisation_rate),
            Max_week=Year_week[which.max(hospitalisation_rate)])

ukrsv_sum<-ukrsv_c %>%
  group_by(Season) %>% 
  summarise(Maximum=max(hospitalisation_rate),
            Max_week=Year_week[which.max(hospitalisation_rate)])

age_sum<-fluage_20_long %>%
  group_by(Season,age_group) %>% 
  summarise(Maximum=max(hospitalisation_rate),
            Max_week=Year_week[which.max(hospitalisation_rate)])

age_1820sum<-fluage_1820long %>%
  group_by(Season,age_group) %>% 
  summarise(Maximum=max(hospitalisation_cases),
            Max_week=Year_week[which.max(hospitalisation_cases)])

#PLOT MAIN FLU GRAPH - England Flu Season By Year comparison
#Generate column to label x axis with Calendar week
xaxis_index<-ukflu_sub %>%
  select(Week_num) %>%
  slice(1:52) %>%
  slice(which(row_number() %% 2==1)) %>%
  mutate(Week_num = as.factor(Week_num))

###MAIN FLU SEASON TREND 
flu_sub<-ggplot(data=subset(ukflu_c,(as.integer(Season) %in% c(2,3,4,7,8))),
                aes(x=Season_week,y=hospitalisation_rate,colour=Season))+
  geom_line(size=0.8) + 
  labs(title="Flu hospitalisation rates in England (2017-2023)") + 
  scale_color_manual(values = c("#f4b18d", "#f77935", "#b1b2b3", 
                                "#18cdf1", "#088199")) +
  scale_x_continuous(name="Calendar week number",expand=c(0,0),breaks=seq(1,52,by=2),
                     labels=xaxis_index$Week_num) +
  scale_y_continuous(name="Hospitalisation rate (per 100k)",expand=c(0,0),
                     limits=c(0,20)) +
  theme(panel.border = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.background = element_rect(fill = "transparent"),
        legend.position=c(0.1,0.7),
        text=element_text(size=14,family="Arial"),
        plot.title=element_text(hjust=0.5))

flu_sub #only showing pre post covid trends
ggsave("/Users/giojacob/Desktop/HDS_23_24/Data Challenge/england_flu.png",
       plot=flu_sub,width=7,height=4)

#RSV plots
###MAIN RSV PLOT - by season
rsv_sub<-ggplot(data=subset(ukrsv_c,as.integer(Season) %in% c(1,2,3,6,7)),
                aes(x=Season_week,y=hospitalisation_rate,
                              colour=Season))+
  geom_line(size=0.8) + 
  labs(title="RSV hospitalisation rates in England (2017-2023)") + 
  scale_color_manual(values = c("#f4b18d", "#f77935", "#b1b2b3", 
                                "#18cdf1", "#088199")) +
  scale_x_continuous(name="Calendar week number",expand=c(0,0),breaks=seq(1,52,by=2),
                     labels=xaxis_index$Week_num) +
  scale_y_continuous(name="Hospitalisation rate (per 100k)",expand=c(0,0)) +
  theme(panel.border = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.background = element_rect(fill = "transparent"),
        legend.position=c(0.1,0.7),
        text=element_text(size=14,family="Arial"),
        plot.title=element_text(hjust=0.5))

rsv_sub #only showing pre post covid trends
ggsave("/Users/giojacob/Desktop/HDS_23_24/Data Challenge/england_rsv.png",
       plot=rsv_sub,width=7,height=4)

#Smoothed out curve
rsv_sub2<-ggplot(ukrsv_sub,aes(x=Season_week,y=hospitalisation_rate,colour=Season))+
  geom_smooth(se=FALSE,size=0.7) + 
  labs(title="RSV hospitalisation rates in England (2017-2023)") + 
  scale_color_manual(values = colorRampPalette(brewer.pal(9, "Purples"))(9)[4:9]) +
  scale_x_continuous(name="Week number",expand=c(0,0)) +
  scale_y_continuous(name="Hospitalisation rate (per 100k)",expand=c(0,0),limits=c(0,3)) +
  theme(panel.grid.major = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.background = element_rect(fill = "white"),
        plot.title=element_text(hjust=0.5))

rsv_sub2 #Smoothed lines, looks cleaner
ggsave("rsvseason_subsmooth.png",plot=rsv_sub2,width=6,height=3.5)

#COVID DATA EXPLORATION
covid_c <- na.omit(covid) %>%
  mutate(date=as.Date(date))
str(covid_c)

#Combined plot - flu, rsv, covid
combplot<-ggplot() +
  geom_line(data=ukflu_s,aes(x=Date,y=hospitalisation_rate),color="skyblue",size=0.7) +
  geom_line(data=ukrsv_c,aes(x=Date,y=hospitalisation_rate),color="purple",size=0.7) +
  geom_line(data=covid_c,aes(x=date,y=weekly_hosp_admissions_per_million/50),
            color="grey",linetype='dashed') +
  labs(title="Influenza,RSV & COVID-19 hospitalisation trends in UK") +
  scale_colour_brewer(palette="Dark2") +
  scale_y_continuous(name="Hospitalisation rate (per 100k)",expand=c(0,0),
                     limits=c(0,15),sec.axis=sec_axis(trans=~.*50,
                                                      name="Weekly COVID-19 hospital admissions (per million)")) +
  scale_x_date(date_labels="%yW%W",date_breaks="2 months",
               limits=c(as.Date("2017-01-02"),as.Date("2023-12-18"))) +
  theme(panel.grid.major = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.background = element_rect(fill = "transparent"),
        axis.text.x = element_text(angle=90, vjust = 0.5, hjust=1),
        plot.title=element_text(hjust=0.5))

#Combined plot - flu and rsv only
flu_rsv<-ggplot() +
  geom_line(data=ukflu_s,aes(x=Date,y=hospitalisation_rate),color="skyblue",size=0.7) +
  geom_line(data=ukrsv_c,aes(x=Date,y=hospitalisation_rate),color="purple",size=0.7) +
  labs(title="Influenza & RSV hospitalisation trends in UK") +
  scale_y_continuous(name="Hospitalisation rate (per 100k)",expand=c(0,0),
                     limits=c(0,10)) +
  scale_x_date(date_labels="%yW%W",date_breaks="2 months",
               limits=c(as.Date("2017-01-02"),as.Date("2023-12-18"))) +
  theme(panel.grid.major = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.background = element_rect(fill = "transparent"),
        axis.text.x = element_text(angle=90, vjust = 0.5, hjust=1),
        plot.title=element_text(hjust=0.5))

flu_rsv
ggsave("combinedplot.png",plot=combplot,width=11,height=5)
ggsave("/Users/giojacob/Desktop/HDS_23_24/Data Challenge/flursvplot.png",plot=flu_rsv,width=11,height=4)

#Flu variant exploration
fluvar<-ukflu %>%
  select(Year,Month,Week_num,hospitalisation_rate,Flu_A,Flu_B) %>%
  mutate(Year_week=paste(Year,Week_num,"1",sep='_'),
         Date=as.Date(Year_week,'%Y_%W_%u'),
         Year=factor(Year),
         hospitalisation_rate=as.numeric(hospitalisation_rate))

fluvar<-left_join(x=fluvar,y=season_week,by="Year_week")

#Flu variant plots
flu_b<-ggplot(data=subset(fluvar,as.integer(Season) %in% c(3,7,8)),
              aes(x=Season_week,color=Season)) +
  geom_line(aes(y=Flu_B),se=FALSE,size=0.7) +
  labs(title="Influenza B hospitalisation trends (by type) in UK") +
  scale_colour_brewer(palette="Blues") +
  scale_y_continuous(name="Hospital admissions per week",expand=c(0,0),
                     limits=c(0,25)) +
  scale_x_continuous(name="Season week",expand=c(0,0),breaks=seq(0,52,by=2)) +
  theme(panel.grid.major = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.background = element_rect(fill = "transparent"),
        axis.text.x = element_text(angle=90, vjust = 0.5, hjust=1),
        plot.title=element_text(hjust=0.5),
        legend.position=c(0.15,0.5))

flu_a<-ggplot(data=subset(fluvar,as.integer(Season) %in% c(3,7,8)),
              aes(x=Season_week,color=Season)) +
  geom_line(aes(y=Flu_A),se=FALSE,size=0.8) +
  labs(title="Influenza A hospitalisation trends (by type) in UK") +
  scale_colour_brewer(palette="BuGn") +
  scale_y_continuous(name="Hospital admissions per week",expand=c(0,0),
                     breaks=seq(0,700,by=100)) +
  scale_x_continuous(name="Season week",expand=c(0,0),
                     breaks=seq(0,52,by=2)) +
  theme(panel.grid.major = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.background = element_rect(fill = "transparent"),
        axis.text.x = element_text(angle=90, vjust = 0.5, hjust=1),
        plot.title=element_text(hjust=0.5),
        legend.position=c(0.15,0.5))

flu_a

var_plot<-grid.arrange(flu_a,flu_b,nrow=2)
ggsave("Variants.png",var_plot,height=6,width=8,bg='transparent')

#Plot age differences
ggplot(data=fluage20_senior,aes(x=Season_week,y=hospitalisation_rate,
                                colour=Season)) +
  geom_line(size=0.7) +
  labs(title="Influenza hospitalisation trends in UK by age") +
  scale_colour_brewer(palette="Dark2") +
  scale_y_continuous(name="Hospitalisation rate (per 100k)",expand=c(0,0)) +
  scale_x_continuous(expand=c(0,0)) +
  theme(panel.grid.major = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.background = element_rect(fill = "white"),
        axis.text.x = element_text(angle=90, vjust = 0.5, hjust=1),
        plot.title=element_text(hjust=0.5))

#Do facet plot
ageplot_post<-ggplot(data=subset(fluage_20_long,as.integer(Season) %in% c(6,7,8)),
                     aes(x=Season_week,
                         y=hospitalisation_rate,
                         color=Season)) + 
  geom_line() + 
  facet_grid(rows=vars(age_group),
             col=vars(Season),
             scales='free_y') +
  theme(axis.line = element_line(colour = "black"),
        panel.background = element_rect(fill = "white"),
        axis.text.x = element_text(angle=90, vjust = 0.5, hjust=1),
        plot.title=element_text(hjust=0.5))

ageplot_pre<-ggplot(data=subset(fluage_1820long,as.integer(Season) %in% c(2,3)),
                    aes(x=Season_week,
                        y=hospitalisation_cases,
                        color=Season)) +
  geom_line() + 
  facet_grid(rows=vars(age_group),
             cols=vars(Season),
             scales='free_y') +
  theme(axis.line = element_line(colour = "black"),
        panel.background = element_rect(fill = "white"),
        axis.text.x = element_text(angle=90, vjust = 0.5, hjust=1),
        plot.title=element_text(hjust=0.5))

ggsave("age_post.png",ageplot_post,width=6,height=6)
ggsave("age_pre.png",ageplot_pre,width=6,height=6)