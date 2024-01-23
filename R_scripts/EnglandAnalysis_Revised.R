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

#Filter dataset
ukflu<-fluraw %>%
  filter(Country=="England")

ukflu_s<-read.csv("/Users/giojacob/Desktop/HDS_23_24/Data Challenge/Datasets/ukflu_clean.csv")

ukflu_s<-ukflu_s %>% #Added columns with season + season week
  mutate(Year=factor(Year),
         Season=factor(Season),
         Year_week=paste(Year,Week_num,"1",sep='_'),
         Date=as.Date(Year_week,'%Y_%W_%u'),
         Year=factor(Year),
         hospitalisation_rate=as.numeric(hospitalisation_rate))

str(ukflu_s)

#Filter out covid years
ukflu_sub<-ukflu_s %>%
  filter(as.integer(Season) %in% c(2,3,4,7,8))

ukrsv_sub<-ukrsv_c %>%
  filter(as.integer(Season) %in% c(2,3,7,8))

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

#Plot data and explore
flu_timeser<-ggplot(ukflu_s,aes(x=Date,y=hospitalisation_rate))+
  geom_line(size=0.8,colour="skyblue") + 
  labs(title="Flu hospitalisation rates in England (2017-2023)",
       x="Week Number",y="Hospitalisation rate (per 100k)") + 
  scale_colour_brewer(palette="Blues") +
  scale_x_date(date_labels="%yW%W",date_breaks="3 months") +
  scale_y_continuous(expand=c(0,0)) +
  theme(panel.grid.major = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.background = element_rect(fill = "white"),
        axis.text.x = element_text(angle=90, vjust = 0.5, hjust=1),
        plot.title=element_text(hjust=0.5))

flu_timeser #full time series flu hosp trends
ggsave("flutimeseries.png",plot=flu_timeser,width=8,height=3)

#CODE FOR COUNTRY GRAPH - England Flu Season By Year comparison

#Generate column for correct week number label 
xaxis_index<-ukflu_sub %>%
  select(Week_num) %>%
  slice(1:52) %>%
  mutate(WeekNum = as.factor(Week_num))

#Flu Season trends
flu_sub<-ggplot(ukflu_sub,aes(x=Season_week,y=hospitalisation_rate,colour=Season))+
  geom_line(size=0.8) + 
  labs(title="Flu hospitalisation rates in England (2017-2023)",
       x="Week Number",y="Hospitalisation rate (per 100k)") + 
  scale_color_manual(values = c("#f38143", "#f77935", "#b1b2b3", 
                                "#18cdf1", "#088199")) +
  scale_x_continuous(name="ISO Week number",expand=c(0,0),breaks=seq(1,52),
                     labels=xaxis_index$Week_num) +
  scale_y_continuous(name="Hospitalisation rate (per 100k)",expand=c(0,0),
                     limits=c(0,20)) +
  theme(panel.border = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.background = element_rect(fill = "transparent"),
        legend.position=c(0.05,0.85),
        text=element_text(size=14,family="Arial"),
        plot.title=element_text(hjust=0.5))

flu_sub #only showing pre post covid trends
ggsave("fluseason_subset.png",plot=flu_sub,width=6,height=3.5)

#RSV plots
rsv_timeser<-ggplot(ukrsv_c,aes(x=Date,y=hospitalisation_rate))+
  geom_line(size=0.8,colour="purple") + 
  labs(title="RSV hospitalisation rates per 100k in the UK (2017-2023)") + 
  theme_light() + #scale_x_date(breaks = date_breaks("months")) +
  scale_color_brewer(palette="Greens") + 
  scale_x_date(date_labels="%yW%W",date_breaks="3 months") +
  scale_y_continuous(name="Hospitalisation rate (per 100k)",expand=c(0,0),limit=c(0,6)) +
  theme(panel.grid.major = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.background = element_rect(fill = "white"),
        axis.text.x = element_text(angle=90, vjust = 0.5, hjust=1),
        plot.title=element_text(hjust=0.5))

rsv_timeser
ggsave("rsvtimeseries.png",plot=rsv_timeser,width=8,height=3)

rsv_season<-ggplot(ukrsv_c,aes(x=Season_week,y=hospitalisation_rate,colour=Season))+
  geom_line(size=0.7) + 
  labs(title="RSV hospitalisation rates in England (2017-2023)",
       x="Week Number",y="Hospitalisation rate (per 100k)") + 
  scale_colour_brewer(palette="Purples") +
  scale_x_continuous(name="Week number",expand=c(0,0)) +
  scale_y_continuous(name="Hospitalisation rate (per 100k)",expand=c(0,0)) +
  theme(panel.grid.major = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.background = element_rect(fill = "white"),
        legend.position=c(0.9,0.5),
        plot.title=element_text(hjust=0.5))

rsv_season #Shows all trends
ggsave("rsvseason_all.png",plot=rsv_season,width=6,height=3.5)

rsv_sub<-ggplot(ukrsv_sub,aes(x=Season_week,y=hospitalisation_rate,colour=Season))+
  geom_line(size=0.8) + 
  labs(title="RSV hospitalisation rates in England (2017-2023)") + 
  scale_color_manual(values = colorRampPalette(brewer.pal(9, "Purples"))(9)[4:9]) +
  scale_x_continuous(name="Week number",expand=c(0,0),breaks=seq(0,52,by=2)) +
  scale_y_continuous(name="Hospitalisation rate (per 100k)",expand=c(0,0)) +
  theme(panel.grid.major = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.background = element_rect(fill = "transparent"),
        legend.position=c(0.1,0.5),
        plot.title=element_text(hjust=0.5))

rsv_sub
ggsave("rsvseason_subset.png",plot=rsv_sub,width=6,height=3.5)

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

ggplot(covid_c,aes(x=date,y=weekly_hosp_admissions_per_million))+
  geom_line(colour="grey",size=0.8) + 
  labs(title="COVID hospitalisation rates in England (2017-2023)") + 
  #scale_color_manual(values = colorRampPalette(brewer.pal(9, "Purples"))(9)[4:9]) +
  scale_x_date(date_labels="%yW%W",date_breaks="3 months") +
  scale_y_continuous(name="Hospitalisation rate (per 100k)",expand=c(0,0)) +
  theme(panel.grid.major = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.background = element_rect(fill = "white"),
        axis.text.x = element_text(angle=90, vjust = 0.5, hjust=1),
        plot.title=element_text(hjust=0.5))

#Combined plot
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

flu_rsv<-ggplot() +
  geom_line(data=ukflu_s,aes(x=Date,y=hospitalisation_rate),color="skyblue",size=0.7) +
  geom_line(data=ukrsv_c,aes(x=Date,y=hospitalisation_rate),color="purple",size=0.7) +
  labs(title="Influenza & RSV hospitalisation trends in UK") +
  scale_y_continuous(name="Hospitalisation rate (per 100k)",expand=c(0,0),
                     limits=c(0,15)) +
  scale_x_date(date_labels="%yW%W",date_breaks="2 months",
               limits=c(as.Date("2017-01-02"),as.Date("2023-12-18"))) +
  theme(panel.grid.major = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.background = element_rect(fill = "transparent"),
        axis.text.x = element_text(angle=90, vjust = 0.5, hjust=1),
        plot.title=element_text(hjust=0.5))

combplot
ggsave("combinedplot.png",plot=combplot,width=11,height=5)
ggsave("flursvplot.png",plot=flu_rsv,width=11,height=5)

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