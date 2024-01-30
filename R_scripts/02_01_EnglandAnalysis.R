#####DATA CLEANING AND ANALYSIS FOR ENGLAND#####
#Updated 30 Jan 2024
#Author: Gio

#Load packages
install.packages("ggthemes")
install.packages("RColorBrewer")
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
rsvraw <- read_excel(paste0(path,"/Dataset/Consolidated_dataset_MASTER.xlsx"), 
                     sheet = "RSV")
fluage_18_20 <- read_excel(paste0(path,"/Dataset/Consolidated_dataset_MASTER.xlsx"), 
                           sheet = "England_Flu_17_20_age")
fluage_20 <- read_excel(paste0(path,"/Dataset/Consolidated_dataset_MASTER.xlsx"), 
                        sheet = "UKFlu_20-23_age")
rsv_age <- read_excel(paste0(path,"/Dataset/Consolidated_dataset_MASTER.xlsx"),
                      sheet = "England_RSV_age")
covid<-read.csv("https://covid.ourworldindata.org/data/owid-covid-data.csv") %>%
  select(location, date,weekly_hosp_admissions,weekly_hosp_admissions_per_million) %>%
  filter(location=="United Kingdom")

season_week<-read.csv(paste0(path,"/Dataset/seasonweek.csv"))
########################################################################
#Data processing - factorise relevant columns, generate date from week-year
########################################################################
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
#left join to generate Flu Season tags + Season week for main charts

ukflu_c<-ukflu_c %>% mutate(Season = factor(ukflu_c$Season),
                            Year.y = factor(Year.y)) #Factorise season + Year

#Create sub dataset containing only pre and post covid years - unused
ukflu_sub<-ukflu_c %>%
  filter(as.integer(Season) %in% c(2,3,4,7,8))

############################################
#Load & pre-process rsv data for analysis
############################################
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

###################################################################
#Load & pre-process age breakdown data for analysis (2020 onwards)
###################################################################
fluage_20<-fluage_20 %>%
  mutate(Year_week=paste(Year,`Week number`,"1",sep='_'), #generate date
         Date=as.Date(Year_week,'%Y_%W_%u'),
         Year=factor(Year))

fluage_20<-left_join(x=fluage_20,y=season_week,by="Year_week") 
#left join to get season + season week tags

#Pivot data from long to wide to create charts
fluage_20long<-fluage_20 %>%
  pivot_longer(cols=!c("Year.x","Week number","Year_week","Date",
                       "Season","Season_week","Year.y","Week_num"),
               names_to='age_group',
               values_to='hospitalisation_rate') %>%
  mutate(age_group=as.factor(age_group),
         Season=factor(Season)) %>% drop_na()

###################################################################
#Load & pre-process age breakdown data for analysis (2018-2019)
#Age breakdown is different in earlier years
###################################################################
fluage_1820long<-fluage_18_20 %>% 
  pivot_longer(cols=!c("Year","Week"),
               names_to='age_group',
               values_to='hospitalisation_cases') %>%
  mutate(Year_week=paste(Year,Week,"1",sep='_'),
         Date=as.Date(Year_week,'%Y_%W_%u'),
         Year=factor(Year),
         age_group=factor(age_group))

fluage_1820long<-left_join(x=fluage_1820long,y=season_week,by="Year_week")
fluage_1820long<-fluage_1820long %>%
  mutate(Season=factor(Season))

##############################################################
#Summary tables - peak hosp week by flu & RSV season
##############################################################
ukflu_sum<-ukflu_c %>%
  group_by(Season) %>% 
  summarise(Maximum=max(hospitalisation_rate),
            Max_week=Year_week[which.max(hospitalisation_rate)])

ukrsv_sum<-ukrsv_c %>%
  group_by(Season) %>% 
  summarise(Maximum=max(hospitalisation_rate),
            Max_week=Year_week[which.max(hospitalisation_rate)])

age_sum<-fluage_20long %>%
  group_by(Season,age_group) %>% 
  summarise(Maximum=max(hospitalisation_rate),
            Max_week=Year_week[which.max(hospitalisation_rate)])

age_1820sum<-fluage_1820long %>%
  group_by(Season,age_group) %>% 
  summarise(Maximum=max(hospitalisation_cases),
            Max_week=Year_week[which.max(hospitalisation_cases)])

########################################################################
#PLOT MAIN FLU GRAPH - England Flu Season By Year comparison
########################################################################
#Generate column to label x axis with Calendar week instead of season week
xaxis_index<-ukflu_sub %>%
  select(Week_num.x) %>%
  slice(1:52) %>%
  slice(which(row_number() %% 2==1)) %>% #To align with axis markings which only shows every 2 weeks
  mutate(Week_num.x = as.factor(Week_num.x))

###MAIN FLU SEASON TREND 
flu_sub<-ggplot(data=subset(ukflu_c,(as.integer(Season) %in% c(2,3,4,7,8))),
                aes(x=Season_week,y=hospitalisation_rate,colour=Season))+
  geom_line(size=0.8) + 
  labs(title="Flu hospitalisation rates in England (2017-2023)") + 
  scale_color_manual(values = c("#f4b18d", "#f77935", "#b1b2b3", 
                                "#18cdf1", "#088199")) +
  scale_x_continuous(name="Calendar week number",expand=c(0,0),breaks=seq(1,52,by=2),
                     labels=xaxis_index$Week_num.x) +
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
ggsave(paste0(path,"/Output/graphs/02_England/england_flu.png"),
       plot=flu_sub,width=7,height=4)

########################################################################
#PLOT MAIN RSV GRAPH - England RSV Season By Year comparison
########################################################################
rsv_sub<-ggplot(data=subset(ukrsv_c,as.integer(Season) %in% c(1,2,3,6,7)),
                aes(x=Season_week,y=hospitalisation_rate,
                              colour=Season))+
  geom_line(size=0.8) + 
  labs(title="RSV hospitalisation rates in England (2017-2023)") + 
  scale_color_manual(values = c("#f4b18d", "#f77935", "#b1b2b3", 
                                "#18cdf1", "#088199")) +
  scale_x_continuous(name="Calendar week number",expand=c(0,0),breaks=seq(1,52,by=2),
                     labels=xaxis_index$Week_num.x) +
  scale_y_continuous(name="Hospitalisation rate (per 100k)",expand=c(0,0)) +
  theme(panel.border = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.background = element_rect(fill = "transparent"),
        legend.position=c(0.1,0.7),
        text=element_text(size=14,family="Arial"),
        plot.title=element_text(hjust=0.5))

rsv_sub #only showing pre post covid trends
ggsave(paste0(path,"/Output/graphs/02_England//england_rsv.png"),
       plot=rsv_sub,width=7,height=4)

#COVID DATA EXPLORATION
covid_c <- na.omit(covid) %>% mutate(date=as.Date(date))

#Combined plot - flu, rsv, covid - unused
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

########################################################
#Combined time series plot - Flu & RSV only
########################################################
flu_rsv<-ggplot() +
  geom_line(data=ukflu_c,aes(x=Date,y=hospitalisation_rate),color="#088199",size=0.7) +
  geom_line(data=ukrsv_c,aes(x=Date,y=hospitalisation_rate),color="#f77935",size=0.7) +
  labs(title="Influenza & RSV hospitalisation trends in UK") +
  scale_y_continuous(name="Hospitalisation rate (per 100k)",expand=c(0,0),
                     limits=c(0,20)) +
  scale_x_date(date_labels="%yW%W",date_breaks="2 months",
               limits=c(as.Date("2017-01-02"),as.Date("2023-12-18"))) +
  theme(panel.grid.major = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.background = element_rect(fill = "transparent"),
        axis.text.x = element_text(angle=90, vjust = 0.5, hjust=1),
        plot.title=element_text(hjust=0.5))

flu_rsv
ggsave(paste0(path,"/Output/graphs/02_England/flursvplot.png"),plot=flu_rsv,width=11,height=4)

########################################################
#Flu variant data pre-processing and cleaning
########################################################
fluvar<-ukflu %>%
  #Filter for only relevant columns
  select(Year,Month,Week_num,hospitalisation_rate,Flu_A,Flu_B) %>%
  #Generate date based on year and weeks
  mutate(Year_week=paste(Year,Week_num,"1",sep='_'),
         Date=as.Date(Year_week,'%Y_%W_%u'),
         #Factor year and change rate to numeric
         Year=factor(Year),
         hospitalisation_rate=as.numeric(hospitalisation_rate))

fluvar<-left_join(x=fluvar,y=season_week,by="Year_week")
fluvar<-fluvar %>% mutate(Season=factor(Season))

########################################################
#FLU VARIANT TRENDS BY SEASON
########################################################
flu_b<-ggplot(data=subset(fluvar,as.integer(Season) %in% c(3,4,7,8)),
              aes(x=Season_week,color=Season)) +
  geom_line(aes(y=Flu_B),size=0.7) +
  scale_color_manual(values = c("#f77935", "#b1b2b3", 
                                "#18cdf1", "#088199")) +
  labs(title="Influenza B hospitalisation trends (by type) in UK") +
  scale_y_continuous(name="Hospital admissions per week",expand=c(0,0),
                     limits=c(0,25)) +
  scale_x_continuous(name="Season week",expand=c(0,0),breaks=seq(0,51,by=2),
                     labels=xaxis_index$Week_num) +
  theme(panel.grid.major = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.background = element_rect(fill = "transparent"),
        axis.text.x = element_text(angle=90, vjust = 0.5, hjust=1),
        plot.title=element_text(hjust=0.5),
        legend.position=c(0.15,0.5))

flu_a<-ggplot(data=subset(fluvar,as.integer(Season) %in% c(3,4,7,8)),
              aes(x=Season_week,color=Season)) +
  geom_line(aes(y=Flu_A),size=0.7) +
  scale_color_manual(values = c("#f77935", "#b1b2b3", 
                                "#18cdf1", "#088199")) +
  labs(title="Influenza A hospitalisation trends (by type) in UK") +
  scale_y_continuous(name="Hospital admissions per week",expand=c(0,0),
                     breaks=seq(0,700,by=100)) +
  scale_x_continuous(name="Season week",expand=c(0,0),
                     breaks=seq(0,51,by=2),labels=xaxis_index$Week_num) +
  theme(panel.grid.major = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.background = element_rect(fill = "transparent"),
        axis.text.x = element_text(angle=90, vjust = 0.5, hjust=1),
        plot.title=element_text(hjust=0.5),
        legend.position=c(0.15,0.5))

flu_b

var_plot<-grid.arrange(flu_a,flu_b,nrow=2)
ggsave(paste0(path,"/Output/graphs/02_England/Variants.png"),
       var_plot,height=6,width=8,bg='transparent')

########################################################
#FLU HOSPITALISATION TRENDS BY AGE
########################################################
#Plot for pre-covid baseline
agepre<-ggplot(subset(fluage_1820long,as.integer(Season) %in% c(1,2,3)), #select only 2022-23
               aes(x=Week_num, y=hospitalisation_cases)) +
  #group by flu season
  geom_line(aes(color=Season), size=.8) +
  #put individual graphs by age group into one visualisation
  facet_wrap(~ age_group,scales='free_y') +
  #set colors for flu season
  scale_color_manual(values = c("#f4b18d", "#f77935","#b1b2b3")) + 
  #set labels
  xlab("Week (since begining of season)") + 
  ylab("Rate of Hospitalisations (per 100k)") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(color = "Season") +  
  #set plot design
  theme_minimal() +  
  scale_y_continuous(expand=c(0,0)) + 
  theme(panel.border = element_blank(),  
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),  
        text=element_text(size=14, 
                          family="Arial"), 
        axis.line = element_line(colour = "black")) 

ggsave(paste0(path,"/Output/graphs/02_England/age_pre.png"),agepre,width=6,height=6)

#######
#create plot (2022-23) - y-axis = hospitalisations, x-axis = Season week
#######
agepost<-ggplot(subset(fluage_20long,as.integer(Season) %in% c(2,3)), #select only 2022-23
       aes(x=Week_num, y=hospitalisation_rate)) +
  #group by flu season
  geom_line(aes(color=Season), size=.8) +
  #put individual graphs by age group into one visualisation
  facet_wrap(~ age_group) +
  #set colors for flu season
  scale_color_manual(values = c("#18cdf1", "#088199")) + 
  #set labels
  xlab("Week (since begining of season)") + 
  ylab("Rate of Hospitalisations (per 100k)") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(color = "Season") +  
  #set plot design
  theme_minimal() +  
  scale_y_continuous(expand=c(0,0)) + 
  theme(panel.border = element_blank(),  
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),  
        text=element_text(size=14, 
                          family="Arial"), 
        axis.line = element_line(colour = "black")) 

ggsave(paste0(path,"/Output/graphs/02_England/age_post.png"),agepost,width=6,height=6)
################################################################
#Alternative viz for for hosp, age breakdown and flu season
# Plot is facet wrapped by age and flu season
################################################################
ggplot(data=subset(fluage_20_long,as.integer(Season) %in% c(6,7,8)),
                     aes(x=Season_week,y=hospitalisation_rate, color=Season)) + 
  geom_line() + 
  facet_grid(rows=vars(age_group),col=vars(Season),scales='free_y') +
  theme(axis.line = element_line(colour = "black"),
        panel.background = element_rect(fill = "white"),
        axis.text.x = element_text(angle=90, vjust = 0.5, hjust=1),
        plot.title=element_text(hjust=0.5))