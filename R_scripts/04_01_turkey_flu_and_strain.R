#### This is the country analysis for turkey ####
#Date: 24 JAN 2024
#Author: Kai (with thanks to script from Gio^^)

library(RColorBrewer)
library(tidyverse)
library(ggthemes)
library(readxl)
library(scales)
library(gridExtra)

#setwd("C:/Users/three/Neu Kasten_5440/022 master degree LSHTM/LSHTM/013 Data Challenge/My Sanofi_5440/data_challenge_2024_sanofi")
(path<-getwd())


#+++++++++
#loading##
#+++++++++

df_flu<-read_excel(paste0(path,"/Dataset/Consolidated_dataset_MASTER.xlsx"),
                   sheet="Flu",
                   col_types = c("text","text","numeric","numeric","numeric","text","text","numeric","numeric","numeric","numeric","numeric"))
df_flu<-df_flu[df_flu$Country=="Türkiye",]

df_rsv<-read_excel(paste0(path,"/Dataset/Consolidated_dataset_MASTER.xlsx"),
                   sheet="RSV",
                   col_types = c("text","text","numeric","numeric","numeric","text","text","numeric","numeric","numeric","numeric","numeric"))
df_rsv<-df_rsv[df_rsv$Country=="Türkiye",]

# covid_raw <- read_csv("https://covid.ourworldindata.org/data/owid-covid-data.csv")
# #https://ourworldindata.org/grapher/daily-covid-cases-deaths?country=~TUR
# 
# df_covid_raw <- covid_raw %>% 
#   filter(location=='Turkey') %>%
#   select(date, new_cases_per_million, total_deaths_per_million) %>%
#   #hosp_patients, hosp_patients_per_million missing
#   mutate(year = year(date),
#          Week_num = isoweek(date)) %>% 
#   filter(year > 2016 & year < 2024) 
# 
# #weekly
# df_covid <- df_covid_raw %>% 
#   group_by(year, Week_num) %>%
#   summarise(new_cases_per_million = sum(new_cases_per_million, na.rm=TRUE),#watch out the NA
#             .groups = 'drop') %>%
#   mutate(year_week = paste(year, Week_num, sep = "-"))


#++++++++++++++++++++
## data tangling ####
#++++++++++++++++++++

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
         Date=as.Date(Year_week,'%Y_%W_%u'),#yday 369 in year 2020 is invalid
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



#+++++++++++++++++++++++++++++++++
##plot 1: flu + RSV over 7 years ####
#+++++++++++++++++++++++++++++++++

plot_flu_RSV <- ggplot()+

  geom_line(data=df_flu,aes(x=Date,y=hospitalisation_rate),color="#088199",size=0.7) +
  geom_line(data=df_rsv,aes(x=Date,y=hospitalisation_rate),color="#f77935",size=0.7)+
  labs(title="Influenza & RSV Hospitalisation Trends in Türkiye",
       x="Year-week") +
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

ggsave("Output/graphs/04_turkey/01_Turkiye_flu_rsv_7yeartrend.png",
       plot = plot_flu_RSV, 
       width = 11, height = 5, dpi = 300)


#++++++++++++++++++++++++
##plot 2: flu overlap ####
#++++++++++++++++++++++++

x_axis_index<-c(seq(21,51,by=2),seq(1,20,by=2))

df_flu_subset<- df_flu[df_flu$Season!="2020/21"&
                         df_flu$Season!="2023/24", ]

plot_flu_overlap<-ggplot(data=  df_flu_subset ,
                aes(x=Season_week,
                    y=hospitalisation_rate,
                    colour=Season))+
  geom_line(size=0.8)+ 
  labs(title="Flu hospitalisation rates in Türkiye (2017-2023)") + 
 scale_color_manual(values = c("#f4b18d", "#f77935", "#b1b2b3",
                              "#18cdf1", "#088199")) +
  scale_x_continuous(name="Calendar week number",expand=c(0,0),breaks=seq(1,52,by=2),
                     labels=x_axis_index  ) +
  scale_y_continuous(name="Hospitalisation rate (per 100k)",expand=c(0,0),
                     limits=c(0,1.20)) +
  
  theme(panel.border = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.background = element_rect(fill = "transparent"),
        legend.position=c(0.1,0.7),
        text=element_text(size=14,family="Arial"),
        plot.title=element_text(hjust=0.5),
        legend.text=element_text(size=15))

print(plot_flu_overlap)

ggsave("Output/graphs/04_turkey/02_turkey_flu_overlap.png",
       plot = plot_flu_overlap,
       width = 7, height = 4)


#++++++++++++++++++++++++++
## plot 3: RSV overlap ####
#++++++++++++++++++++++++++

x_axis_index<-c(seq(21,51,by=2),seq(1,20,by=2))

df_rsv_subset<- df_rsv[df_rsv$Season!="2020/21"&
                         df_rsv$Season!="2023/24", ]

plot_rsv_overlap<-ggplot(data=  df_rsv_subset ,
                         aes(x=Season_week,
                             y=hospitalisation_rate,
                             colour=Season))+
  geom_line(size=0.8)+ 
  labs(title="RSV hospitalisation rates in Türkiye (2017-2023)") + 
  scale_color_manual(values = c("#f4b18d", "#f77935", "#b1b2b3",
                                "#18cdf1", "#088199")) +
  scale_x_continuous(name="Calendar week number",expand=c(0,0),breaks=seq(1,52,by=2),
                     labels=x_axis_index  ) +
  scale_y_continuous(name="Hospitalisation rate (per 100k)",expand=c(0,0),
                     limits=c(0,0.7)) +
  
  theme(panel.border = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.background = element_rect(fill = "transparent"),
        legend.position=c(0.1,0.7),
        text=element_text(size=14,family="Arial"),
        plot.title=element_text(hjust=0.5),
        legend.text=element_text(size=15))

print(plot_rsv_overlap)

ggsave("Output/graphs/04_turkey/02_turkey_rsv_overlap.png",
       plot = plot_rsv_overlap,
       width = 7, height = 4)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#PLot 4: Strain
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

df_flu$hospitalisation_rate_A<-as.numeric(1000000*df_flu$Flu_A/df_flu$Population    )
df_flu$hospitalisation_rate_B<-as.numeric(1000000*df_flu$Flu_B/df_flu$Population    )


df_flu_subset<- df_flu[df_flu$Season!="2020/21"&
                         df_flu$Season!="2023/24", ]


## A
plot_flu_overlap_A<-ggplot(data=  df_flu_subset ,
                         aes(x=Season_week,
                             y=hospitalisation_rate_A,
                             colour=Season))+
  geom_line(size=0.8)+ 
  labs(title="Influenza A hospitalisation trends") + 
  scale_color_manual(values = c("#f4b18d", "#f77935", "#b1b2b3",
                                "#18cdf1", "#088199")) +
  scale_x_continuous(name="Calendar week number",expand=c(0,0),breaks=seq(1,52,by=2),
                     labels=x_axis_index  ) +
  scale_y_continuous(name="Hospitalisation rate (per 100k)",expand=c(0,0),
                     limits=c(0,0.9)) +
  
  theme(panel.border = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.background = element_rect(fill = "transparent"),
        legend.position=c(0.1,0.7),
        text=element_text(size=14,family="Arial"),
        plot.title=element_text(hjust=0.5),
        legend.text=element_text(size=15))

## B
plot_flu_overlap_B<-ggplot(data=  df_flu_subset ,
                           aes(x=Season_week,
                               y=hospitalisation_rate_B,
                               colour=Season))+
  geom_line(size=0.8)+ 
  labs(title="Influenza B hospitalisation trends") + 
  scale_color_manual(values = c("#f4b18d", "#f77935", "#b1b2b3",
                                "#18cdf1", "#088199")) +
  scale_x_continuous(name="Calendar week number",expand=c(0,0),breaks=seq(1,52,by=2),
                     labels=x_axis_index  ) +
  scale_y_continuous(name="Hospitalisation rate (per 100k)",expand=c(0,0),
                     limits=c(0,0.3)) +
  
  theme(panel.border = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.background = element_rect(fill = "transparent"),
        legend.position=c(0.1,0.7),
        text=element_text(size=14,family="Arial"),
        plot.title=element_text(hjust=0.5),
        legend.text=element_text(size=15))

#print
print(plot_flu_overlap_A)
print(plot_flu_overlap_B)

#install.packages("ggpubr")
library(ggpubr)

plot_flu_overlap_AB<-ggarrange(
         plot_flu_overlap_A, plot_flu_overlap_B, 
         # labels = c("A", "B"),
          ncol = 1, nrow = 2)

print(plot_flu_overlap_AB)


ggsave("Output/graphs/04_turkey/03_turkey_flu_strain.png",
       plot = plot_flu_overlap_AB,
       width = 8, height = 6)
