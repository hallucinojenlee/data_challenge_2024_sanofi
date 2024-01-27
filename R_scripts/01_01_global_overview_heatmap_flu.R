#### This is the flu global overview analysis using heatmap ####
#Date: 2- JAN 2024
#Author: Kai

#++++++++++++
## set up####
#++++++++++++
#directory

setwd("C:/Users/three/Neu Kasten_5440/022 master degree LSHTM/LSHTM/013 Data Challenge/My Sanofi_5440/data_challenge_2024_sanofi")
(path<-getwd())

if (!require("readxl")) install.packages("readxl") 
library(readxl)
library(ggplot2)


df_raw<-read_excel(paste0(path,"/Dataset/Consolidated_dataset_MASTER.xlsx"),
                   sheet="Flu",
                   col_types = c("text","text","numeric","numeric","numeric","text","text","numeric","numeric","numeric","numeric","numeric"))
                  #ensuring the correct data type of population variable

#++++++++++++++++++++
## data cleaning ####
#++++++++++++++++++++
names(df_raw)
summary(df_raw)

##cleaning date postponded
df_raw$Week_date


##population
unique(df_raw[is.na(df_raw$Population),"Country"])

df_raw[df_raw$Country=="England"&
         is.na(df_raw$Population),"Population"]<-
  unique(df_raw[df_raw$Country=="England"&df_raw$Year==2021,"Population"])
 #using 2021 for 2022, 2023


## incidence rate per 100,000 
flag_no_rate<-is.na(df_raw$hospitalisation_rate)
summary(df_raw$hospitalisation_rate)

df_raw[flag_no_rate,"hospitalisation_rate"] <-(df_raw[flag_no_rate,"hospitalisation_num"] / 
                                              df_raw[flag_no_rate,"Population"]*
                                              100000)
#missing weeks, fine
df_raw[is.na(df_raw$hospitalisation_rate),]


df<-df_raw

#++++++++++++++++++++++++++++++++++++++++++++++++++
## Heat Map 1: global, desegregated by country #### 
#++++++++++++++++++++++++++++++++++++++++++++++++++
# start using df
df<-df[order(df$Country,df$Year,decreasing = TRUE),]
# create year-country variable
df$Year_Country<-paste0(df$Year,"-",df$Country)

levels_country_year <-unique(df$Year_Country)


df$Year_Country<-factor(df$Year_Country, 
                        levels = levels_country_year)

df$hospitalisation_rate_scaled<-scale(df$hospitalisation_rate)
df$hospitalisation_rate_log<-log(df$hospitalisation_rate)

#transforming to season week
season_week_cut<-20
df$Season_week<- ifelse( df$Week_num> season_week_cut, #iso week 21 become season week 1
                             df$Week_num -season_week_cut,
                             df$Week_num+52-season_week_cut)

## season encoding
df$Season<-NA

for (year in 2017:2024) {
  condition_1 <- (df$Year == year & df$Week_num <= season_week_cut)
  condition_2 <- (
    (df$Year == year & df$Week_num > season_week_cut) |
      ((df$Year == year + 1) & df$Week_num <= season_week_cut)
  )
  
  df$Season <- ifelse(condition_1, paste0(year - 1, "/", substr(as.character(year), 3, 4)), df$Season)
  df$Season <- ifelse(condition_2, paste0(year, "/", substr(as.character(year + 1), 3, 4)), df$Season)
}
#check<-df[,c("Year","Season_week","Week_num","Season")]

## hemisphere
df$hemisphere <- "North Hemisphere"
df[df$Country=="Australia"|df$Country=="Brazil","hemisphere"] <- "South Hemisphere"

tick_week_break <- c(1,seq(5,30,5),33,37,42,47,52)
label_for_sequence_season_week <- c(21,25,30,35,40,45,50,1,5,10,15,20)

## omit missing, so that no need to assign color for NA
df<-df[!is.na(df$hospitalisation_rate_log) &
         !is.infinite(df$hospitalisation_rate_log),]  #log (0) = -Inf, need to exclude them


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## NORTH ##
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
df_north<-df[df$hemisphere=="North Hemisphere",]

# df_north<- df[df$hemisphere=="North Hemisphere" &
#                         df$Season!="2016/17"&
#                         df$Season!="2020/21",]


heatmap_bycountryyear_north_flu<-ggplot(
      data=df_north, 
       aes(x = Season_week,  #Week_num_season
           y = Season,       #Year_Country
           fill = hospitalisation_rate_log)) +  #hospitalisation_rate_scaled
  geom_raster()          +
  scale_x_continuous(breaks=tick_week_break,
                     labels= label_for_sequence_season_week)+
  
  scale_fill_gradientn(colours=c("#FFFFFFFF","#088199"),
                       na.value = "white")  +         
  facet_grid(rows = vars(Country)
             #,cols = vars(hemisphere) 
  )+
  labs(title="Log Influenza Hospitalisation Rate", 
       x="", #Calendar week number label in deck
       y="Influenza season",
       fill="Log Rate",
       )+
  theme_minimal()+
  theme(strip.text = element_text(
    size = 22, color = "black"),
        panel.border = element_blank(),  
        panel.grid.major = element_blank(), 
        legend.position=c(0.07,0.55), 
        panel.grid.minor = element_blank(),  
        text=element_text(size=17, 
                          family="Arial"), 
        axis.line = element_line(colour = "black")) 
  
print(heatmap_bycountryyear_north_flu)

ggsave("Output/graphs/01_01_global overview/01_globaloverview_heatmap_flu_north.png",
       plot = heatmap_bycountryyear_north_flu,
       width = 10, height = 8, dpi = 300)


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## SOUTH ##
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#df_south<-df[df$hemisphere=="South Hemisphere",]
df_south<- df[df$hemisphere=="South Hemisphere" &df$Year!= 2017,]

year_lable_south <-c(2018:2023)
tick_week_break_south <- c(1,seq(5,50,5),52)


heatmap_bycountryyear_south_flu<-ggplot(
  data=  df_south , 
  aes(x = Week_num,  #Week_num_season
      y = Year,   #Year_Country
      fill = hospitalisation_rate_log)) +  #hospitalisation_rate_scaled
  geom_raster()          +
  scale_y_continuous(breaks = year_lable_south)+
  scale_x_continuous(breaks=tick_week_break_south
                     #,labels= label_for_sequence_season_week
                     )+
   scale_fill_gradientn(colours=c("#FFFFFFFF","#088199"),
                       na.value = "white")  +         
     facet_grid(rows = vars(Country)
                #,cols = vars(hemisphere) 
     )+
     labs(title="Log Influenza Hospitalisation Rate", 
          x="",#Calendar week number
          y="Year",
          fill="Log Rate",
     )+
     theme_minimal()+
     theme(strip.text = element_text(
       size = 22, color = "black"),
           panel.border = element_blank(),  
           panel.grid.major = element_blank(), 
           legend.position=c(0.1,0.75), 
           panel.grid.minor = element_blank(),  
           text=element_text(size=18, 
                             family="Arial"), 
           axis.line = element_line(colour = "black")) 

print(heatmap_bycountryyear_south_flu)

max(df_south[df_south$Country=="Brazil","Week_num"])# there is a week 53

##save
ggsave("Output/graphs/01_01_global overview/01_globaloverview_heatmap_flu_south.png",
       plot = heatmap_bycountryyear_south_flu,
       width = 10, height = 4.5, dpi = 300)

###save df

write.csv(df, paste0(path,"/Dataset/dataframe_master_cleaned_flu.csv"))


### 
# #++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ## Heat Map 2: global, not desegregated by country ####
# #++++++++++++++++++++++++++++++++++++++++++++++++++++++
# library("lattice") #heat mapping
# library(viridisLite)
# colors <- viridis(100)
# library(RColorBrewer)
# coul <- colorRampPalette(brewer.pal(8, "PiYG"))(25)
# 
# 
# y_scale <- list(at=seq(2017,2024,1))
# x_scale <- list(at=c(1,seq(5,50,5),52))
# 
# heatmap_byyear<-levelplot(hospitalisation_rate ~ Week_num * Year, 
#                           data=df  ,
#                           scales=list(x=x_scale, y=y_scale),
#                           
#                           
#                           xlab="# Week",
#                           ylab="Year",
#                           main="Influenza Hospitalization Rate (per 100,0000), Study Countries",
#                           col.regions = coul
# )
# #cannot tell difference due to out liears and incomparable across countries
# print(heatmap_byyear)
# 
# ggsave("Output/graphs/01_global_flu_byyear.png",
#        plot = heatmap_byyear)



#+++++++++++
#WO test####
#+++++++++++
# 
# install.packages("seastests")
# library(seastests)
# test<-df[df$Country=="US",]
# 
# test2<-rbind(
# test[test$Year==2022,"hospitalisation_rate"][1:30,],
# test[test$Year==2023,"hospitalisation_rate"][1:30,])
# 
# test3<-ts(test2, frequency = 30)
# 
# 
# xx<-seastests::combined_test(test3)
# 
# x <- ts(rnorm(80), frequency=4)
# x<-seastests::combined_test(x)
# plot(decompose(x))
# 
# library(tidyverse)
# 
# test<-df |>
#   group_by(Season)|>
#   mutate(peak_week = which(hospitalisation_rate== max(hospitalisation_rate)))
# test[=="peak_week",]


## double zxis#####
## try doulbe axis
# ann_text_iso_week <- tribble(
#   ~Season_week, ~Country, ~Season, ~ hospitalisation_rate_log
#   #--|--|--|----
#   1, "US", "2016/17",-2
# )
# df_north$Month
# # install.packages("ggh4x")
# library(ggh4x)
# x = interaction(quarter, year)
# ggplot(
#   data=df_north, 
#   aes(x = interaction(Month, Season_week),  #Week_num_season
#       y = Season,       #Year_Country
#       fill = hospitalisation_rate_log)) +  #hospitalisation_rate_scaled
#   geom_raster()          +
# 
#   scale_x_discrete(NULL, guide = "axis_nested")  +
#   
#  
#   # scale_x_continuous(breaks=tick_week_break,
#   #                    labels= label_for_sequence_season_week)+
#   # 
#   scale_fill_gradientn(colours=c("#FFFFFFFF","#088199"),
#                        na.value = "white")  +         
#   facet_grid(rows = vars(Country)
#              #,cols = vars(hemisphere) 
#   )
# # +
#   # labs(title="Log Influenza Hospitalisation Rate", 
#   #      x="Calendar week number",
#   #      y="Influenza season",
#   #      fill="Log Rate",
#   # )+
#   # theme_minimal()+
#   # theme(strip.text = element_text(
#   #   size = 22, color = "black"),
#   #   panel.border = element_blank(),  
#   #   panel.grid.major = element_blank(), 
#   #   legend.position=c(0.07,0.55), 
#   #   panel.grid.minor = element_blank(),  
#   #   text=element_text(size=17, 
#   #                     family="Arial"), 
#   #   axis.line = element_line(colour = "black"))
