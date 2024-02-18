#### This is a synthesizing analysis using scatter plot with line ####
#Date: 17 - FEB - 2024
#Author: Kai
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++


#setwd("C:/Users/three/Neu Kasten_5440/022 master degree LSHTM/LSHTM/013 Data Challenge/My Sanofi_5440/data_challenge_2024_sanofi")
(path<-getwd())

library(ggplot2)
library(dplyr)
library(tidyr)

#+++++++++++++++++++++++++++++++
## load  cleaned flu rsv df ####
#+++++++++++++++++++++++++++++++
df_flu<-read.csv(paste0(path,"/Dataset/dataframe_master_cleaned_flu.csv"))
df_flu$Disease<-"Influenza"

df_rsv<-read.csv(paste0(path,"/Dataset/dataframe_master_cleaned_rsv.csv"))

#+++++++++++++++++++++++++++
## redefine season/year ####
#+++++++++++++++++++++++++++

## season cut point
season_week_cut <- 18 #iso week 19 become season week 1

df_flu$Season_week<- ifelse( df_flu$Week_num > season_week_cut, 
                             df_flu$Week_num - season_week_cut,
                             df_flu$Week_num + 52 - season_week_cut)
df_rsv$Season_week<- ifelse( df_rsv$Week_num > season_week_cut, 
                             df_rsv$Week_num - season_week_cut,
                             df_rsv$Week_num + 52 - season_week_cut)

## season encoding
df_flu$Season <- NA
for (year in 2017:2024) {
  condition_1 <- (df_flu$Year == year & df_flu$Week_num <= season_week_cut)
  condition_2 <- (
    (df_flu$Year == year & df_flu$Week_num > season_week_cut) |
      ((df_flu$Year == year + 1) & df_flu$Week_num <= season_week_cut) )
  df_flu$Season <- ifelse(condition_1, paste0(year - 1, "/", substr(as.character(year), 3, 4)), df_flu$Season)
  df_flu$Season <- ifelse(condition_2, paste0(year, "/", substr(as.character(year + 1), 3, 4)), df_flu$Season)
}

df_rsv$Season <- NA
for (year in 2017:2024) {
  condition_1 <- (df_rsv$Year == year & df_rsv$Week_num <= season_week_cut)
  condition_2 <- (
    (df_rsv$Year == year & df_rsv$Week_num > season_week_cut) |
      ((df_rsv$Year == year + 1) & df_rsv$Week_num <= season_week_cut) )
  df_rsv$Season <- ifelse(condition_1, paste0(year - 1, "/", substr(as.character(year), 3, 4)), df_rsv$Season)
  df_rsv$Season <- ifelse(condition_2, paste0(year, "/", substr(as.character(year + 1), 3, 4)), df_rsv$Season)
}

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# identify peak week, specific to hemisphere & season/year
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#North => season
#South => year

temp_flu_seasonpeak <- df_flu |> 
  group_by(Country,Season) |> 
  select(Disease,Country,Year,Season,Week_num,hospitalisation_rate,Season_week,hemisphere,Month)|>
  filter(hemisphere=="North Hemisphere")|>
  slice(which.max(hospitalisation_rate))

temp_flu_yearpeak <- df_flu |> 
  group_by(Country,Year) |> 
  select(Disease,Country,Year,Season,Week_num,hospitalisation_rate,Season_week,hemisphere,Month)|>
  filter(hemisphere=="South Hemisphere")|>
  slice(which.max(hospitalisation_rate))

temp_rsv_seasonpeak<-df_rsv |> 
  group_by(Country,Season) |> # for north using season is better than groupby year
  select(Disease,Country,Year,Season,Week_num,hospitalisation_rate,Season_week,hemisphere,Month)|>
  filter(hemisphere=="North Hemisphere")|>
  slice(which.max(hospitalisation_rate))

temp_rsv_yearpeak<-df_rsv |> 
  group_by(Country,Year) |> 
  select(Disease,Country,Year,Season,Week_num,hospitalisation_rate,Season_week,hemisphere,Month)|>
  filter(hemisphere=="South Hemisphere")|>
  slice(which.max(hospitalisation_rate))

df_peak<-rbind(temp_flu_seasonpeak,temp_flu_yearpeak,
               temp_rsv_seasonpeak,temp_rsv_yearpeak)
rm(temp_flu_seasonpeak,temp_flu_yearpeak,temp_rsv_seasonpeak,temp_rsv_yearpeak)


###!!REFINE wierd points!! ####
#(1)france's 2023 data is not updated to end of year, so omit
df_peak<-df_peak[!(df_peak$Season=="2023/24" &df_peak$Country=="France"),]

#df_peak[df_peak$Year==2020 &df_peak$Country=="Brazil","era"]<-"Post-pandemic" # week 12 should be regarded post
#keep it prepan
#df_peak<-df_peak[!(df_peak$Week_num==52 &df_peak$Country=="Brazil"),]
# 2021 week 52 and 2022 week 1 are too close
# df_peak[df_peak$Week_num==1 &
#           df_peak$Country=="Brazil"&
#           df_peak$Year==2022,"Season"] <- "2022/23"
#change it to 2022/23 to fill up the lack of 2022/23

df_peak <- df_peak[df_peak$Season!="2016/17",]#England' week 5 in 2017

#++++++++++++++++++++++++++++++
## prepare spider plot flu ####
#++++++++++++++++++++++++++++++
# create season's level
df_peak$Season_level <-  match(df_peak$Season, 
                               unique(df_peak$Season))#careful, it is correct because the original one is ordered already
# df_peak[,c("Season_level","Season")] #check

#relevel
df_peak$Country <- factor(df_peak$Country,levels = c("England","France","Türkiye","US","Australia","Brazil"))


#+++++++++++++++++++
## plotting  ####
#+++++++++++++++++++




ggplot(data = df_peak , #[df_peak$Disease == "Influenza", ],
       mapping = aes(x = Season_week, 
                     y = Season_level, 
                     col = Country
                     )) + 
  facet_grid(vars(Disease))+
  geom_point(size = 2.6) +
  geom_path(    #not geom_segment
    aes( group = Country ),
    position = position_dodge(width = 0.5),
    arrow = arrow(type = "open",
                  length = unit(0.15, "inches"),
                  ends = "last")#arrow direction to ends
  )

  # 

lag(df_peak$Season_week)
lag(df_peak$Season_level)

 write.csv(file=paste0(path,"/Output/tables/table_peakweek_spiderbycountry_flu.csv"),
           df_wide_flu_plot)



    


