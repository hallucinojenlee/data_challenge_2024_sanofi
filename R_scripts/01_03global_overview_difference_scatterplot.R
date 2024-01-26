#### This is a synthesizing analysis ####
#Date: 26- JAN 2024
#Author: Kai

#setwd("C:/Users/three/Neu Kasten_5440/022 master degree LSHTM/LSHTM/013 Data Challenge/My Sanofi_5440/data_challenge_2024_sanofi")
(path<-getwd())

library(ggplot2)
library(dplyr)

#+++++++++++++++++++++++++++++++++
## load  cleaned flu rsv df ####
#+++++++++++++++++++++++++++++++++
df_flu<-read.csv(paste0(path,"/Dataset/dataframe_master_cleaned_flu.csv"))
df_rsv<-read.csv(paste0(path,"/Dataset/dataframe_master_cleaned_rsv.csv"))


# claculating peak week in theri hemisphere
df_flu_seasonpeak<-df_flu |> 
  group_by(Country,Season) |> # for north using season is better than groupby year
  select(Disease,Country,Year,Season,Week_num,hospitalisation_rate)|>
  filter(Country!="Australia"&Country!="Brazil")|>
  slice(which.max(hospitalisation_rate))

df_flu_yearpeak<-df_flu |> 
  group_by(Country,Year) |> 
  select(Disease,Country,Year,Season,Week_num,hospitalisation_rate)|>
  filter(Country=="Australia"&Country=="Brazil")|>
  slice(which.max(hospitalisation_rate))

df_rsv_seasonpeak<-df_rsv |> 
  group_by(Country,Season) |> # for north using season is better than groupby year
  select(Disease,Country,Year,Season,Week_num,hospitalisation_rate)|>
  filter(Country!="Australia"&Country!="Brazil")|>
  slice(which.max(hospitalisation_rate))

df_rsv_yearpeak<-df_rsv |> 
  group_by(Country,Year) |> 
  select(Disease,Country,Year,Season,Week_num,hospitalisation_rate)|>
  filter(Country=="Australia"&Country=="Brazil")|>
  slice(which.max(hospitalisation_rate))

df_peak<-rbind(df_flu_seasonpeak,df_flu_yearpeak,
      df_rsv_seasonpeak,df_rsv_yearpeak)



flu_rate_max <- flu %>% group_by(Country,Year) %>% slice(which.max(hospitalisation_rate))





