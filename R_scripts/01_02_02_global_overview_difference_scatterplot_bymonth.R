#### This is a synthesizing analysis for month peak####
#Date: 3 Feb 2024
#Author: Kai

#setwd("C:/Users/three/Neu Kasten_5440/022 master degree LSHTM/LSHTM/013 Data Challenge/My Sanofi_5440/data_challenge_2024_sanofi")
(path<-getwd())

library(ggplot2)
library(dplyr)
library(tidyr)
#+++++++++++++++++++++++++++++++++
## load  cleaned flu rsv df ####
#+++++++++++++++++++++++++++++++++
df_flu<-read.csv(paste0(path,"/Dataset/dataframe_master_cleaned_flu.csv"))
df_rsv<-read.csv(paste0(path,"/Dataset/dataframe_master_cleaned_rsv.csv"))

df_flu$Disease<-"Influenza"


#Address missing month ####
#correcting Month
df_flu[df_flu$Year==2018 &df_flu$Week_num==1,"Month"]<- 1
df_flu[df_flu$Year==2019 &df_flu$Week_num==1,"Month"]<- 1
#assign month
reference_month<-unique(df_flu[!is.na(df_flu$Month),c("Year","Week_num","Month")])
reference_month<-reference_month[order(reference_month$Year,reference_month$Week_num),]
names(reference_month)<-c("Year","Week_num","month" )#lowercase m

df_flu <- merge(df_flu,
          reference_month,
          all.x=TRUE,
          by.x = c("Year","Week_num"),
          by.y = c("Year","Week_num"))

sum(is.na(df_flu$month))
df_flu$Month<-NULL

df_rsv <- merge(df_rsv,
                reference_month,
                all.x=TRUE,
                by.x = c("Year","Week_num"),
                by.y = c("Year","Week_num"))
df_rsv[is.na(df_rsv$month),"month"]<-12 # it wa s week number 53
sum(is.na(df_rsv$month))
df_rsv$Month<-NULL


# calculating peak month in their hemisphere ####
df_flu_monthlysum <- aggregate(df_flu$hospitalisation_rate, 
                    by=list(Country=df_flu$Country,
                            Year=df_flu$Year,
                            month=df_flu$month),
                    FUN=sum)
names(df_flu_monthlysum)[4]<-"monthly_rate_sum"

df_rsv_monthlysum <- aggregate(df_rsv$hospitalisation_rate, 
                               by=list(Country=df_rsv$Country,
                                       Year=df_rsv$Year,
                                       month=df_rsv$month),
                               FUN=sum)
names(df_rsv_monthlysum)[4]<-"monthly_rate_sum"


# assign  max back to df####
df_flu<-merge(df_flu,
      df_flu_monthlysum,
      all.x=TRUE)
df_rsv<-merge(df_rsv,
              df_rsv_monthlysum,
              all.x=TRUE)



# Pick maximum month ####
df_flu_seasonpeak<-df_flu |> 
  group_by(Country,Season) |> # for north using season is better than groupby year
  select(Disease,Country,Year,Season,monthly_rate_sum,hemisphere,month)|>
  filter(Country!="Australia"&Country!="Brazil")|>
  slice(which.max(monthly_rate_sum))

df_flu_yearpeak<-df_flu |> 
  group_by(Country,Year) |> 
  select(Disease,Country,Year,Season,monthly_rate_sum,hemisphere,month)|>
  filter(Country=="Australia"|Country=="Brazil")|>
  slice(which.max(monthly_rate_sum))

df_rsv_seasonpeak<-df_rsv |> 
  group_by(Country,Season) |> # for north using season is better than groupby year
  select(Disease,Country,Year,Season,monthly_rate_sum,hemisphere,month)|>
  filter(Country!="Australia" & Country!="Brazil")|>
  slice(which.max(monthly_rate_sum))

df_rsv_yearpeak<-df_rsv |> 
  group_by(Country,Year) |> 
  select(Disease,Country,Year,Season,monthly_rate_sum,hemisphere,month)|>
  filter(Country=="Australia"|Country=="Brazil")|>
  slice(which.max(monthly_rate_sum))

df_peak<-rbind(df_flu_seasonpeak,df_flu_yearpeak,
      df_rsv_seasonpeak,df_rsv_yearpeak)


## flag era
df_peak$era<- "Pre-pandemic"
df_peak$era<- ifelse(df_peak$Season %in% c("2020/21","2021/22","2022/23","2023/24"),
                     "Post-pandemic",df_peak$era)


write.csv(df_peak,
          paste0(path,"/Output/tables/table_peak_month_by_seasonyear_country.csv"))
