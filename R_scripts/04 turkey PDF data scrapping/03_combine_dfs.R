library(dplyr)
source("C:/Users/three/OneDrive - London School of Hygiene and Tropical Medicine/Neu Kasten/022 master degree LSHTM/LSHTM/013 Data Challenge/My Sanofi/Turkey data/02_03 df2.r")

getwd()


rm(df)

df_raw<-rbind(df1 , df2)


df_raw$month_week<-sub(".pdf","",rownames(df_raw))
rownames(df_raw)<-NULL

df_raw$year<-substr(df_raw$month_week, start=1, stop=4)
df_raw$week_raw<-sub(".*_(.*)", "\\1", df_raw$month_week)

#####################
### WEEK cleaning ###
#####################
#reorder by week_raw
df_raw$week<-df_raw$week_raw

week_order <- substr(df_raw$week_raw, nchar(df_raw$week_raw) - 1,nchar(df_raw$week_raw))

df_raw<-df_raw[with(df_raw, order(year,week_raw,decreasing = TRUE)),]

flag_multiple_week<-nchar(df_raw$week_raw)>2


#2023, 2022
df_raw[flag_multiple_week &
       (df_raw$year=="2023"|
       df_raw$year=="2022") ,"week"]<-c(39,35,30,24,39)
#2019
df_raw[flag_multiple_week &df_raw$year=="2019" ,"week"]<-c(39,32,26)
df_raw[flag_multiple_week &df_raw$year=="2019" ,"flu"]<-c(1,2,8)
#df_raw[flag_multiple_week &df_raw$year=="2019" ,"fluA"]<-c(0,0,0)
df_raw[flag_multiple_week &df_raw$year=="2019" ,"fluB"]<-c(1,2,8)
df_raw[flag_multiple_week &df_raw$year=="2019" ,"RSV"]<-c(0,1,0)

#2018
df_raw[flag_multiple_week &df_raw$year=="2018" ,"week"]<-c(39,32,28,24)
df_raw[flag_multiple_week &df_raw$year=="2018" ,"flu"]<-c(5,0,1,0)
df_raw[flag_multiple_week &df_raw$year=="2018" ,"fluA"]#<-c(4,0,0,0)
df_raw[flag_multiple_week &df_raw$year=="2018" ,"fluB"]<-c(1,0,1,0)
df_raw[flag_multiple_week &df_raw$year=="2018" ,"RSV"]<-c(0,2,1,1)


testt<-df_raw[flag_multiple_week ,]

###########
#population
###########
#~2022:   https://data.tuik.gov.tr/Bulten/Index?p=The-Results-of-Address-Based-Population-Registration-System-2022-49685&dil=2
#2023~:   https://www.worldometers.info/world-population/turkey-population/
value_pop<-c(80810525,	82003882,	83154997,	83614362,	84680273,	85279553, 85816199, 86061408 )
year_for_pop<- 2017:2024  
population<-data.frame(year=year_for_pop,
             population=value_pop)

#join to df_raw
df_raw<-merge(df_raw,
              population,
              all.x = TRUE)

df_raw$year<-as.integer(df_raw$year)
df_raw$week<-as.integer(df_raw$week)
#########################
#Month#
#######
#the beging of turkey's week starts from Monday, ISO week!!
isoweek_ref<-read.csv("C:/Users/three/OneDrive - London School of Hygiene and Tropical Medicine/Neu Kasten/022 master degree LSHTM/LSHTM/013 Data Challenge/My Sanofi/Turkey data/data/isoweek_reference.csv")
isoweek_ref<-isoweek_ref[,c(1,2,3,5)]
sum(duplicated(isoweek_ref))


df_raw<-merge(df_raw,
            isoweek_ref,
            by.x=c("year","week"),
            by.y=c("Year","Week_num"),
              all.x = TRUE)

df_raw<-df_raw[-which(df_raw$Week_date=="31/12/2018"),] #error
sum(duplicated(df_raw))
########################################
##flu

df_flu<-data.frame(
  Country ="Türkiye",
  Disease ="Influenza",
  Year = as.integer(df_raw$year),
  Month =as.integer(df_raw$Month),
  Week_num =as.integer(df_raw$week),
  Week_type = "isoweek",
  Week_date	=df_raw$Week_date,
  hospitalisation_num	=as.integer(df_raw$flu),
  hospitalisation_rate	=NA,
  Flu_A	=as.integer(df_raw$fluA),
  Flu_B	=as.integer(df_raw$fluB),
  Population= as.integer(df_raw$population)
  
)
names(df_flu)

#final order
df_flu<-df_flu[order(df_flu$Year,df_flu$Week_num,decreasing = TRUE),]

##FLU -B =A
df_flu$Flu_A <- df_flu$hospitalisation_num - df_flu$Flu_B

write.csv(df_flu,
          "C:/Users/three/OneDrive - London School of Hygiene and Tropical Medicine/Neu Kasten/022 master degree LSHTM/LSHTM/013 Data Challenge/My Sanofi/Turkey data/data/dataframe_turkey_influenza.csv",
   row.names=FALSE)





###################
##RSV
df_RSV<-data.frame(
  Country ="Türkiye",
  Disease ="RSV",
  Year = as.integer(df_raw$year),
  Month =as.integer(df_raw$Month),
  Week_num =as.integer(df_raw$week),
  Week_type = "isoweek",
  Week_date	=df_raw$Week_date,
  hospitalisation_num	=as.integer(df_raw$RSV),
  hospitalisation_rate	=NA,
  RSV_A	=NA,
  RSV_B	=NA,
  Population= as.integer(df_raw$population)
)


df_RSV<-df_RSV[order(df_RSV$Year,df_RSV$Week_num,decreasing = TRUE),]




write.csv(df_RSV,
          "C:/Users/three/OneDrive - London School of Hygiene and Tropical Medicine/Neu Kasten/022 master degree LSHTM/LSHTM/013 Data Challenge/My Sanofi/Turkey data/data/dataframe_turkey_RSV.csv",
          row.names=FALSE)



