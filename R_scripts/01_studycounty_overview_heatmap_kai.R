#### This is the global overview analysis using heatmap ####
#Date: 2- JAN 2024
#Author: Kai

#++++++++++++
## set up####
#++++++++++++
#directory

#setwd("C:/Users/three/Neu Kasten_5440/022 master degree LSHTM/LSHTM/013 Data Challenge/My Sanofi_5440/data_challenge_2024_sanofi")
(path<-getwd())

if (!require("readxl")) install.packages("readxl") 
library(readxl)
library(ggplot2)
library("lattice") #heat mapping




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


##pop
unique(df_raw[is.na(df_raw$Population),"Country"])

df_raw[df_raw$Country=="Australia","Population"]<-26582504
df_raw[df_raw$Country=="Brazil","Population"]<-217092900
df_raw[df_raw$Country=="England","Population"]<-67860917 # UK?
df_raw[df_raw$Country=="France","Population"]<-64825831
df_raw[df_raw$Country=="US","Population"]<-341004904
#next step: should use population from each year


## rate per 100,000 (right?)
flag_no_rate<-is.na(df_raw$hospitalisation_rate)
summary(df_raw$hospitalisation_rate)


df_raw[flag_no_rate,"hospitalisation_rate"] <-(df_raw[flag_no_rate,"hospitalisation_num"] / 
                                              df_raw[flag_no_rate,"Population"]*
                                              100000)
#missing weeks, fine
df_raw[is.na(df_raw$hospitalisation_rate),]


df<-df_raw

#++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Heat Map 1: global, not desegregated by country ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++

library(viridisLite)
colors <- viridis(100)
library(RColorBrewer)
coul <- colorRampPalette(brewer.pal(8, "PiYG"))(25)


y_scale <- list(at=seq(2017,2024,1))
x_scale <- list(at=c(1,seq(5,50,5),52))

levelplot(hospitalisation_rate ~ Week_num * Year, 
          data=df  ,
          scales=list(x=x_scale, y=y_scale),
          
          
          xlab="# Week",
          ylab="Year",
          main="Influenza Hospitalization Rate (per 100,0000), Study Countries",
          col.regions = coul
          )
#cannot tell difference due to out liears and incomparable across countries

?levelplot

#++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Heat Map 2: global, desegregated by country ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++

# create year-country variable
df$Year_Country<-paste0(df$Year,"-",df$Country)

# levelplot(hospitalisation_rate ~ Week_num * Year_Country, 
#           data=df  ,
#           scales=list(x=x_scale, y=y_scale),
#           
#           
#           xlab="# Week",
#           ylab="Year",
#           main="Influenza Hospitalization Rate (per 100,0000), Study Countries",
#           col.regions = coul
# )
#non numeric error
?scale
df$hospitalisation_rate_scaled<-scale(df$hospitalisation_rate)
ggplot(data=df,
       aes(x = Week_num,
           y = Year_Country, 
           fill = hospitalisation_rate_scaled)) +
  geom_raster()          +
  scale_fill_gradientn(colours=c("#FFFFFFFF","blue","#FF0000FF"))  +         
  scale_x_continuous(breaks=c(1,seq(5,50,5),52))+
  labs(title="global influenza hospitalisation rate", caption="need fixing Australia in South H.",   x="# Weeks", y="Year-Country")
                       
#country year

