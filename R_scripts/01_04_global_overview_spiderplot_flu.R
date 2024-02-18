#### This is a synthesizing analysis using spiderplot ####
#Date: 12 - FEB - 2024
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
df_rsv<-read.csv(paste0(path,"/Dataset/dataframe_master_cleaned_rsv.csv"))


df_flu$Disease<-"Influenza"

#+++++++++++++++++++++++++++
## redefine season/year ####
#+++++++++++++++++++++++++++

## season cut point
season_week_cut <- 18 #iso week 29 become season week 1

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

table(df_peak$Week_num)
hist(df_peak$Week_num)


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

#!!relevel into 6 levels, due to omitting 2020/21 data(season_level=4) #### 
df_peak$Season_level <- ifelse(df_peak$Season_level > 4,
                         df_peak$Season_level-1, df_peak$Season_level)

# transpose from long to wide
df_wide_flu <- as.data.frame(
               pivot_wider(df_peak[df_peak$Disease=="Influenza",], 
                         id_cols=Season, 
                         values_from=Season_level, #Year
                         names_from=Week_num,
                         names_prefix="",
                         values_fn =unique))#due to duplicate peak weeks
rownames(df_wide_flu) <- df_wide_flu$Season

df_wide_flu <- df_wide_flu |> select( -Season )

### insert missing peaks as NA ####
missing_peakweeks <- setdiff(1:52, #complete range of weeks in a year
                             colnames(df_wide_flu))#what I had

temp_missing_peakweeks <- matrix(nrow= nrow(df_wide_flu),
                                 ncol= length(missing_peakweeks))
colnames(temp_missing_peakweeks) <- missing_peakweeks

df_wide_flu <- cbind(df_wide_flu,temp_missing_peakweeks)

setdiff(1:52,colnames(df_wide_flu))#check

#reorder columns (don't use order()==> wrong)
df_wide_flu <- df_wide_flu[,as.character(52:1)] 


# set min/max row
max_row <- setNames(rep(6, ncol(df_wide_flu)), names(df_wide_flu))
min_row <- setNames(rep(1, ncol(df_wide_flu)), names(df_wide_flu))

df_wide_flu_plot <- rbind.data.frame(max_row, min_row, #not rbind
                                     df_wide_flu)
rownames(df_wide_flu_plot)[c(1,2)] <- c("max","min")

write.csv(file=paste0(path,"/Output/tables/table_peakweek_spider_flu.csv"),
          df_wide_flu_plot)


#+++++++++++++++++++
## plotting flu ####
#+++++++++++++++++++
library(fmsb)

##omit insufficient observations in 2020/21
#must be composed of more than 3 variables as axes and the rows
df_wide_flu_plot <- df_wide_flu_plot[-which(rownames(df_wide_flu_plot)=="2020/21"),]


# colors_manual_flu <- c("purple", "#f77935","blue" ,"yellow",
#                        "green", "#18cdf1","grey","pink")

colors_manual_flu <- c(rep("#f77935",3),rep("deepskyblue2",3))


legend_labels <- rownames(df_wide_flu_plot)[-c(1,2)]#no min/max



     radarchart(df_wide_flu_plot,
                axistype=1, # show center axis 
                caxislabels =legend_labels , 
                axislabcol="gray",
                calcex= 0.8,
                na.itp = FALSE,# true =weird lines for low data points
                maxmin = TRUE,
                pcol=colors_manual_flu,
                cglty = 1,       # Grid line type,
                pty = 19,        # dot type (31 = filled)
                plwd = 0.5,        # dot line width
                seg = 5,         # Number of segments
                cglwd = 0.1,     # Grid line width
                cglcol = "gray", # Grid line color
                vlcex = 0.8,     # Label size,
)

    
legend(x="topright", 
       title="Period",
       legend = c("Pre-pandemic","Post-pandemic"), 
       col = c("#f77935","deepskyblue2"),
       pch = c(19, 19),
#       lty=1:1, #solid line
       cex = 0.6,
       box.lty=0,
       text.width = 2  # Limit the width of the legend box
)



#Alternative: library("ggradar"); ggradar(plot.data = df_wide_flu )
#not able to deal with NA as well




    


