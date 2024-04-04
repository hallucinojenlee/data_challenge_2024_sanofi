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
#season_week_cut <- 46 #iso week 19 become season week 1
season_week_cut_north <- 18 
season_week_cut_south <- 44 

# FLU: north/south
df_flu_north <- df_flu[df_flu$hemisphere=="North Hemisphere",]
df_flu_south <- df_flu[df_flu$hemisphere=="South Hemisphere",]


df_flu_north$Season_week<- ifelse( df_flu_north$Week_num > season_week_cut_north, 
                                   df_flu_north$Week_num - season_week_cut_north,
                                   df_flu_north$Week_num + 52 - season_week_cut_north)

df_flu_south$Season_week<- ifelse( df_flu_south$Week_num > season_week_cut_south, 
                                   df_flu_south$Week_num - season_week_cut_south,
                                   df_flu_south$Week_num + 52 - season_week_cut_south)

#RSV: north/south
df_rsv_north <- df_rsv[df_rsv$hemisphere=="North Hemisphere",]
df_rsv_south <- df_rsv[df_rsv$hemisphere=="South Hemisphere",]

df_rsv_north$Season_week<- ifelse( df_rsv_north$Week_num > season_week_cut_north, 
                                   df_rsv_north$Week_num - season_week_cut_north,
                                   df_rsv_north$Week_num + 52 - season_week_cut_north)
df_rsv_south$Season_week<- ifelse( df_rsv_south$Week_num > season_week_cut_south, 
                                   df_rsv_south$Week_num - season_week_cut_south,
                                   df_rsv_south$Week_num + 52 - season_week_cut_south)



## season encoding ####
#(1) flu north
df_flu_north$Season <- NA
for (year in 2017:2024) {
  condition_1 <- (df_flu_north$Year == year & df_flu_north$Week_num <= season_week_cut_north)
  condition_2 <- (
    (df_flu_north$Year == year & df_flu_north$Week_num > season_week_cut_north) |
      ((df_flu_north$Year == year + 1) & df_flu_north$Week_num <= season_week_cut_north) )
  df_flu_north$Season <- ifelse(condition_1, paste0(year - 1, "/", substr(as.character(year), 3, 4)), df_flu_north$Season)
  df_flu_north$Season <- ifelse(condition_2, paste0(year, "/", substr(as.character(year + 1), 3, 4)), df_flu_north$Season)
}

#(2) flu south
df_flu_south$Season <- NA
for (year in 2017:2024) {
  condition_1 <- (df_flu_south$Year == year & df_flu_south$Week_num <= season_week_cut_south)
  condition_2 <- (
    (df_flu_south$Year == year & df_flu_south$Week_num > season_week_cut_south) |
      ((df_flu_south$Year == year + 1) & df_flu_south$Week_num <= season_week_cut_south) )
  df_flu_south$Season <- ifelse(condition_1, paste0(year - 1, "/", substr(as.character(year), 3, 4)), df_flu_south$Season)
  df_flu_south$Season <- ifelse(condition_2, paste0(year, "/", substr(as.character(year + 1), 3, 4)), df_flu_south$Season)
}#should south backward a year?

#(3) rsv north
df_rsv_north$Season <- NA
for (year in 2017:2024) {
  condition_1 <- (df_rsv_north$Year == year & df_rsv_north$Week_num <= season_week_cut_north)
  condition_2 <- (
    (df_rsv_north$Year == year & df_rsv_north$Week_num > season_week_cut_north) |
      ((df_rsv_north$Year == year + 1) & df_rsv_north$Week_num <= season_week_cut_north) )
  df_rsv_north$Season <- ifelse(condition_1, paste0(year - 1, "/", substr(as.character(year), 3, 4)), df_rsv_north$Season)
  df_rsv_north$Season <- ifelse(condition_2, paste0(year, "/", substr(as.character(year + 1), 3, 4)), df_rsv_north$Season)
}

#(4) rsv south
df_rsv_south$Season <- NA
for (year in 2017:2024) {
  condition_1 <- (df_rsv_south$Year == year & df_rsv_south$Week_num <= season_week_cut_south)
  condition_2 <- (
    (df_rsv_south$Year == year & df_rsv_south$Week_num > season_week_cut_south) |
      ((df_rsv_south$Year == year + 1) & df_rsv_south$Week_num <= season_week_cut_south) )
  df_rsv_south$Season <- ifelse(condition_1, paste0(year - 1, "/", substr(as.character(year), 3, 4)), df_rsv_south$Season)
  df_rsv_south$Season <- ifelse(condition_2, paste0(year, "/", substr(as.character(year + 1), 3, 4)), df_rsv_south$Season)
}

#combind back
df_flu<- rbind(df_flu_north,df_flu_south)
df_rsv<- rbind(df_rsv_north,df_rsv_south)

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# identify peak week, specific to hemisphere & season/year
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#North => season (week 19-18)
#South => season (week 45-44)
temp_flu_seasonpeak <- df_flu |>
  group_by(Country,Season) |>
  select(Disease,Country,Year,Season,Week_num,hospitalisation_rate,Season_week,hemisphere,Month)|>
  slice(which.max(hospitalisation_rate))

temp_rsv_seasonpeak <- df_rsv |>
  group_by(Country,Season) |> # for north using season is better than groupby year
  select(Disease,Country,Year,Season,Week_num,hospitalisation_rate,Season_week,hemisphere,Month)|>
  slice(which.max(hospitalisation_rate))

df_peak <- rbind(temp_flu_seasonpeak,
                 temp_rsv_seasonpeak)


#relevel
df_peak$Country <- factor(df_peak$Country,levels = c("England","France","Türkiye","US","Australia","Brazil"))


###REFINE data points####
#(1)france's 2023 data is not updated to end of year, so omit
df_peak<-df_peak[!(df_peak$Season=="2023/24" &df_peak$Country=="France"),]

#(2)exclude too early data (year 2016) from england
df_peak <- df_peak[df_peak$Season!="2016/17",]#England' week 5 in 2017


#+++++++++++++++++++++
## plotting north ####
#+++++++++++++++++++++
#tick
(tick_week_break_north <- c(1,2,seq(7,32,5),35,39,44,49,52))
(label_north_seasonweek <- c(19,20,25,30,35,40,45,50,1,5,10,15,18))



plot_north <- ggplot(data = df_peak[df_peak$hemisphere == "North Hemisphere", ],
       mapping = aes(x = Season_week, 
                     y = Season, #Season_level
                     col = Country
                     )) + 
  facet_grid(rows= vars(Disease)
             #,cols = vars(hemisphere)
             )+
  scale_x_continuous(breaks=tick_week_break_north,
                     labels= label_north_seasonweek,
                     limits = c(1, 52))+

  geom_point(size = 2.6) +
  geom_path(    #not geom_segment
    aes( group = Country ),
    position = position_dodge(width = 0.02),
    arrow = arrow(type = "open",
                  length = unit(0.15, "inches"),
                  ends = "last")#arrow direction to ends
  )+
  labs(title = "  Hospitalisation Peak Week",
       x="Calendar Week",
       y="")+
  
  theme_minimal()+
  theme(legend.position="bottom")+
  guides(col = guide_legend(title = NULL))#remove legend title

print(plot_north)

#+++++++++++++++++++++
## plotting south ####
#+++++++++++++++++++++
#tick
(tick_week_break_south <- c(1,6,9,seq(13,48,5)))
(label_south_seasonweek <- c(45,50,1,seq(5,40,5)))


plot_south <- ggplot(data = df_peak[df_peak$hemisphere == "South Hemisphere", ],
                     mapping = aes(x = Season_week, 
                                   y = Season, #Season_level
                                   col = Country
                     )) + 
  facet_grid(rows= vars(Disease)
             #,cols = vars(hemisphere)
  )+
  scale_x_continuous(breaks=tick_week_break_south,
                     labels= label_south_seasonweek,
                     limits = c(1, 52))+
  
  geom_point(size = 2.6) +
  geom_path(    #not geom_segment
    aes( group = Country ),
    position = position_dodge(width = 0.02),
    arrow = arrow(type = "open",
                  length = unit(0.15, "inches"),
                  ends = "last")#arrow direction to ends
  )+
  labs(title = "  Hospitalisation Peak Week",
       x="Calendar Week",
       y="")+
  theme_minimal()+
  theme(legend.position="bottom")+
  scale_color_manual(values= c("Australia" = "red",
                               "Brazil" = "blue"))+
  guides(col = guide_legend(title = NULL))

print(plot_south)


## arrange in one plot ####
library(ggpubr)

plot_global<- ggarrange(plot_north, plot_south, 
          labels = c("North",
                     "South"),
          ncol = 2, nrow = 1)

print(plot_global)


ggsave(paste0("Output/to_think_global_health/pathplot_global_byhemidisease.png"),
       plot = plot_global,
       width = 8, height = 6, dpi = 300)


#++++++++++++++++++++++++++++++++++++++
# difference in pre-and-post peaks ####
#++++++++++++++++++++++++++++++++++++++

df_peak$flag_postcovid <- df_peak$Season %in% c("2020/21","2021/22","2022/23","2023/24")

# Alternative, what if 2020/21 is post-pandemic? 
# df_peak$flag_postcovid <- df_peak$Season %in% c("2021/22","2022/23","2023/24")
# In northern hemisphere post-COVID (since 2019/20 season), the peak hospitalisation occurred averagely 1.8 weeks earlier for influenza epidemic, and 9.3 weeks earlier for RSV, compared to pre-COVID average peaks. In Southern hemisphere, post-COVID peaks were 7.7 weeks earlier for influenza, but 0 week later for RSV (Plot 1).


df_peak[,c("Season","flag_postcovid")]

#aggregate
df_peak_mean <- aggregate(df_peak$Season_week, 
          list(df_peak$Disease,df_peak$flag_postcovid,df_peak$Country), 
          FUN=mean) 

colnames(df_peak_mean)<-c("Disease","PostCOVID","Country","Mean_season_week")

# difference for each country-disease ####
df_diff <- df_peak_mean |>
  group_by(Country, Disease) |>
  summarise(Difference = round(
                           mean(Mean_season_week[PostCOVID == TRUE]) 
                         - mean(Mean_season_week[PostCOVID == FALSE]),1),
                       .groups = 'drop')
  
# mean difference by hemisphere-disease ####
df_diff$hemisphere <- ifelse(df_diff$Country %in% 
  c("Australia","Brazil"),"South Hemisphere",
  "North Hemisphere")

# aprroach 1: including all countries
df_diff_mean_hemisphere <- df_diff |>
  group_by( Disease, hemisphere) |>
  summarise(Difference = round(mean(Difference),1),
            .groups = 'drop')

df_diff_mean <- df_diff |>
  group_by( Disease) |>
  summarise(Difference = round(mean(Difference),1),
            .groups = 'drop')

# approach 2: global but excluding Brazil

df_diff_mean_nobrazil <- df_diff |>
  filter(Country!="Brazil" )|>
  group_by( Disease) |>
  summarise(Difference = round(mean(Difference),1),
            .groups = 'drop')


## merge df_peak back to df_peak_mean ####
#to provide a baseline comparison
df_peak <- merge(df_peak,
      df_peak_mean[df_peak_mean$PostCOVID==FALSE,],
      all.x=TRUE,
      by=c("Country","Disease"))

df_peak<- df_peak|> 
  rename(Precovid_mean_season_week=Mean_season_week)|>
  mutate( Difference_compared_to_precovid  = 
            Season_week - Precovid_mean_season_week)

#see if recent years are less different? 

df_diff_mean_nobrazil_byseason <- df_peak |>
  filter(flag_postcovid==TRUE & Country!="Brazil")|>
  group_by(Disease, Season)|>
  summarise(Mean_Difference = round(mean(Difference_compared_to_precovid),1),
            .groups = 'drop')
print(df_diff_mean_nobrazil_byseason)



## export to think global health ####

write.csv(df_peak,
          "Output/to_think_global_health/table_peak.csv",
          row.names=FALSE)

write.csv(df_diff_mean_hemisphere,
          "Output/to_think_global_health/table_peak_preandpost_mean_byhemisphere.csv",
          row.names=FALSE)

write.csv(df_diff_mean,
          "Output/to_think_global_health/table_peak_preandpost_mean.csv",
          row.names=FALSE)

write.csv(df_flu,
          "Output/to_think_global_health/dataframe_master_flu.csv",
          row.names=FALSE)

write.csv(df_rsv,
          "Output/to_think_global_health/dataframe_master_rsv.csv",
          row.names=FALSE)

write.csv(df_diff_mean_nobrazil,
          "Output/to_think_global_health/table_peak_preandpost_mean_nobrazil.csv",
          row.names=FALSE)
write.csv(df_diff_mean_nobrazil_byseason,
          "Output/to_think_global_health/table_peak_preandpost_mean_nobrazil_byseason.csv",
          row.names=FALSE)

