#### This is a synthesizing analysis ####
#Date: 26- JAN 2024
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


# claculating peak week in theri hemisphere
df_flu_seasonpeak<-df_flu |> 
  group_by(Country,Season) |> # for north using season is better than groupby year
  select(Disease,Country,Year,Season,Week_num,hospitalisation_rate,Season_week,hemisphere,Month)|>
  filter(Country!="Australia"&Country!="Brazil")|>
  slice(which.max(hospitalisation_rate))

df_flu_yearpeak<-df_flu |> 
  group_by(Country,Year) |> 
  select(Disease,Country,Year,Season,Week_num,hospitalisation_rate,Season_week,hemisphere,Month)|>
  filter(Country=="Australia"|Country=="Brazil")|>
  slice(which.max(hospitalisation_rate))

df_rsv_seasonpeak<-df_rsv |> 
  group_by(Country,Season) |> # for north using season is better than groupby year
  select(Disease,Country,Year,Season,Week_num,hospitalisation_rate,Season_week,hemisphere,Month)|>
  filter(Country!="Australia" & Country!="Brazil")|>
  slice(which.max(hospitalisation_rate))

df_rsv_yearpeak<-df_rsv |> 
  group_by(Country,Year) |> 
  select(Disease,Country,Year,Season,Week_num,hospitalisation_rate,Season_week,hemisphere,Month)|>
  filter(Country=="Australia"|Country=="Brazil")|>
  slice(which.max(hospitalisation_rate))

df_peak<-rbind(df_flu_seasonpeak,df_flu_yearpeak,
      df_rsv_seasonpeak,df_rsv_yearpeak)


## flag era
df_peak$era<- "Pre-pandemic"
df_peak$era<- ifelse(df_peak$Season %in% c("2020/21","2021/22","2022/23","2023/24"),
                     "Post-pandemic",df_peak$era)


##!! REFINE wired points!!
#df_peak[df_peak$Year==2020 &df_peak$Country=="Brazil","era"]<-"Post-pandemic" # week 12 should be regarded post
    #keep it prepan
#df_peak<-df_peak[!(df_peak$Week_num==52 &df_peak$Country=="Brazil"),]
  # 2021 week 52 and 2022 week 1 are too close
df_peak[df_peak$Week_num==1 &
        df_peak$Country=="Brazil"&
        df_peak$Year==2022,"Season"] <- "2022/23"
    #change it to 2022/23 to fill up the lack of 2022/23


df_peak<-df_peak[!(df_peak$Season=="2023/24" &df_peak$Country=="France"),]
  #cause france 2023/24 data is not comprehensive enough 



# before mean by week and season week
temp1<-aggregate(df_peak$Week_num, 
                 list(df_peak$Disease,df_peak$era,df_peak$Country), 
                  FUN=mean) 
colnames(temp1)<-c("Disease","era","Country","Mean_peak_cal_week")

temp2<-aggregate(df_peak$Week_num, 
                 list(df_peak$Disease,df_peak$era,df_peak$Country), 
                 FUN=median) 
colnames(temp2)<-c("Disease","era","Country","Median_peak_cal_week")

temp3<-aggregate(df_peak$Season_week, 
                 list(df_peak$Disease,df_peak$era,df_peak$Country), 
                 FUN=mean) 
colnames(temp3)<-c("Disease","era","Country","Mean_peak_season_week")
temp4<-aggregate(df_peak$Season_week, 
                 list(df_peak$Disease,df_peak$era,df_peak$Country), 
                 FUN=median)
colnames(temp4)<-c("Disease","era","Country","Median_peak_season_week")

table_peak_week_mean_median_byeracountrydisease<-cbind(
  temp1,temp2[4],temp3[4],temp4[4])

#++++++++++++++++++++++++++++++++++++++++++++++++++
### merge back, then post  minus pre's average ####
#++++++++++++++++++++++++++++++++++++++++++++++++++
df_peak <- merge(df_peak,
      table_peak_week_mean_median_byeracountrydisease[table_peak_week_mean_median_byeracountrydisease$era=="Pre-pandemic",-2],
      by= c("Disease","Country"),
      all.x = TRUE)

      #df_peak$week_difference_mean<-df_peak$Week_num - df_peak$Mean_peak_week
          #don't use calender week for north, causing error, due to the baseline as week 1
df_peak$week_diff_median<- ifelse(df_peak$hemisphere=="North Hemisphere",
                                  df_peak$Season_week - df_peak$Median_peak_season_week,
                                  df_peak$Week_num - df_peak$Median_peak_cal_week)

df_peak$week_diff_mean<- ifelse(df_peak$hemisphere=="North Hemisphere",
                                  df_peak$Season_week - df_peak$Mean_peak_season_week,
                                  df_peak$Week_num - df_peak$Mean_peak_cal_week)


# save
write.csv(df_peak,
          paste0(path,"/Output/tables/table_peak_week_by_seasonyear_country.csv"))
write.csv(table_peak_week_mean_median_byeracountrydisease,
          paste0(path,"/Output/tables/table_peak_week_mean_median_byeracountrydisease_preandpost.csv"))


#+++++++++++++++++++
## 1. scatter plot for mean diff####
#+++++++++++++++++++

df_post<-df_peak[df_peak$era=="Post-pandemic",]

df_post$Country<-factor(df_post$Country, 
                        levels = c("England", "France", "Türkiye","US","Australia","Brazil"))

#long to wide
df_post_wide_meandiff<-   pivot_wider(df_post, 
                         id_cols=c(Country,
                                   Season), #A set of columns that uniquely identify each observation. Typically used when you have redundant variables
                         values_from=week_diff_mean, 
                         names_from=Disease,
                         names_prefix="",
                         values_fn = list)
   #some are null => correct , after cross validation
df_post_wide_meandiff<-df_post_wide_meandiff[order(df_post_wide_meandiff$Country),]       
       print(df_post_wide_meandiff)
df_post_wide_meandiff<-df_post_wide_meandiff |>
         unnest(c(Influenza, RSV))        
       
#save
write.csv(df_post_wide_meandiff,
          paste0(path,"/Output/tables/table_wide_mean_difference_in_peak_seasonweek.csv"))

#+++++++++++++++++++++++++++++++++++
## 1-1. plot difference in mean ####
df_post_wide_meandiff_nona<-   na.omit(df_post_wide_meandiff)

p1_diff_mean_omitna<-ggplot(
       data=df_post_wide_meandiff_nona, 
       mapping=aes(x=Influenza, 
                   y=RSV, 
                   col=Country
                   ))+ 
  geom_point(size=6.5)+
  geom_hline(yintercept=0, linetype="dashed", 
             color = "black")+
  geom_vline(xintercept=0, linetype="dashed", 
             color = "black")+
  scale_x_continuous(breaks= c(-15,-10,-5,0,5,10,15,20,25,30,35,40,45))+
  scale_y_continuous(breaks= c(-20,-15,-10,-5,0,5,10,15,20,25))+
  scale_color_manual(values=c("#E69F00", "#56B4E9","deeppink","darkgreen","#999999","purple1"))+
  
  labs(title="Variation in peak weeks compared to pre-pandemic average peak", 
       caption="7 data points are removed due to incomplete reporting of mutual diseases.", 
       #subtitle ="Pre-pandemic baseline is the avrage of the peak weeks before the countries peak weeks of the disease" ,
       x="Difference in Influenza peak (week)",
       y="Difference in RSV peak (week)")+
  theme_minimal()+
  theme(strip.text = element_text(
    size = 15, color = "black"),
    panel.border = element_blank(),  
    panel.grid.major = element_blank(), 
    legend.position=c(0.91,0.82), 
    panel.grid.minor = element_blank(),  
    text=element_text(size=19, 
                      family="Arial"), 
    axis.line = element_line(colour = "black")
    ) 

print(p1_diff_mean_omitna)

ggsave("Output/graphs/01_01_global overview/03_scatterplot_diff_mean.png",
       plot = p1_diff_mean_omitna,
       width = 10, height = 8, dpi = 300)

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 1-2. plot difference by two disease facet with all data points ####


p2_diff_mean_desease_facet<-     ggplot(data=df_post, 
       mapping=aes(x=Season, 
                   y=week_diff_mean, 
                   col=Country
       ))+   
     geom_point(size=6.5)+
     facet_wrap(vars(Disease))+
     geom_hline(yintercept=0, linetype="dashed", 
               color = "black")+
    scale_y_continuous(breaks= c(-20,-15,-10,-5,0,5,10,15,20,25,30,35,40))+
  scale_color_manual(values=c("#E69F00", "#56B4E9","deeppink","darkgreen","#999999","purple1"))+
  
  labs(title="Variation in peak weeks compared to pre-pandemic average peak", 
       x="Season",
       y="Difference in peak (week)")+
  theme_minimal()+
  theme(strip.text = element_text(
    size = 23, color = "black"),
    panel.border = element_blank(),  
    panel.grid.major = element_blank(), 
    legend.position=c(0.93,0.9), 
    panel.grid.minor = element_blank(),  
    text=element_text(size=19, 
                      family="Arial"), 
    axis.line = element_line(colour = "black")
  ) 
print(p2_diff_mean_desease_facet)


ggsave("Output/graphs/01_01_global overview/03_scatterplot_diff_mean_facet.png",
       plot = p2_diff_mean_desease_facet,
       width = 10, height = 8, dpi = 300)


#==============
## 1-2-2. plot difference by two disease facet with all data points ####
#delete 2020/21, 2021/22
#==============

df_post_twoseasons <- df_post|> 
        filter(Season!="2020/21"&Season!="2021/22")


p2_diff_mean_desease_facet_twoseasons<-     ggplot(data=df_post_twoseasons, 
                                        mapping=aes(x=Season, 
                                                    y=week_diff_mean, 
                                                    col=Country
                                        ))+   
  geom_point(size=6.5)+
  facet_wrap(vars(Disease))+
  geom_hline(yintercept=0, linetype="dashed", 
             color = "black")+
  scale_y_continuous(breaks= c(-20,-15,-10,-5,0,5,10,15,20,25))+
  scale_color_manual(values=c("#E69F00", "#56B4E9","deeppink","darkgreen","#999999","purple1"))+
  
  labs(title="Variation in peak weeks compared to pre-pandemic average peak", 
       x="Season",
       y="Difference in peak (week)")+
  theme_minimal()+
  theme(strip.text = element_text(
    size = 23, color = "black"),
    panel.border = element_blank(),  
    panel.grid.major = element_blank(), 
  #  legend.position=c(0.92,0.91), 
    panel.grid.minor = element_blank(),  
    text=element_text(size=23, 
                      family="Arial"), 
    axis.line = element_line(colour = "black")
  ) 
print(p2_diff_mean_desease_facet_twoseasons)

ggsave("Output/graphs/01_01_global overview/03_scatterplot_diff_mean_facet_twoseasons.png",
       plot = p2_diff_mean_desease_facet_twoseasons,
       width = 10, height = 8, dpi = 300)
