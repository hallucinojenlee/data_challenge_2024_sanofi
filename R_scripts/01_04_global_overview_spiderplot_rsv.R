#### This is a RSV synthesizing analysis using spiderplot ####
#Date: 14 - FEB - 2024
#Author: Kai
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++


#setwd("C:/Users/three/Neu Kasten_5440/022 master degree LSHTM/LSHTM/013 Data Challenge/My Sanofi_5440/data_challenge_2024_sanofi")
(path<-getwd())

library(ggplot2)
library(dplyr)
library(tidyr)

#+++++++++++++++++++++++++++++++
## load  cleaned df_peak ####
#+++++++++++++++++++++++++++++++
print(df_peak)




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
## prepare spider plot RSV ####
#++++++++++++++++++++++++++++++
# create season's level
df_peak$Season_level <-  match(df_peak$Season, 
                               unique(df_peak$Season))#careful, it is correct because the original one is ordered already

#above is all the same with flu's df_peak

##!!relevel into 6 levels, due to omitting 2020/21 data(season_level=4) #### 
df_peak$Season_level <- ifelse(df_peak$Season_level > 4,
                         df_peak$Season_level-1, df_peak$Season_level)




# transpose from long to wide
df_wide_rsv <- as.data.frame(
               pivot_wider(df_peak[df_peak$Disease=="RSV",], 
                         id_cols=Season, 
                         values_from=Season_level, #Year
                         names_from=Week_num,
                         names_prefix="",
                         values_fn =unique))#due to duplicate peak weeks
rownames(df_wide_rsv) <- df_wide_rsv$Season

df_wide_rsv <- df_wide_rsv |> select( -Season )

### insert missing peaks as NA ####
missing_peakweeks_rsv <- setdiff(1:52, #complete range of weeks in a year
                             colnames(df_wide_rsv))#what I had

temp_missing_peakweeks_rsv <- matrix(nrow= nrow(df_wide_rsv),
                                 ncol= length(missing_peakweeks_rsv))
colnames(temp_missing_peakweeks_rsv) <- missing_peakweeks_rsv

df_wide_rsv <- cbind(df_wide_rsv,temp_missing_peakweeks_rsv)

setdiff(1:52,colnames(df_wide_rsv))#check

#reorder columns (don't use order()==> wrong)
df_wide_rsv <- df_wide_rsv[,as.character(52:1)] 


# set min/max row
max_row <- setNames(rep(6, ncol(df_wide_rsv)), names(df_wide_rsv))
min_row <- setNames(rep(1, ncol(df_wide_rsv)), names(df_wide_rsv))

df_wide_rsv_plot <- rbind.data.frame(max_row, min_row, #not rbind
                                     df_wide_rsv)
rownames(df_wide_rsv_plot)[c(1,2)] <- c("max","min")

#+++++++++++++++++++
## plotting flu ####
#+++++++++++++++++++
library(fmsb)

##omit observations in 2020/21, the same as flu's approach
df_wide_rsv_plot <- df_wide_rsv_plot[-which(rownames(df_wide_rsv_plot)=="2020/21"),]

#omit observations in 2023/24, as they only have two data points
df_wide_rsv_plot <- df_wide_rsv_plot[-which(rownames(df_wide_rsv_plot)=="2023/24"),]


# colors_manual_flu <- c("purple", "#f77935","blue" ,"yellow",
#                        "green", "#18cdf1","grey","pink")

colors_manual_rsv <- c(rep("#f77935",3),rep("deepskyblue2",3))


legend_labels <- rownames(df_wide_rsv_plot)[-c(1,2)]#no min/max



     radarchart(df_wide_rsv_plot,
                axistype=1, # show center axis 
                caxislabels =legend_labels , 
                axislabcol="gray",
                calcex= 0.8,
                na.itp = FALSE,# true =weird lines for low data points
                maxmin = TRUE,
                pcol=colors_manual_rsv,
                cglty = 1,       # Grid line type,
                pty = 19,        # dot type (31 = filled)
                plwd = 0.5,        # dot line width
                seg = 5,         # Number of segments
                cglwd = 0.1,     # Grid line width
                cglcol = "gray", # Grid line color
                vlcex = 0.8,     # Label size,
                title = "RSV peak week"
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




    


