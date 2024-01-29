library(lubridate)
library(tidyverse)

#RSV

#open-wrangling data
total_age <- read.csv('https://raw.githubusercontent.com/hallucinojenlee/data_challenge_2024_sanofi/main/Dataset/brazil_age_rsv.csv')
total_age$year <- isoyear(total_age$start_week)
total_age$week_num <- isoweek(total_age$start_week)
total_age <- total_age[total_age$start_week >= "2019-02-24" & total_age$start_week < "2024-01-01" ,]
total_age$start_week <- as.Date(total_age$start_week)
total_age$age_group <-paste(total_age$age_group, 'yr')
total_age$age_group <- factor(total_age$age_group, levels =c("<1 yr", "1-4 yr", "5-17 yr", "18-49 yr", "50-64 yr", '65+ yr'))

total_age$kat_year <- NA
total_age[total_age$week_num %in% c(9:53) & total_age$year %in% c(2019),"kat_year"] <- '2019/2020'
total_age[total_age$week_num  %in% c(1:8) & total_age$year %in% c(2020),"kat_year"] <- '2019/2020'
total_age[total_age$week_num  %in% c(9:53) & total_age$year %in% c(2020),"kat_year"] <- '2020/2021'
total_age[total_age$week_num  %in% c(1:8) & total_age$year %in% c(2021),"kat_year"] <- '2020/2021'
total_age[total_age$week_num  %in% c(9:53) & total_age$year %in% c(2021),"kat_year"] <- '2021/2022'
total_age[total_age$week_num  %in% c(1:8) & total_age$year %in% c(2022),"kat_year"] <- '2021/2022'
total_age[total_age$week_num  %in% c(9:53) & total_age$year %in% c(2022),"kat_year"] <- '2022/2023'
total_age[total_age$week_num  %in% c(1:8) & total_age$year %in% c(2023),"kat_year"] <- '2022/2023'
total_age[total_age$week_num  %in% c(9:53) & total_age$year %in% c(2023),"kat_year"] <- '2023/2024'
total_age[total_age$week_num  %in% c(1:8) & total_age$year %in% c(2024),"kat_year"] <- '2023/2024'
total_age$week_num <- factor(total_age$week_num, levels=c(9:53,1:8))
total_age <- na.omit(total_age)
total_age <- subset(total_age, total_age$kat_year!='2020/2021' & total_age$kat_year!='2021/2022')

#plot
everyother <- function(x) x[seq_along(x) %% 12 == 0]
ggplot(total_age, aes(x = as.factor(week_num), y = n, group = interaction(kat_year, age_group), color = kat_year)) + geom_line(size = 0.9) + 
  scale_x_discrete(name = 'Calendar week number', breaks = everyother) + scale_y_continuous('Hospitalisation Cases') +
  facet_wrap(~age_group, nrow = 3) + scale_color_manual(values = c ("#b1b2b3", 
                                                                    "#18cdf1", "#088199")) +theme(panel.border = element_blank(), 
                                                                                                  panel.grid.minor = element_blank(),
                                                                                                  axis.line = element_line(colour = "black"),
                                                                                                  panel.background = element_rect(fill = "transparent"),
                                                                                                  legend.position='right',
                                                                                                  strip.text = element_text(size = 12),
                                                                                                  text=element_text(size=14,family="Arial"),
                                                                                                  strip.background = element_blank(),
                                                                                                  plot.title=element_text(hjust=0.5)) + guides(color = guide_legend(title = "Year")) 


# influenza
# open-wrangling data

total_age <- read.csv('https://raw.githubusercontent.com/hallucinojenlee/data_challenge_2024_sanofi/main/Dataset/brazil_age_flu.csv')
total_age$year <- isoyear(total_age$start_week)
total_age$week_num <- isoweek(total_age$start_week)
total_age <- total_age[total_age$start_week >= "2019-02-24" & total_age$start_week < "2024-01-01" ,]
total_age$start_week <- as.Date(total_age$start_week)
total_age$age_group <-paste(total_age$age_group, 'yr')
total_age$age_group <- factor(total_age$age_group, levels =c("<1 yr", "1-4 yr", "5-17 yr", "18-49 yr", "50-64 yr", '65+ yr'))

total_age$kat_year <- NA
total_age[total_age$week_num %in% c(9:53) & total_age$year %in% c(2019),"kat_year"] <- '2019/2020'
total_age[total_age$week_num  %in% c(1:8) & total_age$year %in% c(2020),"kat_year"] <- '2019/2020'
total_age[total_age$week_num  %in% c(9:53) & total_age$year %in% c(2020),"kat_year"] <- '2020/2021'
total_age[total_age$week_num  %in% c(1:8) & total_age$year %in% c(2021),"kat_year"] <- '2020/2021'
total_age[total_age$week_num  %in% c(9:53) & total_age$year %in% c(2021),"kat_year"] <- '2021/2022'
total_age[total_age$week_num  %in% c(1:8) & total_age$year %in% c(2022),"kat_year"] <- '2021/2022'
total_age[total_age$week_num  %in% c(9:53) & total_age$year %in% c(2022),"kat_year"] <- '2022/2023'
total_age[total_age$week_num  %in% c(1:8) & total_age$year %in% c(2023),"kat_year"] <- '2022/2023'
total_age[total_age$week_num  %in% c(9:53) & total_age$year %in% c(2023),"kat_year"] <- '2023/2024'
total_age[total_age$week_num  %in% c(1:8) & total_age$year %in% c(2024),"kat_year"] <- '2023/2024'
total_age$week_num <- factor(total_age$week_num, levels=c(9:53,1:8))
total_age <- na.omit(total_age)
total_age <- subset(total_age, total_age$kat_year!='2020/2021' & total_age$kat_year!='2021/2022')

# plot data

everyother <- function(x) x[seq_along(x) %% 12 == 0]
ggplot(total_age, aes(x = as.factor(week_num), y = n, group = interaction(kat_year, age_group), color = kat_year)) + geom_line(size = 0.9) + 
  scale_x_discrete(name = 'Calendar week number', breaks = everyother) + scale_y_continuous('Hospitalisation Cases') +
  facet_wrap(~age_group, nrow = 3) + scale_color_manual(values = c ("#b1b2b3", 
                                                                    "#18cdf1", "#088199")) +theme(panel.border = element_blank(), 
                                                                                                  panel.grid.minor = element_blank(),
                                                                                                  axis.line = element_line(colour = "black"),
                                                                                                  panel.background = element_rect(fill = "transparent"),
                                                                                                  legend.position='right',
                                                                                                  strip.text = element_text(size = 12),
                                                                                                  text=element_text(size=14,family="Arial"),
                                                                                                  strip.background = element_blank(),
                                                                                                  plot.title=element_text(hjust=0.5)) + guides(color = guide_legend(title = "Year")) 


