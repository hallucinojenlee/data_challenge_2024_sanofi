
############# INSTALL PACKAGES ############

install.packages('readxl')
install.packages('tidyverse')

############## LOAD LIBRARIES ####################

library(readxl)
library(tidyverse)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

##### Time series graph: Comparing flu seasonality pre and post COVID pandemic in USA

## Read in data
us.flu <- read_excel("Consolidated_dataset_MASTER.xlsx")

us.flu %>%
  #filter to USA
  filter(Country == 'US') %>%
  #create season variable to use instead of year
  group_by(Week_num) %>%
  mutate(row = row_number()) %>%
  ungroup() %>%
  mutate(Season = case_when(
    row == 1 & Year %in% c(2017,2018) ~ '2017-18',
    row == 2 ~ '2018-19',
    row == 3 ~ '2019-20',
    row == 1 & Year == 2020 ~ '2019-20',
    row == 4 ~ '2020-21',
    row == 5 ~ '2021-22',
    row == 1 & Year %in% c(2022) ~ '2021-22',
    row == 6 ~ '2022-23',
    row == 7 ~ '2023-24')) %>%
  filter(!Season %in% c('2020-21', '2021-22', '2023-24')) %>% #exclude because of insufficient data
  #transform hospitalisation rate metric for y-axis
  mutate(hosp.rate = as.numeric(hospitalisation_rate)) %>%
  filter(!is.na(hosp.rate)) %>%
  #transform calendar week for x-axis to better show seasonality across years
  mutate(week = factor(Week_num, levels = c(40,41,42,43,44,45,46,47,48,49,50,51,52,53,1,2,3,4,5,6,7,8,9,10,
                                             11,12,13,14,15,16,17,18,19,20,21,22,23))) %>%
  #create time series plot: calendar week on x-axis, hospitalisation rate on y-axis, group by flu seasons
  ggplot(aes(x=week, y=hosp.rate, group=as.factor(Season))) +
  #have different color and legend for flu seasons
  geom_line(aes(color=as.factor(Season)), size=.8) +
  #manually set color scheme
  scale_color_manual(values = c("#f4b18d", "#f77935", "#b1b2b3", "#18cdf1", "#088199")) + 
  #add axis labels and plot title
  xlab("Week Number") + 
  ylab("Rate of Hospitalisations (per 100k)") + 
  ggtitle("Rate of Influenza Hospitalisations in US") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(color = "Season") +  
  #manually set plot design
  theme_minimal() +  
  #scale_x_discrete(expand=c(0,0)) + 
  scale_y_continuous(expand=c(0,0)) + 
  theme(panel.border = element_blank(),  
        panel.grid.major = element_blank(), 
        legend.position=c(0.05,0.85), 
        panel.grid.minor = element_blank(),  
        text=element_text(size=14, 
                          family="Arial"), 
        axis.line = element_line(colour = "black")) 


##### Time series graph: Comparing RSV seasonality pre and post COVID pandemic in USA

## Read in data
us.rsv <- read_excel("Consolidated_dataset_MASTER.xlsx", sheet = "RSV")

us.rsv %>%
  #filter to US
  filter(Country == 'US') %>%
  #create season variable to use instead of year
  group_by(Week_num) %>%
  mutate(row = row_number()) %>%
  ungroup() %>%
  mutate(Season = case_when(
    row == 1 & Year %in% c(2018,2019) ~ '2018-19',
    row == 2  & Year %in% c(2019,2020) ~ '2019-20',
    row == 3 & Year %in% c(2020,2021) ~ '2020-21',
    row == 1 & Year %in% c(2020,2021) ~ '2020-21',
    row == 4 ~ '2021-22',
    row == 2 & Year %in% c(2022) ~ '2021-22',
    row == 5 ~ '2022-23',
    row == 3 & Year == 2023 ~ '2022-23',
    row == 6 ~ '2023-24')) %>%
  filter(!Season %in% c('2020-21', '2021-22', '2023-24')) %>% #exclude because of insufficient data
  #transform hospitalisation rate metric for y-axis
  mutate(hosp.rate = as.numeric(hospitalisation_rate)) %>%
  filter(!is.na(hosp.rate)) %>%
  #transform calendar week for x-axis to better show seasonality across years
  mutate(week = factor(Week_num, levels = c(40,41,42,43,44,45,46,47,48,49,50,51,52,53,1,2,3,4,5,6,7,8,9,10,
                                            11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39))) %>%
  #create time series plot: calendar week on x-axis, hospitalisation rate on y-axis, group by flu seasons
  ggplot(aes(x=week, y=hosp.rate, group=as.factor(Season))) +
  #have different color and legend for flu seasons
  geom_line(aes(color=as.factor(Season)), size=.8) +
  #manually set color scheme
  scale_color_manual(values = c("#f4b18d", "#f77935", "#b1b2b3", "#18cdf1", "#088199")) + 
  #add axis labels and plot title
  xlab("Week Number") + 
  ylab("Rate of Hospitalisations (per 100k)") + 
  ggtitle("Rate of Influenza Hospitalisations in US") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(color = "Season") +  
  #manually set plot design
  theme_minimal() +  
  #scale_x_discrete(expand=c(0,0)) + 
  scale_y_continuous(expand=c(0,0)) + 
  theme(panel.border = element_blank(),  
        panel.grid.major = element_blank(), 
        legend.position=c(0.75,0.85), 
        panel.grid.minor = element_blank(),  
        text=element_text(size=14, 
                          family="Arial"), 
        axis.line = element_line(colour = "black")) 


######## Time series graph: comparing peak season for Flu, RSV and COVID-19 across all seasons in USA

## Create RSV data to use in plot
rsv_ts <-
  #filter to relevant categories and rsv hospitalisations
  us.rsv %>%
  #filter to USA
  filter(Country == 'US') %>%
  #transform hospitalisations metric to numeric variable
  mutate(hosp.rate = as.numeric(hospitalisation_rate)) %>%
  #transform calendar week as date
  mutate(Year_week=paste(Year,Week_num,"1",sep='_'),
         Date=as.Date(Year_week,'%Y_%W_%u'))

## Create Flu data to use in plot
flu_ts <-
  us.flu %>%
  #filter to USA
  filter(Country == 'US') %>%
  #transform hospitalisations metric to numeric variable
  mutate(hosp.rate = as.numeric(hospitalisation_rate)) %>%
  #transform calendar week as date
  mutate(Year_week=paste(Year,Week_num,"1",sep='_'),
         Date=as.Date(Year_week,'%Y_%W_%u'))

#Create time series plot using the r datasets (rsv,flu and COVID) created above
combplot<-
  #create plot and set color and line size for all three viruses
  ggplot() +
  geom_line(data=flu_ts,aes(x=Date,y=hosp.rate),color="#088199",size=0.7) +
  geom_line(data=rsv_ts,aes(x=Date,y=hosp.rate),color="#f77935",size=0.7) +
  #labs(title="Influenza, RSV Hospitalisation trends in US") +
  scale_colour_brewer(palette="Dark2") +
  #set labels
  scale_y_continuous(name="Hospitalisation rate (per 100k)",expand=c(0,0),
                     limits=c(0,15)) +
  scale_x_date(date_labels="%yW%W",date_breaks="3 months") +
  #set plot design
  theme(panel.grid.major = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.background = element_rect(fill = "white"),
        axis.text.x = element_text(angle=90, vjust = 0.5, hjust=1),
        plot.title=element_text(hjust=0.5))
#view plot
plot(combplot)
#save plot to local working directory
ggsave("flursvplot in us.png",plot=combplot,width=11,height=5)


##### Time series graph: Comparing flu seasonality across seasons by age groups in USA

## Read in teh data
us_flu_age <- read_excel("Consolidated_dataset_MASTER.xlsx", sheet = "US_Flu_age")

test<-us_flu_age %>%
  filter(!YEAR %in% c('2020-21', '2023-24')) %>% #exclude because of insufficient data
  #transform hospitalisation rate metric for y-axis
  mutate(hosp.rate = as.numeric(`WEEKLY RATE`)) %>%
  filter(!is.na(hosp.rate)) %>%
  #transform age category
  mutate(age.group = ifelse(`AGE CATEGORY` == '>= 65 yr','65+ yr',`AGE CATEGORY`)) %>%
  mutate(age.group = gsub('0-< 1 yr','0-< 1 yr',age.group)) %>%
  mutate(age.group = gsub('yr','years', age.group)) %>%
  #create variable to specify weeks from when the season began
  group_by(YEAR, age.group) %>%
  mutate(week2 = row_number()) %>%
  #transform calendar week for x-axis to better show seasonality across years
  #create plot - y-axis = hospitalisations, x-axis = weeks since season began (seasons start at calendar week 40)
  ggplot(aes(x=week2, y=hosp.rate)) +
  #group by flu season
  geom_line(aes(color=YEAR), size=.8) +
  #put individual graphs by age group into one visualisation
  facet_wrap(~ age.group) +
  #set colors for flu season
  scale_color_manual(values = c("#f4b18d", "#f77935", "#b1b2b3", "#18cdf1", "#088199")) + 
  #set labels
  xlab("Week (since beginning of season)") + 
  ylab("Rate of Hospitalisations (per 100k)") + 
  ggtitle("") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(color = "Year") +  
  #set plot design
  theme_minimal() +  
  #scale_x_discrete(expand=c(0,0)) + 
  scale_y_continuous(expand=c(0,0)) + 
  theme(panel.border = element_blank(),  
        panel.grid.major = element_blank(), 
        #legend.position=c(0.05,0.85), 
        panel.grid.minor = element_blank(),  
        text=element_text(size=14, 
                          family="Arial"), 
        axis.line = element_line(colour = "black")) 


##### Time series graph: Comparing RSV seasonality across seasons by age groups in USA

## Read in data
us_rsv_age <- read_excel("Consolidated_dataset_MASTER.xlsx", sheet = "US_RSV_age")

us_rsv_age %>%
  filter(!Season %in% c('2020-21', '2023-24')) %>% #exclude because of insufficient data
  #create varaible that shows week since a season began (RSV season begins at week 40)
  arrange(`Week Ending Date`) %>%
  group_by(Season, `Age group`) %>%
  mutate(week2 = row_number()) %>%
  #create plot: y-axis = rate of hospitalisations, x-axis = weeks since beginning of season
  ggplot(aes(x=week2, y=`Weekly Rate`)) +
  #group by RSV season
  geom_line(aes(color=Season), size=.8) +
  #put individual graphs by age group into one visualisation
  facet_wrap(~ `Age group`) +
  #set colors
  scale_color_manual(values = c("#f4b18d", "#f77935", "#b1b2b3", "#18cdf1", "#088199")) + 
  #set labels
  xlab("Week (since beginning of season)") + 
  ylab("Rate of Hospitalisations (per 100k)") + 
  ggtitle("") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(color = "Year") +  
  #set plot design
  theme_minimal() +  
  #scale_x_discrete(expand=c(0,0)) + 
  scale_y_continuous(expand=c(0,0)) + 
  theme(panel.border = element_blank(),  
        panel.grid.major = element_blank(), 
        #legend.position=c(0.05,0.85), 
        panel.grid.minor = element_blank(),  
        text=element_text(size=14, 
                          family="Arial"), 
        axis.line = element_line(colour = "black")) 
