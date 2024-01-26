
##### Time series graph: Comparing flu seasonality pre and post COVID pandemic in USA

flu2017 %>%
  #filter to overall categories and relevant flu seasons
  filter(SEX.CATEGORY == 'Overall') %>%
  filter(RACE.CATEGORY == 'Overall') %>%
  filter(AGE.CATEGORY == 'Overall') %>%
  filter(!YEAR %in% c('2020-21', '2023-24')) %>% #exclude because of insufficient data
  #transform hospitalisation rate metric for y-axis
  mutate(hosp.rate = as.numeric(WEEKLY.RATE)) %>%
  #transform calendar week for x-axis to better show seasonality across years
  mutate(week = factor(MMWR.WEEK, levels = c(40,41,42,43,44,45,46,47,48,49,50,51,52,53,1,2,3,4,5,6,7,8,9,10,
                        11,12,13,14,15,16,17,18,19,20,21,22,23))) %>%
  #create time series plot: calendar week on x-axis, hospitalisation rate on y-axis, group by flu seasons
  ggplot(aes(x=week, y=hosp.rate, group=as.factor(YEAR))) +
  #have different color and legend for flu seasons
  geom_line(aes(color=as.factor(YEAR)), size=.8) +
  #manually set color scheme
  scale_color_manual(values = c("#f4b18d", "#f77935", "#b1b2b3", "#18cdf1", "#088199")) + 
  #add axis labels and plot title
  xlab("Week number") + 
  ylab("Rate of Hospitalisations (per 100k)") + 
  ggtitle("Rate of Influenza Hospitalisations in US") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(color = "Year") +  
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

resp_net %>%
  #filter to overall categories and relevant seasons
  filter(Surveillance.Network == 'RSV-NET') %>%
  filter(Sex == 'Overall') %>%
  filter(Race.Ethnicity == 'Overall') %>%
  filter(Site == 'Overall') %>%
  filter(Age.group == 'Overall') %>%
  filter(!Season %in% c('2020-21', '2023-24')) %>% #exclude because of insufficient data
  #transform calendar week for x-axis to better show seasonality across years
  mutate(week = factor(MMWR.Week, levels = c(40,41,42,43,44,45,46,47,48,49,50,51,52,1,2,3,4,5,6,7,8,9,10,
                                             11,12,13,14,15,16,17,18,19,20,21,22,23))) %>%
  #remove missing data
  filter(!is.na(week)) %>%
  #create time series plot: calendar week on x-axis, hospitalisation rate on y-axis, group by flu seasons
  ggplot(aes(x=week, y=Weekly.Rate, group=as.factor(Season))) +
  #have different color and legend for flu seasons
  geom_line(aes(color=as.factor(Season)), size=.8) +
  #manually set color scheme
  scale_color_manual(values = c("#f4b18d", "#f77935", "#b1b2b3", "#18cdf1", "#088199")) + 
  #add axis labels and plot title
  xlab("Week number") + 
  ylab("Rate of Hospitalisations (per 100k)") + 
  ggtitle("Rate of RSV hospitalisations in US") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(color = "Year") + 
  #manually set plot design
  theme_minimal() +  
  #scale_x_discrete(expand=c(0,0)) + 
  scale_y_continuous(expand=c(0,0)) + 
  theme(panel.border = element_blank(),  
        panel.grid.major = element_blank(), 
        legend.position=c(0.80,0.85), 
        panel.grid.minor = element_blank(),  
        text=element_text(size=14, 
                          family="Arial"), 
        axis.line = element_line(colour = "black")) 
#save plot to local directory
ggsave('flu in US.png', width = 7, height = 5)

######## Time series graph: comparing peak season for Flu, RSV and COVID-19 across all seasons in USA

## Create RSV data to use in plot
rsv_ts <-
  #filter to relevant categories and rsv hospitalisations
  resp_net %>%
  filter(Surveillance.Network == 'RSV-NET') %>%
  filter(Sex == 'Overall') %>%
  filter(Race.Ethnicity == 'Overall') %>%
  filter(Site == 'Overall') %>%
  filter(Age.group == 'Overall') %>%
  #transform calendar week as date
  mutate(Year_week=paste(MMWR.Year,MMWR.Week,"1",sep='_'),
         Date=as.Date(Year_week,'%Y_%W_%u'))

## Create Flu data to use in plot
flu_ts <-
  #filter to relevant categories
  flu2017 %>%
  filter(SEX.CATEGORY == 'Overall') %>%
  filter(RACE.CATEGORY == 'Overall') %>%
  filter(AGE.CATEGORY == 'Overall') %>%
  #transform hospitalisations metric to numeric variable
  mutate(hosp.rate = as.numeric(WEEKLY.RATE)) %>%
  #transform calendar week as date
  mutate(Year_week=paste(MMWR.YEAR,MMWR.WEEK,"1",sep='_'),
         Date=as.Date(Year_week,'%Y_%W_%u'))

## Create COVID-19 data to use in plot
covid_ts <-
  #filter to relevant categories and COVID-19 hospitalisations
  resp_net %>%
  filter(Surveillance.Network == 'COVID-NET') %>%
  filter(Sex == 'Overall') %>%
  filter(Race.Ethnicity == 'Overall') %>%
  filter(Site == 'Overall') %>%
  filter(Age.group == 'Overall') %>%
  #transform calendar week as date
  mutate(Year_week=paste(MMWR.Year,MMWR.Week,"1",sep='_'),
         Date=as.Date(Year_week,'%Y_%W_%u'))

#Create time series plot using the r datasets (rsv,flu and COVID) created above
combplot<-
  #create plot and set color and line size for all three viruses
  ggplot() +
  geom_line(data=flu_ts,aes(x=Date,y=hosp.rate),color="#088199",size=0.7) +
  geom_line(data=rsv_ts,aes(x=Date,y=Weekly.Rate),color="#f77935",size=0.7) +
  #geom_line(data=covid_ts,aes(x=Date,y=Weekly.Rate),color="grey",linetype='dashed') +
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
combplot
#save plot to local working directory
ggsave("flursvplot in us.png",plot=combplot,width=11,height=5)

##### Time series graph: Comparing flu seasonality across seasons by age groups in USA
flu2017 %>%
  #filter to relevant categories and seasons
  filter(SEX.CATEGORY == 'Overall') %>%
  filter(RACE.CATEGORY == 'Overall') %>%
  filter(AGE.CATEGORY %in% c('0-< 1 yr', '1-4 yr', '5-17 yr','18-49 yr', 
                             '50-64 yr', '>= 65 yr')) %>%
  filter(!YEAR %in% c('2020-21', '2023-24')) %>% #exclude because of insufficient data
  #transform y-axis hospitalisation metric to a numeric variable
  mutate(hosp.rate = as.numeric(WEEKLY.RATE)) %>%
  #create variable to specify weeks from when the season began
  group_by(YEAR, AGE.CATEGORY) %>%
  mutate(week2 = row_number()) %>%
  #mutate(week = factor(MMWR.WEEK, levels = c(40,41,42,43,44,45,46,47,48,49,50,51,52,53,1,2,3,4,5,6,7,8,9,10,
                                             #11,12,13,14,15,16,17,18,19,20,21,22,23))) %>%
  #create plot - y-axis = hospitalisations, x-axis = weeks since season began (seasons start at calendar week 40)
  ggplot(aes(x=week2, y=hosp.rate)) +
  #group by flu season
  geom_line(aes(color=YEAR), size=.8) +
  #put individual graphs by age group into one visualisation
  facet_wrap(~ AGE.CATEGORY) +
  #set colors for flu season
  scale_color_manual(values = c("#f4b18d", "#f77935", "#b1b2b3", "#18cdf1", "#088199")) + 
  #set labels
  xlab("Week number") + 
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

resp_net %>%
  #filter to relevant virus and categories-specify age categories of interest
  filter(Surveillance.Network == 'FluSurv-NET') %>%
  filter(Sex == 'Overall') %>%
  filter(Race.Ethnicity == 'Overall') %>%
  filter(Site == 'Overall') %>%
  filter(!Season %in% c('2020-21', '2023-24')) %>%
  filter(Age.group %in% c('0-<1 year', '1-4 years', '5-17 years','18-49 years', '50-64 years', '65+ years')) %>%
  #create varaible that shows week since a season began (RSV season begins at week 40)
  arrange(Week.Ending.Date) %>%
  group_by(Season, Age.group) %>%
  mutate(week2 = row_number()) %>%
  #use x=MMWR.week for chronological year
  #create plot: y-axis = rate of hospitalisations, x-axis = weeks since beginning of season
  ggplot(aes(x=week2, y=Weekly.Rate)) +
  #group by RSV season
  geom_line(aes(color=Season), size=.8) +
  #put individual graphs by age group into one visualisation
  facet_wrap(~ Age.group) +
  #set colors
  scale_color_manual(values = c("#f4b18d", "#f77935", "#b1b2b3", "#18cdf1", "#088199")) + 
  #set labels
  xlab("Week number") + 
  ylab("Rate of Hospitalisations (per 100k)") + 
  ggtitle("Rate of RSV Hospitalisations in US \nby Age Group") + 
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

