#Merge with season year and season week for France data
franceflu<- fluraw %>%
  filter(Country=="France")%>%
  select(Year,Month,Week_num,hospitalisation_num) %>%
  mutate(Year_week=paste(Year,Week_num,"1",sep='_'),
         Date=as.Date(Year_week,'%Y_%W_%u'),
         Year=factor(Year),
         hospitalisation_num=as.numeric(hospitalisation_num))

franceflu_c<-left_join(x=franceflu,y=season_week,by="Year_week")

#Merge rsv data
francersv<- rsvraw %>%
  filter(Country=="France")%>%
  select(Year,Month,Week_num,hospitalisation_num) %>%
  mutate(Year_week=paste(Year,Week_num,"1",sep='_'),
         Date=as.Date(Year_week,'%Y_%W_%u'),
         Year=factor(Year),
         hospitalisation_num=as.numeric(hospitalisation_num))

francersv_c<-left_join(x=francersv,y=season_week,by="Year_week")
str(franceflu_c)

#Generate column for correct week number label 
xaxis_index<-ukflu_sub %>%
  select(Week_num) %>%
  slice(1:52) %>%
  mutate(WeekNum = as.factor(Week_num))

#Flu Season trends
ggplot(data=subset(franceflu_c,as.integer(Season) %in% c(2,3,4,7,8)),
                               aes(x=Season_week,y=hospitalisation_num,
                                   colour=Season))+
  geom_line(size=0.8) + 
  labs(title="Flu hospitalisation rates in France (2017-2023)",
       x="Week Number",y="Hospitalisation rate (per 100k)") + 
  scale_color_manual(values = c(, "#f77935", "#b1b2b3", 
                                "#18cdf1")) +
  scale_x_continuous(name="Week number",expand=c(0,0),breaks=seq(1,52),
                     labels=xaxis_index$Week_num) +
  scale_y_continuous(name="Hospitalisation rate (per 100k)",expand=c(0,0)) +
  theme(panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.background = element_rect(fill = "transparent"),
        legend.position=c(0.1,0.5),
        plot.title=element_text(hjust=0.5))

flu_sub #only showing pre post covid trends