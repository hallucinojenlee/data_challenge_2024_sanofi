## Load libraries
library(tidyverse)

## read in US data - flu, rsv, covid (combined)
resp_net <- read.csv("~/Desktop/Caitlynne (With A Capital C)/resp_net.csv")

############################################################################################################################
###################################  DATA PREP ####################################

### CREATE DATA SET FOR GRAPHS

## filter to overall for all
all3 <- resp_net %>%
  filter(Surveillance.Network != 'Combined') %>%
  filter(Sex == 'Overall') %>%
  filter(Race.Ethnicity == 'Overall') %>%
  filter(Site == 'Overall') %>%
  #filter(Age.group %in% c('0-<1 year', '1-4 years', '5-17 years','18-49 years', '50-64 years', '65+ years')) %>%
  #filter(Season != '2023-24') %>%
  filter(Age.group == 'Overall') %>%
  mutate(disease = case_when(
    Surveillance.Network == 'COVID-NET' ~ 'COVID-19',
    Surveillance.Network == 'FluSurv-NET' ~ 'Influenza',
    Surveillance.Network == 'RSV-NET' ~ 'RSV')) %>%
  mutate(year.week = paste(MMWR.Year, MMWR.Week, sep='-')) %>%
  select(Week.Ending.Date, disease, Weekly.Rate) %>%
  pivot_wider(names_from = disease, values_from = Weekly.Rate) %>%
  arrange(Week.Ending.Date) %>%
  mutate(week2 = row_number()) %>%
  pivot_longer(cols=c('COVID-19','Influenza', 'RSV'), names_to = 'disease', values_to ='Weekly.Rate')

## check - group_by disease
table(all3$Surveillance.Network)
table(all3$disease)
table(all3$MMWR.Year)
table(all3$year.week)
table(all3$Season)
table(all3$Sex)
table(all3$Race.Ethnicity)
table(all3$Age.group)

test<- all3 %>%
  group_by(disease, MMWR.Year, MMWR.Week) %>%
  select(disease, MMWR.Year, MMWR.Week, year.week, Weekly.Rate) %>%
  pivot_wider(names_from = disease, values_from = Weekly.Rate)

##################################################################################################################################
##################################### TIME SERIES AND AGE VISUALIZATIONS ############################################

##### TIME SERIES FLU, COVID AND RSV (2018-2023)

all3 %>%
  #group_by(disease, MMWR.Year, MMWR.Week) %>%
  #select(disease, MMWR.Year, MMWR.Week, year.week, Weekly.Rate, week2) %>%
  ggplot(aes(x=week2, y=Weekly.Rate, group=as.factor(disease))) +
  geom_line(aes(color=as.factor(disease))) +
  scale_x_continuous(
    breaks = c(1,14,45,56,97,149,165,202),
    label = c(2018,2019,2020,'Lockdowns \nbegin',2021,2022,'Federal mask \nmandate ends',2023)
  ) +
  geom_vline(xintercept = 56) + 
  geom_vline(xintercept = 165) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0, hjust=0)) +
  labs(title = "Seasonal Trends in all COVID-19, Influenza & RSV (2018-2023)", y = "Hospitalizations per 100,000", x = 'Year-Week', color = "Disease")

# add policy markers - march 19th - California has stay at home order, April 2022 - federal mask mandates are lifted

## graph 2: flu + covid
# all3 %>%
#   filter(disease !='RSV') %>%
#   group_by(disease, MMWR.Year, MMWR.Week) %>%
#   select(disease, MMWR.Year, MMWR.Week, year.week, Weekly.Rate) %>%
#   ggplot(aes(x=year.week, y=Weekly.Rate, group=as.factor(disease))) +
#   geom_line(aes(color=as.factor(disease))) +
#   labs(title = "Seasonal Trends in Flu and COVID", y = "Hospitalizations per 100,000", x = 'Year-Week', color = "Disease")


##### TIME SERIES FOR FLU AND RSV

all3 %>%
  filter(disease != 'COVID-19') %>%
  ggplot(aes(x=week2, y=Weekly.Rate, group=as.factor(disease))) +
  geom_line(aes(color=as.factor(disease))) +
  scale_x_continuous(
    breaks = c(1,14,45,56,97,149,165,202),
    label = c(2018,2019,2020,'Lockdowns \nbegin',2021,2022,'Federal mask \nmandate ends',2023)
  ) +
  geom_vline(xintercept = 56) + 
  geom_vline(xintercept = 165) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0, hjust=0)) +
  labs(title = "Seasonal Trends in Flu and RSV", y = "Hospitalizations per 100,000", x = 'Year-Week', color = "Disease")

scale_x_continuous(
  breaks = c(1,14,45,77,129,181),
  label = c(2018,2019,2020,2021,2022,2023)
) 

##### FLU BY SEASON

flu <- 
  resp_net %>%
  filter(Surveillance.Network == 'FluSurv-NET') %>%
  filter(Sex == 'Overall') %>%
  filter(Race.Ethnicity == 'Overall') %>%
  filter(Site == 'Overall') %>%
  filter(Age.group == 'Overall') %>%
  mutate(year.week = paste(MMWR.Year, MMWR.Week, sep='-')) %>%
  arrange(Week.Ending.Date) %>%
  group_by(Season) %>%
  mutate(week2 = row_number()) %>%
  #use x=MMWR.week for chronological year
  ggplot(aes(x=week2, y=Weekly.Rate, group=as.factor(Season))) +
  geom_line(aes(color=as.factor(Season))) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0, hjust=0)) +
  labs(title = "Shift in Influenza Peaks - US", y = "Hospitalizations per 100,000", x = 'Week (from start of season)', color = "Flu Season")

##### RSV BY SEASON

rsv <- 
  resp_net %>%
  filter(Surveillance.Network == 'RSV-NET') %>%
  filter(Sex == 'Overall') %>%
  filter(Race.Ethnicity == 'Overall') %>%
  filter(Site == 'Overall') %>%
  filter(Age.group == 'Overall') %>%
  mutate(year.week = paste(MMWR.Year, MMWR.Week, sep='-')) %>%
  arrange(Week.Ending.Date) %>%
  group_by(Season) %>%
  mutate(week2 = row_number()) %>%
  #use x=MMWR.week for chronological year
  ggplot(aes(x=week2, y=Weekly.Rate, group=as.factor(Season))) +
  geom_line(aes(color=as.factor(Season))) +
 # theme(axis.text.x = element_text(angle = 90, vjust = 0, hjust=0)) +
  labs(title = "Shift in RSV Peaks - US", y = "Hospitalizations per 100,000", x = 'Week (from start of season)', color = "RSV Season")


###### TIME SERIES FOR FLU BY AGE GROUP (2018-2023)

age.flu <- 
  resp_net %>%
  filter(Surveillance.Network == 'FluSurv-NET') %>%
  filter(Sex == 'Overall') %>%
  filter(Race.Ethnicity == 'Overall') %>%
  filter(Site == 'Overall') %>%
  filter(Age.group %in% c('0-<1 year', '1-4 years', '5-17 years','18-49 years', '50-64 years', '65+ years')) %>%
  #filter(Season != '2023-24') %>%
  mutate(year.week = paste(MMWR.Year, MMWR.Week, sep='-')) %>%
  #select(year.week, Week.Ending.Date, Age.group, Weekly.Rate) %>%
  #pivot_wider(names_from = Age.group, values_from = Weekly.Rate) %>%
  arrange(Week.Ending.Date) %>%
  group_by(Age.group) %>%
  mutate(week2 = row_number()) %>%
  ggplot(aes(x=week2, y=Weekly.Rate, group=as.factor(Age.group))) +
  geom_line(aes(color=as.factor(Age.group))) +
  scale_x_continuous(
    breaks = c(1,14,45,54,63,76,90,112),
    label = c(2018,2019,2020,'Lockdowns \nbegin',2021,2022,'Federal mask \nmandate ends',2023)
  ) +
  geom_vline(xintercept = 54) + 
  geom_vline(xintercept = 90) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0, hjust=0)) +
  labs(title = "Comparison of influenza by Age Group - US (2018-2023)", y = "Hospitalizations per 100,000", x = 'Year', color = "Age Group")


## TIMES SERIES FOR RSV BY AGE GROUP (2018-2023)

age.rsv <- 
  resp_net %>%
  filter(Surveillance.Network == 'RSV-NET') %>%
  filter(Sex == 'Overall') %>%
  filter(Race.Ethnicity == 'Overall') %>%
  filter(Site == 'Overall') %>%
  filter(Age.group %in% c('0-<1 year', '1-4 years', '5-17 years','18-49 years', '50-64 years', '65+ years')) %>%
  #filter(Season != '2023-24') %>%
  mutate(year.week = paste(MMWR.Year, MMWR.Week, sep='-')) %>%
  #select(year.week, Week.Ending.Date, Age.group, Weekly.Rate) %>%
  #pivot_wider(names_from = Age.group, values_from = Weekly.Rate) %>%
  arrange(Week.Ending.Date) %>%
  group_by(Age.group) %>%
  mutate(week2 = row_number()) %>%
  ggplot(aes(x=week2, y=Weekly.Rate, group=as.factor(Age.group))) +
  geom_line(aes(color=as.factor(Age.group))) +
  scale_x_continuous(
    breaks = c(1,14,45,56,77,129,144,181),
    label = c(2018,2019,2020,'Lockdowns \nbegin',2021,2022,'Federal mask \nmandate ends',2023)
  ) +
  geom_vline(xintercept = 56) + 
  geom_vline(xintercept = 144) +
  theme(axis.text.x = element_text(angle = 45, vjust = .5, hjust=0)) +
  labs(title = "Comparison of RSV by Age Group - US (2018-2023)", y = "Hospitalizations per 100,000", x = 'Year', color = "Age Group")

##best to look at proportional difference in peaks from ages - which age groups changed more

##### FLU BY AGE GROUP AND SEASON

flu.age2 <- 
  resp_net %>%
  filter(Surveillance.Network == 'FluSurv-NET') %>%
  filter(Sex == 'Overall') %>%
  filter(Race.Ethnicity == 'Overall') %>%
  filter(Site == 'Overall') %>%
  filter(Age.group %in% c('0-<1 year', '1-4 years', '5-17 years','18-49 years', '50-64 years', '65+ years')) %>%
  mutate(year.week = paste(MMWR.Year, MMWR.Week, sep='-')) %>%
  arrange(Week.Ending.Date) %>%
  #group_by(Age.group) %>%
  group_by(Season, Age.group) %>%
  mutate(week2 = row_number()) %>%
  #use x=MMWR.week for chronological year
  ggplot(aes(x=week2, y=Weekly.Rate)) +
  geom_line(aes(color=Season)) +
  facet_wrap(~ Age.group) +
  #theme(axis.text.x = element_text(angle = 90, vjust = 0, hjust=0)) +
  labs(title = "Influenza by Age Group - US", y = "Hospitalizations per 100,000", x = 'Week (from start of season)', color = "Flu Season")

##### RSV BY AGE GROUP AND SEASON

#rsv.age2 <- 
  resp_net %>%
  filter(Surveillance.Network == 'RSV-NET') %>%
  filter(Sex == 'Overall') %>%
  filter(Race.Ethnicity == 'Overall') %>%
  filter(Site == 'Overall') %>%
  filter(Age.group %in% c('0-<1 year', '1-4 years', '5-17 years','18-49 years', '50-64 years', '65+ years')) %>%
  mutate(year.week = paste(MMWR.Year, MMWR.Week, sep='-')) %>%
  arrange(Week.Ending.Date) %>%
  #group_by(Age.group) %>%
  group_by(Season, Age.group) %>%
  mutate(week2 = row_number()) %>%
  #use x=MMWR.week for chronological year
  ggplot(aes(x=week2, y=Weekly.Rate)) +
  geom_line(aes(color=Season)) +
  facet_wrap(~ Age.group) +
  #theme(axis.text.x = element_text(angle = 90, vjust = 0, hjust=0)) +
  labs(title = "RSV by Age Group and Season - US", y = "Hospitalizations per 100,000", x = 'Week (from start of season)', color = "RSV Season")

##### FLU BY AGE GROUP
  
resp_net %>%
  filter(Surveillance.Network == 'FluSurv-NET') %>%
  filter(Sex == 'Overall') %>%
  filter(Race.Ethnicity == 'Overall') %>%
  filter(Site == 'Overall') %>%
  filter(Age.group %in% c('0-<1 year', '1-4 years', '5-17 years','18-49 years', '50-64 years', '65+ years')) %>%
  mutate(year.week = paste(MMWR.Year, MMWR.Week, sep='-')) %>%
  arrange(Week.Ending.Date) %>%
  group_by(Age.group) %>%
  #group_by(Season, Age.group) %>%
  mutate(week2 = row_number()) %>%
  #use x=MMWR.week for chronological year
  ggplot(aes(x=week2, y=Weekly.Rate)) +
  geom_line(aes(color=Age.group)) +
  facet_wrap(~ Age.group) +
  #theme(axis.text.x = element_text(angle = 90, vjust = 0, hjust=0)) +
  labs(title = "Influenza by Age Group - US", y = "Hospitalizations per 100,000", x = 'Week', color = "Flu Season")


  