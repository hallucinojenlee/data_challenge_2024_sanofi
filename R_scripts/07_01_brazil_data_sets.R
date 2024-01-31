
library(data.table)
library(lubridate)
library(tidyverse)


data1 <- fread("https://d26692udehoye.cloudfront.net/SRAG/2019/INFLUD19.csv", 
               select = c("DT_NOTIFIC", 'NU_IDADE_N', 'TP_FLU_PCR', 'PCR_FLUASU', 'PCR_FLUBLI', 'PCR_VSR','CLASSI_FIN'))
data1$DT_NOTIFIC = as.Date(data1$DT_NOTIFIC, format = "%d/%m/%Y")
data1 <- data1[order(data1$DT_NOTIFIC),]
data1 <- replace(data1, is.na(data1), 0)
data1$rsv <- ifelse(data1$PCR_VSR == 1, 1, 0)
data1$week_type <- 'isoweek'
data1$start_week <-floor_date(data1$DT_NOTIFIC, 'week') + 1
data1$week_num <- isoweek(ymd(data1$start_week))
data1$year <- isoyear(data1$start_week)
ag_data1 <- data1 %>% group_by(start_week, year, week_num, week_type) %>% summarise(rsv = sum(rsv)) 

#age group data1
data1 <- subset(data1, PCR_VSR != 0) 
data1 <- data1 %>%
  mutate(age_group = cut(NU_IDADE_N, breaks = c(0, 1, 4, 17, 49, 64, 999), 
                         labels = c("<1", "1-4", "5-17", "18-49", "50-64", '65+'), 
                         include.lowest = TRUE))

age_group1 <- data1 %>%
  select(start_week, week_num, year, age_group) %>%
  group_by(start_week, week_num, year, age_group) %>%
  tally()

data2 <- fread("https://s3.sa-east-1.amazonaws.com/ckan.saude.gov.br/SRAG/2020/INFLUD20-01-05-2023.csv", 
               select = c("DT_NOTIFIC", 'NU_IDADE_N', 'TP_FLU_PCR', 'PCR_FLUASU', 'PCR_FLUBLI', 'PCR_VSR','CLASSI_FIN')) 
data2$DT_NOTIFIC = as.Date(data2$DT_NOTIFIC, format = "%d/%m/%Y")
data2 <- data2[order(data2$DT_NOTIFIC),]
data2 <- replace(data2, is.na(data2), 0)
data2$`covid-19` <- ifelse(data2$CLASSI_FIN == 5, 1, 0)
data2$rsv <- ifelse(data2$PCR_VSR == 1, 1, 0)
data2$week_type <- 'isoweek'
data2$start_week <-floor_date(data2$DT_NOTIFIC, 'week') + 1
data2$week_num <- isoweek(ymd(data2$start_week))
data2$year <- isoyear(data2$start_week)
ag_data2 <- data2 %>% group_by(start_week, year, week_num, week_type) %>% summarise(rsv = sum(rsv)) 

#age group data1
data2 <- subset(data2, PCR_VSR != 0) 
data2 <- data2 %>%
  mutate(age_group = cut(NU_IDADE_N, breaks = c(0, 1, 4, 17, 49, 64, 999), 
                         labels = c("<1", "1-4", "5-17", "18-49", "50-64", '65+'), 
                         include.lowest = TRUE))

age_group2 <- data2 %>%
  select(start_week, week_num, year, age_group) %>%
  group_by(start_week, week_num, year, age_group) %>%
  tally()

data3 <- fread("https://s3.sa-east-1.amazonaws.com/ckan.saude.gov.br/SRAG/2021/INFLUD21-01-05-2023.csv", 
               select = c("DT_NOTIFIC", 'NU_IDADE_N', 'PCR_VSR')) 
data3$DT_NOTIFIC = as.Date(data3$DT_NOTIFIC, format = "%d/%m/%Y")
data3 <- data3[order(data3$DT_NOTIFIC),]
data3 <- replace(data3, is.na(data3), 0)
data3$rsv <- ifelse(data3$PCR_VSR == 1, 1, 0)
data3$week_type <- 'isoweek'
data3$start_week <-floor_date(data3$DT_NOTIFIC, 'week') + 1
data3$week_num <- isoweek(ymd(data3$start_week))
data3$year <- isoyear(data3$start_week)
ag_data3 <- data3 %>% group_by(start_week, year, week_num, week_type) %>% summarise(rsv = sum(rsv))

#age group data1
data3 <- subset(data3, PCR_VSR != 0) 
data3 <- data3 %>%
  mutate(age_group = cut(NU_IDADE_N, breaks = c(0, 1, 4, 17, 49, 64, 999), 
                         labels = c("<1", "1-4", "5-17", "18-49", "50-64", '65+'), 
                         include.lowest = TRUE))

age_group3 <- data3 %>% count(start_week,age_group)

data4 <- fread("https://s3.sa-east-1.amazonaws.com/ckan.saude.gov.br/SRAG/2022/INFLUD22-03-04-2023.csv", 
               select = c("DT_NOTIFIC", 'NU_IDADE_N', 'PCR_VSR')) 
data4$DT_NOTIFIC = as.Date(data4$DT_NOTIFIC, format = "%d/%m/%Y")
data4 <- data4[order(data4$DT_NOTIFIC),]
data4 <- replace(data4, is.na(data4), 0)
data4$rsv <- ifelse(data4$PCR_VSR == 1, 1, 0)
data4$week_type <- 'isoweek'
data4$start_week <-floor_date(data4$DT_NOTIFIC, 'week') + 1
data4$week_num <- isoweek(ymd(data4$start_week))
data4$year <- isoyear(data4$start_week)
ag_data4 <- data4 %>% group_by(start_week, year, week_num, week_type) %>% summarise(rsv = sum(rsv)) 

#age group data1
data4 <- subset(data4, PCR_VSR != 0) 
data4 <- data4 %>%
  mutate(age_group = cut(NU_IDADE_N, breaks = c(0, 1, 4, 17, 49, 64, 999), 
                         labels = c("<1", "1-4", "5-17", "18-49", "50-64", '65+'), 
                         include.lowest = TRUE))

age_group4 <- data4 %>% count(start_week,age_group)
table(data4$age_group)

data5 <- fread("https://s3.sa-east-1.amazonaws.com/ckan.saude.gov.br/SRAG/2023/INFLUD23-08-01-2024.csv", 
               select = c("DT_NOTIFIC", 'NU_IDADE_N', 'PCR_VSR')) 
data5$DT_NOTIFIC = as.Date(data5$DT_NOTIFIC, format = "%d/%m/%Y")
data5 <- data5[order(data5$DT_NOTIFIC),]
data5 <- replace(data5, is.na(data5), 0)
data5$rsv <- ifelse(data5$PCR_VSR == 1, 1, 0)
data5$week_type <- 'isoweek'
data5$start_week <-floor_date(data5$DT_NOTIFIC, 'week') + 1
data5$week_num <- isoweek(ymd(data5$start_week))
data5$year <- isoyear(data5$start_week)
data5$covid <- ifelse(data5$CLASSI_FIN == 5, 1, 0)
ag_data5 <- data5 %>% group_by(start_week, year, week_num, week_type) %>% summarise(rsv = sum(rsv)) 

#age group data1
data5 <- subset(data5, PCR_VSR != 0) 
data5 <- data5 %>%
  mutate(age_group = cut(NU_IDADE_N, breaks = c(0, 1, 4, 17, 49, 64, 999), 
                         labels = c("<1", "1-4", "5-17", "18-49", "50-64", '65+'), 
                         include.lowest = TRUE))

age_group5 <- data5 %>% count(start_week,age_group)


write.csv(ag_data1,'brazil1.csv')
write.csv(ag_data2,'brazil2.csv')
write.csv(ag_data3,'brazil3.csv')
write.csv(ag_data4,'brazil4.csv')
write.csv(ag_data5,'brazil5.csv')

ag_data <- bind_rows(ag_data1, ag_data2, ag_data3, ag_data4, ag_data5)
total <- ag_data %>% group_by(start_week, year, week_num, week_type) %>% summarise(rsv = sum(rsv)) 

age_data <- bind_rows(age_group1, age_group2, age_group3, age_group4, age_group5)
total_age <- age_data %>% group_by(start_week, week_num, year, age_group) %>% summarise(n = sum(n)) 
plot(total$rsv)
tail(total)
write.csv(total,'brazil_rsv.csv')
write.csv(total_age,'brazil_age_rsv.csv')


total$hosp_pop <- round(ifelse(total$year == 2019, (total$rsv/(211.8*10^6))*100000, 
                               ifelse(total$year == 2020, (total$rsv/(213.2*10^6))*100000,
                                      ifelse(total$year == 2021, (total$rsv/(214.3*10^6))*100000,
                                             ifelse(total$year == 2022, (total$rsv/(215.3*10^6))*100000,
                                                    ifelse(total$year == 2023, (total$rsv/(216.4*10^6))*100000, 
                                                           ifelse(total$year == 2023, (total$rsv/(217.6*10^6))*100000, NA)))))),2)


# influenza

data1 <- fread("https://d26692udehoye.cloudfront.net/SRAG/2019/INFLUD19.csv", 
               select = c("DT_NOTIFIC", 'NU_IDADE_N', 'TP_FLU_PCR','CLASSI_FIN'))
data1$DT_NOTIFIC = as.Date(data1$DT_NOTIFIC, format = "%d/%m/%Y")
data1 <- data1[order(data1$DT_NOTIFIC),]
data1 <- replace(data1, is.na(data1), 0)
data1$flu <- ifelse(data1$CLASSI_FIN == 1, 1, 0)
data1$flu_a <- ifelse(data1$TP_FLU_PCR == 1, 1, 0)
data1$flu_b <- ifelse(data1$TP_FLU_PCR == 2, 1, 0)
data1$week_type <- 'isoweek'
data1$start_week <-floor_date(data1$DT_NOTIFIC, 'week') + 1
data1$week_num <- isoweek(ymd(data1$start_week))
data1$year <- isoyear(data1$start_week)
ag_data1 <- data1 %>% group_by(start_week, year, week_num, week_type) %>% summarise(hospitalization = sum(flu), flu_a = sum(flu_a), flu_b = sum(flu_b) ) 

#age group data1
data1 <- subset(data1, flu != 0) 
data1 <- data1 %>%
  mutate(age_group = cut(NU_IDADE_N, breaks = c(0, 1, 4, 17, 49, 64, 999), 
                         labels = c("<1", "1-4", "5-17", "18-49", "50-64", '65+'), 
                         include.lowest = TRUE))

age_group1 <- data1 %>% count(start_week,age_group)

data2 <- fread("https://s3.sa-east-1.amazonaws.com/ckan.saude.gov.br/SRAG/2020/INFLUD20-01-05-2023.csv", 
               select = c("DT_NOTIFIC", 'NU_IDADE_N',  'TP_FLU_PCR','CLASSI_FIN')) 
data2$DT_NOTIFIC = as.Date(data2$DT_NOTIFIC, format = "%d/%m/%Y")
data2 <- data2[order(data2$DT_NOTIFIC),]
data2 <- replace(data2, is.na(data2), 0)
data2$flu <- ifelse(data2$CLASSI_FIN == 1, 1, 0)
data2$flu_a <- ifelse(data2$TP_FLU_PCR == 1, 1, 0)
data2$flu_b <- ifelse(data2$TP_FLU_PCR == 2, 1, 0)
data2$week_type <- 'isoweek'
data2$start_week <-floor_date(data2$DT_NOTIFIC, 'week') + 1
data2$week_num <- isoweek(ymd(data2$start_week))
data2$year <- isoyear(data2$start_week)
ag_data2 <- data2 %>% group_by(start_week, year, week_num, week_type) %>% summarise(hospitalization = sum(flu), flu_a = sum(flu_a), flu_b = sum(flu_b) ) 

#age group data2
data2 <- subset(data2, flu != 0) 
data2 <- data2 %>%
  mutate(age_group = cut(NU_IDADE_N, breaks = c(0, 1, 4, 17, 49, 64, 999), 
                         labels = c("<1", "1-4", "5-17", "18-49", "50-64", '65+'), 
                         include.lowest = TRUE))

age_group2 <- data2 %>% count(start_week,age_group)


data3 <- fread("https://s3.sa-east-1.amazonaws.com/ckan.saude.gov.br/SRAG/2021/INFLUD21-01-05-2023.csv", 
               select = c("DT_NOTIFIC",  'NU_IDADE_N', 'TP_FLU_PCR','CLASSI_FIN')) 
data3$DT_NOTIFIC = as.Date(data3$DT_NOTIFIC, format = "%d/%m/%Y")
data3 <- data3[order(data3$DT_NOTIFIC),]
data3 <- replace(data3, is.na(data3), 0)
data3$flu <- ifelse(data3$CLASSI_FIN == 1, 1, 0)
data3$flu_a <- ifelse(data3$TP_FLU_PCR == 1, 1, 0)
data3$flu_b <- ifelse(data3$TP_FLU_PCR == 2, 1, 0)
data3$week_type <- 'isoweek'
data3$start_week <-floor_date(data3$DT_NOTIFIC, 'week') + 1
data3$week_num <- isoweek(ymd(data3$start_week))
data3$year <- isoyear(data3$start_week)
ag_data3 <- data3 %>% group_by(start_week, year, week_num, week_type) %>% summarise(hospitalization = sum(flu), flu_a = sum(flu_a), flu_b = sum(flu_b) ) 

#age group data3
data3 <- subset(data3, flu != 0) 
data3 <- data3 %>%
  mutate(age_group = cut(NU_IDADE_N, breaks = c(0, 1, 4, 17, 49, 64, 999), 
                         labels = c("<1", "1-4", "5-17", "18-49", "50-64", '65+'), 
                         include.lowest = TRUE))

age_group3 <- data3 %>% count(start_week,age_group)

data4 <- fread("https://s3.sa-east-1.amazonaws.com/ckan.saude.gov.br/SRAG/2022/INFLUD22-03-04-2023.csv", 
               select = c("DT_NOTIFIC",  'NU_IDADE_N', 'TP_FLU_PCR','CLASSI_FIN')) 
data4$DT_NOTIFIC = as.Date(data4$DT_NOTIFIC, format = "%d/%m/%Y")
data4 <- data4[order(data4$DT_NOTIFIC),]
data4 <- replace(data4, is.na(data4), 0)
data4$flu <- ifelse(data4$CLASSI_FIN == 1, 1, 0)
data4$flu_a <- ifelse(data4$TP_FLU_PCR == 1, 1, 0)
data4$flu_b <- ifelse(data4$TP_FLU_PCR == 2, 1, 0)
data4$week_type <- 'isoweek'
data4$start_week <-floor_date(data4$DT_NOTIFIC, 'week') + 1
data4$week_num <- isoweek(ymd(data4$start_week))
data4$year <- isoyear(data4$start_week)
ag_data4 <- data4 %>% group_by(start_week, year, week_num, week_type) %>% summarise(hospitalization = sum(flu), flu_a = sum(flu_a), flu_b = sum(flu_b) ) 


#age group data4
data4 <- subset(data4, flu != 0) 
data4 <- data4 %>%
  mutate(age_group = cut(NU_IDADE_N, breaks = c(0, 1, 4, 17, 49, 64, 999), 
                         labels = c("<1", "1-4", "5-17", "18-49", "50-64", '65+'), 
                         include.lowest = TRUE))

age_group4 <- data4 %>% count(start_week,age_group)

data5 <- fread("https://s3.sa-east-1.amazonaws.com/ckan.saude.gov.br/SRAG/2023/INFLUD23-08-01-2024.csv", 
               select = c("DT_NOTIFIC",  'NU_IDADE_N', 'TP_FLU_PCR','CLASSI_FIN'))  
data5$DT_NOTIFIC = as.Date(data5$DT_NOTIFIC, format = "%d/%m/%Y")
data5 <- data5[order(data5$DT_NOTIFIC),]
data5 <- replace(data5, is.na(data5), 0)
data5$flu <- ifelse(data5$CLASSI_FIN == 1, 1, 0)
data5$flu_a <- ifelse(data5$TP_FLU_PCR == 1, 1, 0)
data5$flu_b <- ifelse(data5$TP_FLU_PCR == 2, 1, 0)
data5$week_type <- 'isoweek'
data5$start_week <-floor_date(data5$DT_NOTIFIC, 'week') + 1
data5$week_num <- isoweek(ymd(data5$start_week))
data5$year <- isoyear(data5$start_week)
ag_data5 <- data5 %>% group_by(start_week, year, week_num, week_type) %>% summarise(hospitalization = sum(flu), flu_a = sum(flu_a), flu_b = sum(flu_b) ) 

#age group data5
data5 <- subset(data5, flu != 0) 
data5 <- data5 %>%
  mutate(age_group = cut(NU_IDADE_N, breaks = c(0, 1, 4, 17, 49, 64, 999), 
                         labels = c("<1", "1-4", "5-17", "18-49", "50-64", '65+'), 
                         include.lowest = TRUE))

age_group5 <- data5 %>% count(start_week,age_group)


ag_data <- bind_rows(ag_data1, ag_data2, ag_data3, ag_data4, ag_data5)
total <- ag_data %>% group_by(start_week, year, week_num, week_type) %>% summarise(hospitalization = sum(hospitalization), flu_a = sum(flu_a), flu_b = sum(flu_b) ) 

age_data <- bind_rows(age_group1, age_group2, age_group3, age_group4, age_group5)
total_age <- age_data %>% group_by(start_week, age_group) %>% summarise(n = sum(n)) 

write.csv(total_age,'brazil_age_flu.csv')


total$hosp_pop <- round(ifelse(total$year == 2019, (total$hospitalization/(211.8*10^6))*100000, 
                               ifelse(total$year == 2020, (total$hospitalization/(213.2*10^6))*100000,
                                      ifelse(total$year == 2021, (total$hospitalization/(214.3*10^6))*100000,
                                             ifelse(total$year == 2022, (total$hospitalization/(215.3*10^6))*100000,
                                                    ifelse(total$year == 2023, (total$hospitalization/(216.4*10^6))*100000, 
                                                           ifelse(total$year == 2023, (total$hospitalization/(217.6*10^6))*100000, NA)))))),2)
write.csv(total,'brazil_flu.csv')

