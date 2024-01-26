# importing the required library 
library(rio) 
library(data.table)
library(dplyr)
library(formattable)
library(tidyr)
library(kableExtra)
library(lubridate)

# reading data from all sheets
mast <- import_list('https://github.com/hallucinojenlee/data_challenge_2024_sanofi/raw/main/Dataset/Consolidated_dataset_MASTER.xlsx') 
flu <- mast$Flu
rsv <- mast$RSV

flu$Week_date <- as.numeric(flu$Week_date)
flu$Week_date <- as.Date(flu$Week_date, origin = "1899-12-30")

rsv$Week_date <- as.numeric(rsv$Week_date)
rsv$Week_date <- as.Date(rsv$Week_date, origin = "1899-12-30")

before <- as.Date('2020-03-01')
after <- as.Date('2023-05-01')

flu_max <- flu %>% group_by(Country,Year) %>% slice(which.max(hospitalisation_num))
flu_rate_max <- flu %>% group_by(Country,Year) %>% slice(which.max(hospitalisation_rate))
rsv_max <- rsv %>% group_by(Country,Year) %>% slice(which.max(hospitalisation_num))
rsv_rate_max <- rsv %>% group_by(Country,Year) %>% slice(which.max(hospitalisation_rate))

flu$period <- ifelse(flu$Year <= isoyear(before), 'before', 
                     ifelse(flu$Year >= '2022', 'after', 'unlist'))
rsv$period <- ifelse(rsv$Year <= isoyear(before), 'before', 
                     ifelse(rsv$Year >= '2022', 'after', 'unlist'))
flu <- flu  %>% filter(Year %in% c(2017,2018,2019, 2023))
rsv <- rsv  %>% filter(Year %in% c(2017,2018,2019, 2023))
flu %>% pivot_wider(names_from = period, values_from = hospitalisation_num)


#UK and #US is hospitalisation rate using average
brazil_flu<-flu %>% filter(Country == 'Brazil') 
brazil_flu <- brazil_flu %>% pivot_wider(names_from = period, values_from = hospitalisation_num)
france_flu<-flu %>% filter(Country == 'France')
france_flu <- france_flu %>% pivot_wider(names_from = period, values_from = hospitalisation_num)
uk_flu <- flu %>% filter(Country == 'England')
uk_flu <- uk_flu %>% pivot_wider(names_from = period, values_from = hospitalisation_rate)
uk_flu$before <- as.numeric(uk_flu$before)
uk_flu$after <- as.numeric(uk_flu$after)
turkey_flu <- flu %>% filter(Country == 'Türkiye')
turkey_flu <- turkey_flu %>% pivot_wider(names_from = period, values_from = hospitalisation_num)
australia_flu <- flu %>% filter(Country == 'Australia')
australia_flu <- australia_flu %>% pivot_wider(names_from = period, values_from = hospitalisation_num)
usa_flu <- flu %>% filter(Country == 'US')
usa_flu <- usa_flu %>% pivot_wider(names_from = period, values_from = hospitalisation_rate)
usa_flu$before <- as.numeric(usa_flu$before)
usa_flu$after <- as.numeric(usa_flu$after)

#ttest
brazil_t <- t.test(brazil_flu$before, brazil_flu$after, var.equal=FALSE, na.rm = TRUE)
france_t<- t.test(france_flu$before, france_flu$after, var.equal=FALSE, na.rm = TRUE)
uk_t<- t.test(uk_flu$before, uk_flu$after, var.equal=FALSE, na.rm = TRUE)
turkey_t <- t.test(turkey_flu$before, turkey_flu$after, var.equal=FALSE, na.rm = TRUE)
australia_t<- t.test(australia_flu$before, australia_flu$after, var.equal=FALSE, na.rm = TRUE)
usa_t <- t.test(usa_flu$before, usa_flu$after, var.equal=FALSE, na.rm = TRUE)

brazil_rsv<-rsv %>% filter(Country == 'Brazil') 
brazil_rsv <- brazil_rsv %>% pivot_wider(names_from = period, values_from = hospitalisation_num)
france_rsv<-rsv %>% filter(Country == 'France')
france_rsv <- france_rsv %>% pivot_wider(names_from = period, values_from = hospitalisation_num)
uk_rsv <- rsv %>% filter(Country == 'England')
uk_rsv <- uk_rsv %>% pivot_wider(names_from = period, values_from = hospitalisation_rate)
uk_rsv$before <- as.numeric(uk_rsv$before)
uk_rsv$after <- as.numeric(uk_rsv$after)
turkey_rsv <- rsv %>% filter(Country == 'Türkiye')
turkey_rsv <- turkey_rsv %>% pivot_wider(names_from = period, values_from = hospitalisation_num)
australia_rsv <- rsv %>% filter(Country == 'Australia')
australia_rsv <- australia_rsv %>% pivot_wider(names_from = period, values_from = hospitalisation_num)
usa_rsv <- rsv %>% filter(Country == 'US')
usa_rsv <- usa_rsv %>% pivot_wider(names_from = period, values_from = hospitalisation_rate)
usa_rsv$before <- as.numeric(usa_rsv$before)
usa_rsv$after <- as.numeric(usa_rsv$after)

#ttest
brazil_rsv_t <- t.test(brazil_rsv$before, brazil_rsv$after, var.equal=FALSE, na.rm = TRUE)
france_rsv_t<- t.test(france_rsv$before, france_rsv$after, var.equal=FALSE, na.rm = TRUE)
uk_rsv_t<- t.test(uk_rsv$before, uk_rsv$after, var.equal=FALSE, na.rm = TRUE)
turkey_rsv_t<- t.test(turkey_rsv$before, turkey_rsv$after, var.equal=FALSE, na.rm = TRUE)
usa_rsv_t <- t.test(usa_rsv$before, usa_rsv$after, var.equal=FALSE, na.rm = TRUE)


flu_clean <- flu %>% select(Country, hospitalisation_num, period) 
flu_clean[is.na(flu_clean)] <- 0
flu1 <- flu_clean %>%
  pivot_wider(names_from = period,
              values_from = hospitalisation_num, values_fn=mean, values_fill=0)
flu_clean2 <- flu %>% select(Country, hospitalisation_rate, period) 
flu_clean2[is.na(flu_clean2)] <- 0
flu_clean2$hospitalisation_rate <- as.numeric(flu_clean2$hospitalisation_rate)
flu2 <- flu_clean2 %>%
  pivot_wider(names_from = period,
              values_from = hospitalisation_rate, values_fn=mean)

flu_table <- merge(x = flu1, y = flu2, by = "Country", all.x=TRUE)
flu_table <- flu_table %>% mutate(after.x = ifelse(after.x == 0, after.y, after.x), 
                                  before.x = ifelse(before.x == 0, before.y, before.x)) %>% select(Country, before.x, after.x) 

rsv_clean <- rsv %>% select(Country, hospitalisation_num, period) 
rsv_clean[is.na(rsv_clean)] <- 0
rsv1 <- rsv_clean %>%
  pivot_wider(names_from = period,
              values_from = hospitalisation_num, values_fn=mean, values_fill=0)
rsv_clean2 <- rsv %>% select(Country, hospitalisation_rate, period) 
rsv_clean2[is.na(rsv_clean2)] <- 0
rsv_clean2$hospitalisation_rate <- as.numeric(rsv_clean2$hospitalisation_rate)
rsv2 <- rsv_clean2 %>%
  pivot_wider(names_from = period,
              values_from = hospitalisation_rate, values_fn=mean)

rsv_table <- merge(x = rsv1, y = rsv2, by = "Country", all.x=TRUE)
rsv_table <- rsv_table %>% mutate(after.x = ifelse(after.x == 0, after.y, after.x), 
                                  before.x = ifelse(before.x == 0, before.y, before.x)) %>% select(Country, before.x, after.x) 
final_table <- merge(x = flu_table, y = rsv_table, by = "Country", all.x=TRUE)
final_table$before.x.x <- round(final_table$before.x.x, 2)
final_table$before.x.y <- round(final_table$before.x.y, 2)
final_table$after.x.x <- round(final_table$after.x.x, 2)
final_table$after.x.y <- round(final_table$after.x.y, 2)
final_table$diff_flu  <- round((final_table$after.x.x - final_table$before.x.x),2)
final_table$diff_rsv  <- final_table$after.x.y - final_table$before.x.y
final_table$pvalue_flu <- round(c(australia_t$p.value, brazil_t$p.value, uk_t$p.value, france_t$p.value, turkey_t$p.value, usa_t$p.value),3)
final_table$pvalue_rsv <- round(c(NA, brazil_rsv_t$p.value, uk_rsv_t$p.value, france_rsv_t$p.value, turkey_rsv_t$p.value, usa_rsv_t$p.value),3)
p_flu <- as.character(final_table$pvalue_flu)
p_rsv <- as.character(final_table$pvalue_rsv)
final_table$pvalue_flu <- ifelse(final_table$pvalue_flu < 0.05, '<0.05*', p_flu)
final_table$pvalue_rsv <- ifelse(final_table$pvalue_rsv < 0.05, '<0.05*', p_rsv)

#data table
col_order <- c("Country", "before.x.x", "after.x.x", "diff_flu", 'pvalue_flu', "before.x.y", "after.x.y", "diff_rsv", 'pvalue_rsv')
final_table <- final_table[, col_order]
final_table <- final_table %>% rename(`Flu Before` = before.x.x, `Flu After` = after.x.x, `Flu Diff` = diff_flu, `P` = pvalue_flu,
                                      `RSV Before` = before.x.y, `RSV After` = after.x.y, `RSV Diff` = diff_rsv, `P ` = pvalue_rsv)
final_table$Country <- ifelse(final_table$Country %in% c('England', 'US'), paste(final_table$Country, "*", sep = ""), final_table$Country)
final_table <- final_table %>% 
  arrange(factor(Country, levels = c('Australia','Brazil','France','Türkiye','England*','US*')))


customGreen0 = "#DeF7E9"

customGreen = "#71CA97"

customRed = "#ff7f7f"
improvement_formatter <- formatter("span", style = x ~ style(font.weight = "bold", 
                                                             color = ifelse(x > 0, customRed, ifelse(x < 0, customGreen, "black"))), 
                                   x ~ icontext(ifelse(x>0, "arrow-up", "arrow-down"), x))
x <- formattable(final_table, align =c("c","r","r","r","c", "r", 'r', 'r', 'c'), list(
  `Country` = formatter("span", style = ~ style(color = ifelse(Country == c("England*",'US*'), "black", "gray"),
                                                font.weight = "bold")), 
  `Flu Before`= color_tile(customGreen0, customGreen),
  `Flu After`= color_tile(customGreen0, customGreen),
  `Flu Diff`= improvement_formatter,
  `P` = formatter("span", 
                  ~ icontext(ifelse(`P` == '<0.05*', "ok", "remove"), ifelse(`P` == '<0.05*', "<0.05*", `P`)), 
                  style =  ~ style(color = ifelse(`P` == '<0.05*', "green", "gray"))),
  `RSV Before`= color_tile(customGreen0, customGreen),
  `RSV After`= color_tile(customGreen0, customGreen),
  `RSV Diff`= improvement_formatter,
  `P ` = formatter("span", 
                  ~ icontext(ifelse(`P ` == '<0.05*', "ok", "remove"), ifelse(`P ` == '<0.05*', "<0.05*", `P `)), 
                  style =  ~ style(color = ifelse(`P ` == '<0.05*', "green", "gray")))
  )) 

x
