##GAM practice part two 

#load libraries
library(data.table)
library(ggplot2)
library(tidyverse)
library(lubridate)
library(cowplot)
library(zoo)
library(matrixStats)
library(robsurvey)
library(mgcv)
library(visreg)

#set cowplot theme
theme_set(cowplot::theme_cowplot(font_size = 10) + theme(strip.background = element_blank()))

#set data path
data_path <-"C:\\Users\\emiel\\Documents\\LSHTM\\Fellowship\\Project\\comix_mobility\\Data\\"

#import contact data
cnts <- qs::qread(file.path(data_path, "part_cnts.qs"))

#filter out participants of a certain age
cnts <- cnts[part_age >= 18]

#order by date
cnts_date <- cnts[order(date)]
cnts_date <- cnts[date <= ymd("2022-03-02")]

#create data table with subset of variables
num <- cnts_date[, .(date, part_id, panel, part_age, survey_round, weekday, 
                     day_weight, area = area_rural_urban_code,
                     home = n_cnt_home, work = n_cnt_work, 
                     other = n_cnt_other, all = n_cnt)]
num[, t := as.numeric(date - ymd("2020-01-01"))]
num <- num[area == 1 | area  == 2]
num$area <- factor(num$area, levels = c(1, 2), labels = c("Urban", "Rural"))

#create study column
num[, study := "CoMix"]

#create sequence of dates
date <- seq(as.Date("2020-03-02"), as.Date("2022-03-02"), by = "days")
lockdowns <- as.data.table(as.Date(date))
lockdowns$lockdown_status <- 0
colnames(lockdowns) <- c("date", "status")

#create time intervals for different types of restrictions
T1 <- interval(ymd("2020-03-02"), ymd("2020-03-22"))
L1 <- interval(ymd("2020-03-23"), ymd("2020-05-31"))
T2 <- interval(ymd("2020-06-01"), ymd("2020-07-04"))
F1 <- interval(ymd("2020-07-05"), ymd("2020-09-13"))
T3 <- interval(ymd("2020-09-14"), ymd("2020-11-04"))
L2 <- interval(ymd("2020-11-05"), ymd("2020-12-01"))
T4 <- interval(ymd("2020-12-02"), ymd("2021-01-05"))
L3 <- interval(ymd("2021-01-06"), ymd("2021-03-07"))
T5 <- interval(ymd("2021-03-08"), ymd("2021-07-18"))
F2 <- interval(ymd("2021-07-19"), ymd("2021-12-07"))
T6 <- interval(ymd("2021-12-08"), ymd("2022-02-21"))

#assign value to each type of restriction
lockdowns$status <- ifelse(ymd(lockdowns$date) %within% T1, 1, 
                           ifelse(ymd(lockdowns$date) %within% L1, 2, 
                           ifelse(ymd(lockdowns$date) %within% T2, 1, 
                           ifelse(ymd(lockdowns$date) %within% T3, 1, 
                           ifelse(ymd(lockdowns$date) %within% L2, 2, 
                           ifelse(ymd(lockdowns$date) %within% T4, 1, 
                           ifelse(ymd(lockdowns$date) %within% L3, 2, 
                           ifelse(ymd(lockdowns$date) %within% T5, 1,
                           ifelse(ymd(lockdowns$date) %within% T6, 1, 0)))))))))

#create factor
lockdown_fac <- factor(lockdowns$status, levels = c(0, 1, 2, 3),
                       labels = c("No restrictions", "Some restrictions",
                                  "Lockdown", "Pre-Pandemic"))
lockdowns$status <- lockdown_fac

#merge contact data and lockdown information
cnts_l <- merge(num, lockdowns, by = "date", all.y = F)
num <- cnts_l

#create second database which shifts the survey rounds and dates
num2 <- rlang::duplicate(num)
num2[, date := date + 7]
num2[, survey_round := survey_round + 1]

#merge the two 
num_merge <- rbind(num, num2) 

#get dates in week
week <- unique(as.data.table(as.Date(num_merge$date)))
colnames(week) <- "date"
week <- week[, week := isoweek(date)]

#calculate non home contacts
num_merge[, nonhome := all - home]

#import data for stringency index
ox <- qs::qread(file.path(data_path, "stringency.qs"))
ox$date <- as.Date(ox$date)
num_merge <- merge(num_merge, ox)

merge_train <- num_merge[study == "CoMix" & panel == "A" | panel == "C" | panel == "E"]
merge_test <- num_merge[study == "CoMix" & panel == "B" | panel == "D" | panel == "F"]

#get weighted means by week
weighted_train <- merge_train[, .(study, status,
                                  stringency_index = mean(stringency_index),
                                  work = weighted.mean(work, day_weight),
                                  other = weighted.mean(other, day_weight),
                                  nonhome = weighted.mean(nonhome, day_weight)),
                              by = .(area,
                                     week = paste(year(date), "/", isoweek(date)))]  
weighted_train <- unique(weighted_train)
weighted_test <- merge_test[, .(study, status,
                                stringency_index = mean(stringency_index),
                                work = weighted.mean(work, day_weight),
                                other = weighted.mean(other, day_weight),
                                nonhome = weighted.mean(nonhome, day_weight)),
                            by = .(area,
                                   week = paste(year(date), "/", isoweek(date)))]  
weighted_test <- unique(weighted_test)

#import mobility data
mob <- qs::qread(file.path(data_path, "google_mob.qs"))

#subset for same date range
mob_sub <- mob[date >= "2020-03-23" & date <= "2022-03-02"]

#duplicate google mobility data and rename columns
gm2 <- rlang::duplicate(mob_sub)
names(gm2) <- str_replace(names(gm2), "_percent_change_from_baseline", "")
names(gm2) <- str_replace(names(gm2), "_and", "")

#turn mobility data into decimals instead of percentages
gm2[, retail_recreation := (100 + retail_recreation) * 0.01]
gm2[, grocery_pharmacy  := (100 + grocery_pharmacy ) * 0.01]
gm2[, parks             := (100 + parks            ) * 0.01]
gm2[, transit_stations  := (100 + transit_stations ) * 0.01]
gm2[, workplaces        := (100 + workplaces       ) * 0.01]
gm2[, residential       := (100 + residential      ) * 0.01]
gm2[, study := "CoMix"]

#get means for google mobility data
gm <- gm2[, .(workplaces = mean(workplaces),
              residential = mean(residential),
              retail = mean(retail_recreation), 
              grocery = mean(grocery_pharmacy), 
              transit = mean(transit_stations),
              parks = mean(parks)),
          by = .(week = ifelse(study == "CoMix",
                               paste(year(date), "/", isoweek(date)),
                               rep(0, length(date))), study)]

#create predictor for 'other' contacts
gm[, predictor := retail * 0.333 + transit * 0.334 + grocery * 0.333]

#merge
mob_cnt_train <- merge(weighted_train, gm, by = c("week", "study"))
mob_cnt_test <- merge(weighted_test, gm, by = c("week", "study"))

#scale data by POLYMOD data point 
mob_cnt_train <- mob_cnt_train[order(week)]
mob_cnt_train <- mob_cnt_train[, .(week, study, status, area,
                                   stringency_index, work, other, nonhome,
                                   workplaces, retail, grocery, parks, transit,
                                   residential, predictor)]
mob_cnt_test <- mob_cnt_test[order(week)]
mob_cnt_test <- mob_cnt_test[, .(week, study, status, area,
                                 stringency_index, work, other, nonhome,
                                 workplaces, retail, grocery, parks, transit, 
                                 residential, predictor)]

#model work data using GAM 
fit1 <- gam(work ~ s(workplaces, by = area) + area, data = mob_cnt_train, 
              method = "REML")
visreg(fit1, xvar = "workplaces",
       by = "area", data = mob_cnt_train,
       method = "REML")

#model work data using GAM 
fit2 <- gam(work ~ s(workplaces, by = area) + area + s(stringency_index), 
            data = mob_cnt_train, method = "REML")
visreg(fit2, xvar = "workplaces",
       by = "area", data = mob_cnt_train,
       method = "REML")

#model nonhome data using GAM 
fit3 <- gam(nonhome ~ s(residential, by = area) + area, data = mob_cnt_train, 
            method = "REML")
visreg(fit3, xvar = "residential",
       by = "area", data = mob_cnt_train,
       method = "REML")

#model nonhome data using GAM 
fit4 <- gam(nonhome ~ s(residential, by = area) + area + s(stringency_index), 
            data = mob_cnt_train, method = "REML")
visreg(fit4, xvar = "residential",
       by = "area", data = mob_cnt_train,
       method = "REML")

#model other data using GAM 
fit5 <- gam(other ~ s(predictor, by = area) + area, data = mob_cnt_train, 
            method = "REML")
visreg(fit5, xvar = "predictor",
       by = "area", data = mob_cnt_train,
       method = "REML")

#model other data using GAM 
fit6 <- gam(other ~ s(predictor, by = area) + area + s(stringency_index), 
            data = mob_cnt_train, method = "REML")
visreg(fit6, xvar = "predictor",
       by = "area", data = mob_cnt_train,
       method = "REML")
