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

#create data table with subset of variables
num <- cnts_date[, .(date, part_id, part_age, survey_round, weekday, day_weight, 
                     area = area_rural_urban_code, home = n_cnt_home, 
                     work = n_cnt_work, other = n_cnt_other, all = n_cnt)]
num[, t := as.numeric(date - ymd("2020-01-01"))]
num <- num[area == 1 | area  == 2]
num$area <- factor(num$area, levels = c(1, 2), labels = c("Urban", "Rural"))

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
F2 <- interval(ymd("2021-07-19"), ymd("2022-03-02"))

#assign value to each type of restriction
lockdowns$status <- ifelse(ymd(lockdowns$date) %within% T1, 1, 
                           ifelse(ymd(lockdowns$date) %within% L1, 2, 
                           ifelse(ymd(lockdowns$date) %within% T2, 1, 
                           ifelse(ymd(lockdowns$date) %within% T3, 1, 
                           ifelse(ymd(lockdowns$date) %within% L2, 2, 
                           ifelse(ymd(lockdowns$date) %within% T4, 1, 
                           ifelse(ymd(lockdowns$date) %within% L3, 2, 
                           ifelse(ymd(lockdowns$date) %within% T5, 1, 0))))))))

#create factor
lockdown_fac <- factor(lockdowns$status, levels = c(0, 1, 2, 3),
                       labels = c("No restrictions", "Some restrictions",
                                  "Lockdown", "Pre-Pandemic"))
lockdowns$status <- lockdown_fac

#merge contact data and lockdown information
cnts_l <- merge(num, lockdowns, by = "date", all.y = F)

#create second database which shifts the survey rounds and dates
num2 <- rlang::duplicate(cnts_l)
num2[, date := date + 7]
num2[, survey_round := survey_round + 1]

#merge the two 
num_merge <- rbind(cnts_l, num2) 

#get dates in week
week <- unique(as.data.table(as.Date(num_merge$date)))
colnames(week) <- "date"
week <- week[, week := isoweek(date)]

#calculate non home contacts
num_merge[, nonhome := all - home]

#get weighted means by week
weighted_date <- num_merge[, .(status, area, 
                               work = weighted.mean(work, day_weight),
                               other = weighted.mean(other, day_weight),
                               nonhome = weighted.mean(nonhome, day_weight)),
                           by = .(week = paste(year(date), "/", isoweek(date)))]  
weighted_date <- unique(weighted_date)

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

#get means for google mobility data
gm <- gm2[, .(workplaces = mean(workplaces),
              residential = mean(residential),
              retail = mean(retail_recreation), 
              grocery = mean(grocery_pharmacy), 
              transit = mean(transit_stations),
              parks = mean(parks)),
          by = .(week = paste(year(date), "/", isoweek(date)))]

#merge
mob_cnt <- merge(weighted_date, gm, by = "week")

#scale data by POLYMOD data point 
mob_cnt <- mob_cnt[order(week)]
mob_cnt <- mob_cnt[, .(week, status, area, work, other, nonhome,
                       work_frac = work/head(work, 1),
                       other_frac = other/head(other, 1),
                       nonhome_frac = nonhome/head(nonhome, 1),
                       workplaces, retail, grocery, parks, transit, residential)]

#model work data using GAM - 1 (k-1) d.f. for smoothing term
gam_w1 <- gam(work ~ s(workplaces, by = area) + area, data = mob_cnt, method = "REML")

#predict using 'new' data
work_p1 = data.table(workplaces = seq(0, 1, by = 0.01), 
                     area = ifelse(runif(101, 0, 1) > 0.5, 2, 1))
work_p1$area <- factor(work_p1$area, levels = c(1, 2), 
                       labels = c("Urban", "Rural"))
work_p1[, work := pmax(0.0, predict(gam_w1, work_p1, type = "response"))]

#plot
plw1 = ggplot(mob_cnt) + 
  geom_point(aes(x = workplaces, y = work, col = status)) + 
  geom_line(data = work_p1, aes(x = workplaces, y = work)) +
  labs(x = "Google Mobility\n'workplaces' visits", col = "Status",
       y = "Number of work contacts")
plw1

plot(gam_w1, pages = 1, all.terms = T)

#create predictor for 'other' contacts
mob_cnt[, predictor := retail * 0.345 + transit * 0.445 + grocery * 0.210]

#model other data using GAM
gam_o1 <- gam(other ~ s(predictor, by = area) + area, data = mob_cnt,
              method = "REML")

#predict using 'new' data
other_p1 = data.table(predictor = seq(0, 1, by = 0.01), 
                     area = ifelse(runif(101, 0, 1) > 0.5, 2, 1))
other_p1$area <- factor(other_p1$area, levels = c(1, 2), 
                        labels = c("Urban", "Rural"))
other_p1[, other := pmax(0.0, predict(gam_o1, other_p1, type = "response"))]

#plot
plo = ggplot(mob_cnt) + 
  geom_point(aes(x = predictor, y = other, col = status)) + 
  geom_line(data = other_p1, aes(x = predictor, y = other)) + 
  labs(x = "Google Mobility weighted 'transit stations',\n'retail and recreation', and 'grocery and pharmacy' visits", 
       y = "Number of 'other' contacts", colour = "Status")
plo

#model non-home data using GAM
gam_h1 <- gam(nonhome ~ s(residential, by = area) + area, data = mob_cnt,
              method = "REML")

#predict using 'new' data
nonhome_p1 = data.table(residential = seq(1, 1.3, by = 0.01), 
                        area = ifelse(runif(31, 0, 1) > 0.5, 2, 1))
nonhome_p1$area <- factor(nonhome_p1$area, levels = c(1, 2), 
                          labels = c("Urban", "Rural"))
nonhome_p1[, nonhome := pmax(0.0, predict(gam_h1, nonhome_p1, type = "response"))]

#plot
plh = ggplot(mob_cnt) + 
  geom_point(aes(x = residential, y = nonhome, col = status)) + 
  geom_line(data = nonhome_p1, aes(x = residential, y = nonhome)) +
  labs(x = "Google Mobility time at 'residential' location",
       y = "Number of non-home contacts", colour = "Status")
plh
