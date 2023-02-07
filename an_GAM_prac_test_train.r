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
num <- cnts_date[, .(date, part_id, panel, part_age, survey_round, weekday, 
                     day_weight, home = n_cnt_home, work = n_cnt_work, 
                     other = n_cnt_other, all = n_cnt)]
num[, t := as.numeric(date - ymd("2020-01-01"))]

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

#import edited polymod data
pnum <- qs::qread(file.path(data_path, "polymod.qs"))

#create study column
pnum[, study := "POLYMOD"]

#add information for lockdown status (i.e. none)
pnum[, status := "Pre-Pandemic"]

#add weighting to polymod data
pnum[, weekday := lubridate::wday(date, label = T, abbr = F)]
pnum[, day_weight := ifelse(weekday == "Saturday", 2/7, 
                            ifelse(weekday == "Sunday", 2/7, 5/7))]

#bind the rows together
num <- rbind(cnts_l, pnum, fill = TRUE)

#remove participants of certain age from POLYMOD
num <- rbind(
  num[study == "CoMix"],
  num[study == "POLYMOD" & part_age >= 18]
)

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
pdate <- pnum[, .(date, stringency_index = 0)]
ox <- rbind(ox, pdate)
ox <- unique(ox)
num_merge <- merge(num_merge, ox)

merge_train <- num_merge[study == "CoMix" & panel == "A" | panel == "C" | panel == "E"]
poly_train <- num_merge[study == "POLYMOD"]
merge_train <- rbind(merge_train, poly_train)
merge_test <- num_merge[study == "CoMix" & panel == "B" | panel == "D" | panel == "F"]
poly_test <- num_merge[study == "POLYMOD"]
merge_test <- rbind(merge_test, poly_test)

#get weighted means by week
weighted_train <- merge_train[, .(study, status, 
                                stringency_index = mean(stringency_index),
                                work = weighted.mean(work, day_weight),
                                other = weighted.mean(other, day_weight),
                                nonhome = weighted.mean(nonhome, day_weight)),
                            by = .(week = paste(year(date), "/", isoweek(date)))]  
weighted_train <- unique(weighted_train)
weighted_test <- merge_test[, .(study, status, 
                                stringency_index = mean(stringency_index),
                                work = weighted.mean(work, day_weight),
                                other = weighted.mean(other, day_weight),
                                nonhome = weighted.mean(nonhome, day_weight)),
                            by = .(week = paste(year(date), "/", isoweek(date)))]  
weighted_test <- unique(weighted_test)

#get mean of polymod data so there is one baseline point
poly <- weighted_train[study == "POLYMOD"]
poly <- poly[, .(week = 0, study, status, 
                 stringency_index = mean(stringency_index),
                 work = mean(work, na.rm = T),
                 other = mean(other, na.rm = T),
                 nonhome = mean(nonhome, na.rm = T))]
poly <- unique(poly)
weighted_train <- weighted_train[study == "CoMix"]
weighted_train <- rbind(weighted_train, poly)
poly <- weighted_test[study == "POLYMOD"]
poly <- poly[, .(week = 0, study, status, 
                 stringency_index = mean(stringency_index),
                 work = mean(work, na.rm = T),
                 other = mean(other, na.rm = T),
                 nonhome = mean(nonhome, na.rm = T))]
poly <- unique(poly)
weighted_test <- weighted_test[study == "CoMix"]
weighted_test <- rbind(weighted_test, poly)

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

#add mobility to polymod dates
pnum2 <- pnum[, .(study, date, retail_recreation = 1, grocery_pharmacy = 1, 
                  parks = 1, transit_stations = 1, workplaces = 1,
                  residential = 1)]
gm2 <- gm2[, .(date, study, retail_recreation, grocery_pharmacy, parks, 
               transit_stations, workplaces, residential)]
gm2$date <- as.Date(gm2$date)
gm2 <- rbind(gm2, pnum2)

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

#merge
mob_cnt_train <- merge(weighted_train, gm, by = c("week", "study"))
mob_cnt_test <- merge(weighted_test, gm, by = c("week", "study"))

#scale data by POLYMOD data point 
mob_cnt_train <- mob_cnt_train[order(week)]
mob_cnt_train <- mob_cnt_train[, .(week, study, status, stringency_index,
                                   work, other, nonhome,
                                   work_frac = work/head(work, 1),
                                   other_frac = other/head(other, 1),
                                   nonhome_frac = nonhome/head(nonhome, 1),
                                   workplaces, retail, grocery, parks, transit,
                                   residential)]
mob_cnt_test <- mob_cnt_test[order(week)]
mob_cnt_test <- mob_cnt_test[, .(week, study, status, stringency_index,
                                 work, other, nonhome,
                                 work_frac = work/head(work, 1),
                                 other_frac = other/head(other, 1),
                                 nonhome_frac = nonhome/head(nonhome, 1),
                                 workplaces, retail, grocery, parks, transit, 
                                 residential)]

#model work data using GAM 
gam_w <- gam(work ~ s(workplaces), data = mob_cnt_train, method = "REML")

#predict using 'new' data
work_p <- mob_cnt_test
work_p[, work := pmax(0.0, predict(gam_w, work_p, type = "response"))]

#plot
plw = ggplot(mob_cnt_train) + 
  geom_point(aes(x = workplaces, y = work, col = status)) + 
  geom_line(data = work_p, aes(x = workplaces, y = work)) +
  labs(x = "Google Mobility\n'workplaces' visits", col = "Status",
       y = "Number of work contacts")
plw

#model work data using GAM 
gam_w1 <- gam(work ~ s(workplaces, by = status) + status, data = mob_cnt_train, 
              method = "REML")

#predict using 'new' data
work_p1 <- mob_cnt_test
work_p1[, work := pmax(0.0, predict(gam_w1, work_p1, type = "response"))]

#plot
plw1 = ggplot(mob_cnt_train) + 
  geom_point(aes(x = workplaces, y = work, col = status)) + 
  geom_line(data = work_p1, aes(x = workplaces, y = work)) +
  labs(x = "Google Mobility\n'workplaces' visits", col = "Status",
       y = "Number of work contacts")
plw1

#model work data using GAM 
gam_w2 <- gam(work ~ s(workplaces) + s(stringency_index), data = mob_cnt_train, 
              method = "REML")

#predict using 'new' data
work_p2 = mob_cnt_test
work_p2[, work := pmax(0.0, predict(gam_w2, work_p2, type = "response"))]

#plot
plw2 = ggplot(mob_cnt_train) + 
  geom_point(aes(x = workplaces, y = work, col = status)) + 
  geom_line(data = work_p2, aes(x = workplaces, y = work)) +
  labs(x = "Google Mobility\n'workplaces' visits", col = "Status",
       y = "Number of work contacts")
plw2

#model non-home data using GAM
gam_h <- gam(nonhome ~ s(residential), data = mob_cnt_train, method = "REML")

#predict using 'new' data
nonhome_p = mob_cnt_test
nonhome_p[, nonhome := pmax(0.0, predict(gam_h, nonhome_p, type = "response"))]

#plot
plh = ggplot(mob_cnt_train) + 
  geom_point(aes(x = residential, y = nonhome, col = status)) + 
  geom_line(data = nonhome_p, aes(x = residential, y = nonhome)) +
  labs(x = "Google Mobility time at 'residential' location",
       y = "Number of non-home contacts", colour = "Status")
plh

#model non-home data using GAM
gam_h1 <- gam(nonhome ~ s(residential, by = status) + status,
              data = mob_cnt_train, method = "REML")

#predict using 'new' data
nonhome_p1 = mob_cnt_test
nonhome_p1[, nonhome := pmax(0.0, predict(gam_h1, nonhome_p1, type = "response"))]

#plot
plh1 = ggplot(mob_cnt_train) + 
  geom_point(aes(x = residential, y = nonhome, col = status)) + 
  geom_line(data = nonhome_p1, aes(x = residential, y = nonhome)) +
  labs(x = "Google Mobility time at 'residential' location",
       y = "Number of non-home contacts", colour = "Status")
plh1

#model non-home data using GAM
gam_h2 <- gam(nonhome ~ s(residential) + s(stringency_index), 
              data = mob_cnt_train, method = "REML")

#predict using 'new' data
nonhome_p2 = mob_cnt_test
nonhome_p2[, nonhome := pmax(0.0, predict(gam_h2, nonhome_p2, type = "response"))]

#plot
plh2 = ggplot(mob_cnt_train) + 
  geom_point(aes(x = residential, y = nonhome, col = status)) + 
  geom_line(data = nonhome_p2, aes(x = residential, y = nonhome)) +
  labs(x = "Google Mobility time at 'residential' location",
       y = "Number of non-home contacts", colour = "Status")
plh2

###

#create predictor for 'other' contacts
mob_cnt[, predictor := retail * 0.345 + transit * 0.445 + grocery * 0.210]

#model other data using GAM
gam_o1 <- gam(other ~ s(predictor) + s(stringency_index), data = mob_cnt)

#predict using 'new' data
other_p1 = data.table(predictor = seq(0, 1, by = 0.01),
                      stringency_index = seq(0, 88, length.out = 101))
other_p1[, other := pmax(0.0, predict(gam_o1, other_p1, type = "response"))]

#plot
plo1 = ggplot(mob_cnt) + 
  geom_point(aes(x = predictor, y = other, col = status)) + 
  geom_line(data = other_p1, aes(x = predictor, y = other)) + 
  labs(x = "Google Mobility weighted 'transit stations',\n'retail and recreation', and 'grocery and pharmacy' visits", 
       y = "Number of 'other' contacts", colour = "Status")
plo1

#create predictor for 'other' contacts
mob_cnt[, predictor := retail * 0.333 + transit * 0.333 + grocery * 0.334]

#model other data using GAM
gam_o2 <- gam(other ~ s(predictor) + s(stringency_index), data = mob_cnt)

#predict using 'new' data
other_p2 = data.table(predictor = seq(0, 1, by = 0.01),
                     stringency_index = seq(0, 88, length.out = 101))
other_p2[, other := pmax(0.0, predict(gam_o2, other_p2, type = "response"))]

#plot
plo2 = ggplot(mob_cnt) + 
  geom_point(aes(x = predictor, y = other, col = status)) + 
  geom_line(data = other_p2, aes(x = predictor, y = other)) + 
  labs(x = "Google Mobility weighted 'transit stations',\n'retail and recreation', and 'grocery and pharmacy' visits", 
       y = "Proportion of pre-pandemic\n'other' contacts", colour = "Status")
plo2