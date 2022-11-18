## Using POLYMOD to scale CoMix data

#load libraries
library(data.table)
library(ggplot2)
library(tidyverse)
library(mgcv)
library(lubridate)
library(cowplot)

#set cowplot theme
theme_set(cowplot::theme_cowplot(font_size = 10) + theme(strip.background = element_blank()))

#set data path
data_path <-"C:\\Users\\emiel\\Documents\\LSHTM\\Fellowship\\Project\\comix_mobility\\Data\\"

#import mobility data
mob <- qs::qread(file.path(data_path, "google_mob.qs"))

#subset for same date range
mob_sub <- mob[date >= "2020-03-23" & date <= "2022-03-02"]

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

#merge mobility subset and lockdown information
mob_sub_l <- merge(mob_sub, lockdowns, by = "date", all.y = F)

#import contact data
cnts <- qs::qread(file.path(data_path,"part_cnts.qs"))

#filter out participants of a certain age
cnts <- cnts[part_age >= 18 & part_age <= 65]

#order by date
cnts_date <- cnts[order(date)]

#create data table with subset of variables
num <- cnts_date[, .(date, part_id, part_age, home = n_cnt_home, 
                     work = n_cnt_work, school = n_cnt_school, other = n_cnt_other)]
num[, t := as.numeric(date - ymd("2020-01-01"))]

#duplicate google mobility data and rename columns
gm2 <- rlang::duplicate(mob_sub_l)
names(gm2) <- str_replace(names(gm2), "_percent_change_from_baseline", "")
names(gm2) <- str_replace(names(gm2), "_and", "")

#merge data tables 
num <- merge(num, gm2[, c(1, 10:16)], by = "date", all.x = T)

#create study column
num[, study := "CoMix"]

#import edited polymod data
pnum <- qs::qread(file.path(data_path, "polymod.qs"))

#create study column
pnum[, study := "POLYMOD"]

#add information for lockdown status (i.e. none)
pnum[, status := "Pre-Pandemic"]

#bind the rows together
num <- rbind(num, pnum, fill = TRUE)

#turn mobility data into decimals instead of percentages
num[, retail_recreation := (100 + retail_recreation) * 0.01]
num[, grocery_pharmacy  := (100 + grocery_pharmacy ) * 0.01]
num[, parks             := (100 + parks            ) * 0.01]
num[, transit_stations  := (100 + transit_stations ) * 0.01]
num[, workplaces        := (100 + workplaces       ) * 0.01]
num[, residential       := (100 + residential      ) * 0.01]

#un-oversample young people from POLYMOD
num <- rbind(
  num[study == "CoMix"],
  num[study == "POLYMOD" & part_age <= 20][seq(0, .N, by = 2)],
  num[study == "POLYMOD" & part_age > 20]
)

#get fortnightly work data
another <- num[, .(status = status, work = mean(work), workplaces = mean(workplaces)),
               by = .(week = ifelse(study == "CoMix", paste(year(date), "/", 
                                                      ceiling(week(date)/2)), 
                                    rep(0, length(date))), study)]
another <- unique(another)

#scale data by POLYMOD data point 
another <- another[, work_frac := work/tail(work, 1)]

#model using GAM
model <- gam(work_frac ~ s(workplaces), family = gaussian, data = another)

#predict using 'new' data
work_f <- data.table(workplaces = seq(0, 1.25, by = 0.01));
work_f[, work := pmax(0.0, predict(model, work_f, type = "response"))]

#plot
plw <- ggplot(another) + 
  geom_point(aes(x = workplaces, y = work_frac, colour = status)) + 
  geom_line(data = work_f, aes(x = workplaces, y = work)) +
  ylim(0, 1) + xlim(0, 1) +
  labs(x = "Google Mobility\n'workplaces' visits",
       y = "Proportion of pre-pandemic\ncontacts", colour = "Status") 
plw

#plot without GAM
plw <- ggplot(another) + 
  geom_point(aes(x = workplaces, y = work_frac, colour = status)) + 
  ylim(0, 1) + xlim(0, 1) +
  labs(x = "Google Mobility\n'workplaces' visits",
       y = "Proportion of pre-pandemic\ncontacts", colour = "Status") 
plw

#get fortnightly 'other' data
another <- num[, .(status = status, other = mean(other), 
                   retail = mean(retail_recreation), 
                   grocery = mean(grocery_pharmacy), 
                   transit = mean(transit_stations)), 
               by = .(week = ifelse(study == "CoMix", paste(year(date), "/", 
                                                      ceiling(week(date)/2)), 
                                    rep(0, length(date))), study)]
another <- unique(another)

#scale data by POLYMOD data point 
another <- another[, other_frac := other/tail(other, 1)]

#create predictor from weighting of variables
another[, predictor := retail * 0.345 + transit * 0.445 + grocery * 0.210]

#model using GAM
model <- gam(other_frac ~ s(predictor), family = gaussian, data = another)

#predict using 'new' data
other_f <- data.table(predictor = seq(0, 1.25, by = 0.01));
other_f[, other := pmax(0.0, predict(model, other_f, type = "response"))]

#plot
plo <- ggplot(another) + 
  geom_point(aes(x = predictor, y = other_frac, colour = status)) + 
  geom_line(data = other_f, aes(x = predictor, y = other)) +
  xlim(0, 1) + ylim(0, 1) + 
  labs(x = "Google Mobility weighted 'transit stations',\n'retail and recreation', and 'grocery and pharmacy' visits", 
       y = "Proportion of pre-pandemic\ncontacts", colour = "Status")
plo

#plot without GAM
plo <- ggplot(another) + 
  geom_point(aes(x = predictor, y = other_frac, colour = status)) +
  xlim(0, 1) + ylim(0, 1) + 
  labs(x = "Google Mobility weighted 'transit stations',\n'retail and recreation', and 'grocery and pharmacy' visits", 
       y = "Proportion of pre-pandemic\ncontacts", colour = "Status")
plo
