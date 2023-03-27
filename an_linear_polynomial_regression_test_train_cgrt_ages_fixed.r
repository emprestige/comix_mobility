#load libraries
library(data.table)
library(ggplot2)
library(tidyverse)
library(lubridate)
library(cowplot)
library(zoo)
library(survey)
library(ggrepel)

#set cowplot theme
theme_set(cowplot::theme_cowplot(font_size = 10) + theme(strip.background = element_blank()))

#set data path
data_path <-"C:\\Users\\emiel\\Documents\\LSHTM\\Fellowship\\Project\\comix_mobility\\Data\\"

#import contact data
cnts <- qs::qread(file.path(data_path, "part_cnts.qs"))
cnts[, week := paste(isoyear(date), "/", sprintf("%02d", isoweek(date)))]

#fix age groups
cnts <- cnts %>%
  mutate(part_age_group = case_when(part_age >= 18 & part_age <= 29 ~ "18-29",
                                    part_age >= 30 & part_age <= 39 ~ "30-39",
                                    part_age >= 40 & part_age <= 49 ~ "40-49",
                                    part_age >= 50 & part_age <= 59 ~ "50-59",
                                    part_age >= 60 & part_age <= 69 ~ "60-69",
                                    part_age >= 70 ~ "70+"))
cnts <- cnts %>%
  filter(!is.na(part_age_group))

#order by date
cnts_date <- cnts[order(date)]
cnts_date <- cnts_date[date <= ymd("2022-03-02")]

#create data table with subset of variables
num <- cnts_date[, .(date, part_id, panel, part_age, survey_round, weekday, 
                     day_weight, home = n_cnt_home, work = n_cnt_work, 
                     other = n_cnt_other, all = n_cnt)]

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

#import edited polymod data
pnum <- qs::qread(file.path(data_path, "polymod.qs"))

#create study column
pnum[, study := "POLYMOD"]
pnum[, social_weight := 1]

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

#add column for special dates 
summer <- interval(ymd("2020-08-03"), ymd("2020-08-09"))
num_merge[, special := ifelse(date == ymd("2020-12-25"), "Xmas",
                       ifelse(date == ymd("2021-12-25"), "Xmas",
                       ifelse(date == ymd("2020-12-31"), "NYE",
                       ifelse(date == ymd("2021-12-31"), "NYE",
                       ifelse(date == ymd("2021-01-01"), "NYD",
                       ifelse(date == ymd("2022-01-01"), "NYD",
                       ifelse(date == ymd("2020-04-13"), "Easter",
                       ifelse(date == ymd("2021-04-05"), "Easter", 
                       ifelse(date %within% summer, "Summer Hol", NA)))))))))]

merge_train <- num_merge[study == "CoMix" & panel == "A" | panel == "C" | panel == "E"]
poly_train <- num_merge[study == "POLYMOD"]
merge_train <- rbind(merge_train, poly_train)
merge_test <- num_merge[study == "CoMix" & panel == "B" | panel == "D" | panel == "F"]
poly_test <- num_merge[study == "POLYMOD"]
merge_test <- rbind(merge_test, poly_test)

#get weighted means by week
weighted_train <- merge_train[, .(study, status, special,
                                  work = weighted.mean(work, day_weight),
                                  other = weighted.mean(other, day_weight),
                                  nonhome = weighted.mean(nonhome, day_weight)),
                  by = .(week = paste(isoyear(date), "/", sprintf("%02d", isoweek(date))))]  
weighted_train <- unique(weighted_train)
weighted_test <- merge_test[, .(study, status, special,
                                work = weighted.mean(work, day_weight),
                                other = weighted.mean(other, day_weight),
                                nonhome = weighted.mean(nonhome, day_weight)),
                 by = .(week = paste(isoyear(date), "/", sprintf("%02d", isoweek(date))))]  
weighted_test <- unique(weighted_test)

#import data for stringency index components
ox <- qs::qread(file.path(data_path, "stringency_component.qs"))
pdate <- pnum[, .(date, C1M_School.closing = 0, C2M_Workplace.closing = 0,
                  C7M_Restrictions.on.internal.movement = 0,
                  H1_Public.information.campaigns = 0)]
ox <- rbind(ox, pdate)
ox <- unique(ox)

#get means by week
ox_week <- ox[, .(school_closure = mean(C1M_School.closing),
                  work_closure = mean(C2M_Workplace.closing),
                  internal_movement = mean(C7M_Restrictions.on.internal.movement),
                  public_info = mean(H1_Public.information.campaigns)),
                  by = .(week = paste(isoyear(date), "/", sprintf("%02d", isoweek(date))))] 

#merge with comix 
weighted_train_merge <- merge(weighted_train, ox_week, all.x = T)
weighted_test_merge <- merge(weighted_test, ox_week, all.x = T)

#get mean of polymod data so there is one baseline point
poly <- weighted_train_merge[study == "POLYMOD"]
poly <- poly[, .(week = 0, study, status, special,
                 school_closure = mean(school_closure, na.rm = T),
                 work_closure = mean(work_closure, na.rm = T),
                 internal_movement = mean(internal_movement, na.rm = T),
                 public_info = mean(public_info, na.rm = T), 
                 work = mean(work, na.rm = T),
                 other = mean(other, na.rm = T),
                 nonhome = mean(nonhome, na.rm = T))]
poly <- unique(poly)
weighted_train_merge <- weighted_train_merge[study == "CoMix"]
weighted_train_merge <- rbind(weighted_train_merge, poly)
poly <- weighted_test_merge[study == "POLYMOD"]
poly <- poly[, .(week = 0, study, status, special,
                 school_closure = mean(school_closure, na.rm = T),
                 work_closure = mean(work_closure, na.rm = T),
                 internal_movement = mean(internal_movement, na.rm = T),
                 public_info = mean(public_info, na.rm = T), 
                 work = mean(work, na.rm = T),
                 other = mean(other, na.rm = T),
                 nonhome = mean(nonhome, na.rm = T))]
poly <- unique(poly)
weighted_test_merge <- weighted_test_merge[study == "CoMix"]
weighted_test_merge <- rbind(weighted_test_merge, poly)

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
                               paste(year(date), "/", sprintf("%02d", isoweek(date))),
                               rep(0, length(date))), study)]

#create predictor for 'other' contacts
gm[, predictor := retail * 0.333 + transit * 0.334 + grocery * 0.333]

#merge
mob_cnt_train <- merge(weighted_train_merge, gm, by = c("week", "study"))
mob_cnt_test <- merge(weighted_test_merge, gm, by = c("week", "study"))

#scale data by POLYMOD data point 
mob_cnt_train <- mob_cnt_train[order(week)]
mob_cnt_train <- mob_cnt_train[, .(week, study, status, special, school_closure,
                                   work_closure, internal_movement, public_info,
                                   work, other, nonhome, workplaces, retail, 
                                   grocery, parks, transit, residential, predictor)]
mob_cnt_test <- mob_cnt_test[order(week)]
mob_cnt_test <- mob_cnt_test[, .(week, study, status, special, school_closure,
                                 work_closure, internal_movement, public_info,
                                 work, other, nonhome, workplaces, retail, 
                                 grocery, parks, transit, residential, predictor)]

#set variables to medians for predictions
mob_cnt_test[, school_closure := median(school_closure)]
mob_cnt_test[, work_closure := median(work_closure)]
mob_cnt_test[, internal_movement := median(internal_movement)]
mob_cnt_test[, public_info := median(public_info)]

#model work data using linear regression
lm_w1 <- lm(work ~ workplaces + work_closure + school_closure + public_info + 
              internal_movement, data = mob_cnt_train)
summary(lm_w1)

#predict using 'new' data
pred <- predict(lm_w1, newdata = mob_cnt_test, interval = "confidence")
pred <- as.data.table(pred)
mob_cnt_test[, work := pred$fit]
mob_cnt_test[, w_lwr := pred$lwr]
mob_cnt_test[, w_uppr := pred$upr]

#plot
plw1 = ggplot(mob_cnt_train, aes(x = workplaces, y = work, 
        label = ifelse(status == "No restrictions", week, special))) + 
  geom_point(aes(col = status)) + geom_text_repel(size = 2.5, max.overlaps = 100) +
  geom_line(data = mob_cnt_test, aes(x = workplaces, y = work)) +
  geom_ribbon(data = mob_cnt_test, aes(ymin = w_lwr, ymax = w_uppr), alpha = 0.1) +
  labs(x = "Google Mobility\n'workplaces' visits", col = "Status",
       y = "Number of work contacts") +
  scale_colour_manual(values = c("No restrictions" = "#00BA38", 
                                 "Some restrictions" = "#619CFF", 
                                 "Lockdown" = "#F8766D",
                                 "Pre-Pandemic" = "purple"), 
                      labels = c("No restrictions", "Some restrictions", 
                                 "Lockdown", "Pre-Pandemic"))
plw1

#model work data using polynomial regression
lm_w2 <- lm(work ~ poly(workplaces, 2) + work_closure + school_closure + 
              public_info + internal_movement, data = mob_cnt_train)
summary(lm_w2)

# #predict using 'new' data
# pred <- predict(lm_w2, newdata = mob_cnt_test, interval = "confidence")
# pred <- as.data.table(pred)
# mob_cnt_test[, work := pred$fit]
# mob_cnt_test[, w_lwr := pred$lwr]
# mob_cnt_test[, w_uppr := pred$upr]
# 
# #plot
# plw2 = ggplot(mob_cnt_train, aes(x = workplaces, y = work,
#         label = ifelse(status == "No restrictions", week, special))) +
#   geom_point(aes(col = status)) + geom_text_repel(size = 2.5, max.overlaps = 100) +
#   geom_line(data = mob_cnt_test, aes(x = workplaces, y = work)) +
#   geom_ribbon(data = mob_cnt_test, aes(ymin = w_lwr, ymax = w_uppr), alpha = 0.1) +
#   labs(x = "Google Mobility\n'workplaces' visits", col = "Status",
#        y = "Number of work contacts") +
#   scale_colour_manual(values = c("No restrictions" = "#00BA38",
#                                  "Some restrictions" = "#619CFF",
#                                  "Lockdown" = "#F8766D",
#                                  "Pre-Pandemic" = "purple"),
#                       labels = c("No restrictions", "Some restrictions",
#                                  "Lockdown", "Pre-Pandemic"))
# plw2
 
#model work data using linear regression
lm_w3 <- lm(work ~ workplaces + school_closure + public_info + internal_movement, 
            data = mob_cnt_train)
summary(lm_w3)

# #predict using 'new' data
# pred <- predict(lm_w3, newdata = mob_cnt_test, interval = "confidence")
# pred <- as.data.table(pred)
# mob_cnt_test[, work := pred$fit]
# mob_cnt_test[, w_lwr := pred$lwr]
# mob_cnt_test[, w_uppr := pred$upr]
# 
# #plot
# plw3 = ggplot(mob_cnt_train, aes(x = workplaces, y = work,
#         label = ifelse(status == "No restrictions", week, special))) +
#   geom_point(aes(col = status)) + geom_text_repel(size = 2.5, max.overlaps = 100) +
#   geom_line(data = mob_cnt_test, aes(x = workplaces, y = work)) +
#   geom_ribbon(data = mob_cnt_test, aes(ymin = w_lwr, ymax = w_uppr), alpha = 0.1) +
#   labs(x = "Google Mobility\n'workplaces' visits", col = "Status",
#        y = "Number of work contacts") +
#   scale_colour_manual(values = c("No restrictions" = "#00BA38",
#                                  "Some restrictions" = "#619CFF",
#                                  "Lockdown" = "#F8766D",
#                                  "Pre-Pandemic" = "purple"),
#                       labels = c("No restrictions", "Some restrictions",
#                                  "Lockdown", "Pre-Pandemic"))
# plw3

#model work data using polynomial regression
lm_w4 <- lm(work ~ poly(workplaces, 2) + school_closure + public_info + 
              internal_movement, data = mob_cnt_train)
summary(lm_w4)

# #predict using 'new' data
# pred <- predict(lm_w4, newdata = mob_cnt_test, interval = "confidence")
# pred <- as.data.table(pred)
# mob_cnt_test[, work := pred$fit]
# mob_cnt_test[, w_lwr := pred$lwr]
# mob_cnt_test[, w_uppr := pred$upr]
# 
# #plot
# plw4 = ggplot(mob_cnt_train, aes(x = workplaces, y = work, 
#         label = ifelse(status == "No restrictions", week, special))) + 
#   geom_point(aes(col = status)) + geom_text_repel(size = 2.5, max.overlaps = 100) +
#   geom_line(data = mob_cnt_test, aes(x = workplaces, y = work)) +
#   geom_ribbon(data = mob_cnt_test, aes(ymin = w_lwr, ymax = w_uppr), alpha = 0.1) +
#   labs(x = "Google Mobility\n'workplaces' visits", col = "Status",
#        y = "Number of work contacts") +
#   scale_colour_manual(values = c("No restrictions" = "#00BA38", 
#                                  "Some restrictions" = "#619CFF", 
#                                  "Lockdown" = "#F8766D",
#                                  "Pre-Pandemic" = "purple"), 
#                       labels = c("No restrictions", "Some restrictions", 
#                                  "Lockdown", "Pre-Pandemic"))
# plw4

#model work data using linear regression
lm_w5 <- lm(work ~ polyworkplaces + school_closure + public_info, data = mob_cnt_train)
summary(lm_w5)

#predict using 'new' data
pred <- predict(lm_w5, newdata = mob_cnt_test, interval = "confidence")
pred <- as.data.table(pred)
mob_cnt_test[, work := pred$fit]
mob_cnt_test[, w_lwr := pred$lwr]
mob_cnt_test[, w_uppr := pred$upr]

#plot
plw5 = ggplot(mob_cnt_train, aes(x = workplaces, y = work, 
        label = ifelse(status == "No restrictions", week, special))) + 
  geom_point(aes(col = status)) + geom_text_repel(size = 2.5, max.overlaps = 100) +
  geom_line(data = mob_cnt_test, aes(x = workplaces, y = work)) +
  geom_ribbon(data = mob_cnt_test, aes(ymin = w_lwr, ymax = w_uppr), alpha = 0.1) +
  labs(x = "Google Mobility\n'workplaces' visits", col = "Status",
       y = "Number of work contacts") +
  scale_colour_manual(values = c("No restrictions" = "#00BA38", 
                                 "Some restrictions" = "#619CFF", 
                                 "Lockdown" = "#F8766D",
                                 "Pre-Pandemic" = "purple"), 
                      labels = c("No restrictions", "Some restrictions", 
                                 "Lockdown", "Pre-Pandemic"))
plw5

#model work data using linear regression
lm_w6 <- lm(work ~ poly(workplaces, 2) + school_closure + public_info, 
            data = mob_cnt_train)
summary(lm_w6)

#predict using 'new' data
pred <- predict(lm_w6, newdata = mob_cnt_test, interval = "confidence")
pred <- as.data.table(pred)
mob_cnt_test[, work := pred$fit]
mob_cnt_test[, w_lwr := pred$lwr]
mob_cnt_test[, w_uppr := pred$upr]

#plot
plw6 = ggplot(mob_cnt_train, aes(x = workplaces, y = work, 
        label = ifelse(status == "No restrictions", week, special))) + 
  geom_point(aes(col = status)) + geom_text_repel(size = 2.5, max.overlaps = 100) +
  geom_line(data = mob_cnt_test, aes(x = workplaces, y = work)) +
  geom_ribbon(data = mob_cnt_test, aes(ymin = w_lwr, ymax = w_uppr), alpha = 0.1) +
  labs(x = "Google Mobility\n'workplaces' visits", col = "Status",
       y = "Number of work contacts") +
  scale_colour_manual(values = c("No restrictions" = "#00BA38", 
                                 "Some restrictions" = "#619CFF", 
                                 "Lockdown" = "#F8766D",
                                 "Pre-Pandemic" = "purple"), 
                      labels = c("No restrictions", "Some restrictions", 
                                 "Lockdown", "Pre-Pandemic"))
plw6

#model non-home data using linear regression
lm_h1 <- lm(nonhome ~ residential + work_closure + school_closure + public_info +
              internal_movement, data = mob_cnt_train)
summary(lm_h1)

#predict using 'new' data
pred <- predict(lm_h1, newdata = mob_cnt_test, interval = "confidence")
pred <- as.data.table(pred)
mob_cnt_test[, nonhome := pred$fit]
mob_cnt_test[, h_lwr := pred$lwr]
mob_cnt_test[, h_uppr := pred$upr]

#plot
plh1 = ggplot(mob_cnt_train, aes(x = residential, y = nonhome,
        label = ifelse(status == "No restrictions", week, special))) +
  geom_point(aes(col = status)) + geom_text_repel(size = 2.5, max.overlaps = 100) +
  geom_line(data = mob_cnt_test, aes(x = residential, y = nonhome)) +
  geom_ribbon(data = mob_cnt_test, aes(ymin = h_lwr, ymax = h_uppr), alpha = 0.1) +
  labs(x = "Google Mobility time at 'residential' location",
       y = "Number of non-home contacts", colour = "Status") +
  scale_colour_manual(values = c("No restrictions" = "#00BA38",
                                 "Some restrictions" = "#619CFF",
                                 "Lockdown" = "#F8766D",
                                 "Pre-Pandemic" = "purple"),
                      labels = c("No restrictions", "Some restrictions",
                                 "Lockdown", "Pre-Pandemic"))
plh1

#model non-home data using polynomial regression
lm_h2 <- lm(nonhome ~ poly(residential, 2) + work_closure + school_closure +
              public_info + internal_movement, data = mob_cnt_train)
summary(lm_h2)

# #predict using 'new' data
# pred <- predict(lm_h2, newdata = mob_cnt_test, interval = "confidence")
# pred <- as.data.table(pred)
# mob_cnt_test[, nonhome := pred$fit]
# mob_cnt_test[, h_lwr := pred$lwr]
# mob_cnt_test[, h_uppr := pred$upr]
# 
# #plot
# plh2 = ggplot(mob_cnt_train, aes(x = residential, y = nonhome, 
#           label = ifelse(status == "No restrictions", week, special))) + 
#   geom_point(aes(col = status)) + geom_text_repel(size = 2.5, max.overlaps = 100) + 
#   geom_line(data = mob_cnt_test, aes(x = residential, y = nonhome)) +
#   geom_ribbon(data = mob_cnt_test, aes(ymin = h_lwr, ymax = h_uppr), alpha = 0.1) +
#   labs(x = "Google Mobility time at 'residential' location",
#        y = "Number of non-home contacts", colour = "Status") +
#   scale_colour_manual(values = c("No restrictions" = "#00BA38", 
#                                  "Some restrictions" = "#619CFF", 
#                                  "Lockdown" = "#F8766D",
#                                  "Pre-Pandemic" = "purple"), 
#                       labels = c("No restrictions", "Some restrictions", 
#                                  "Lockdown", "Pre-Pandemic"))
# plh2
                   
#model 'other' data using linear regression 
lm_o1 <- lm(other ~ predictor + work_closure + school_closure + public_info +
              internal_movement, data = mob_cnt_train)
summary(lm_o1)

#predict using 'new' data
pred <- predict(lm_o1, newdata = mob_cnt_test, interval = "confidence")
pred <- as.data.table(pred)
mob_cnt_test[, other := pred$fit]
mob_cnt_test[, o_lwr := pred$lwr]
mob_cnt_test[, o_uppr := pred$upr]

#plot
plo1 = ggplot(mob_cnt_train, aes(x = predictor, y = other, 
          label = ifelse(status == "No restrictions", week, special))) + 
  geom_point(aes(col = status)) + geom_text_repel(size = 2.5, max.overlaps = 100) +
  geom_line(data = mob_cnt_test, aes(x = predictor, y = other)) +
  geom_ribbon(data = mob_cnt_test, aes(ymin = o_lwr, ymax = o_uppr), alpha = 0.1) +
  labs(x = "Google Mobility weighted 'transit stations',\n'retail and recreation', and 'grocery and pharmacy' visits",
       y = "Number of 'other' contacts",  col = "Status") +
  scale_colour_manual(values = c("No restrictions" = "#00BA38", 
                                 "Some restrictions" = "#619CFF", 
                                 "Lockdown" = "#F8766D",
                                 "Pre-Pandemic" = "purple"), 
                      labels = c("No restrictions", "Some restrictions", 
                                 "Lockdown", "Pre-Pandemic"))
plo1

#model 'other' data using polynomial regression 
lm_o2 <- lm(other ~ poly(predictor, 2) + work_closure + school_closure + 
              public_info + internal_movement, data = mob_cnt_train)
summary(lm_o2)
lrtest(lm_o1, lm_o2)

#predict using 'new' data
pred <- predict(lm_o2, newdata = mob_cnt_test, interval = "confidence")
pred <- as.data.table(pred)
mob_cnt_test[, other := pred$fit]
mob_cnt_test[, o_lwr := pred$lwr]
mob_cnt_test[, o_uppr := pred$upr]

#plot
plo2 = ggplot(mob_cnt_train, aes(x = predictor, y = other,
          label = ifelse(status == "No restrictions", week, special))) +
  geom_point(aes(col = status)) + geom_text_repel(size = 2.5, max.overlaps = 100) +
  geom_line(data = mob_cnt_test, aes(x = predictor, y = other)) +
  geom_ribbon(data = mob_cnt_test, aes(ymin = o_lwr, ymax = o_uppr), alpha = 0.1) +
  labs(x = "Google Mobility weighted 'transit stations',\n'retail and recreation', and 'grocery and pharmacy' visits",
       y = "Number of 'other' contacts",  col = "Status") +
  scale_colour_manual(values = c("No restrictions" = "#00BA38",
                                 "Some restrictions" = "#619CFF",
                                 "Lockdown" = "#F8766D",
                                 "Pre-Pandemic" = "purple"),
                      labels = c("No restrictions", "Some restrictions",
                                 "Lockdown", "Pre-Pandemic"))
plo2
