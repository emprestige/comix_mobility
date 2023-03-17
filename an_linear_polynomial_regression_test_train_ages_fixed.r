##linear and polynomial regression 

#load libraries
library(data.table)
library(ggplot2)
library(tidyverse)
library(lubridate)
library(cowplot)
library(visreg)
library(ggrepel)
library(lmtest)

#set cowplot theme
theme_set(cowplot::theme_cowplot(font_size = 10) + theme(strip.background = element_blank()))

#set data path
data_path <-"C:\\Users\\emiel\\Documents\\LSHTM\\Fellowship\\Project\\comix_mobility\\Data\\"

#import contact data
cnts <- qs::qread(file.path(data_path, "part_cnts.qs"))

#filter out participants of a certain age
cnts <- cnts[sample_type == "adult"]

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
cnts_date <- cnts[date <= ymd("2022-03-02")]

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
weighted_train <- merge_train[, .(study, status, special,
                                stringency_index = mean(stringency_index),
                                work = weighted.mean(work, day_weight),
                                other = weighted.mean(other, day_weight),
                                nonhome = weighted.mean(nonhome, day_weight)),
                            by = .(week = paste(isoyear(date), "/", sprintf("%02d", isoweek(date))))]  
weighted_train <- unique(weighted_train)
weighted_test <- merge_test[, .(study, status, special,
                                stringency_index = mean(stringency_index),
                                work = weighted.mean(work, day_weight),
                                other = weighted.mean(other, day_weight),
                                nonhome = weighted.mean(nonhome, day_weight)),
                            by = .(week = paste(isoyear(date), "/", sprintf("%02d", isoweek(date))))]  
weighted_test <- unique(weighted_test)

#get mean of polymod data so there is one baseline point
poly <- weighted_train[study == "POLYMOD"]
poly <- poly[, .(week = 0, study, status, special,
                 stringency_index = mean(stringency_index),
                 work = mean(work, na.rm = T),
                 other = mean(other, na.rm = T),
                 nonhome = mean(nonhome, na.rm = T))]
poly <- unique(poly)
weighted_train <- weighted_train[study == "CoMix"]
weighted_train <- rbind(weighted_train, poly)
poly <- weighted_test[study == "POLYMOD"]
poly <- poly[, .(week = 0, study, status, special,
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
                               paste(year(date), "/", sprintf("%02d", isoweek(date))),
                               rep(0, length(date))), study)]

#create predictor for 'other' contacts
gm[, predictor := retail * 0.333 + transit * 0.334 + grocery * 0.333]

#merge
mob_cnt_train <- merge(weighted_train, gm, by = c("week", "study"))
mob_cnt_test <- merge(weighted_test, gm, by = c("week", "study"))

#scale data by POLYMOD data point 
mob_cnt_train <- mob_cnt_train[order(week)]
mob_cnt_train <- mob_cnt_train[, .(week, study, status, special, 
                                   stringency_index, work, other, nonhome,
                                   workplaces, retail, grocery, parks, transit,
                                   residential, predictor)]
mob_cnt_test <- mob_cnt_test[order(week)]
mob_cnt_test <- mob_cnt_test[, .(week, study, status, special,
                                 stringency_index, work, other, nonhome,
                                 workplaces, retail, grocery, parks, transit, 
                                 residential, predictor)]

#model work data using linear regression
lm_w1 <- lm(work ~ workplaces, data = mob_cnt_train)
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

#model work data using polynomial regresion
lm_w2 <- lm(work ~ poly(workplaces, 2), data = mob_cnt_train)
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
#           label = ifelse(status == "No restrictions", week, special))) + 
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
#
# #model work data using linear regression with status included as effect modifier
# nopoly_train <- mob_cnt_train[study == "CoMix"]
# nopoly_test <- mob_cnt_test[study == "CoMix"]
# fit <- lm(work ~ workplaces, data = nopoly_train)
# fit_w <- lm(work ~ workplaces + status, data = nopoly_train)
# lrtest(fit_w, fit)
# fit_w1 <- lm(work ~ workplaces * status, data = nopoly_train)
# summary(fit_w1)
# lrtest(fit_w1, fit_w)
# 
# #predict using 'new' data
# pred <- predict(fit_w1, newdata = nopoly_test, interval = "confidence")
# pred <- as.data.table(pred)
# nopoly_test[, work := pred$fit]
# nopoly_test[, w_lwr := pred$lwr]
# nopoly_test[, w_uppr := pred$upr]
# 
# plwf1 = ggplot(nopoly_train, aes(x = workplaces, y = work, 
#           label = ifelse(status == "No restrictions", week, special))) + 
#   geom_point(aes(col = status)) + geom_text_repel(size = 2.5, max.overlaps = 100) +
#   geom_line(data = nopoly_test, aes(x = workplaces, y = work)) +
#   geom_ribbon(data = nopoly_test, aes(ymin = w_lwr, ymax = w_uppr), alpha = 0.1) +
#   labs(x = "Google Mobility\n'workplaces' visits", col = "Status",
#        y = "Number of work contacts") +
#   scale_colour_manual(values = c("No restrictions" = "#00BA38", 
#                                  "Some restrictions" = "#619CFF", 
#                                  "Lockdown" = "#F8766D",
#                                  "Pre-Pandemic" = "purple"), 
#                       labels = c("No restrictions", "Some restrictions", 
#                                  "Lockdown", "Pre-Pandemic"))
# plwf1 + facet_grid(cols = vars(status))
# 
# #model work data using polynomial regression with status as effect modifier
# nopoly_train <- mob_cnt_train[study == "CoMix"]
# nopoly_test <- mob_cnt_test[study == "CoMix"]
# fit_w2 <- lm(work ~ poly(workplaces, 2) * status, data = nopoly_train)
# summary(fit_w2)
# lrtest(fit_w2, fit_w1)
# 
# #predict using 'new' data
# pred <- predict(fit_w2, newdata = nopoly_test, interval = "confidence")
# pred <- as.data.table(pred)
# nopoly_test[, work := pred$fit]
# nopoly_test[, w_lwr := pred$lwr]
# nopoly_test[, w_uppr := pred$upr]
# 
# plwf2 = ggplot(nopoly_train, aes(x = workplaces, y = work, 
#           label = ifelse(status == "No restrictions", week, special))) + 
#   geom_point(aes(col = status)) + geom_text_repel(size = 2.5, max.overlaps = 100) +
#   geom_line(data = nopoly_test, aes(x = workplaces, y = work)) +
#   geom_ribbon(data = nopoly_test, aes(ymin = w_lwr, ymax = w_uppr), alpha = 0.1) +
#   labs(x = "Google Mobility\n'workplaces' visits", col = "Status",
#        y = "Number of work contacts") +
#   scale_colour_manual(values = c("No restrictions" = "#00BA38", 
#                                  "Some restrictions" = "#619CFF", 
#                                  "Lockdown" = "#F8766D",
#                                  "Pre-Pandemic" = "purple"), 
#                       labels = c("No restrictions", "Some restrictions", 
#                                  "Lockdown", "Pre-Pandemic"))
# plwf2 + facet_grid(cols = vars(status))

#model work data using linear regression including stringency index
lm_w3 <- lm(work ~ workplaces + stringency_index, data = mob_cnt_train)
summary(lm_w3)

#predict using 'new' data
mob_cnt_test[, stringency_index := median(stringency_index)]
pred <- predict(lm_w3, newdata = mob_cnt_test, interval = "confidence")
pred <- as.data.table(pred)
mob_cnt_test[, work := pred$fit]
mob_cnt_test[, w_lwr := pred$lwr]
mob_cnt_test[, w_uppr := pred$upr]

#plot
plw3 = ggplot(mob_cnt_train, aes(x = workplaces, y = work, 
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
plw3

#model work data using linear regression including stringency index
lm_w4 <- lm(work ~ poly(workplaces, 2) + stringency_index, data = mob_cnt_train)
summary(lm_w4)

# #predict using 'new' data
# mob_cnt_test[, stringency_index := median(stringency_index)]
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

#model non-home data using linear regression
lm_h1 <- lm(nonhome ~ residential, data = mob_cnt_train)
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
lm_h2 <- lm(nonhome ~ poly(residential, 2), data = mob_cnt_train)
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
#
# #model nonhome data using linear regression with status included as effect modifier
# nopoly_train <- mob_cnt_train[study == "CoMix"]
# nopoly_test <- mob_cnt_test[study == "CoMix"]
# fit_h <- lm(nonhome ~ predictor, data = nopoly_train)
# fit_h.5 <- lm(nonhome ~ predictor + status, data = nopoly_train)
# summary(fit_h.5)
# lrtest(fit_h.5, fit_h)
# fit_h1 <- lm(nonhome ~ residential * status, data = nopoly_train)
# summary(fit_h1)
# 
# #predict using 'new' data
# pred <- predict(fit_h1, newdata = nopoly_test, interval = "confidence")
# pred <- as.data.table(pred)
# nopoly_test[, nonhome := pred$fit]
# nopoly_test[, h_lwr := pred$lwr]
# nopoly_test[, h_uppr := pred$upr]
# 
# plhf1 = ggplot(nopoly_train, aes(x = residential, y = nonhome, 
#           label = ifelse(status == "No restrictions", week, special))) + 
#   geom_point(aes(col = status)) + geom_text_repel(size = 2.5, max.overlaps = 100) +
#   geom_line(data = nopoly_test, aes(x = residential, y = nonhome)) +
#   geom_ribbon(data = nopoly_test, aes(ymin = h_lwr, ymax = h_uppr), alpha = 0.1) +
#   labs(x = "Google Mobility time at 'residential' location", col = "Status",
#        y = "Number of non-home contacts") +
#   scale_colour_manual(values = c("No restrictions" = "#00BA38", 
#                                  "Some restrictions" = "#619CFF", 
#                                  "Lockdown" = "#F8766D",
#                                  "Pre-Pandemic" = "purple"), 
#                       labels = c("No restrictions", "Some restrictions", 
#                                  "Lockdown", "Pre-Pandemic"))
# plhf1 + facet_grid(cols = vars(status))
# 
# #model nonhome data using polynomial regression with status as effect modifier
# nopoly_train <- mob_cnt_train[study == "CoMix"]
# nopoly_test <- mob_cnt_test[study == "CoMix"]
# fit_h2 <- lm(nonhome ~ poly(residential, 2) * status, data = nopoly_train)
# summary(fit_h2)
# lrtest(fit_h2, fit_h1)
# 
# #predict using 'new' data
# pred <- predict(fit_h2, newdata = nopoly_test, interval = "confidence")
# pred <- as.data.table(pred)
# nopoly_test[, nonhome := pred$fit]
# nopoly_test[, h_lwr := pred$lwr]
# nopoly_test[, h_uppr := pred$upr]
# 
# plhf2 = ggplot(nopoly_train, aes(x = residential, y = nonhome, 
#           label = ifelse(status == "No restrictions", week, special))) + 
#   geom_point(aes(col = status)) + geom_text_repel(size = 2.5, max.overlaps = 100) +
#   geom_line(data = nopoly_test, aes(x = residential, y = nonhome)) +
#   geom_ribbon(data = nopoly_test, aes(ymin = h_lwr, ymax = h_uppr), alpha = 0.1) +
#   labs(x = "Google Mobility time at 'residential' location", col = "Status",
#        y = "Number of non-home contacts") +
#   scale_colour_manual(values = c("No restrictions" = "#00BA38", 
#                                  "Some restrictions" = "#619CFF", 
#                                  "Lockdown" = "#F8766D",
#                                  "Pre-Pandemic" = "purple"), 
#                       labels = c("No restrictions", "Some restrictions", 
#                                  "Lockdown", "Pre-Pandemic"))
# plhf2 + facet_grid(cols = vars(status))
                   
#model non-home data using linear regression including stringency index
lm_h3 <- lm(nonhome ~ residential + stringency_index, data = mob_cnt_train)
summary(lm_h3)

# #predict using 'new' data
# mob_cnt_test[, stringency_index := median(stringency_index)]
# pred <- predict(lm_h3, newdata = mob_cnt_test, interval = "confidence")
# pred <- as.data.table(pred)
# mob_cnt_test[, nonhome := pred$fit]
# mob_cnt_test[, h_lwr := pred$lwr]
# mob_cnt_test[, h_uppr := pred$upr]
# 
# #plot
# plh3 = ggplot(mob_cnt_train, aes(x = residential, y = nonhome, 
#         label = ifelse(status == "No restrictions", week, special))) + 
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
# plh3

#model non-home data using polynomial regression including stringency index
lm_h4 <- lm(nonhome ~ poly(residential, 2) + stringency_index, data = mob_cnt_train)
summary(lm_h4)
# lrtest(lm_h4, lm_h3)
# 
# #predict using 'new' data
# mob_cnt_test[, stringency_index := median(stringency_index)]
# pred <- predict(lm_h4, newdata = mob_cnt_test, interval = "confidence")
# pred <- as.data.table(pred)
# mob_cnt_test[, nonhome := pred$fit]
# mob_cnt_test[, h_lwr := pred$lwr]
# mob_cnt_test[, h_uppr := pred$upr]
# 
# #plot
# plh4 = ggplot(mob_cnt_train, aes(x = residential, y = nonhome, 
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
# plh4

#model 'other' data using linear regression 
lm_o1 <- lm(other ~ predictor, data = mob_cnt_train)
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
lm_o2 <- lm(other ~ poly(predictor, 2), data = mob_cnt_train)
summary(lm_o2)
#lrtest(lm_o2, lm_o1)

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

# #model 'other' data using linear regression with status included as effect modifier
# nopoly_train <- mob_cnt_train[study == "CoMix"]
# nopoly_test <- mob_cnt_test[study == "CoMix"]
# fit_o1 <- lm(other ~ predictor * status, data = nopoly_train)
# summary(fit_o1)
# 
# #predict using 'new' data
# pred <- predict(fit_o1, newdata = nopoly_test, interval = "confidence")
# pred <- as.data.table(pred)
# nopoly_test[, other := pred$fit]
# nopoly_test[, o_lwr := pred$lwr]
# nopoly_test[, o_uppr := pred$upr]
# 
# plof1 = ggplot(nopoly_train, aes(x = predictor, y = other, 
#           label = ifelse(status == "No restrictions", week, special))) + 
#   geom_point(aes(col = status)) + geom_text_repel(size = 2.5, max.overlaps = 100) +
#   geom_line(data = nopoly_test, aes(x = predictor, y = other)) +
#   geom_ribbon(data = nopoly_test, aes(ymin = o_lwr, ymax = o_uppr), alpha = 0.1) +
#   labs(x = "Google Mobility weighted 'transit stations',\n'retail and recreation', and 'grocery and pharmacy' visits",
#        y = "Number of 'other' contacts",  col = "Status") +
#   scale_colour_manual(values = c("No restrictions" = "#00BA38", 
#                                  "Some restrictions" = "#619CFF", 
#                                  "Lockdown" = "#F8766D",
#                                  "Pre-Pandemic" = "purple"), 
#                       labels = c("No restrictions", "Some restrictions", 
#                                  "Lockdown", "Pre-Pandemic"))
# plof1 + facet_grid(cols = vars(status))
# 
# #model 'other' data using polynomial regression with status as effect modifier
# nopoly_train <- mob_cnt_train[study == "CoMix"]
# nopoly_test <- mob_cnt_test[study == "CoMix"]
# fit_o2 <- lm(other ~ poly(predictor, 2) * status, data = nopoly_train)
# summary(fit_o2)
# lrtest(fit_o2, fit_o1)
# 
# #predict using 'new' data
# pred <- predict(fit_o2, newdata = nopoly_test, interval = "confidence")
# pred <- as.data.table(pred)
# nopoly_test[, other := pred$fit]
# nopoly_test[, o_lwr := pred$lwr]
# nopoly_test[, o_uppr := pred$upr]
# 
# plof2 = ggplot(nopoly_train, aes(x = predictor, y = other, 
#           label = ifelse(status == "No restrictions", week, special))) + 
#   geom_point(aes(col = status)) + geom_text_repel(size = 2.5, max.overlaps = 100) +
#   geom_line(data = nopoly_test, aes(x = predictor, y = other)) +
#   geom_ribbon(data = nopoly_test, aes(ymin = o_lwr, ymax = o_uppr), alpha = 0.1) +
#   labs(x = "Google Mobility weighted 'transit stations',\n'retail and recreation', and 'grocery and pharmacy' visits",
#        y = "Number of 'other' contacts",  col = "Status") +
#   scale_colour_manual(values = c("No restrictions" = "#00BA38", 
#                                  "Some restrictions" = "#619CFF", 
#                                  "Lockdown" = "#F8766D",
#                                  "Pre-Pandemic" = "purple"), 
#                       labels = c("No restrictions", "Some restrictions", 
#                                  "Lockdown", "Pre-Pandemic"))
# plof2 + facet_grid(cols = vars(status))

#model 'other' data using linear regression including stringency index
lm_o3 <- lm(other ~ predictor + stringency_index, data = mob_cnt_train)
summary(lm_o3)

# #predict using 'new' data
# mob_cnt_test[, stringency_index := median(stringency_index)]
# pred <- predict(lm_o3, newdata = mob_cnt_test, interval = "confidence")
# pred <- as.data.table(pred)
# mob_cnt_test[, other := pred$fit]
# mob_cnt_test[, o_lwr := pred$lwr]
# mob_cnt_test[, o_uppr := pred$upr]
# 
# #plot
# plo3 = ggplot(mob_cnt_train, aes(x = predictor, y = other, 
#           label = ifelse(status == "No restrictions", week, special))) + 
#   geom_point(aes(col = status)) + geom_text_repel(size = 2.5, max.overlaps = 100) +
#   geom_line(data = mob_cnt_test, aes(x = predictor, y = other)) +
#   geom_ribbon(data = mob_cnt_test, aes(ymin = o_lwr, ymax = o_uppr), alpha = 0.1) +
#   labs(x = "Google Mobility weighted 'transit stations',\n'retail and recreation', and 'grocery and pharmacy' visits",
#        y = "Number of 'other' contacts",  col = "Status") +
#   scale_colour_manual(values = c("No restrictions" = "#00BA38", 
#                                  "Some restrictions" = "#619CFF", 
#                                  "Lockdown" = "#F8766D",
#                                  "Pre-Pandemic" = "purple"), 
#                       labels = c("No restrictions", "Some restrictions", 
#                                  "Lockdown", "Pre-Pandemic"))
# plo3
# 
# #model 'other' data using polynomial regression including stringency index
# lm_o4 <- lm(other ~ poly(predictor, 2) + stringency_index, data = mob_cnt_train)
# summary(lm_o4)
# lrtest(lm_o4, lm_o3)
# lrtest(lm_o4, lm_o2)
# 
# #predict using 'new' data
# mob_cnt_test[, stringency_index := median(stringency_index)]
# pred <- predict(lm_o4, newdata = mob_cnt_test, interval = "confidence")
# pred <- as.data.table(pred)
# mob_cnt_test[, other := pred$fit]
# mob_cnt_test[, o_lwr := pred$lwr]
# mob_cnt_test[, o_uppr := pred$upr]
# 
# #plot
# plo4 = ggplot(mob_cnt_train, aes(x = predictor, y = other, 
#           label = ifelse(status == "No restrictions", week, special))) + 
#   geom_point(aes(col = status)) + geom_text_repel(size = 2.5, max.overlaps = 100) +
#   geom_line(data = mob_cnt_test, aes(x = predictor, y = other)) +
#   geom_ribbon(data = mob_cnt_test, aes(ymin = o_lwr, ymax = o_uppr), alpha = 0.1) +
#   labs(x = "Google Mobility weighted 'transit stations',\n'retail and recreation', and 'grocery and pharmacy' visits",
#        y = "Number of 'other' contacts",  col = "Status") +
#   scale_colour_manual(values = c("No restrictions" = "#00BA38", 
#                                  "Some restrictions" = "#619CFF", 
#                                  "Lockdown" = "#F8766D",
#                                  "Pre-Pandemic" = "purple"), 
#                       labels = c("No restrictions", "Some restrictions", 
#                                  "Lockdown", "Pre-Pandemic"))
# plo4
