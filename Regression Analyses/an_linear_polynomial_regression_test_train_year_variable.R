##linear and polynomial regression and GAMs

#load libraries
library(data.table)
library(ggplot2)
library(tidyverse)
library(lubridate)
library(cowplot)
library(visreg)
library(ggrepel)
library(lmtest)
library(interactions)
library(mgcv)

#set cowplot theme
theme_set(cowplot::theme_cowplot(font_size = 10) + theme(strip.background = element_blank()))

#set data path
data_path <-"C:\\Users\\emiel\\Documents\\LSHTM\\Fellowship\\Project\\comix_mobility\\Data\\"

#import contact data
cnts <- qs::qread(file.path(data_path, "cnts_weight_test_new2.qs"))

#filter out participants of a certain age
cnts <- cnts[sample_type == "adult"]
cnts <- cnts %>%
  filter(!is.na(part_age_group))

#order by date
cnts_date <- cnts[order(date)]
cnts_date <- cnts[date <= ymd("2022-03-02")]

#create data table with subset of variables
num <- cnts_date[, .(date, part_id, panel, part_age, survey_round, weekday, 
                     home = n_cnt_home, work = n_cnt_work, other = n_cnt_other, 
                     all = n_cnt_home + n_cnt_work + n_cnt_other, day_weight, 
                     social_weight = weight_raw)]
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

#create intervals for pandemic year 
year1 <- interval(ymd("2020-03-02"), ymd("2021-03-31"))
year2 <- interval(ymd("2021-04-01"), ymd("2022-03-02"))

#create pandemic year variable
cnts_l[, p_year := ifelse(ymd(cnts_l$date) %within% year1, 1,
                   ifelse(ymd(cnts_l$date) %within% year2, 2, NA))]

#import edited polymod data
pnum <- qs::qread(file.path(data_path, "polymod.qs"))

#create study column
pnum[, study := "POLYMOD"]

#add information for lockdown status (i.e. none)
pnum[, status := "Pre-Pandemic"]

#add weightings to polymod data
pnum[, weekday := lubridate::wday(date, label = T, abbr = F)]
pnum[, day_weight := ifelse(weekday == "Saturday", 2/7, 
                            ifelse(weekday == "Sunday", 2/7, 5/7))]
pnum[, social_weight := 1]

#add variable for pandemic year (0)
pnum[, p_year := 0]

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
weighted_train <- merge_train[, .(study, status, special, p_year, 
                                  stringency_index = mean(stringency_index),
                                  work = weighted.mean(work, day_weight*social_weight),
                                  other = weighted.mean(other, day_weight*social_weight),
                                  nonhome = weighted.mean(nonhome, day_weight*social_weight)),
                              by = .(week = paste(isoyear(date), "/", sprintf("%02d", isoweek(date))))]  
weighted_train <- unique(weighted_train)
weighted_test <- merge_test[, .(study, status, special, p_year,
                                stringency_index = mean(stringency_index),
                                work = weighted.mean(work, day_weight*social_weight),
                                other = weighted.mean(other, day_weight*social_weight),
                                nonhome = weighted.mean(nonhome, day_weight*social_weight)),
                            by = .(week = paste(isoyear(date), "/", sprintf("%02d", isoweek(date))))]  
weighted_test <- unique(weighted_test)

#get mean of polymod data so there is one baseline point
poly <- weighted_train[study == "POLYMOD"]
poly <- poly[, .(week = 0, study, status, special, p_year,
                 stringency_index = mean(stringency_index),
                 work = mean(work, na.rm = T),
                 other = mean(other, na.rm = T),
                 nonhome = mean(nonhome, na.rm = T))]
poly <- unique(poly)
weighted_train <- weighted_train[study == "CoMix"]
weighted_train <- rbind(weighted_train, poly)
poly <- weighted_test[study == "POLYMOD"]
poly <- poly[, .(week = 0, study, status, special, p_year,
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
mob_cnt_train <- mob_cnt_train[, .(week, study, status, special, p_year,
                                   stringency_index, work, other, nonhome,
                                   workplaces, retail, grocery, parks, transit,
                                   residential, predictor)]
mob_cnt_test <- mob_cnt_test[order(week)]
mob_cnt_test <- mob_cnt_test[, .(week, study, status, special, p_year,
                                 stringency_index, work, other, nonhome,
                                 workplaces, retail, grocery, parks, transit, 
                                 residential, predictor)]

#linear regression including interaction term for pandemic year
lm_w1 <- lm(work ~ workplaces*p_year, data = mob_cnt_train)
summary(lm_w1)

#plot interaction
interact_plot(lm_w1, pred = workplaces, modx = p_year, modx.values = c(1, 2), plot.points = T)

#predict using 'new' data (one for each pandemic year)
mob_cnt_test1 <- rlang::duplicate(mob_cnt_test)
mob_cnt_test2 <- rlang::duplicate(mob_cnt_test)
mob_cnt_test1[, p_year := 1]
mob_cnt_test2[, p_year := 2]

#predict using both data sets
pred1 <- predict(lm_w1, newdata = mob_cnt_test1, interval = "confidence")
pred1 <- as.data.table(pred1)
mob_cnt_test1[, work := pred1$fit]
mob_cnt_test1[, w_lwr := pred1$lwr]
mob_cnt_test1[, w_uppr := pred1$upr]
pred2 <- predict(lm_w1, newdata = mob_cnt_test2, interval = "confidence")
pred2 <- as.data.table(pred2)
mob_cnt_test2[, work := pred2$fit]
mob_cnt_test2[, w_lwr := pred2$lwr]
mob_cnt_test2[, w_uppr := pred2$upr]

#plot
plw1 = ggplot(mob_cnt_train, aes(x = workplaces, y = work,
          label = ifelse(status == "No restrictions", week, special))) +
  geom_point(aes(col = status)) + geom_text_repel(size = 2.5, max.overlaps = 100) +
  geom_line(data = mob_cnt_test1, aes(x = workplaces, y = work, linetype = "Year 1")) +
  geom_ribbon(data = mob_cnt_test1, aes(ymin = w_lwr, ymax = w_uppr), alpha = 0.1) +
  geom_line(data = mob_cnt_test2, aes(x = workplaces, y = work, linetype = "Year 2")) +
  geom_ribbon(data = mob_cnt_test2, aes(ymin = w_lwr, ymax = w_uppr), alpha = 0.1) +
  labs(x = "Google Mobility\n'workplaces' visits", col = "Status",
       y = "Number of work contacts", linetype = "Pandemic Year") +
  scale_colour_manual(values = c("No restrictions" = "#00BA38",
                                 "Some restrictions" = "#619CFF",
                                 "Lockdown" = "#F8766D",
                                 "Pre-Pandemic" = "purple")) +
  scale_linetype_manual(values = c("Year 1" = 2, "Year 2" = 3))
plw1

#quadratic regression including interaction term for pandemic year
lm_w2 <- lm(work ~ poly(workplaces, 2)*p_year, data = mob_cnt_train)
summary(lm_w2)

# #GAM
# gam_w1 <- gam(work ~ s(workplaces, by = p_year) + p_year, data = mob_cnt_train)
# summary(gam_w1)
# 
# #predict using 'new' data (one for each pandemic year)
# mob_cnt_test1 <- rlang::duplicate(mob_cnt_test)
# mob_cnt_test2 <- rlang::duplicate(mob_cnt_test)
# mob_cnt_test1[, p_year := 1]
# mob_cnt_test2[, p_year := 2]
# 
# #predict using both data sets
# pred1 <- predict(gam_w1, newdata = mob_cnt_test1, type = "response", se.fit = T)
# pred1 <- as.data.table(pred1)
# mob_cnt_test1[, work := pred1$fit]
# mob_cnt_test1[, w_lwr := pred1$fit - 2*pred1$se.fit]
# mob_cnt_test1[, w_uppr := pred1$fit + 2*pred1$se.fit]
# pred2 <- predict(gam_w1, newdata = mob_cnt_test2, type = "response", se.fit = T)
# pred2 <- as.data.table(pred2)
# mob_cnt_test2[, work := pred2$fit]
# mob_cnt_test2[, w_lwr := pred2$fit - 2*pred2$se.fit]
# mob_cnt_test2[, w_uppr := pred2$fit + 2*pred2$se.fit]
# 
# #plot
# plw2 = ggplot(mob_cnt_train, aes(x = workplaces, y = work,
#           label = ifelse(status == "No restrictions", week, special))) +
#   geom_point(aes(col = status)) + geom_text_repel(size = 2.5, max.overlaps = 100) +
#   geom_line(data = mob_cnt_test1, aes(x = workplaces, y = work, linetype = "Year 1")) +
#   #geom_ribbon(data = mob_cnt_test1, aes(ymin = w_lwr, ymax = w_uppr), alpha = 0.1) +
#   geom_line(data = mob_cnt_test2, aes(x = workplaces, y = work, linetype = "Year 2")) +
#   #geom_ribbon(data = mob_cnt_test2, aes(ymin = w_lwr, ymax = w_uppr), alpha = 0.1) +
#   labs(x = "Google Mobility\n'workplaces' visits", col = "Status",
#        y = "Number of work contacts", linetype = "Pandemic Year") +
#   scale_colour_manual(values = c("No restrictions" = "#00BA38",
#                                  "Some restrictions" = "#619CFF",
#                                  "Lockdown" = "#F8766D",
#                                  "Pre-Pandemic" = "purple")) +
#   scale_linetype_manual(values = c("Year 1" = 2, "Year 2" = 3))
# plw2

#linear regression including interaction term for pandemic year
lm_h1 <- lm(nonhome ~ residential*p_year, data = mob_cnt_train)
summary(lm_h1)

#plot interaction
interact_plot(lm_h1, pred = residential, modx = p_year, modx.values = c(1, 2), plot.points = T)

#predict using 'new' data (one for each pandemic year)
mob_cnt_test1 <- rlang::duplicate(mob_cnt_test)
mob_cnt_test2 <- rlang::duplicate(mob_cnt_test)
mob_cnt_test1[, p_year := 1]
mob_cnt_test2[, p_year := 2]

#predict using both data sets
pred1 <- predict(lm_h1, newdata = mob_cnt_test1, interval = "confidence")
pred1 <- as.data.table(pred1)
mob_cnt_test1[, nonhome := pred1$fit]
mob_cnt_test1[, h_lwr := pred1$lwr]
mob_cnt_test1[, h_uppr := pred1$upr]
pred2 <- predict(lm_h1, newdata = mob_cnt_test2, interval = "confidence")
pred2 <- as.data.table(pred2)
mob_cnt_test2[, nonhome := pred2$fit]
mob_cnt_test2[, h_lwr := pred2$lwr]
mob_cnt_test2[, h_uppr := pred2$upr]

#plot
plh1 = ggplot(mob_cnt_train, aes(x = residential, y = nonhome,
          label = ifelse(status == "No restrictions", week, special))) +
  geom_point(aes(col = status)) + geom_text_repel(size = 2.5, max.overlaps = 100) +
  geom_line(data = mob_cnt_test1, aes(x = residential, y = nonhome, linetype = "Year 1")) +
  geom_ribbon(data = mob_cnt_test1, aes(ymin = h_lwr, ymax = h_uppr), alpha = 0.1) +
  geom_line(data = mob_cnt_test2, aes(x = residential, y = nonhome, linetype = "Year 2")) +
  geom_ribbon(data = mob_cnt_test2, aes(ymin = h_lwr, ymax = h_uppr), alpha = 0.1) +
  labs(x = "Google Mobility time at 'residential' location", colour = "Status",
       y = "Number of non-home contacts", linetype = "Pandemic Year") +
  scale_colour_manual(values = c("No restrictions" = "#00BA38",
                                 "Some restrictions" = "#619CFF",
                                 "Lockdown" = "#F8766D",
                                 "Pre-Pandemic" = "purple")) +
  scale_linetype_manual(values = c("Year 1" = 2, "Year 2" = 3))
plh1

#quadratic regression including interaction term for pandemic year
lm_h2 <- lm(nonhome ~ poly(residential, 2)*p_year, data = mob_cnt_train)
summary(lm_h2)
lrtest(lm_h2, lm_h1)

#plot interaction
interact_plot(lm_h2, pred = residential, modx = p_year, modx.values = c(1, 2), plot.points = T)

#predict using 'new' data (one for each pandemic year)
mob_cnt_test1 <- rlang::duplicate(mob_cnt_test)
mob_cnt_test2 <- rlang::duplicate(mob_cnt_test)
mob_cnt_test1[, p_year := 1]
mob_cnt_test2[, p_year := 2]

#predict using both data sets
pred1 <- predict(lm_h2, newdata = mob_cnt_test1, interval = "confidence")
pred1 <- as.data.table(pred1)
mob_cnt_test1[, nonhome := pred1$fit]
mob_cnt_test1[, h_lwr := pred1$lwr]
mob_cnt_test1[, h_uppr := pred1$upr]
pred2 <- predict(lm_h2, newdata = mob_cnt_test2, interval = "confidence")
pred2 <- as.data.table(pred2)
mob_cnt_test2[, nonhome := pred2$fit]
mob_cnt_test2[, h_lwr := pred2$lwr]
mob_cnt_test2[, h_uppr := pred2$upr]

#plot
plh2 = ggplot(mob_cnt_train, aes(x = residential, y = nonhome,
          label = ifelse(status == "No restrictions", week, special))) +
  geom_point(aes(col = status)) + geom_text_repel(size = 2.5, max.overlaps = 100) +
  geom_line(data = mob_cnt_test1, aes(x = residential, y = nonhome, linetype = "Year 1")) +
  geom_ribbon(data = mob_cnt_test1, aes(ymin = h_lwr, ymax = h_uppr), alpha = 0.1) +
  geom_line(data = mob_cnt_test2, aes(x = residential, y = nonhome, linetype = "Year 2")) +
  geom_ribbon(data = mob_cnt_test2, aes(ymin = h_lwr, ymax = h_uppr), alpha = 0.1) +
  labs(x = "Google Mobility time at 'residential' location", colour = "Status",
       y = "Number of non-home contacts", linetype = "Pandemic Year") +
  scale_colour_manual(values = c("No restrictions" = "#00BA38",
                                 "Some restrictions" = "#619CFF",
                                 "Lockdown" = "#F8766D",
                                 "Pre-Pandemic" = "purple")) +
  scale_linetype_manual(values = c("Year 1" = 2, "Year 2" = 3))
plh2

#linear regression including interaction term for pandemic year
lm_o1 <- lm(other ~ predictor*p_year, data = mob_cnt_train)
summary(lm_o1)

#plot interaction
interact_plot(lm_o1, pred = predictor, modx = p_year, modx.values = c(1, 2), plot.points = T)

#predict using 'new' data (one for each pandemic year)
mob_cnt_test1 <- rlang::duplicate(mob_cnt_test)
mob_cnt_test2 <- rlang::duplicate(mob_cnt_test)
mob_cnt_test1[, p_year := 1]
mob_cnt_test2[, p_year := 2]

#predict using both data sets
pred1 <- predict(lm_o1, newdata = mob_cnt_test1, interval = "confidence")
pred1 <- as.data.table(pred1)
mob_cnt_test1[, other := pred1$fit]
mob_cnt_test1[, o_lwr := pred1$lwr]
mob_cnt_test1[, o_uppr := pred1$upr]
pred2 <- predict(lm_o1, newdata = mob_cnt_test2, interval = "confidence")
pred2 <- as.data.table(pred2)
mob_cnt_test2[, other := pred2$fit]
mob_cnt_test2[, o_lwr := pred2$lwr]
mob_cnt_test2[, o_uppr := pred2$upr]

#plot
plo1 = ggplot(mob_cnt_train, aes(x = predictor, y = other,
          label = ifelse(status == "No restrictions", week, special))) +
  geom_point(aes(col = status)) + geom_text_repel(size = 2.5, max.overlaps = 100) +
  geom_line(data = mob_cnt_test1, aes(x = predictor, y = other, linetype = "Year 1")) +
  geom_ribbon(data = mob_cnt_test1, aes(ymin = o_lwr, ymax = o_uppr), alpha = 0.1) +
  geom_line(data = mob_cnt_test2, aes(x = predictor, y = other, linetype = "Year 2")) +
  geom_ribbon(data = mob_cnt_test2, aes(ymin = o_lwr, ymax = o_uppr), alpha = 0.1) +
  labs(x = "Google Mobility weighted 'transit stations',\n'retail and recreation', and 'grocery and pharmacy' visits",
       y = "Number of 'other' contacts",  col = "Status", linetype = "Pandemic Year") +
  scale_colour_manual(values = c("No restrictions" = "#00BA38",
                                 "Some restrictions" = "#619CFF",
                                 "Lockdown" = "#F8766D",
                                 "Pre-Pandemic" = "purple")) +
  scale_linetype_manual(values = c("Year 1" = 2, "Year 2" = 3))
plo1

#quadratic regression including interaction term for pandemic year
lm_o2 <- lm(other ~ poly(predictor, 2)*p_year, data = mob_cnt_train)
summary(lm_o2)
lrtest(lm_o2, lm_o1)

#plot interaction
interact_plot(lm_o2, pred = predictor, modx = p_year, modx.values = c(1, 2), plot.points = T)

#predict using 'new' data (one for each pandemic year)
mob_cnt_test1 <- rlang::duplicate(mob_cnt_test)
mob_cnt_test2 <- rlang::duplicate(mob_cnt_test)
mob_cnt_test1[, p_year := 1]
mob_cnt_test2[, p_year := 2]

#predict using both data sets
pred1 <- predict(lm_o2, newdata = mob_cnt_test1, interval = "confidence")
pred1 <- as.data.table(pred1)
mob_cnt_test1[, other := pred1$fit]
mob_cnt_test1[, o_lwr := pred1$lwr]
mob_cnt_test1[, o_uppr := pred1$upr]
pred2 <- predict(lm_o2, newdata = mob_cnt_test2, interval = "confidence")
pred2 <- as.data.table(pred2)
mob_cnt_test2[, other := pred2$fit]
mob_cnt_test2[, o_lwr := pred2$lwr]
mob_cnt_test2[, o_uppr := pred2$upr]

#plot
plo2 = ggplot(mob_cnt_train, aes(x = predictor, y = other,
          label = ifelse(status == "No restrictions", week, special))) +
  geom_point(aes(col = status)) + geom_text_repel(size = 2.5, max.overlaps = 100) +
  geom_line(data = mob_cnt_test1, aes(x = predictor, y = other, linetype = "Year 1")) +
  geom_ribbon(data = mob_cnt_test1, aes(ymin = o_lwr, ymax = o_uppr), alpha = 0.1) +
  geom_line(data = mob_cnt_test2, aes(x = predictor, y = other, linetype = "Year 2")) +
  geom_ribbon(data = mob_cnt_test2, aes(ymin = o_lwr, ymax = o_uppr), alpha = 0.1) +
  labs(x = "Google Mobility weighted 'transit stations',\n'retail and recreation', and 'grocery and pharmacy' visits",
       y = "Number of 'other' contacts",  col = "Status", linetype = "Pandemic Year") +
  scale_colour_manual(values = c("No restrictions" = "#00BA38",
                                 "Some restrictions" = "#619CFF",
                                 "Lockdown" = "#F8766D",
                                 "Pre-Pandemic" = "purple")) +
  scale_linetype_manual(values = c("Year 1" = 2, "Year 2" = 3))
plo2

#GAM
gam_o1 <- gam(other ~ s(predictor, by = p_year) + p_year, data = mob_cnt_train)
summary(gam_o1)

# #predict using 'new' data (one for each pandemic year)
# mob_cnt_test1 <- rlang::duplicate(mob_cnt_test)
# mob_cnt_test2 <- rlang::duplicate(mob_cnt_test)
# mob_cnt_test1[, p_year := 1]
# mob_cnt_test2[, p_year := 2]
# 
# #predict using both data sets
# pred1 <- predict(gam_o1, newdata = mob_cnt_test1, type = "response", se.fit = T)
# pred1 <- as.data.table(pred1)
# mob_cnt_test1[, other := pred1$fit]
# mob_cnt_test1[, o_lwr := pred1$fit - 2*pred1$se.fit]
# mob_cnt_test1[, o_uppr := pred1$fit + 2*pred1$se.fit]
# pred2 <- predict(gam_o1, newdata = mob_cnt_test2, type = "response", se.fit = T)
# pred2 <- as.data.table(pred2)
# mob_cnt_test2[, other := pred2$fit]
# mob_cnt_test2[, o_lwr := pred2$fit - 2*pred2$se.fit]
# mob_cnt_test2[, o_uppr := pred2$fit + 2*pred2$se.fit]
# 
# #plot
# plo3 = ggplot(mob_cnt_train, aes(x = predictor, y = other,
#           label = ifelse(status == "No restrictions", week, special))) +
#   geom_point(aes(col = status)) + geom_text_repel(size = 2.5, max.overlaps = 100) +
#   geom_line(data = mob_cnt_test1, aes(x = predictor, y = other, linetype = "Year 1")) +
#   #geom_ribbon(data = mob_cnt_test1, aes(ymin = w_lwr, ymax = w_uppr), alpha = 0.1) +
#   geom_line(data = mob_cnt_test2, aes(x = predictor, y = other, linetype = "Year 2")) +
#   #geom_ribbon(data = mob_cnt_test2, aes(ymin = w_lwr, ymax = w_uppr), alpha = 0.1) +
#   labs(x = "Google Mobility weighted 'transit stations',\n'retail and recreation', and 'grocery and pharmacy' visits",
#        y = "Number of 'other' contacts",  col = "Status", linetype = "Pandemic Year") +
#   scale_colour_manual(values = c("No restrictions" = "#00BA38",
#                                  "Some restrictions" = "#619CFF",
#                                  "Lockdown" = "#F8766D",
#                                  "Pre-Pandemic" = "purple")) +
#   scale_linetype_manual(values = c("Year 1" = 2, "Year 2" = 3))
# plo3
