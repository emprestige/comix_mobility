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
library(interactions)

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

# #import edited polymod data
# pnum <- qs::qread(file.path(data_path, "polymod.qs"))
# 
# #create study column
# pnum[, study := "POLYMOD"]
# 
# #add information for lockdown status (i.e. none)
# pnum[, status := "Pre-Pandemic"]
# 
# #add weightings to polymod data
# pnum[, weekday := lubridate::wday(date, label = T, abbr = F)]
# pnum[, day_weight := ifelse(weekday == "Saturday", 2/7, 
#                             ifelse(weekday == "Sunday", 2/7, 5/7))]
# pnum[, social_weight := 1]

# #add variable for pandemic year (0)
# pnum[, p_year := 0]

# #bind the rows together
# num <- rbind(cnts_l, pnum, fill = TRUE)
# 
# #remove participants of certain age from POLYMOD
# num <- rbind(
#   num[study == "CoMix"],
#   num[study == "POLYMOD" & part_age >= 18]
# )

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

#add column for special dates 
summer <- interval(ymd("2020-08-03"), ymd("2020-08-09"))
num_merge[, special := ifelse(date == ymd("2020-12-25"), "Xmas",
                       ifelse(date == ymd("2021-12-25"), "Xmas",
                       ifelse(date == ymd("2020-12-31"), "NYE",
                       ifelse(date == ymd("2021-12-31"), "NYE",
                       ifelse(date == ymd("2020-04-13"), "Easter",
                       ifelse(date == ymd("2021-04-05"), "Easter", 
                       ifelse(date %within% summer, "Summer Hol", NA)))))))]

# #import data for stringency index
# ox <- qs::qread(file.path(data_path, "stringency.qs"))
# ox$date <- as.Date(ox$date)
# pdate <- pnum[, .(date, stringency_index = 0)]
# ox <- rbind(ox, pdate)
# ox <- unique(ox)
# num_merge <- merge(num_merge, ox)

#get weighted means by week
weighted_means <- num_merge[, .(study, status, special, p_year, 
                                work = weighted.mean(work, day_weight*social_weight),
                                other = weighted.mean(other, day_weight*social_weight),
                                nonhome = weighted.mean(nonhome, day_weight*social_weight)),
                  by = .(week = paste(isoyear(date), "/", sprintf("%02d", isoweek(date))))]  
weighted_means <- unique(weighted_means)

# #get mean of polymod data so there is one baseline point
# poly <- weighted_means[study == "POLYMOD"]
# poly <- poly[, .(week = 0, study, status, special, p_year,
#                  work = mean(work, na.rm = T),
#                  other = mean(other, na.rm = T),
#                  nonhome = mean(nonhome, na.rm = T))]
# poly <- unique(poly)
# weighted_means <- weighted_means[study == "CoMix"]
# weighted_means <- rbind(weighted_means, poly)

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

# #add mobility to polymod dates
# pnum2 <- pnum[, .(study, date, retail_recreation = 1, grocery_pharmacy = 1, 
#                   parks = 1, transit_stations = 1, workplaces = 1,
#                   residential = 1)]
# gm2 <- gm2[, .(date, study, retail_recreation, grocery_pharmacy, parks, 
#                transit_stations, workplaces, residential)]
# gm2$date <- as.Date(gm2$date)
# gm2 <- rbind(gm2, pnum2)

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
mob_cnt <- merge(weighted_means, gm, by = c("week", "study"))

#scale data by POLYMOD data point 
mob_cnt <- mob_cnt[order(week)]
mob_cnt <- mob_cnt[, .(week, study, status, special, p_year, work, other, 
                       nonhome, workplaces, retail, grocery, parks, transit,
                       residential, predictor)]
mob_cnt1 <- mob_cnt[p_year == 1]
mob_cnt2 <- mob_cnt[p_year == 2]

#linear regression including interaction term for pandemic year 1
lm_w1_y1 <- lm(work ~ workplaces, data = mob_cnt1)
summary(lm_w1_y1)

#predict using both data sets
pred1 <- predict(lm_w1_y1, interval = "confidence")
pred1 <- as.data.table(pred1)
mob_cnt1[, pred := pred1$fit]
mob_cnt1[, w_lwr := pred1$lwr]
mob_cnt1[, w_uppr := pred1$upr]

#plot
plw1 = ggplot(mob_cnt1, aes(x = workplaces, y = work, label = special)) +
  geom_point(aes(col = status)) + geom_text_repel(size = 2.5) +
  geom_line(data = mob_cnt1, aes(x = workplaces, y = pred)) +
  geom_ribbon(data = mob_cnt1, aes(ymin = w_lwr, ymax = w_uppr), alpha = 0.1) +
  labs(x = "Google Mobility\n'workplaces' visits", col = "Status", 
       y = "Number of work contacts") +
  scale_colour_manual(values = c("No restrictions" = "#00BA38",
                                 "Some restrictions" = "#619CFF",
                                 "Lockdown" = "#F8766D")) 
plw1

#linear regression including interaction term for pandemic year 2
lm_w1_y2 <- lm(work ~ workplaces, data = mob_cnt2)
summary(lm_w1_y2)

#predict using both data sets
pred1 <- predict(lm_w1_y2, interval = "confidence")
pred1 <- as.data.table(pred1)
mob_cnt2[, pred := pred1$fit]
mob_cnt2[, w_lwr := pred1$lwr]
mob_cnt2[, w_uppr := pred1$upr]

#plot
plw2 = ggplot(mob_cnt2, aes(x = workplaces, y = work, label = special)) +
  geom_point(aes(col = status)) + geom_text_repel(size = 2.5) +
  geom_line(data = mob_cnt2, aes(x = workplaces, y = pred)) +
  geom_ribbon(data = mob_cnt2, aes(ymin = w_lwr, ymax = w_uppr), alpha = 0.1) +
  labs(x = "Google Mobility\n'workplaces' visits", col = "Status", 
       y = "Number of work contacts") +
  scale_colour_manual(values = c("No restrictions" = "#00BA38",
                                 "Some restrictions" = "#619CFF",
                                 "Lockdown" = "#F8766D")) 
plw2

#quadratic regression including interaction term for pandemic year
lm_w2_y1 <- lm(work ~ poly(workplaces, 2, raw = T), data = mob_cnt1)
summary(lm_w2_y1)

#predict using both data sets
pred1 <- predict(lm_w2_y1, newdata = mob_cnt1, interval = "confidence")
pred1 <- as.data.table(pred1)
mob_cnt1[, pred := pred1$fit]
mob_cnt1[, w_lwr := pred1$lwr]
mob_cnt1[, w_uppr := pred1$upr]

#plot
plw3 = ggplot(mob_cnt1, aes(x = workplaces, y = work, label = special)) +
  geom_point(aes(col = status)) + geom_text_repel(size = 2.5) +
  geom_line(data = mob_cnt1, aes(x = workplaces, y = pred)) +
  geom_ribbon(data = mob_cnt1, aes(ymin = w_lwr, ymax = w_uppr), alpha = 0.1) +
  labs(x = "Google Mobility\n'workplaces' visits", col = "Status", 
       y = "Number of work contacts") +
  scale_colour_manual(values = c("No restrictions" = "#00BA38",
                                 "Some restrictions" = "#619CFF",
                                 "Lockdown" = "#F8766D")) +
  scale_linetype_manual(values = c("Year 1" = 2, "Year 2" = 3))
plw3

#linear regression including interaction term for pandemic year as factor
lm_w2_y2 <- lm(work ~ poly(workplaces, 2, raw = T), data = mob_cnt2)
summary(lm_w2_y2)

#predict using both data sets
pred1 <- predict(lm_w2_y2, newdata = mob_cnt2, interval = "confidence")
pred1 <- as.data.table(pred1)
mob_cnt2[, pred := pred1$fit]
mob_cnt2[, w_lwr := pred1$lwr]
mob_cnt2[, w_uppr := pred1$upr]

#plot
plw4 = ggplot(mob_cnt2, aes(x = workplaces, y = work, label = special)) +
  geom_point(aes(col = status)) + geom_text_repel(size = 2.5) +
  geom_line(data = mob_cnt2, aes(x = workplaces, y = pred)) +
  geom_ribbon(data = mob_cnt2, aes(ymin = w_lwr, ymax = w_uppr), alpha = 0.1) +
  labs(x = "Google Mobility\n'workplaces' visits", col = "Status", 
       y = "Number of work contacts") +
  scale_colour_manual(values = c("No restrictions" = "#00BA38",
                                 "Some restrictions" = "#619CFF",
                                 "Lockdown" = "#F8766D")) 
plw4
