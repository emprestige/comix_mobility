##linear and polynomial regression 

#load libraries
library(data.table)
library(ggplot2)
library(tidyverse)
library(lubridate)
library(cowplot)
library(visreg)
library(lmtest)
library(ggpubr)

#set cowplot theme
theme_set(cowplot::theme_cowplot(font_size = 14) + theme(strip.background = element_blank(),
                                                         legend.box.margin = margin(l = 10, r = 10, b = 5),
                                                         plot.margin = margin(l = 10, r = 10, b = 5, t = 5)))

#set data path
data_path <-"C:\\Users\\emiel\\Documents\\LSHTM\\Fellowship\\Project\\comix_mobility\\Data\\"

#import contact data
cnts <- qs::qread(file.path(data_path, "cnts_weight_work_middate.qs"))

#filter out participants of a certain age
cnts <- cnts[sample_type == "adult"]
cnts <- cnts %>%
  filter(!is.na(part_age_group))

#order by date
cnts_date <- cnts[order(date)]
cnts_date <- cnts[date <= ymd("2021-03-31")]

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

#create second database which shifts the survey rounds and dates
num2 <- rlang::duplicate(cnts_l)
num2[, date := date + 7]
num2[, survey_round := survey_round + 1]

#merge the two 
num_merge <- rbind(cnts_l, num2) 

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

#get middate for fornight periods 
num_merge[, fortnight := paste(isoyear(date), "/", sprintf("%02d", ceiling(isoweek(date)/2)))]
num_merge[, start_date := min(date), by = .(fortnight)]
num_merge[, end_date := max(date), by = .(fortnight)]
num_merge[, mid_date := start_date + floor((end_date - start_date)/2) , by = .(fortnight)]

#get weighted means by fortnight
weighted_means <- num_merge[, .(study, status, special, 
                                work = weighted.mean(work, day_weight*social_weight),
                                other = weighted.mean(other, day_weight*social_weight),
                                nonhome = weighted.mean(nonhome, day_weight*social_weight)),
                  by = .(mid_date)]  
weighted_means <- unique(weighted_means)

#import mobility data
mob <- qs::qread(file.path(data_path, "google_mob.qs"))

#subset for same date range
mob_sub <- mob[date >= "2020-03-23" & date <= "2021-03-31"]

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

#get middate for fornight periods 
gm2[, fortnight := paste(isoyear(date), "/", sprintf("%02d", ceiling(isoweek(date)/2)))]
gm2[, start_date := min(date), by = .(fortnight)]
gm2[, end_date := max(date), by = .(fortnight)]
gm2[, mid_date := start_date + floor((end_date - start_date)/2) , by = .(fortnight)]

#get means for google mobility data
gm <- gm2[, .(workplaces = mean(workplaces),
              residential = mean(residential),
              retail = mean(retail_recreation), 
              grocery = mean(grocery_pharmacy), 
              transit = mean(transit_stations),
              parks = mean(parks)), by = .(mid_date)]
gm[, study := "CoMix"]

#create predictor for 'other' contacts
gm[, predictor := retail * 0.333 + transit * 0.334 + grocery * 0.333]

#merge
mob_cnt <- merge(weighted_means, gm, by = c("mid_date", "study"))

#scale data by POLYMOD data point 
mob_cnt <- mob_cnt[order(mid_date)]
mob_cnt <- mob_cnt[, .(mid_date, study, status, special, work, other, 
                       nonhome, workplaces, retail, grocery, parks, transit,
                       residential, predictor)]

#linear regression including interaction term for pandemic year 1
lm_w1 <- lm(work ~ workplaces, data = mob_cnt)
summary(lm_w1)
confint(lm_w1)
AIC(lm_w1)

#predict 
pred <- predict(lm_w1, interval = "confidence")
pred <- as.data.table(pred)
names(pred) <- c("w1_fit", "w1_lwr", "w1_uppr")
mob_cnt <- cbind(mob_cnt, pred)

#plot
plw1 = ggplot(mob_cnt, aes(x = workplaces, y = work, label = special)) +
  geom_line(data = mob_cnt, aes(x = workplaces, y = w1_fit), linewidth = 0.8) +
  geom_ribbon(data = mob_cnt, aes(ymin = w1_lwr, ymax = w1_uppr), alpha = 0.1) +
  labs(x = "Google Mobility 'workplaces' Visits", col = "Status",
       y = "Mean Work Contacts")
plw1

#quadratic regression including interaction term for pandemic year 1
lm_w2 <- lm(work ~ poly(workplaces, 2, raw = T), data = mob_cnt)
summary(lm_w2)
confint(lm_w2)
AIC(lm_w2)

#predict
pred <- predict(lm_w2, interval = "confidence")
pred <- as.data.table(pred)
names(pred) <- c("w2_fit", "w2_lwr", "w2_uppr")
mob_cnt <- cbind(mob_cnt, pred)

#plot
plw2 = ggplot(mob_cnt, aes(x = workplaces, y = work, label = special)) +
  geom_line(data = mob_cnt, aes(x = workplaces, y = w2_fit), linewidth = 0.8) +
  geom_ribbon(data = mob_cnt, aes(ymin = w2_lwr, ymax = w2_uppr), alpha = 0.1) +
  labs(x = "Google Mobility 'workplaces' Visits", col = "Status",
       y = "Mean Work Contacts")
plw2

mob_cnt[, mob1 := workplaces*1.9529120]
mob_cnt[, workplaces2 := workplaces*workplaces]
mob_cnt[, mob2 := workplaces2*1.9529120]

plw = ggplot(mob_cnt) +
  geom_line(aes(x = workplaces, y = w1_fit, col = "lin"), linewidth = 0.8) +
  geom_ribbon(aes(x = workplaces, ymin = w1_lwr, ymax = w1_uppr, fill = "lin"), alpha = 0.1) +
  geom_line(aes(x = workplaces, y = w2_fit, col = "quad"), linewidth = 0.8) +
  geom_ribbon(aes(x = workplaces, ymin = w2_lwr, ymax = w2_uppr, fill = "quad"), alpha = 0.1) +
  geom_line(aes(x = workplaces, y = mob1, col = "mob"), linewidth = 0.8) +
  geom_line(aes(x = workplaces, y = mob2, col = "mob2"), linewidth = 0.8) +
  geom_point(aes(x = workplaces, y = work), size = 2) +
  labs(x = "Google Mobility ''Workplaces'' Visits", col = " ",
       y = "Mean ''Work'' Contacts", fill = "Model Type") +
  scale_color_manual(breaks = c("mob", "mob2", "lin", "quad"),
                     values = c("#CC79A7", "#D55E00", "#0072B2", "#F0E442"),
                     labels = c("Mobility", "Mobility Squared", 
                                "Linear Model", "Quadratic Model")) +
  scale_fill_manual(breaks = c("lin", "quad"),
                    values = c("#0072B2", "#F0E442"),
                    labels = c("Linear Model", "Quadratic Model"))

plw

#import contact data
cnts <- qs::qread(file.path(data_path, "cnts_weight_other_middate.qs"))

#filter out participants of a certain age
cnts <- cnts[sample_type == "adult"]
cnts <- cnts %>%
  filter(!is.na(part_age_group))

#order by date
cnts_date <- cnts[order(date)]
cnts_date <- cnts[date <= ymd("2021-03-31")]

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

#create second database which shifts the survey rounds and dates
num2 <- rlang::duplicate(cnts_l)
num2[, date := date + 7]
num2[, survey_round := survey_round + 1]

#merge the two 
num_merge <- rbind(cnts_l, num2) 

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

#get middate for fornight periods 
num_merge[, fortnight := paste(isoyear(date), "/", sprintf("%02d", ceiling(isoweek(date)/2)))]
num_merge[, start_date := min(date), by = .(fortnight)]
num_merge[, end_date := max(date), by = .(fortnight)]
num_merge[, mid_date := start_date + floor((end_date - start_date)/2) , by = .(fortnight)]

#get weighted means by fortnight
weighted_means <- num_merge[, .(study, status, special, 
                                work = weighted.mean(work, day_weight*social_weight),
                                other = weighted.mean(other, day_weight*social_weight),
                                nonhome = weighted.mean(nonhome, day_weight*social_weight)),
                            by = .(mid_date)]  
weighted_means <- unique(weighted_means)

#import mobility data
mob <- qs::qread(file.path(data_path, "google_mob.qs"))

#subset for same date range
mob_sub <- mob[date >= "2020-03-23" & date <= "2021-03-31"]

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

#get middate for fornight periods 
gm2[, fortnight := paste(isoyear(date), "/", sprintf("%02d", ceiling(isoweek(date)/2)))]
gm2[, start_date := min(date), by = .(fortnight)]
gm2[, end_date := max(date), by = .(fortnight)]
gm2[, mid_date := start_date + floor((end_date - start_date)/2) , by = .(fortnight)]

#get means for google mobility data
gm <- gm2[, .(workplaces = mean(workplaces),
              residential = mean(residential),
              retail = mean(retail_recreation), 
              grocery = mean(grocery_pharmacy), 
              transit = mean(transit_stations),
              parks = mean(parks)), by = .(mid_date)]
gm[, study := "CoMix"]

#create predictor for 'other' contacts
gm[, predictor := retail * 0.333 + transit * 0.334 + grocery * 0.333]

#merge
mob_cnt <- merge(weighted_means, gm, by = c("mid_date", "study"))

#scale data by POLYMOD data point 
mob_cnt <- mob_cnt[order(mid_date)]
mob_cnt <- mob_cnt[, .(mid_date, study, status, special, work, other, 
                       nonhome, workplaces, retail, grocery, parks, transit,
                       residential, predictor)]

#linear regression including interaction term for pandemic year 1
lm_o1 <- lm(other ~ predictor, data = mob_cnt)
summary(lm_o1)
confint(lm_o1)
AIC(lm_o1)

#predict 
pred <- predict(lm_o1, interval = "confidence")
pred <- as.data.table(pred)
names(pred) <- c("o1_fit", "o1_lwr", "o1_uppr")
mob_cnt <- cbind(mob_cnt, pred)

#plot
plo1 = ggplot(mob_cnt, aes(x = predictor, y = other, label = special)) +
  geom_line(data = mob_cnt, aes(x = predictor, y = o1_fit), linewidth = 0.8) +
  geom_ribbon(data = mob_cnt, aes(ymin = o1_lwr, ymax = o1_uppr), alpha = 0.1) +
  labs(x = "Google Mobility 'other' Visits",
       y = "Mean Other Contacts", col = "Status") 
plo1

#quadratic regression including interaction term for pandemic year 1
lm_o2 <- lm(other ~ poly(predictor, 2, raw = T), data = mob_cnt)
summary(lm_o2)
confint(lm_o2)
AIC(lm_o2)

#predict 
pred <- predict(lm_o2, interval = "confidence")
pred <- as.data.table(pred)
names(pred) <- c("o2_fit", "o2_lwr", "o2_uppr")
mob_cnt <- cbind(mob_cnt, pred)

#plot
plo2 = ggplot(mob_cnt, aes(x = predictor, y = other, label = special)) +
  geom_line(data = mob_cnt, aes(x = predictor, y = o2_fit), linewidth = 0.8) +
  geom_ribbon(data = mob_cnt, aes(ymin = o2_lwr, ymax = o2_uppr), alpha = 0.1) +
  labs(x = "Google Mobility 'other' Visits",
       y = "Mean Other Contacts",  col = "Status") 
plo2

mob_cnt[, mob1 := predictor*3.4837340]
mob_cnt[, predictor2 := predictor*predictor]
mob_cnt[, mob2 := predictor2*3.4837340]

plo = ggplot(mob_cnt) +
  geom_line(aes(x = predictor, y = o1_fit, col = "lin"), linewidth = 0.8) +
  geom_ribbon(aes(x = predictor, ymin = o1_lwr, ymax = o1_uppr, fill = "lin"), alpha = 0.1) +
  geom_line(aes(x = predictor, y = o2_fit, col = "quad"), linewidth = 0.8) +
  geom_ribbon(aes(x = predictor, ymin = o2_lwr, ymax = o2_uppr, fill = "quad"), alpha = 0.1) +
  geom_line(aes(x = predictor, y = mob1, col = "mob"), linewidth = 0.8) +
  geom_line(aes(x = predictor, y = mob2, col = "mob2"), linewidth = 0.8) +
  geom_point(aes(x = predictor, y = other), size = 2) +
  labs(x = "Google Mobility ''Other'' Visits", col = " ", 
       y = "Mean ''Other'' Contacts", fill = "Model Type") +
  scale_color_manual(breaks = c("mob", "mob2", "lin", "quad"),
                     values = c("#CC79A7", "#D55E00", "#0072B2", "#F0E442"),
                     labels = c("Mobility", "Mobility Squared", 
                                "Linear Model", "Quadratic Model")) +
  scale_fill_manual(breaks = c("lin", "quad"),
                    values = c("#0072B2", "#F0E442"),
                    labels = c("Linear Model", "Quadratic Model"))

plo

#plot work and other together
ggarrange(plw, plo, labels = "AUTO", common.legend = T, legend = "bottom")
