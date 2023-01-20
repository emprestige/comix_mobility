## investigating outliers

#load libraries
library(data.table)
library(ggplot2)
library(tidyverse)
library(mgcv)
library(lubridate)
library(cowplot)
library(zoo)

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

#create second database which shifts the survey rounds and dates
num2 <- rlang::duplicate(cnts_date)
num2[, date := date + 7]
num2[, survey_round := survey_round + 1]

#merge the two 
num_merge <- rbind(cnts_date, num2) 

#merge contact data and lockdown information
cnts_l <- merge(num_merge, lockdowns, by = "date", all.y = F)

#get dates in week
week <- unique(as.data.table(as.Date(cnts_l$date)))
colnames(week) <- "date"
week <- week[, week := isoweek(date)]

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
gm2 <- gm2[, .(date, study, retail_recreation, grocery_pharmacy, parks, 
               transit_stations, workplaces, residential)]

#get means for google mobility data
gm <- gm2[, .(workplaces = mean(workplaces),
              residential = mean(residential),
              retail = mean(retail_recreation), 
              grocery = mean(grocery_pharmacy), 
              transit = mean(transit_stations),
              parks = mean(parks)),
          by = .(week = paste(year(date), "/", isoweek(date)))]

#get means for different types of contacts
cnt <- cnts_l[, .(status, shop = weighted.mean(n_cnt_shop, day_weight), 
                  healthcare = weighted.mean(n_cnt_health_facility, day_weight),
                  public_transport = weighted.mean(n_cnt_public_transport, day_weight),
                  supermarket = weighted.mean(n_cnt_supermarket, day_weight), 
                  bar_rest = weighted.mean(n_cnt_bar_rest, day_weight),
                  outside = weighted.mean(n_cnt_outside, day_weight),
                  n = length(n_cnt), date_length = length(unique(date))),
              by = .(week = paste(year(date), "/", isoweek(date)))]

#merge
mob_cnt <- merge(cnt, gm, by = c("week"))
mob_cnt <- mob_cnt %>% distinct(across(-status))

weeks <- as.data.table(cbind(mob_cnt$week, mob_cnt$n, mob_cnt$date_length))
names(weeks) <- c("week", "n_part", "n_dates")

#save
write.csv(weeks, "number_used.csv")

#get dates in week
week <- unique(as.data.table(as.Date(cnts_l$date)))
colnames(week) <- "date"
week <- week[, week := isoweek(date)]

#look at first lockdown - most outliers are from here
first_lock <- cnts_date[date >= "2020-03-23" & date <= "2020-05-05"]

#employment status during first lockdown
employment_first <- first_lock %>% 
  group_by(part_employstatus) %>%
  tally()
employment_first <- as.data.table(employment_first)
employment_first[, percentage := (n/sum(n))*100]

#employment status throughout
employment_all <- cnts_date %>% 
  group_by(part_employstatus) %>% 
  tally()
employment_all <- as.data.table(employment_all)
employment_all[, percentage := (n/sum(n))*100]

#merge
employment <- merge(employment_first, employment_all, 
                    by = "part_employstatus", all = T)
names(employment) <- c("part_employstatus", "n_lock1", "percent_lock1",
                       "n_all", "percent_all")

#occupation during first lockdown
occupation_first <- first_lock %>%
  group_by(part_occupation) %>%
  tally()
occupation_first <- as.data.table(occupation_first)
occupation_first[, percentage := (n/sum(n))*100]

#occupation throughout
occupation_all <- cnts_date %>%
  group_by(part_occupation) %>%
  tally()
occupation_all <- as.data.table(occupation_all)
occupation_all[, percentage := (n/sum(n))*100]

#merge
occupation <- merge(occupation_first, occupation_all, by = "part_occupation", all = T)
names(occupation) <- c("part_occupation", "n_lock1", "percent_lock1", 
                       "n_all", "percent_all")
occupation[is.na(occupation)] <- 0

#ages during first lockdown
age_first <- first_lock %>%
  group_by(part_age) %>%
  tally()
age_first <- as.data.table(age_first)
age_first[, percentage := (n/sum(n))*100]

#ages throughout
age_all <- cnts_date %>%
  group_by(part_age) %>%
  tally()
age_all <- as.data.table(age_all)
age_all[, percentage := (n/sum(n))*100]

#merge
age <- merge(age_first, age_all, by = "part_age", all = T)
age[is.na(age)] <- 0
names(age) <- c("part_age", "n_lock1", "percent_lock1", "n_all", "percent_all")

#combine occupation, employment, and age?
characteristics <- rbind(employment, occupation, age, use.names = F)
names(characteristics) <- c("characteristic", "n_lock1", "percent_lock1", 
                            "n_all", "percent_all")

#save
write.csv(characteristics, "part_characteristics.csv")
