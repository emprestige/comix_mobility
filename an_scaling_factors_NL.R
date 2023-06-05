##linear and polynomial regression 

#load libraries
library(data.table)
library(tidyverse)
library(lubridate)

#set data path
data_path <-"C:\\Users\\emiel\\Documents\\LSHTM\\Fellowship\\Project\\comix_mobility\\Data\\"

#import contact data
cnts <- qs::qread(file.path(data_path, "part_cnts_NL.qs"))

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
                     all = n_cnt_home + n_cnt_work + n_cnt_other, day_weight)]
num[, t := as.numeric(date - ymd("2020-01-01"))]

#create study column
num[, study := "CoMix"]

#create sequence of dates
date <- seq(as.Date("2020-03-02"), as.Date("2022-03-02"), by = "days")
lockdowns <- as.data.table(as.Date(date))
lockdowns$lockdown_status <- 0
colnames(lockdowns) <- c("date", "status")

# #create time intervals for different types of restrictions
T1 <- interval(ymd("2020-03-02"), ymd("2020-03-17")) #pre-lockdown 
L1 <- interval(ymd("2020-03-18"), ymd("2020-05-03")) #first lockdown
T2 <- interval(ymd("2020-05-04"), ymd("2020-11-01")) #post first lockdown
L2 <- interval(ymd("2020-11-02"), ymd("2020-12-13")) #second lockdown
T3 <- interval(ymd("2020-12-14"), ymd("2021-03-23")) #post second lockdown
L3 <- interval(ymd("2021-03-24"), ymd("2021-05-04")) #third lockdown
T4 <- interval(ymd("2021-05-05"), ymd("2022-05-10")) #post third lockdown

#assign value to each type of restriction
lockdowns$status <- ifelse(ymd(lockdowns$date) %within% T1, 1,
                    ifelse(ymd(lockdowns$date) %within% L1, 2,
                    ifelse(ymd(lockdowns$date) %within% T2, 1,
                    ifelse(ymd(lockdowns$date) %within% L2, 2,
                    ifelse(ymd(lockdowns$date) %within% T3, 1,
                    ifelse(ymd(lockdowns$date) %within% L3, 2,
                    ifelse(ymd(lockdowns$date) %within% T4, 1, 0)))))))
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

#add column for special dates 
summer <- interval(ymd("2020-08-03"), ymd("2020-08-09"))
num_merge[, special := ifelse(date == ymd("2020-12-25"), "Xmas",
                       ifelse(date == ymd("2021-12-25"), "Xmas",
                       ifelse(date == ymd("2020-12-31"), "NYE",
                       ifelse(date == ymd("2021-12-31"), "NYE",
                       ifelse(date == ymd("2020-04-13"), "Easter",
                       ifelse(date == ymd("2021-04-05"), "Easter", 
                       ifelse(date %within% summer, "Summer Hol", NA)))))))]

#get weighted means by week
weighted_means <- num_merge[, .(study, status, special,
                                work = weighted.mean(work, day_weight),
                                other = weighted.mean(other, day_weight),
                                nonhome = weighted.mean(nonhome, day_weight)),
                  by = .(week = paste(isoyear(date), "/", sprintf("%02d", isoweek(date))))]  
weighted_means <- unique(weighted_means)

#import mobility data
mob <- qs::qread(file.path(data_path, "google_mob_NL.qs"))

#subset for same date range
mob_sub <- mob[date >= "2020-04-16" & date <= "2021-03-31"]

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
                               paste(year(date), "/", sprintf("%02d", isoweek(date))),
                               rep(0, length(date))), study)]

#create predictor for 'other' contacts
gm[, predictor := retail * 0.333 + transit * 0.334 + grocery * 0.333]

#merge
mob_cnt <- merge(weighted_means, gm, by = c("week", "study"))

#calculate scaling factor
ests <- rlang::duplicate(mob_cnt)
ests <- ests[study == "CoMix"]
ests <- ests[, .(week, status, special, work, work_mob = workplaces,
                 other, other_mob = predictor)]
ests[, work_mob2 := work_mob**2]
ests[, other_mob2 := other_mob**2]
ests[, work_scaling_fac_lin := (-0.4360 + 2.2548*work_mob)/1.9529120]
ests[, work_scaling_fac_quad := (1.2753 -4.5894*work_mob + 6.4437*work_mob2)/1.9529120]
ests[, other_scaling_fac_lin := (-0.2955 + 1.6733*other_mob)/3.4837340]
ests[, other_scaling_fac_quad := (2.8170 - 9.0254*other_mob + 8.8588*other_mob2)/3.4837340]

#save scaling factors 
qs::qsave(ests, file.path(data_path, "scaling_factors_NL.qs"))
write.csv(ests, file.path(data_path, "scaling_factors_NL.csv"))
