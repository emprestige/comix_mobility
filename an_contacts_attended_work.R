## compare workplace google mobility to work contacts of those who attended work

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

#order by date
cnts_date <- cnts[order(date)]

#filter out participants of a certain age
cnts[part_age >= 18 & part_age <= 65]

#create data table with subset of variables
num <- cnts_date[, .(date, study, part_id, part_age, part_employstatus,
                     part_attend_work_yesterday, survey_round, weekday, 
                     day_weight, home = n_cnt_home, work = n_cnt_work,
                     school = n_cnt_school, other = n_cnt_other, 
                     bar_rest = n_cnt_bar_rest, shop = n_cnt_shop)]
num[, t := as.numeric(date - ymd("2020-01-01"))]

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
num2 <- rlang::duplicate(num)
num2[, date := date + 7]
num2[, survey_round := survey_round + 1]

#merge the two 
num_merge <- rbind(num, num2) 

#merge contact data and lockdown information
cnts_l <- merge(num_merge, lockdowns, by = "date", all.y = F)

#create variable for those employed
cnts_l[part_employstatus == "employed full-time (34 hours or more)",
       part_employed := "Full time"]
cnts_l[part_employstatus == "employed part-time (less than 34 hours)", 
       part_employed := "Part time"]
cnts_l[part_employstatus == "self employed", part_employed := "Self employed"]

#get proportion of those who went to work 
num_merge_full <- cnts_l[part_attend_work_yesterday == "yes" & 
                           part_employed == "Full time"]
num_merge_part <- cnts_l[part_attend_work_yesterday == "yes" & 
                           part_employed == "Part time"]
num_merge_self <- cnts_l[part_attend_work_yesterday == "yes" &
                           part_employed == "Self employed"]

#employed people who attended work
num_merge_work <- rbind(num_merge_full, num_merge_part, num_merge_self)

num_merge_worker_full <- cnts_l[part_employed == "Full time"]
num_merge_worker_part <- cnts_l[part_employed == "Part time"]
num_merge_worker_self <- cnts_l[part_employed == "Self employed"]

#employed people
num_merge_worker <- rbind(num_merge_worker_full, num_merge_worker_part, 
                          num_merge_worker_self)

num_merge_work <- num_merge_work[order(date)]
num_merge_worker <- num_merge_worker[order(date)]
attended <- num_merge_work %>%
  group_by(date) %>%
  tally()
all <- num_merge_worker %>% 
  group_by(date) %>%
  tally()
worked <- merge(all, attended, by = "date", all = T)
worked[is.na(worked)] <- 0
colnames(worked) <- c("date", "all", "attended")
worked <- as.data.table(worked)
worked[, proportion := attended/all]
worked <- merge(worked, lockdowns, by = "date", all.y = F)

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

#subset to relevant variables
gm2 <- gm2[, .(date, study, retail_recreation, grocery_pharmacy, parks, 
               transit_stations, workplaces, residential)]

#get means for google mobility data
gm <- gm2[, .(workplaces = mean(workplaces)),
          by = .(week = paste(year(date), "/", week(date)))]

#get means for proportions
cnt <- worked[, .(status, all = mean(all), attended = mean(attended), 
                  proportion = mean(proportion)),
              by = .(week = paste(year(date), "/", week(date)))]

#merge
mob_cnt <- merge(cnt, gm, by = c("week"))
mob_cnt <- unique(mob_cnt)
mob_cnt <- mob_cnt[proportion != 0]

#plot 
plw <- ggplot(mob_cnt) + 
  geom_point(aes(x = workplaces, y = proportion, colour = status, size = all)) + 
  labs(x = "Google Mobility\n'workplaces' visits", size = "Total Employed",
       y = "Proportion of people who went to work", colour = "Status")  

plw + annotate("text", label = "Xmas", 
               x = c(0.3233333, 0.36, 0.3857143, 0.41),
               y = c(0.1963592, 0.2646351, 0.2045587, 0.1822893)) +
  annotate("text", x = 0.6257143, y = 0.0373221, label = "Summer Hol") +
  annotate("text", label = "NYE",
           x = c(0.5016667, 0.5816667), y = c(0.1513413, 0.1548910)) +
  annotate("text", x = 0.5242857, y = 0.2149076, label = "Easter")
