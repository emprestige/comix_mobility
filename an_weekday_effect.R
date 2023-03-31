##summary information about weekday effect

#load libraries
library(data.table)
library(tidyverse)
library(lubridate)
library(cowplot)

#set data path
data_path <-"C:\\Users\\emiel\\Documents\\LSHTM\\Fellowship\\Project\\comix_mobility\\Data\\"

#set cowplot theme
theme_set(cowplot::theme_cowplot(font_size = 10) + theme(strip.background = element_blank()))

#import participant and contact data
cnts <- qs::qread(file.path(data_path, "part_cnts.qs"))
cnts <- cnts[order(date)]

#filter data for NA age group, order it by date
cnts <- cnts %>%
  filter(!is.na(part_age_group))

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

cnts[area_2_name == "Greater", area_2_name := "Greater London"]
cnts[, week := paste(isoyear(date), "/", sprintf("%02d", isoweek(date)))]
# cnts[, weekday_cnt := ifelse(weekday == "Monday", "Sunday", 
#                       ifelse(weekday == "Tuesday", "Monday",
#                       ifelse(weekday == "Wednesday", "Tuesday",
#                       ifelse(weekday == "Thursday", "Wednesday",
#                       ifelse(weekday == "Friday", "Thursday",
#                       ifelse(weekday == "Saturday", "Friday",
#                       ifelse(weekday == "Sunday", "Saturday", NA)))))))]

week <- names(table(cnts$week))
int <- seq(1, 100, 12)
my_list <- week[int]

#day of the week
cnts %>% group_by(weekday) %>%
  summarise(n = n()) %>%
  mutate(freq = (n/sum(n)*100))
# cnts %>% group_by(weekday_cnt) %>%
#   summarise(n = n()) %>%
#   mutate(freq = (n/sum(n)*100))

days <- cnts[, .(n = .N), by = .(weekday, week)][, freq := prop.table(n), by = week]
days$weekday <- factor(days$weekday, weekdays(min(cnts$date) + 0:6))
ggplot(data = days, aes(x = week, y = freq, fill = weekday)) + geom_col() + 
  scale_x_discrete(breaks = my_list)
ggplot(data = days, aes(x = weekday, y = week, fill = freq)) + geom_tile()

days2 <- cnts[, .(n = .N), by = .(weekday, survey_round)][, freq := prop.table(n), by = survey_round]
days2$weekday <- factor(days2$weekday, weekdays(min(cnts$date) + 0:6))
ggplot(data = days2, aes(x = survey_round, y = freq, fill = weekday)) + geom_col() + 
  scale_x_discrete(breaks = my_list)
ggplot(data = days2, aes(x = weekday, y = survey_round, fill = n)) + geom_tile()

cnts2 <- rlang::duplicate(cnts)
days3 <- cnts2[, .(n = .N, work = mean(n_cnt_work), other = mean(n_cnt_other),
                   home = mean(n_cnt_other)), by = .(weekday, 
                                                     month = paste(isoyear(date), "/", month(date)))][, 
                                                                                                      freq := prop.table(n), by = month]
days3$weekday <- factor(days3$weekday, weekdays(min(cnts$date) + 0:6))
month <- names(table(days3$month))
int2 <- seq(1, 26, 6)
my_list2 <- month[int2]
ggplot(data = days3, aes(x = month, y = work, fill = weekday)) + geom_col() + 
  scale_x_discrete(breaks = my_list2)
ggplot(data = days3, aes(x = weekday, y = month, fill = work)) + geom_tile() + 
  scale_y_discrete(breaks = my_list2)
ggplot(data = days3, aes(x = weekday, y = work, fill = weekday)) + geom_col() + 
  facet_grid(cols = vars(month))

cnts3 <- rlang::duplicate(cnts)
cnts3 <- cnts3 %>%
  filter(!part_gender_nb == "other") 
cnts3 <- cnts3 %>%
  filter(!is.na(part_gender_nb)) 
days4 <- cnts3[, .(n = .N, work = mean(n_cnt_work), other = mean(n_cnt_other),
                   home = mean(n_cnt_other)), by = .(weekday, part_gender_nb,
                                                     month = paste(isoyear(date), "/", month(date)))][, 
                                                                                                      freq := prop.table(n), by = month]
days4$weekday <- factor(days4$weekday, weekdays(min(cnts$date) + 0:6))
ggplot(data = days4, aes(x = month, y = work, fill = weekday)) + geom_col() + 
  scale_x_discrete(breaks = my_list2) + facet_grid(rows = vars(part_gender_nb))

cnts4 <- rlang::duplicate(cnts) 
cnts4 <- cnts4 %>%
  filter(!is.na(part_age_group))
days5 <- cnts4[, .(n = .N, work = mean(n_cnt_work), other = mean(n_cnt_other),
                   home = mean(n_cnt_other)), by = .(weekday, part_age_group,
                                                     quarter = paste(isoyear(date), "/", quarter(date)))][, 
                                                                                                          freq := prop.table(n), by = quarter]
days5$weekday <- factor(days5$weekday, weekdays(min(cnts$date) + 0:6))
ggplot(data = days5, aes(x = quarter, y = work, fill = weekday)) + geom_col() + 
  facet_grid(cols = vars(part_age_group)) 
ggplot(data = days5, aes(x = quarter, y = weekday, fill = work)) + geom_tile() + 
  facet_grid(cols = vars(part_age_group)) 

#create sequence of dates
date <- seq(as.Date("2020-03-02"), as.Date("2022-03-02"), by = "days")
lockdowns <- as.data.table(as.Date(date))
lockdowns$lockdown_status <- 0
colnames(lockdowns) <- c("date", "status")

#create time intervals for different types of restrictions
T1 <- interval(ymd("2020-03-02"), ymd("2020-03-22")) #prior to first lockdown
L1 <- interval(ymd("2020-03-23"), ymd("2020-05-31")) #first lockdown
T2 <- interval(ymd("2020-06-01"), ymd("2020-07-04")) #post first lockdown
F1 <- interval(ymd("2020-07-05"), ymd("2020-09-13")) #no restrictions
T3 <- interval(ymd("2020-09-14"), ymd("2020-11-04")) #pre second lockdown
L2 <- interval(ymd("2020-11-05"), ymd("2020-12-01")) #second lockdown 
T4 <- interval(ymd("2020-12-02"), ymd("2021-01-05")) #post second lockdown
L3 <- interval(ymd("2021-01-06"), ymd("2021-03-07")) #third lockdown
T5 <- interval(ymd("2021-03-08"), ymd("2021-07-18")) #post third lockdown
F2 <- interval(ymd("2021-07-19"), ymd("2021-12-07")) #no restrictions
T6 <- interval(ymd("2021-12-08"), ymd("2022-02-21")) #some restrictions 
F3 <- interval(ymd("2022-02-22"), ymd("2022-03-04")) #no restrictions

#assign value to each type of restriction
lockdowns$status <- ifelse(ymd(lockdowns$date) %within% T1, 1, 
                    ifelse(ymd(lockdowns$date) %within% L1, 2, 
                    ifelse(ymd(lockdowns$date) %within% T2, 3,
                    ifelse(ymd(lockdowns$date) %within% F1, 4,
                    ifelse(ymd(lockdowns$date) %within% T3, 5, 
                    ifelse(ymd(lockdowns$date) %within% L2, 6, 
                    ifelse(ymd(lockdowns$date) %within% T4, 7, 
                    ifelse(ymd(lockdowns$date) %within% L3, 8, 
                    ifelse(ymd(lockdowns$date) %within% T5, 9,
                    ifelse(ymd(lockdowns$date) %within% F2, 10,
                    ifelse(ymd(lockdowns$date) %within% T6, 11, 12)))))))))))

#create factor
lockdown_fac <- factor(lockdowns$status, levels = c(1, 2, 3, 4, 5, 6, 7, 
                                                    8, 9, 10, 11, 12),
                       labels = c("T1", "L1", "T2", "F1", "T3", "L2", "T4",
                                  "L3", "T5", "F2", "T6", "F3"))
lockdowns$status <- lockdown_fac

cnts5 <- rlang::duplicate(cnts) 
cnts5 <- cnts5 %>%
  filter(!is.na(part_age_group))
cnts5 <- merge(cnts5, lockdowns)
days6 <- cnts5[, .(n = .N, work = mean(n_cnt_work), other = mean(n_cnt_other),
                   home = mean(n_cnt_other)), by = .(weekday, status)][, 
                                                                       freq := prop.table(n), by = status]
days6$weekday <- factor(days6$weekday, weekdays(min(cnts$date) + 0:6))
ggplot(data = days6, aes(x = weekday, y = work)) + geom_col(fill = "pink") + 
  facet_grid(rows = vars(status))

days7 <- cnts5[, .(n = .N, work = mean(n_cnt_work), other = mean(n_cnt_other),
                   home = mean(n_cnt_other)), by = .(weekday, part_age_group, status)][, 
                   freq := prop.table(n), by = status]
days7$weekday <- factor(days7$weekday, weekdays(min(cnts$date) + 0:6))
ggplot(data = days7, aes(x = status, y = work, fill = weekday)) + geom_col() + 
  facet_grid(cols = vars(part_age_group)) 
ggplot(data = days7, aes(x = status, y = weekday, fill = work)) + geom_tile() + 
  facet_grid(cols = vars(part_age_group))

labs <- c("Mon", "Tue", "Wed", "Thur", "Fri", "Sat", "Sun")
ggplot(data = days7, aes(x = weekday, y = work)) + geom_col(fill = "pink") + 
  scale_x_discrete(labels = labs) + facet_grid(rows = vars(status), cols = vars(part_age_group))  

#import data with weighting
cnts6 <- qs::qread(file.path(data_path, "cnts_weight_test.qs"))
cnts6 <- cnts6 %>%
  filter(!is.na(part_age_group))
cnts6 <- merge(cnts6, lockdowns, by = "date")
weight_mean_weekly <- cnts6 %>% group_by(week) %>% summarise(mean = mean(weight_raw))
ggplot(data = weight_mean_weekly, aes(x = week, y = mean)) + geom_line(group = 1) +
  scale_x_discrete(breaks = my_list) + ylab("Mean Weight") + xlab("Week")
