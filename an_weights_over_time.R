##summary information about weights over time

#load libraries
library(data.table)
library(tidyverse)
library(lubridate)
library(cowplot)

#set data path
data_path <-"C:\\Users\\emiel\\Documents\\LSHTM\\Fellowship\\Project\\comix_mobility\\Data\\"

#set cowplot theme
theme_set(cowplot::theme_cowplot(font_size = 10) + theme(strip.background = element_blank()))

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

#import data with full weighting
cnts <- qs::qread(file.path(data_path, "cnts_weight_test.qs"))
week <- names(table(cnts$week))
int <- seq(1, 100, 12)
my_list <- week[int]
cnts <- cnts %>%
  filter(!is.na(part_age_group))
cnts <- merge(cnts, lockdowns, by = "date")
weight_mean_weekly <- cnts %>% group_by(week) %>% summarise(mean = mean(weight_raw))
ggplot(data = weight_mean_weekly, aes(x = week, y = mean)) + geom_line(group = 1) +
  scale_x_discrete(breaks = my_list) + ylab("Mean Weight (full weighting)") + xlab("Week")
weight_sd_weekly <- cnts %>% group_by(week) %>% summarise(sd = sd(weight_raw))
ggplot(data = weight_sd_weekly, aes(x = week, y = sd)) + geom_line(group = 1) +
  scale_x_discrete(breaks = my_list) + ylab("SD of Weight (full weighting)") + 
  xlab("Week") #+ ylim(0, 500)

#import data with triple weighting 
cnts2 <- qs::qread(file.path(data_path, "cnts_weight_test2.qs"))
cnts2 <- cnts2 %>%
  filter(!is.na(part_age_group))
cnts2 <- merge(cnts2, lockdowns, by = "date")
weight_mean_weekly2 <- cnts2 %>% group_by(week) %>% summarise(mean = mean(weight_raw))
ggplot(data = weight_mean_weekly2, aes(x = week, y = mean)) + geom_line(group = 1) +
  scale_x_discrete(breaks = my_list) + ylab("Mean Weight (triple weighting)") + xlab("Week")
weight_sd_weekly2 <- cnts2 %>% group_by(week) %>% summarise(sd = sd(weight_raw))
ggplot(data = weight_sd_weekly2, aes(x = week, y = sd)) + geom_line(group = 1) +
  scale_x_discrete(breaks = my_list) + ylab("SD of Weight (triple weighting)") + 
  xlab("Week") #+ ylim(0, 500)

#import data with joint weighting 
cnts3 <- qs::qread(file.path(data_path, "cnts_weight_test3.qs"))
cnts3 <- cnts3 %>%
  filter(!is.na(part_age_group))
cnts3 <- merge(cnts3, lockdowns, by = "date")
weight_mean_weekly3 <- cnts3 %>% group_by(week) %>% summarise(mean = mean(weight_raw))
ggplot(data = weight_mean_weekly3, aes(x = week, y = mean)) + geom_line(group = 1) +
  scale_x_discrete(breaks = my_list) + ylab("Mean Weight (joint weighting)") + xlab("Week")
weight_sd_weekly3 <- cnts3 %>% group_by(week) %>% summarise(sd = sd(weight_raw))
ggplot(data = weight_sd_weekly3, aes(x = week, y = sd)) + geom_line(group = 1) +
  scale_x_discrete(breaks = my_list) + ylab("SD of Weight (joint weighting)") + 
  xlab("Week") #+ ylim(0, 500)

#import data with class and age wieghting
cnts4 <- qs::qread(file.path(data_path, "cnts_weight_class_age.qs"))
cnts4 <- cnts4 %>%
  filter(!is.na(part_age_group))
cnts4 <- merge(cnts4, lockdowns, by = "date")
weight_mean_weekly4 <- cnts4 %>% group_by(week) %>% summarise(mean = mean(weight_raw))
ggplot(data = weight_mean_weekly4, aes(x = week, y = mean)) + geom_line(group = 1) +
  scale_x_discrete(breaks = my_list) + ylab("Mean Weight (class and age weighting)") + xlab("Week")
weight_sd_weekly4 <- cnts4 %>% group_by(week) %>% summarise(sd = sd(weight_raw))
ggplot(data = weight_sd_weekly4, aes(x = week, y = sd)) + geom_line(group = 1) +
  scale_x_discrete(breaks = my_list) + ylab("SD of Weight (class and age weighting)") + 
  xlab("Week") #+ ylim(0, 500)

#import data with class weighting
cnts5 <- qs::qread(file.path(data_path, "cnts_weight_class.qs"))
cnts5 <- cnts5 %>%
  filter(!is.na(part_age_group))
cnts5 <- merge(cnts5, lockdowns, by = "date")
weight_mean_weekly5 <- cnts5 %>% group_by(week) %>% summarise(mean = mean(weight_raw))
ggplot(data = weight_mean_weekly5, aes(x = week, y = mean)) + geom_line(group = 1) +
  scale_x_discrete(breaks = my_list) + ylab("Mean Weight (class weighting)") + xlab("Week")
weight_sd_weekly5 <- cnts5 %>% group_by(week) %>% summarise(sd = sd(weight_raw))
ggplot(data = weight_sd_weekly5, aes(x = week, y = sd)) + geom_line(group = 1) +
  scale_x_discrete(breaks = my_list) + ylab("SD of Weight (class weighting)") + 
  xlab("Week") #+ ylim(0, 500)
