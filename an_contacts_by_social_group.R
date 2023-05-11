##mean contacts by social group

#load libraries
library(data.table)
library(ggplot2)
library(tidyverse)
library(lubridate)
library(cowplot)
library(zoo)
library(gghighlight)
library(ggrepel)

#set cowplot theme
theme_set(cowplot::theme_cowplot(font_size = 10) + theme(strip.background = element_blank()))

#set data path
data_path <-"C:\\Users\\emiel\\Documents\\LSHTM\\Fellowship\\Project\\comix_mobility\\Data\\"

#import contact data
cnts <- qs::qread(file.path(data_path, "part_cnts.qs"))

#filter out participants of a certain age
cnts <- cnts[sample_type == "adult"]
cnts <- cnts %>%
  filter(!is.na(part_age_group))

#order by date
cnts_date <- cnts[order(date)]
cnts_date <- cnts[date <= ymd("2022-03-02")]

#create data table with subset of variables
num <- cnts_date[, .(date, part_id, panel, part_age, survey_round, weekday, 
                     part_social_group, home = n_cnt_home, work = n_cnt_work, 
                     other = n_cnt_other, all = n_cnt)]
num[, t := as.numeric(date - ymd("2020-01-01"))]

#create study column
num[, study := "CoMix"]

#create second database which shifts the survey rounds and dates
num2 <- rlang::duplicate(num)
num2[, date := date + 7]
num2[, survey_round := survey_round + 1]

#merge the two 
num_merge <- rbind(num, num2) 

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
num_merge <- merge(num, lockdowns, by = "date", all.y = F)

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
num_merge <- num_merge[order(date)]

#get means by week and social class
means <- num_merge[, .(study, work = mean(work),
                   other = mean(other), nonhome = mean(nonhome)),
                   by = .(month = paste(isoyear(date), "/", 
                          sprintf("%02d", month(date))),
                          social_class = part_social_group)]  
means <- unique(means)

#get labels for graphs
month <- names(table(means$month))
int <- seq(1, 26, 4)
my_list <- month[int]

ggplot(means, aes(month, work, by = social_class)) + 
  geom_line(aes(group = social_class, col = social_class)) +
  scale_x_discrete(breaks = my_list) #+ 
  # geom_rect(aes(xmin = month, xmax = lead(month), ymin = 0, 
  #               ymax = Inf, fill = status), alpha = 0.5) +
  # scale_fill_manual(values = c("No restrictions" = "#00BA38", 
  #                              "Some restrictions" = "#619CFF", 
  #                              "Lockdown" = "#F8766D"), 
  #                   labels = c("No restrictions", "Some restrictions", 
  #                              "Lockdown"))

#get means by week and social class
means2 <- num_merge[, .(work = mean(work), other = mean(other), 
                        nonhome = mean(nonhome)),
                    by = .(social_class = part_social_group)]  
means2 <- unique(means2)

ggplot(means2, aes(social_class, work, fill = social_class)) + 
  geom_bar(stat = "identity") 
       