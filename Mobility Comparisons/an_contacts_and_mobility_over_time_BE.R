##mobility over time 

#load libraries
library(data.table)
library(ggplot2)
library(tidyverse)
library(lubridate)
library(cowplot)
library(gghighlight)
library(ggrepel)

#set cowplot theme
theme_set(cowplot::theme_cowplot(font_size = 10) + theme(strip.background = element_blank()))

#set data path
data_path <-"C:\\Users\\emiel\\Documents\\LSHTM\\Fellowship\\Project\\comix_mobility\\Data\\"

#import contact data
cnts <- qs::qread(file.path(data_path, "part_cnts_BE.qs"))

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

cnts_date[, week := paste(isoyear(date), "/", isoweek(date))]
cnts_date_mid <- cnts_date %>% 
  group_by(survey_round) %>%
  summarise(mid_date = median(date))
cnts_date_mid <- as.data.table(cnts_date_mid)
cnts_date_mid[, week := paste(isoyear(mid_date), "/", isoweek(mid_date))]

#create data table with subset of variables
num <- cnts_date[, .(date, part_id, panel, part_age, survey_round, weekday, 
                     day_weight, home = n_cnt_home, work = n_cnt_work, 
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

num_merge_mid <- num_merge %>% 
  group_by(survey_round) %>%
  summarise(mid_date = median(date))
num_merge_mid <- as.data.table(num_merge_mid)
num_merge_mid[, week := paste(isoyear(mid_date), "/", isoweek(mid_date))]

#create sequence of dates
date <- seq(as.Date("2020-03-02"), as.Date("2021-03-04"), by = "days")
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
num_merge <- merge(num, lockdowns, by = "date", all.y = F)
num_merge <- merge(num_merge, num_merge_mid, by = "survey_round")

#calculate non home contacts
num_merge[, nonhome := all - home]

#add column for special dates 
num_merge[, special := ifelse(date == ymd("2020-12-25"), "Xmas",
                              ifelse(date == ymd("2021-12-25"), "Xmas",
                              ifelse(date == ymd("2020-12-31"), "NYE",
                              ifelse(date == ymd("2021-12-31"), "NYE",
                              ifelse(date == ymd("2021-01-01"), "NYD",
                              ifelse(date == ymd("2022-01-01"), "NYD",
                              ifelse(date == ymd("2020-04-13"), "Easter",
                              ifelse(date == ymd("2021-04-05"), "Easter", NA))))))))]
num_merge <- num_merge[order(date)]

#get weighted means by week
weighted_date <- num_merge[, .(study, special, survey_round, status,
                               work = weighted.mean(work, day_weight),
                               other = weighted.mean(other, day_weight),
                               nonhome = weighted.mean(nonhome, day_weight)),
                 by = .(week = paste(isoyear(date), "/", sprintf("%02d", isoweek(date))))]  
weighted_date <- unique(weighted_date)

#get weighted means by mid week for each survey round
weighted_mid_date <- num_merge[, .(study, special, survey_round, status,
                                   work = weighted.mean(work, day_weight),
                                   other = weighted.mean(other, day_weight),
                                   nonhome = weighted.mean(nonhome, day_weight)),
                     by = .(week = paste(isoyear(mid_date), "/", sprintf("%02d", isoweek(mid_date))))]  
weighted_mid_date <- unique(weighted_mid_date)

#import mobility data
mob <- qs::qread(file.path(data_path, "google_mob_BE.qs"))

#subset for same date range
mob_sub <- mob[date >= "2020-04-16" & date <= "2021-03-04"]

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

#create smoothed average
mob1 <- gm2
mob2 <- rlang::duplicate(mob1)
mob2[, date := date + 7]
gm2 <- rbind(mob1, mob2)
gm2$date <- as.Date(gm2$date)
gm2 <- gm2[order(date)]

#lockdown info and special dates
mob_merge <- merge(gm2, lockdowns, by = "date", all.y = F)
mob_merge[, special := ifelse(date == ymd("2020-12-25"), "Xmas",
                              ifelse(date == ymd("2021-12-25"), "Xmas",
                              ifelse(date == ymd("2020-12-31"), "NYE",
                              ifelse(date == ymd("2021-12-31"), "NYE",
                              ifelse(date == ymd("2021-01-01"), "NYD",
                              ifelse(date == ymd("2022-01-01"), "NYD",
                              ifelse(date == ymd("2020-04-13"), "Easter",
                              ifelse(date == ymd("2021-04-05"), "Easter", NA))))))))]
mob_merge <- mob_merge[order(date)]

#get averages (weekly to start?)
gm_av <- mob_merge[, .(special, status,
                       retail = mean(retail_recreation), 
                       grocery = mean(grocery_pharmacy),
                       parks = mean(parks), 
                       transit = mean(transit_stations),
                       workplaces = mean(workplaces),
                       residential = mean(residential)),
         by = .(week = paste(isoyear(date), "/", sprintf("%02d", isoweek(date))))]
gm_av <- unique(gm_av)

#create predictor for 'other' contacts
gm_av[, predictor := retail * 0.333 + transit * 0.334 + grocery * 0.333]

#remove dates which are missing from comix data
gm_av_sub <- gm_av#[-c(7,8), ]
gm_cnt_merge <- merge(weighted_date, gm_av_sub, all.y = T)
gm_cnt_merge_mid <- merge(weighted_mid_date, gm_av_sub, all.y = T)

#get labels for all plots
int <- seq(1, 50, 12)
my_list <- gm_av_sub$week[int]

#plot weighted predictor 
predictor <- ggplot(gm_av_sub, aes(week, predictor,
                label = ifelse(is.na(special) == F, special, week))) + 
  geom_line(group = 1) + geom_text_repel(size = 2.5, max.overlaps = 80) +
  geom_point(aes(x = week, y = ifelse(is.na(special) == F, predictor, NA))) +
  scale_x_discrete(breaks = my_list) +
  geom_rect(aes(xmin = week, xmax = lead(week), ymin = 0,
                ymax = Inf, fill = status), alpha = 0.5) +
  scale_fill_manual(values = c("Some restrictions" = "#619CFF",
                               "Lockdown" = "#F8766D"),
                    labels = c("Some restrictions", "Lockdown"))

#plot other
other <- ggplot(gm_cnt_merge, aes(week, other,
            label = ifelse(is.na(special) == F, special, week))) + 
  geom_line(group = 1) + geom_text_repel(size = 2.5, max.overlaps = 80) +
  geom_point(aes(x = week, y = ifelse(is.na(special) == F, other, NA))) + 
  scale_x_discrete(breaks = my_list) +
  geom_rect(aes(xmin = week, xmax = lead(week), ymin = 0,
                ymax = Inf, fill = status), alpha = 0.5) +
  scale_fill_manual(values = c("Some restrictions" = "#619CFF",
                               "Lockdown" = "#F8766D"),
                    labels = c("Some restrictions", "Lockdown"))

other_mid <- ggplot(gm_cnt_merge_mid, aes(week, other,
                label = ifelse(is.na(special) == F, special, week))) + 
  geom_point() + geom_text_repel(size = 2.5, max.overlaps = 80) +
  geom_point(aes(x = week, y = ifelse(is.na(special) == F, other, NA))) + 
  scale_x_discrete(breaks = my_list) +
  geom_rect(aes(xmin = week, xmax = lead(week), ymin = 0,
                ymax = Inf, fill = status), alpha = 0.5) +
  scale_fill_manual(values = c("Some restrictions" = "#619CFF",
                               "Lockdown" = "#F8766D"),
                    labels = c("Some restrictions", "Lockdown"))

#plot predictor and other
plot_grid(predictor, other, ncol = 1, align = 'v')
plot_grid(predictor, other_mid, ncol = 1, align = 'v')

#plot retail
retail <- ggplot(gm_av_sub, aes(week, retail,
            label = ifelse(is.na(special) == F, special, week))) + 
  geom_line(group = "status") + geom_text_repel(size = 2.5, max.overlaps = 80) +
  geom_point(aes(x = week, y = ifelse(is.na(special) == F, retail, NA))) +
  scale_x_discrete(breaks = my_list) +
  geom_rect(aes(xmin = week, xmax = lead(week), ymin = 0,
                       ymax = Inf, fill = status), alpha = 0.5) +
  scale_fill_manual(values = c("Some restrictions" = "#619CFF",
                               "Lockdown" = "#F8766D"),
                      labels = c("Some restrictions", "Lockdown"))

#plot retail and other
plot_grid(retail, other, ncol = 1, align = 'v')
plot_grid(retail, other_mid, ncol = 1, align = 'v')

#plot grocery
grocery <- ggplot(gm_av_sub, aes(week, grocery,
              label = ifelse(is.na(special) == F, special, week))) + 
  geom_line(group = 1) + geom_text_repel(size = 2.5, max.overlaps = 80) +
  geom_point(aes(x = week, y = ifelse(is.na(special) == F, grocery, NA))) +
  scale_x_discrete(breaks = my_list) +
  geom_rect(aes(xmin = week, xmax = lead(week), ymin = 0,
                ymax = Inf, fill = status), alpha = 0.5) +
  scale_fill_manual(values = c("Some restrictions" = "#619CFF",
                               "Lockdown" = "#F8766D"),
                    labels = c("Some restrictions", "Lockdown"))

#plot grocery and other
plot_grid(grocery, other, ncol = 1, align = 'v')
plot_grid(grocery, other_mid, ncol = 1, align = 'v')

#plot parks
parks <- ggplot(gm_av_sub, aes(week, parks,
            label = ifelse(is.na(special) == F, special, week))) + 
  geom_line(group = 1) + geom_text_repel(size = 2.5, max.overlaps = 80) +
  geom_point(aes(x = week, y = ifelse(is.na(special) == F, parks, NA))) +
  scale_x_discrete(breaks = my_list) +
  geom_rect(aes(xmin = week, xmax = lead(week), ymin = 0,
                ymax = Inf, fill = status), alpha = 0.5) +
  scale_fill_manual(values = c("Some restrictions" = "#619CFF",
                               "Lockdown" = "#F8766D"),
                    labels = c("Some restrictions", "Lockdown"))

#plot parks and other
plot_grid(parks, other, ncol = 1, align = 'v')
plot_grid(parks, other_mid, ncol = 1, align = 'v')

#plot transit
transit <- ggplot(gm_av_sub, aes(week, transit,
              label = ifelse(is.na(special) == F, special, week))) + 
  geom_line(group = 1) + geom_text_repel(size = 2.5, max.overlaps = 80) +
  geom_point(aes(x = week, y = ifelse(is.na(special) == F, transit, NA))) +
  scale_x_discrete(breaks = my_list) +
  geom_rect(aes(xmin = week, xmax = lead(week), ymin = 0,
                ymax = Inf, fill = status), alpha = 0.5) +
  scale_fill_manual(values = c("Some restrictions" = "#619CFF",
                               "Lockdown" = "#F8766D"),
                    labels = c("Some restrictions", "Lockdown"))

#plot transit and other
plot_grid(transit, other, ncol = 1, align = 'v')
plot_grid(transit, other_mid, ncol = 1, align = 'v')

#plot workplaces
workplaces <- ggplot(gm_av_sub, aes(week, workplaces,
                 label = ifelse(is.na(special) == F, special, week))) + 
  geom_line(group = 1) + geom_text_repel(size = 2.5, max.overlaps = 80) +
  geom_point(aes(x = week, y = ifelse(is.na(special) == F, workplaces, NA))) +
  scale_x_discrete(breaks = my_list) +
  geom_rect(aes(xmin = week, xmax = lead(week), ymin = 0,
                ymax = Inf, fill = status), alpha = 0.5) +
  scale_fill_manual(values = c("Some restrictions" = "#619CFF",
                               "Lockdown" = "#F8766D"),
                    labels = c("Some restrictions", "Lockdown"))

#plot work
work <- ggplot(gm_cnt_merge, aes(week, work,
           label = ifelse(is.na(special) == F, special, week))) + 
  geom_line(group = 1) + geom_text_repel(size = 2.5, max.overlaps = 80) +
  geom_point(aes(x = week, y = ifelse(is.na(special) == F, work, NA))) +
  scale_x_discrete(breaks = my_list) +
  geom_rect(aes(xmin = week, xmax = lead(week), ymin = 0,
                ymax = Inf, fill = status), alpha = 0.5) +
  scale_fill_manual(values = c("Some restrictions" = "#619CFF",
                               "Lockdown" = "#F8766D"),
                    labels = c("Some restrictions", "Lockdown"))

work_mid <- ggplot(gm_cnt_merge_mid, aes(week, work,
               label = ifelse(is.na(special) == F, special, week))) + 
  geom_point() + geom_text_repel(size = 2.5, max.overlaps = 80) +
  geom_point(aes(x = week, y = ifelse(is.na(special) == F, work, NA))) +
  scale_x_discrete(breaks = my_list) +
  geom_rect(aes(xmin = week, xmax = lead(week), ymin = 0,
                ymax = Inf, fill = status), alpha = 0.5) +
  scale_fill_manual(values = c("Some restrictions" = "#619CFF",
                               "Lockdown" = "#F8766D"),
                    labels = c("Some restrictions", "Lockdown"))

#plot workplaces and work
plot_grid(workplaces, work, ncol = 1, align = 'v')
plot_grid(workplaces, work_mid, ncol = 1, align = 'v')

#calculate inverse of residential
gm_av_sub[, residential := 1 - residential]

#plot residential
residential <- ggplot(gm_av_sub, aes(week, residential,
                  label = ifelse(is.na(special) == F, special, week))) + 
  geom_line(group = 1) + geom_text_repel(size = 2.5, max.overlaps = 80) +
  geom_point(aes(x = week, y = ifelse(is.na(special) == F, residential, NA))) +
  scale_x_discrete(breaks = my_list) + ylab("1 - residential") + 
  geom_rect(aes(xmin = week, xmax = lead(week), ymin = -0.25,
                ymax = Inf, fill = status), alpha = 0.5) +
  scale_fill_manual(values = c("Some restrictions" = "#619CFF",
                               "Lockdown" = "#F8766D"),
                    labels = c("Some restrictions", "Lockdown"))

#plot nonhome
nonhome <- ggplot(gm_cnt_merge, aes(week, nonhome,
              label = ifelse(is.na(special) == F, special, week))) + 
  geom_line(group = 1) + geom_text_repel(size = 2.5, max.overlaps = 80) +
  geom_point(aes(x = week, y = ifelse(is.na(special) == F, nonhome, NA))) +
  scale_x_discrete(breaks = my_list) + 
  geom_rect(aes(xmin = week, xmax = lead(week), ymin = 0,
                ymax = Inf, fill = status), alpha = 0.5) +
  scale_fill_manual(values = c("Some restrictions" = "#619CFF",
                               "Lockdown" = "#F8766D"),
                    labels = c("Some restrictions", "Lockdown"))

nonhome_mid <- ggplot(gm_cnt_merge_mid, aes(week, nonhome,
                  label = ifelse(is.na(special) == F, special, week))) + 
  geom_point() + geom_text_repel(size = 2.5, max.overlaps = 80) +
  geom_point(aes(x = week, y = ifelse(is.na(special) == F, nonhome, NA))) +
  scale_x_discrete(breaks = my_list) + 
  geom_rect(aes(xmin = week, xmax = lead(week), ymin = 0,
                ymax = Inf, fill = status), alpha = 0.5) +
  scale_fill_manual(values = c("Some restrictions" = "#619CFF",
                               "Lockdown" = "#F8766D"),
                    labels = c("Some restrictions",
                               "Lockdown"))

#plot residential and non-home
plot_grid(residential, nonhome, ncol = 1, align = 'v')
plot_grid(residential, nonhome_mid, ncol = 1, align = 'v')
