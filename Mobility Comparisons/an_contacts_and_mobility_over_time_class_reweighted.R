#load libraries
library(data.table)
library(ggplot2)
library(tidyverse)
library(lubridate)
library(cowplot)
library(zoo)
library(survey)
library(ggrepel)

#set cowplot theme
theme_set(cowplot::theme_cowplot(font_size = 10) + theme(strip.background = element_blank()))

#set data path
data_path <-"C:\\Users\\emiel\\Documents\\LSHTM\\Fellowship\\Project\\comix_mobility\\Data\\"

#import contact data
cnts <- qs::qread(file.path(data_path, "part_cnts.qs"))
cnts[, week := paste(isoyear(date), "/", sprintf("%02d", isoweek(date)))]

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

week <- names(table(cnts$week))
int <- seq(1, 100, 12)
my_list <- week[int]

cnts2 <- cnts
cnts2[, pop_proportion := ifelse(part_social_group == "A - Upper middle class",
                          0.04, ifelse(part_social_group == "B - Middle class", 0.23, 
                          ifelse(part_social_group == "C1 - Lower middle class", 0.29,
                          ifelse(part_social_group == "C2 - Skilled working class", 0.21, 
                          ifelse(part_social_group == "D - Working class", 0.15, 0.08)))))]
cnts2[, pop_estimate := pop_proportion*67866]
#cnts2[, weekend := ifelse(weekday == "Saturday", T, ifelse(weekday == "Sunday", T, F))]

weightlookup <- cnts2[, .(sample = .N), by = .(part_social_group, week)] #weekend, week)]
weightlookup[, sample_total := sum(sample), by = .(week)]#, weekend)]
weightlookup[, sample_proportion := sample / sample_total]

pop <- cnts2[, .(week, part_social_group, pop_estimate, pop_proportion)]#, weekend)]
pop2 <- unique(pop)
weightlookup2 <- merge(weightlookup, pop2, by = c("part_social_group", "week"))#, "weekend"))
weightlookup2[, weight_raw := pop_estimate/sample]
weightlookup2[, weight_proportion := pop_proportion/sample_proportion]

#merge weights to cnts2
cnts3 <- merge(cnts2, weightlookup2, by = c("part_social_group", "week"))#, "weekend"))

#save 
qs::qsave(cnts3, file.path(data_path, "cnts_weight_class.qs"))

#order by date
cnts_date <- cnts3[order(date)]
cnts_date <- cnts_date[date <= ymd("2022-03-02")]

#create data table with subset of variables
num <- cnts_date[, .(date, part_id, panel, part_age, survey_round, weekday, 
                     home = n_cnt_home, work = n_cnt_work, other = n_cnt_other, 
                     all = n_cnt, social_weight = weight_raw)]
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
num_merge <- num_merge[order(date)]

#get weighted means by week
weighted_date <- num_merge[, .(study, status, special,
                               work = weighted.mean(work, social_weight),
                               other = weighted.mean(other, social_weight),
                               nonhome = weighted.mean(nonhome, social_weight)),
                by = .(week = paste(isoyear(date), "/", sprintf("%02d", isoweek(date))))]  
weighted_date <- unique(weighted_date)

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
                              ifelse(date == ymd("2021-04-05"), "Easter", 
                              ifelse(date %within% summer, "Summer Hol", NA)))))))))]
mob_merge <- mob_merge[order(date)]

#get averages (weekly to start?)
gm_av <- mob_merge[, .(status, special,
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
gm_av_sub <- gm_av[-c(7,8), ]

#get labels for all plots
int <- seq(1, 114, 12)
my_list <- gm_av_sub$week[int]

#plot weighted predictor 
predictor <- ggplot(gm_av_sub, aes(week, predictor,
                label = ifelse(status == "No restrictions", 
                        ifelse(is.na(special) == F, special, week), special))) + 
  geom_line(group = "status") + geom_text_repel(size = 2.5, max.overlaps = 80) +
  geom_point(aes(x = week, y = ifelse(is.na(special) == F, predictor, NA))) +
  scale_x_discrete(breaks = my_list) +
  geom_rect(aes(xmin = week, xmax = lead(week), ymin = 0, 
                ymax = Inf, fill = status), alpha = 0.5) +
  scale_fill_manual(values = c("No restrictions" = "#00BA38", 
                               "Some restrictions" = "#619CFF", 
                               "Lockdown" = "#F8766D"), 
                    labels = c("No restrictions", "Some restrictions", 
                               "Lockdown"))

#plot other
other <- ggplot(weighted_date, aes(week, other,
            label = ifelse(status == "No restrictions", 
                    ifelse(is.na(special) == F, special, week), special))) + 
  geom_line(group = 1) + geom_text_repel(size = 2.5, max.overlaps = 80) +
  geom_point(aes(x = week, y = ifelse(is.na(special) == F, other, NA))) + 
  scale_x_discrete(breaks = my_list) +
  geom_rect(aes(xmin = week, xmax = lead(week), ymin = 0, 
                ymax = Inf, fill = status), alpha = 0.5) +
  scale_fill_manual(values = c("No restrictions" = "#00BA38", 
                               "Some restrictions" = "#619CFF", 
                               "Lockdown" = "#F8766D"), 
                    labels = c("No restrictions", "Some restrictions", 
                               "Lockdown"))

#plot predictor and other
plot_grid(predictor, other, ncol = 1, align = 'v')

#plot retail
retail <- ggplot(gm_av_sub, aes(week, retail,
            label = ifelse(status == "No restrictions", 
                    ifelse(is.na(special) == F, special, week), special))) + 
  geom_line(group = "status") + geom_text_repel(size = 2.5, max.overlaps = 80) +
  geom_point(aes(x = week, y = ifelse(is.na(special) == F, retail, NA))) +
  scale_x_discrete(breaks = my_list) +
  geom_rect(aes(xmin = week, xmax = lead(week), ymin = 0, 
                       ymax = Inf, fill = status), alpha = 0.5) +
  scale_fill_manual(values = c("No restrictions" = "#00BA38", 
                                 "Some restrictions" = "#619CFF", 
                                 "Lockdown" = "#F8766D"), 
                      labels = c("No restrictions", "Some restrictions", 
                                 "Lockdown"))

#plot retail and other
plot_grid(retail, other, ncol = 1, align = 'v')

#plot grocery
grocery <- ggplot(gm_av_sub, aes(week, grocery,
              label = ifelse(status == "No restrictions", 
                      ifelse(is.na(special) == F, special, week), special))) + 
  geom_line(group = "status") + geom_text_repel(size = 2.5, max.overlaps = 80) +
  geom_point(aes(x = week, y = ifelse(is.na(special) == F, grocery, NA))) +
  scale_x_discrete(breaks = my_list) +
  geom_rect(aes(xmin = week, xmax = lead(week), ymin = 0, 
                ymax = Inf, fill = status), alpha = 0.5) +
  scale_fill_manual(values = c("No restrictions" = "#00BA38", 
                               "Some restrictions" = "#619CFF", 
                               "Lockdown" = "#F8766D"), 
                    labels = c("No restrictions", "Some restrictions", 
                               "Lockdown"))

#plot grocery and other
plot_grid(grocery, other, ncol = 1, align = 'v')

#plot parks
parks <- ggplot(gm_av_sub, aes(week, parks,
            label = ifelse(status == "No restrictions", 
                    ifelse(is.na(special) == F, special, week), special))) + 
  geom_line(group = "status") + geom_text_repel(size = 2.5, max.overlaps = 80) +
  geom_point(aes(x = week, y = ifelse(is.na(special) == F, parks, NA))) +
  scale_x_discrete(breaks = my_list) +
  geom_rect(aes(xmin = week, xmax = lead(week), ymin = 0, 
                ymax = Inf, fill = status), alpha = 0.5) +
  scale_fill_manual(values = c("No restrictions" = "#00BA38", 
                               "Some restrictions" = "#619CFF", 
                               "Lockdown" = "#F8766D"), 
                    labels = c("No restrictions", "Some restrictions", 
                               "Lockdown"))

#plot parks and other
plot_grid(parks, other, ncol = 1, align = 'v')

#plot transit
transit <- ggplot(gm_av_sub, aes(week, transit,
              label = ifelse(status == "No restrictions", 
                      ifelse(is.na(special) == F, special, week), special))) + 
  geom_line(group = "status") + geom_text_repel(size = 2.5, max.overlaps = 80) +
  geom_point(aes(x = week, y = ifelse(is.na(special) == F, transit, NA))) +
  scale_x_discrete(breaks = my_list) +
  geom_rect(aes(xmin = week, xmax = lead(week), ymin = 0, 
                ymax = Inf, fill = status), alpha = 0.5) +
  scale_fill_manual(values = c("No restrictions" = "#00BA38", 
                               "Some restrictions" = "#619CFF", 
                               "Lockdown" = "#F8766D"), 
                    labels = c("No restrictions", "Some restrictions", 
                               "Lockdown"))

#plot transit and other
plot_grid(transit, other, ncol = 1, align = 'v')

#plot workplaces
workplaces <- ggplot(gm_av_sub, aes(week, workplaces,
                 label = ifelse(status == "No restrictions", 
                         ifelse(is.na(special) == F, special, week), special))) + 
  geom_line(group = "status") + geom_text_repel(size = 2.5, max.overlaps = 80) +
  geom_point(aes(x = week, y = ifelse(is.na(special) == F, workplaces, NA))) +
  scale_x_discrete(breaks = my_list) +
  geom_rect(aes(xmin = week, xmax = lead(week), ymin = 0, 
                ymax = Inf, fill = status), alpha = 0.5) +
  scale_fill_manual(values = c("No restrictions" = "#00BA38", 
                               "Some restrictions" = "#619CFF", 
                               "Lockdown" = "#F8766D"), 
                    labels = c("No restrictions", "Some restrictions", 
                               "Lockdown"))

#plot work
work <- ggplot(weighted_date, aes(week, work,
           label = ifelse(status == "No restrictions", 
                   ifelse(is.na(special) == F, special, week), special))) + 
  geom_line(group = 1) + geom_text_repel(size = 2.5, max.overlaps = 80) +
  geom_point(aes(x = week, y = ifelse(is.na(special) == F, work, NA))) +
  scale_x_discrete(breaks = my_list) +
  geom_rect(aes(xmin = week, xmax = lead(week), ymin = 0, 
                ymax = Inf, fill = status), alpha = 0.5) +
  scale_fill_manual(values = c("No restrictions" = "#00BA38", 
                               "Some restrictions" = "#619CFF", 
                               "Lockdown" = "#F8766D"), 
                    labels = c("No restrictions", "Some restrictions", 
                               "Lockdown"))
#plot workplaces and work
plot_grid(workplaces, work, ncol = 1, align = 'v')

#calculate inverse of residential
gm_av_sub[, residential := 1 - residential]

#plot residential
residential <- ggplot(gm_av_sub, aes(week, residential,
                  label = ifelse(status == "No restrictions", 
                          ifelse(is.na(special) == F, special, week), special))) + 
  geom_line(group = "status") + geom_text_repel(size = 2.5, max.overlaps = 80) +
  geom_point(aes(x = week, y = ifelse(is.na(special) == F, residential, NA))) +
  scale_x_discrete(breaks = my_list) + ylab("1 - residential") + 
  geom_rect(aes(xmin = week, xmax = lead(week), ymin = -0.3, 
                ymax = Inf, fill = status), alpha = 0.5) +
  scale_fill_manual(values = c("No restrictions" = "#00BA38", 
                               "Some restrictions" = "#619CFF", 
                               "Lockdown" = "#F8766D"), 
                    labels = c("No restrictions", "Some restrictions", 
                               "Lockdown"))

#plot nonhome
nonhome <- ggplot(weighted_date, aes(week, nonhome,
              label = ifelse(status == "No restrictions", 
                      ifelse(is.na(special) == F, special, week), special))) + 
  geom_line(group = 1) + geom_text_repel(size = 2.5, max.overlaps = 80) +
  geom_point(aes(x = week, y = ifelse(is.na(special) == F, nonhome, NA))) +
  scale_x_discrete(breaks = my_list) + 
  geom_rect(aes(xmin = week, xmax = lead(week), ymin = 0, 
                ymax = Inf, fill = status), alpha = 0.5) +
  scale_fill_manual(values = c("No restrictions" = "#00BA38", 
                               "Some restrictions" = "#619CFF", 
                               "Lockdown" = "#F8766D"), 
                    labels = c("No restrictions", "Some restrictions", 
                               "Lockdown"))

#plot residential and non-home
plot_grid(residential, nonhome, ncol = 1, align = 'v')
