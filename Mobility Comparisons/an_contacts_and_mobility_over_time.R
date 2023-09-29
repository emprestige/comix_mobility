##mobility over time 

#load libraries
library(data.table)
library(ggplot2)
library(tidyverse)
library(lubridate)
library(cowplot)
library(zoo)
library(gghighlight)
library(ggrepel)
library(scales)
library(ggpubr)
library(patchwork)

#set cowplot theme
theme_set(cowplot::theme_cowplot(font_size = 18) + theme(strip.background = element_blank(),
                                                         legend.box.margin = margin(l = 10, b = 10)))

#set data path
data_path <-"C:\\Users\\emiel\\Documents\\LSHTM\\Fellowship\\Project\\comix_mobility\\Data\\"

#import contact data
cnts <- qs::qread(file.path(data_path, "cnts_weight_work_middate.qs"))

#filter out participants of a certain age
cnts <- cnts[sample_type == "adult"]
cnts <- cnts %>% filter(!is.na(part_age_group))

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
                              ifelse(date == ymd("2020-12-31"), "NYE",
                              ifelse(date == ymd("2021-12-31"), "Xmas/NYE",
                              ifelse(date == ymd("2020-04-13"), "Easter",
                              ifelse(date == ymd("2021-04-05"), "Easter", 
                              ifelse(date %within% summer, "Summer Hol", NA))))))]
num_merge <- num_merge[order(date)]

#get middate for fornight periods 
num_merge[, fortnight := paste(isoyear(date), "/", sprintf("%02d", ceiling(isoweek(date)/2)))]
num_merge[, start_date := min(date), by = .(fortnight)]
num_merge[, end_date := max(date), by = .(fortnight)]
num_merge[, mid_date := start_date + floor((end_date - start_date)/2) , by = .(fortnight)]

#get weighted means by week
weighted_date <- num_merge[, .(study, status, special,
                               work = weighted.mean(work, day_weight*social_weight),
                               other = weighted.mean(other, day_weight*social_weight),
                               nonhome = weighted.mean(nonhome, day_weight*social_weight)),
                 by = .(mid_date)]  
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
                              ifelse(date == ymd("2020-12-31"), "NYE",
                              ifelse(date == ymd("2021-12-31"), "Xmas/NYE",
                              ifelse(date == ymd("2020-04-13"), "Easter",
                              ifelse(date == ymd("2021-04-05"), "Easter", 
                              ifelse(date %within% summer, "Summer Hol", NA))))))]
mob_merge <- mob_merge[order(date)]

#get middate for fornight periods 
mob_merge[, fortnight := paste(isoyear(date), "/", sprintf("%02d", ceiling(isoweek(date)/2)))]
mob_merge[, start_date := min(date), by = .(fortnight)]
mob_merge[, end_date := max(date), by = .(fortnight)]
mob_merge[, mid_date := start_date + floor((end_date - start_date)/2) , by = .(fortnight)]

#get averages (weekly to start?)
gm_av <- mob_merge[, .(status, special,
                       retail = mean(retail_recreation), 
                       grocery = mean(grocery_pharmacy),
                       parks = mean(parks), 
                       transit = mean(transit_stations),
                       workplaces = mean(workplaces),
                       residential = mean(residential)),
         by = .(mid_date)]
gm_av <- unique(gm_av)

#remove dates which are missing from comix data
gm_av_sub <- gm_av[-c(7,8), ]

#plot workplaces
workplaces <- ggplot(gm_av_sub, aes(mid_date, workplaces,
                 label = ifelse(status == "No restrictions", 
                         ifelse(is.na(special) == F, special, NA), special))) + 
  geom_line(group = "status", size = 0.8) + 
  geom_text_repel(size = 4, max.overlaps = 80, box.padding = 0.25) +
  geom_point(aes(x = mid_date, y = ifelse(is.na(special) == F, workplaces, NA)), size = 2) +
  geom_rect(aes(xmin = mid_date, xmax = lead(mid_date), ymin = 0, 
                ymax = Inf, fill = status), alpha = 0.4) +
  scale_x_date(labels = date_format("%B-%Y")) + 
  scale_y_continuous(limits = c(0, 1.25), breaks = seq(0, 1.25, by = 0.25)) +
  labs(x = "Date", y = "Google Mobility\n'workplaces' Visits", fill = "Status") +
  scale_fill_manual(values = c("No restrictions" = "#00BA38", 
                               "Some restrictions" = "#619CFF", 
                               "Lockdown" = "#F8766D"), 
                    labels = c("No restrictions", "Some restrictions", 
                               "Lockdown"))

#plot work
work <- ggplot(weighted_date, aes(mid_date, work,
           label = ifelse(status == "No restrictions", 
                   ifelse(is.na(special) == F, special, NA), special))) + 
  geom_line(group = 1, size = 0.8) + 
  geom_text_repel(size = 4, max.overlaps = 80, box.padding = 0.25) +
  geom_point(aes(x = mid_date, y = ifelse(is.na(special) == F, work, NA)), size = 2) +
  geom_rect(aes(xmin = mid_date, xmax = lead(mid_date), ymin = 0, 
                ymax = Inf, fill = status), alpha = 0.4) +
  scale_x_date(labels = date_format("%B-%Y")) + 
  scale_y_continuous(limits = c(0, 1.25), breaks = seq(0, 1.25, by = 0.25)) +
  labs(x = "Date", y = "Mean Number of\nWork Contacts", fill = "Status") +
  scale_fill_manual(values = c("No restrictions" = "#00BA38", 
                               "Some restrictions" = "#619CFF", 
                               "Lockdown" = "#F8766D"), 
                    labels = c("No restrictions", "Some restrictions", 
                               "Lockdown"))
 

#plot workplaces and work
works <- plot_grid(workplaces, work, ncol = 1, align = 'v')

#combine datasets
work.plus <- merge(weighted_date, gm_av_sub)

#plot with two y-axes
test.plot1 <- ggplot(data = work.plus, aes(x = mid_date,
                     label = ifelse(status == "No restrictions", 
                     ifelse(is.na(special) == F, special, NA), special))) +
  geom_rect(aes(xmin = mid_date, xmax = lead(mid_date), ymin = 0, 
                ymax = Inf, fill = status), alpha = 0.4) +
  geom_line(aes(y = work, linetype = "CoMix"), group = 1, size = 1) + 
  geom_text_repel(aes(y = work), size = 4, max.overlaps = 80, box.padding = 0.25) +
  geom_point(aes(x = mid_date, y = ifelse(is.na(special) == F, work, NA)), size = 2) +
  geom_line(aes(y = workplaces, linetype = "Google Mobility"), group = "status", size = 1) + 
  geom_point(aes(x = mid_date, y = ifelse(is.na(special) == F, workplaces, NA)), size = 2) +
  geom_text_repel(aes(y = workplaces), size = 4, max.overlaps = 80, box.padding = 0.25) +
  scale_x_date(labels = date_format("%B-%Y")) + 
  scale_y_continuous(limits = c(0, 1.25), breaks = seq(0, 1.25, by = 0.25),
                     name = "Mean Number of Work Contacts",
                     sec.axis = sec_axis(trans = ~ .,
                     name = "Google Mobility 'workplaces' Visits")) +
  labs(x = "Date", fill = "Status", linetype = "Data Type") +
  scale_fill_manual(values = c("No restrictions" = "#00BA38", 
                               "Some restrictions" = "#619CFF", 
                               "Lockdown" = "#F8766D"), 
                    labels = c("No restrictions", "Some restrictions", 
                               "Lockdown")) +
  scale_linetype_manual(values = c("CoMix" = 1, "Google Mobility" = 2)) +
  guides(linetype = guide_legend(keywidth = 3)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

test.plot1

#compare correlations in first year vs second year 
work.plus.y1 <- rlang::duplicate(work.plus)
work.plus.y1 <- work.plus.y1[mid_date <= ymd("2021-03-31")]
work.plus.y2 <- rlang::duplicate(work.plus)
work.plus.y2 <- work.plus.y2[mid_date > ymd("2021-03-31")]
cor(work.plus.y1$work, work.plus.y1$workplaces)
cor.test(work.plus.y1$work, work.plus.y1$workplaces)
cor(work.plus.y2$work, work.plus.y2$workplaces)
cor.test(work.plus.y2$work, work.plus.y2$workplaces)

#create new data set with necessary variables, removing duplicate rows 
work.plus_new <- rlang::duplicate(work.plus)
work.plus_new <- work.plus_new[, .(mid_date, work, workplaces)]
work.plus_new <- unique(work.plus_new)

#try to test correlation using sliding window
compute_sliding_correlation <- function(ts1, ts2, widthW) {
  n <- length(ts1)
  num_windows <- n - widthW + 1
  # Initialize an empty vector to store correlation values
  correlations <- numeric(num_windows)
  for (i in 1:num_windows) {
    # Extract the subseries from both time series for the current window
    window_ts1 <- ts1[i:(i + widthW - 1)]
    window_ts2 <- ts2[i:(i + widthW - 1)]
    # Compute the correlation for the current window
    correlation <- cor(window_ts1, window_ts2)
    # Store the correlation value in the vector
    correlations[i] <- correlation
  }
  return(correlations)
}
width_window <- 9 ## NB needs to be odd
dum <- compute_sliding_correlation(work.plus_new$work, work.plus_new$workplaces, 
                                   width_window)

df_all <- data.frame(
  x = (1:nrow(work.plus_new)),
  contacts = work.plus_new$work,
  mobility = work.plus_new$workplaces,
  slidingCorrelation = c(rep(NA, (width_window - 1)/2), dum, 
                         rep(NA, (width_window - 1)/2))
)

df_all <- cbind(df_all, mid_date = work.plus_new$mid_date)

a <- ggplot(df_all) + geom_line(aes(x = mid_date, y = contacts, col = "con"), size = 0.8) +
  geom_line(aes(x = mid_date, y = mobility, col = "mob"), size = 1) +
  labs(y = "Work Contacts or \nWorkpalce Mobility", col = "Measurement Type", x = "Date") +
  scale_colour_manual(breaks = c("mob", "con"), values = c("blue", "red"),
                      labels = c("Mobility", "Contacts"))
b <- ggplot(df_all) + geom_point(aes(x = mid_date, y = slidingCorrelation), size = 2) +
  geom_line(aes(x = mid_date, y = slidingCorrelation), size = 1) + xlab("Date") +
  ylab("Sliding Window Correlation")

#plot the stuff together
plot_combined <- a / b
plot_combined

#try to test correlation using sliding window
compute_sliding_correlation <- function(ts1, ts2, widthW) {
  n <- length(ts1)
  num_windows <- n - widthW + 1
  # Initialize an empty vector to store correlation values
  correlations <- numeric(num_windows)
  for (i in 1:num_windows) {
    # Extract the subseries from both time series for the current window
    window_ts1 <- ts1[i:(i + widthW - 1)]
    window_ts2 <- ts2[i:(i + widthW - 1)]
    # Compute the correlation for the current window
    correlation <- cor.test(window_ts1, window_ts2)$p.value
    # Store the correlation value in the vector
    correlations[i] <- correlation
  }
  return(correlations)
}
width_window <- 9 ## NB needs to be even 
dum<-compute_sliding_correlation(work.plus_new$work, work.plus_new$workplaces, width_window)

df_all<-data.frame(
  x = (1:nrow(work.plus_new)),
  contacts = work.plus_new$work,
  mobility = work.plus_new$workplaces,
  slidingCorrelation = c(rep(NA, (width_window - 1)/2), dum,
                         rep(NA, (width_window - 1)/2))
)

df_all <- cbind(df_all, mid_date = work.plus_new$mid_date)

a <- ggplot(df_all) + geom_line(aes(x = mid_date, y = contacts, col = "con"), size = 1)+
  geom_line(aes(x = mid_date, y = mobility, col = "mob"), size = 1) +
  labs(x = "Date", y = "Work Contacts and \nWorkplace Mobility", col = "Measurement Type") +
  scale_colour_manual(breaks = c("mob", "con"), values = c("blue", "red"),
                      labels = c("Mobility", "Contacts"))
b <- ggplot(df_all) + geom_point(aes(x = mid_date, y = slidingCorrelation), size = 2)+
  geom_line(aes(x = mid_date, y = slidingCorrelation), size = 1) + xlab("Date") +
  ylab("Sliding Window Correlation \n(p-value)")

plot_combined <- a / b
plot_combined 

work_pval_cor <- plot_combined + scale_colour_manual(guide="none")

#import contact data
cnts <- qs::qread(file.path(data_path, "cnts_weight_other_middate.qs"))

#filter out participants of a certain age
cnts <- cnts[sample_type == "adult"]
cnts <- cnts %>% filter(!is.na(part_age_group))

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
                       ifelse(date == ymd("2020-12-31"), "NYE",
                       ifelse(date == ymd("2021-12-31"), "Xmas/NYE",
                       ifelse(date == ymd("2020-04-13"), "Easter",
                       ifelse(date == ymd("2021-04-05"), "Easter", 
                       ifelse(date %within% summer, "Summer Hol", NA))))))]
num_merge <- num_merge[order(date)]

#get middate for fornight periods 
num_merge[, fortnight := paste(isoyear(date), "/", sprintf("%02d", ceiling(isoweek(date)/2)))]
num_merge[, start_date := min(date), by = .(fortnight)]
num_merge[, end_date := max(date), by = .(fortnight)]
num_merge[, mid_date := start_date + floor((end_date - start_date)/2) , by = .(fortnight)]

#get weighted means by week
weighted_date <- num_merge[, .(study, status, special,
                               work = weighted.mean(work, day_weight*social_weight),
                               other = weighted.mean(other, day_weight*social_weight),
                               nonhome = weighted.mean(nonhome, day_weight*social_weight)),
                           by = .(mid_date)]  
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
                       ifelse(date == ymd("2020-12-31"), "NYE",
                       ifelse(date == ymd("2021-12-31"), "Xmas/NYE",
                       ifelse(date == ymd("2020-04-13"), "Easter",
                       ifelse(date == ymd("2021-04-05"), "Easter", 
                       ifelse(date %within% summer, "Summer Hol", NA))))))]
mob_merge <- mob_merge[order(date)]

#get middate for fornight periods 
mob_merge[, fortnight := paste(isoyear(date), "/", sprintf("%02d", ceiling(isoweek(date)/2)))]
mob_merge[, start_date := min(date), by = .(fortnight)]
mob_merge[, end_date := max(date), by = .(fortnight)]
mob_merge[, mid_date := start_date + floor((end_date - start_date)/2) , by = .(fortnight)]

#get averages (weekly to start?)
gm_av <- mob_merge[, .(status, special,
                       retail = mean(retail_recreation), 
                       grocery = mean(grocery_pharmacy),
                       parks = mean(parks), 
                       transit = mean(transit_stations),
                       workplaces = mean(workplaces),
                       residential = mean(residential)),
                   by = .(mid_date)]
gm_av <- unique(gm_av)

#create predictor for 'other' contacts
gm_av[, predictor := retail * 0.333 + transit * 0.334 + grocery * 0.333]

#remove dates which are missing from comix data
gm_av_sub <- gm_av[-c(7,8), ]

#plot weighted predictor 
predictor <- ggplot(gm_av_sub, aes(mid_date, predictor,
                    label = ifelse(status == "No restrictions", 
                    ifelse(is.na(special) == F, special, NA), special))) + 
  geom_line(group = "status", size = 0.8) + 
  geom_text_repel(size = 4, max.overlaps = 80, box.padding = 0.25) +
  geom_point(aes(x = mid_date, y = ifelse(is.na(special) == F, predictor, NA)), size = 2) +
  geom_rect(aes(xmin = mid_date, xmax = lead(mid_date), ymin = 0, 
                ymax = Inf, fill = status), alpha = 0.4) + 
  scale_x_date(labels = date_format("%B-%Y")) + 
  scale_y_continuous(limits = c(0, 1.25), breaks = seq(0, 1.25, by = 0.25)) +
  labs(y = "Google Mobility\n'other' Visits",
       x = "Date", fill = "Status") + scale_x_date(labels = date_format("%B-%Y")) +
  scale_fill_manual(values = c("No restrictions" = "#00BA38", 
                               "Some restrictions" = "#619CFF", 
                               "Lockdown" = "#F8766D"), 
                    labels = c("No restrictions", "Some restrictions", 
                               "Lockdown"))

#plot other
other <- ggplot(weighted_date, aes(mid_date, other,
                label = ifelse(status == "No restrictions", 
                ifelse(is.na(special) == F, special, NA), special))) + 
  geom_line(group = 1, size = 0.8) + 
  geom_text_repel(size = 4, max.overlaps = 80, box.padding = 0.25) +
  geom_point(aes(x = mid_date, y = ifelse(is.na(special) == F, other, NA)), size = 2) +
  geom_rect(aes(xmin = mid_date, xmax = lead(mid_date), ymin = 0, 
                ymax = Inf, fill = status), alpha = 0.4) +
  scale_x_date(labels = date_format("%B-%Y")) + 
  scale_y_continuous(limits = c(0, 1.25), breaks = seq(0, 1.25, by = 0.25)) +
  labs(x = "Date", y = "Mean Number of\nOther Contacts", fill = "Status") +
  scale_fill_manual(values = c("No restrictions" = "#00BA38", 
                               "Some restrictions" = "#619CFF", 
                               "Lockdown" = "#F8766D"), 
                    labels = c("No restrictions", "Some restrictions", 
                               "Lockdown"))

#plot predictor and other
others <- plot_grid(predictor, other, ncol = 1, align = 'v')

#plot work and other 
ggarrange(work, other, workplaces, predictor, common.legend = T, 
          legend = "bottom", labels = "AUTO")

#combine datasets
other.plus <- merge(weighted_date, gm_av_sub)

#plot with two y-axes
test.plot2 <- ggplot(data = other.plus, aes(x = mid_date,
                     label = ifelse(status == "No restrictions", 
                     ifelse(is.na(special) == F, special, NA), special))) +
  geom_rect(aes(xmin = mid_date, xmax = lead(mid_date), ymin = 0, 
                ymax = Inf, fill = status), alpha = 0.4) +
  geom_line(aes(y = other, linetype = "CoMix"), group = 1, size = 1) + 
  geom_text_repel(aes(y = other), size = 4, max.overlaps = 80, box.padding = 0.25) +
  geom_point(aes(x = mid_date, y = ifelse(is.na(special) == F, other, NA)), size = 2) +
  geom_line(aes(y = predictor, linetype = "Google Mobility"), group = "status", size = 1) + 
  geom_text_repel(aes(y = predictor), size = 4, max.overlaps = 80, box.padding = 0.25) +
  geom_point(aes(x = mid_date, y = ifelse(is.na(special) == F, predictor, NA)), size = 2) +
  scale_x_date(labels = date_format("%B-%Y")) + 
  scale_y_continuous(limits = c(0, 1.25), breaks = seq(0, 1.25, by = 0.25),
                     name = "Mean Number of Other Contacts",
                     sec.axis = sec_axis(trans = ~ .,
                     name = "Google Mobility 'other' Visits")) +
  labs(x = "Date", fill = "Status", linetype = "Data Type") +
  scale_fill_manual(values = c("No restrictions" = "#00BA38", 
                               "Some restrictions" = "#619CFF", 
                               "Lockdown" = "#F8766D"), 
                    labels = c("No restrictions", "Some restrictions", 
                               "Lockdown")) + 
  scale_linetype_manual(values = c("CoMix" = 1, "Google Mobility" = 2)) +
  guides(linetype = guide_legend(keywidth = 3)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

test.plot2

#compare correlations in first year vs second year 
other.plus.y1 <- rlang::duplicate(other.plus)
other.plus.y1 <- other.plus.y1[mid_date <= ymd("2021-03-31")]
other.plus.y2 <- rlang::duplicate(other.plus)
other.plus.y2 <- other.plus.y2[mid_date > ymd("2021-03-31")]
cor(other.plus.y1$other, other.plus.y1$predictor)
cor.test(other.plus.y1$other, other.plus.y1$predictor)
cor(other.plus.y2$other, other.plus.y2$predictor)
cor.test(other.plus.y2$other, other.plus.y2$predictor)

#create new data set with necessary variables, removing duplicate rows 
other.plus_new <- rlang::duplicate(other.plus)
other.plus_new <- other.plus_new[, .(mid_date, other, predictor)]
other.plus_new <- unique(other.plus_new)

#try to test correlation using sliding window
compute_sliding_correlation <- function(ts1, ts2, widthW) {
  n <- length(ts1)
  num_windows <- n - widthW + 1
  # Initialize an empty vector to store correlation values
  correlations <- numeric(num_windows)
  for (i in 1:num_windows) {
    # Extract the subseries from both time series for the current window
    window_ts1 <- ts1[i:(i + widthW - 1)]
    window_ts2 <- ts2[i:(i + widthW - 1)]
    # Compute the correlation for the current window
    correlation <- cor(window_ts1, window_ts2)
    # Store the correlation value in the vector
    correlations[i] <- correlation
  }
  return(correlations)
}
width_window <- 9 ## NB needs to be odd
dum <- compute_sliding_correlation(other.plus_new$other, other.plus_new$predictor, 
                                   width_window)

df_all <- data.frame(
  x = (1:nrow(work.plus_new)),
  contacts = other.plus_new$other,
  mobility = other.plus_new$predictor,
  slidingCorrelation = c(rep(NA, (width_window - 1)/2), dum, 
                         rep(NA, (width_window - 1)/2))
)

df_all <- cbind(df_all, mid_date = other.plus_new$mid_date)

a <- ggplot(df_all) + geom_line(aes(x = mid_date, y = contacts, col = "con"), size = 0.8) +
  geom_line(aes(x = mid_date, y = mobility, col = "mob"), size = 1) +
  labs(y = "Other Contacts and \nOther Mobility", col = "Measurement Type", x = "Date") +
  scale_colour_manual(breaks = c("mob", "con"), values = c("blue", "red"),
                      labels = c("Mobility", "Contacts"))
b <- ggplot(df_all) + geom_point(aes(x = mid_date, y = slidingCorrelation), size = 2) +
  geom_line(aes(x = mid_date, y = slidingCorrelation), size = 1) + xlab("Date") +
  ylab("Sliding Window Correlation")

#plot the stuff together
plot_combined <- a / b
plot_combined

#try to test correlation using sliding window
compute_sliding_correlation <- function(ts1, ts2, widthW) {
  n <- length(ts1)
  num_windows <- n - widthW + 1
  # Initialize an empty vector to store correlation values
  correlations <- numeric(num_windows)
  for (i in 1:num_windows) {
    # Extract the subseries from both time series for the current window
    window_ts1 <- ts1[i:(i + widthW - 1)]
    window_ts2 <- ts2[i:(i + widthW - 1)]
    # Compute the correlation for the current window
    correlation <- cor.test(window_ts1, window_ts2)$p.value
    # Store the correlation value in the vector
    correlations[i] <- correlation
  }
  return(correlations)
}
width_window <- 9 ## NB needs to be even 
dum <- compute_sliding_correlation(other.plus_new$other, other.plus_new$predictor,
                                   width_window)

df_all<-data.frame(
  x = (1:nrow(other.plus_new)),
  contacts = other.plus_new$other,
  mobility = other.plus_new$predictor,
  slidingCorrelation = c(rep(NA, (width_window - 1)/2), dum,
                         rep(NA, (width_window - 1)/2))
)

df_all <- cbind(df_all, mid_date = other.plus_new$mid_date)

a <- ggplot(df_all) + geom_line(aes(x = mid_date, y = contacts, col = "con"), size = 1)+
  geom_line(aes(x = mid_date, y = mobility, col = "mob"), size = 1) +
  labs(x = "Date", y = "Other Contacts and \nOther Mobility", col = "Measurement Type") +
  scale_colour_manual(breaks = c("mob", "con"), values = c("blue", "red"),
                      labels = c("Mobility", "Contacts"))
b <- ggplot(df_all) + geom_point(aes(x = mid_date, y = slidingCorrelation), size = 2)+
  geom_line(aes(x = mid_date, y = slidingCorrelation), size = 1) + xlab("Date") +
  ylab("Sliding Window Correlation \n(p-value)")

plot_combined <- a / b
plot_combined

other_pval_cor <- plot_combined

#plot work and other 
ggarrange(test.plot1, test.plot2, common.legend = T, 
          legend = "bottom", labels = "AUTO")
ggarrange(work_pval_cor, other_pval_cor, common.legend = F, 
          legend = "bottom", labels = "AUTO")
