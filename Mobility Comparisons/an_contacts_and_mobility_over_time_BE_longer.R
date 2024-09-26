##mobility over time 

#load libraries
library(data.table)
library(ggplot2)
library(dplyr)
library(lubridate)
library(cowplot)
library(zoo)
library(gghighlight)
library(ggrepel)
library(scales)
library(ggpubr)
library(patchwork)
library(here)

#set cowplot theme
theme_set(cowplot::theme_cowplot(font_size = 14) + theme(strip.background = element_blank(),
                                                         legend.box.margin = margin(l = 10, b = 10)))

#set data path
data_path <- here("data")

#import contact data
cnts <- qs::qread(file.path(data_path, "part_cnts_work_BE_longer.qs"))
cnts[, sample_type := ifelse(part_age == "[0,1)" | part_age == "[1,6)" | part_age == "[6,12)" | part_age == "[12,18)", "child", "adult")] 
cnts[, date := as.Date(parse_date_time(sday_id, orders = "ymd"))]

#filter out participants of a certain age
cnts <- cnts[sample_type == "adult"]
cnts <- cnts %>% filter(!is.na(part_age))

#order by date
cnts_date <- cnts[base::order(date)]
cnts_date <- cnts_date[date <= ymd("2022-03-02")]

#create data table with subset of variables
num <- cnts_date[, .(date, part_id, part_age, day_weight,
                     work = n_cnt_work, other = n_cnt_other)]
num[, t := as.numeric(date - ymd("2020-01-01"))]

#create study column
num[, study := "CoMix"]

#create second database which shifts the survey rounds and dates
num2 <- rlang::duplicate(num)
num2[, date := date + 7]

#merge the two 
num_merge <- rbind(num, num2) 

#create sequence of dates
date <- seq(as.Date("2020-03-02"), as.Date("2022-03-02"), by = "days")
lockdowns <- as.data.table(as.Date(date))
lockdowns$lockdown_status <- 0
colnames(lockdowns) <- c("date", "status")

#create time intervals for different types of restrictions
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

#add column for special dates
summer1 <- interval(ymd("2020-08-03"), ymd("2020-08-09"))
summer2 <- interval(ymd("2021-08-03"), ymd("2021-08-09"))
summer3 <- interval(ymd("2022-08-03"), ymd("2022-08-09"))
num_merge[, special := ifelse(date == ymd("2020-12-25"), "Xmas/NYE",
                       ifelse(date == ymd("2021-12-31"), "Xmas/NYE",
                       ifelse(date == ymd("2020-04-13"), "Easter",
                       ifelse(date == ymd("2021-04-05"), "Easter",
                       ifelse(date %within% summer1, "Summer Hol",
                       ifelse(date %within% summer2, "Summer Hol",
                       ifelse(date %within% summer3, "Summer Hol", NA)))))))]
num_merge <- num_merge[order(date)]

#get dates in week
week <- unique(as.data.table(as.Date(num_merge$date)))
colnames(week) <- "date"
week <- week[, week := isoweek(date)]

#insert missing dates
misdate <- as.data.table(seq(as.Date("2020-03-23"), as.Date("2022-03-02"), by = "days"))
colnames(misdate) <- "date"
misdate <- merge(misdate, lockdowns, by = "date")
num_merge <- merge(num_merge, misdate, by = c("date", "status"), all = T)

#get middate for fornight periods 
num_merge[, fortnight := paste(isoyear(date), "/", sprintf("%02d", ceiling(isoweek(date)/1)))]
num_merge[, start_date := min(date), by = .(fortnight)]
num_merge[, end_date := max(date), by = .(fortnight)]
num_merge[, mid_date := start_date + floor((end_date - start_date)/1) , by = .(fortnight)]

#get weighted means by week
weighted_date <- num_merge[, .(study, status, special,
                               work = weighted.mean(work, day_weight, na.rm = T),
                               other = weighted.mean(other, day_weight, na.rm = T)),
                           by = .(mid_date)]  
weighted_date <- unique(weighted_date)

#add column for special dates
summer1 <- interval(ymd("2020-08-03"), ymd("2020-08-09"))
summer2 <- interval(ymd("2021-08-03"), ymd("2021-08-09"))
summer3 <- interval(ymd("2022-08-03"), ymd("2022-08-09"))
easter1 <- interval(ymd("2020-04-12"), ymd("2020-04-13"))
easter2 <- interval(ymd("2021-04-04"), ymd("2021-04-05"))
xmas1 <- interval(ymd("2020-12-25"), ymd("2020-12-26"))
xmas2 <- interval(ymd("2021-12-25"), ymd("2021-12-26"))
weighted_date[, special := ifelse(mid_date %within% xmas1, "Xmas/NYE",
                       ifelse(mid_date %within% xmas2, "Xmas/NYE",
                       ifelse(mid_date %within% easter1, "Easter",
                       ifelse(mid_date %within% easter2, "Easter",
                       ifelse(mid_date %within% summer1, "Summer Hol",
                       ifelse(mid_date %within% summer2, "Summer Hol",
                       ifelse(mid_date %within% summer3, "Summer Hol", NA)))))))]

#import mobility data
mob <- qs::qread(file.path(data_path, "google_mob_BE.qs"))

#subset for same date range
mob_sub <- mob[date >= "2020-03-23" & date <= "2022-03-02"]

#duplicate google mobility data and rename columns
gm2 <- rlang::duplicate(mob_sub)
names(gm2) <- stringr::str_replace(names(gm2), "_percent_change_from_baseline", "")
names(gm2) <- stringr::str_replace(names(gm2), "_and", "")

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
mob_merge[, special := ifelse(date == ymd("2020-12-25"), "Xmas/NYE",
                       ifelse(date == ymd("2021-12-31"), "Xmas/NYE",
                       ifelse(date == ymd("2020-04-13"), "Easter",
                       ifelse(date == ymd("2021-04-05"), "Easter", 
                       ifelse(date %within% summer1, "Summer Hol",
                       ifelse(date %within% summer2, "Summer Hol", 
                       ifelse(date %within% summer3, "Summer Hol", NA)))))))]
mob_merge <- mob_merge[order(date)]

#get middate for fortnight periods 
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
#gm_av_sub <- gm_av[-c(7,8), ]

#plot workplaces
workplaces <- ggplot(gm_av, aes(mid_date, workplaces,
                                    label = ifelse(status == "No restrictions", 
                                            ifelse(is.na(special) == F, special, NA), special))) + 
  geom_line(group = "status", size = 0.8) + 
  geom_text_repel(size = 4, max.overlaps = 80, box.padding = 0.25) +
  geom_point(aes(x = mid_date, y = ifelse(is.na(special) == F, workplaces, NA)), size = 2) +
  geom_rect(aes(xmin = mid_date, xmax = lead(mid_date), ymin = 0, 
                ymax = Inf, fill = status), alpha = 0.4) +
  scale_x_date(labels = date_format("%B-%Y")) + 
  scale_y_continuous(limits = c(0, 4), breaks = seq(0, 4, by = 0.5)) +
  labs(x = "Date", y = "Google Mobility\n''Workplaces'' Visits", fill = "Status") +
  scale_fill_manual(values = c("No restrictions" = "#009E73", 
                               "Some restrictions" = "#0072B2", 
                               "Lockdown" = "#D55E00"), 
                    labels = c("No restrictions", "Some restrictions", 
                               "Lockdown"))

#plot work
work <- ggplot(weighted_date, aes(mid_date, work,
                                  label = ifelse(status == "No restrictions", 
                                          ifelse(is.na(special) == F, special, NA), special))) + 
  geom_line(group = "status", size = 0.8) + 
  geom_text_repel(size = 4, max.overlaps = 80, box.padding = 0.25) +
  geom_point(aes(x = mid_date, y = ifelse(is.na(special) == F, work, NA)), size = 2) +
  geom_rect(aes(xmin = mid_date, xmax = lead(mid_date), ymin = 0, 
                ymax = Inf, fill = status), alpha = 0.4) +
  scale_x_date(labels = date_format("%B-%Y")) + 
  scale_y_continuous(limits = c(0, 4), breaks = seq(0, 4, by = 0.5)) +
  labs(x = "Date", y = "Mean Number of\n''Work'' Contacts", fill = "Status") +
  scale_fill_manual(values = c("No restrictions" = "#009E73", 
                               "Some restrictions" = "#0072B2", 
                               "Lockdown" = "#D55E00"),
                    labels = c("No restrictions", "Some restrictions", 
                               "Lockdown"))

#plot workplaces and work
works <- plot_grid(workplaces, work, ncol = 1, align = 'v')

# #combine datasets
# work.plus1 <- merge(weighted_date, gm_av, by = c("mid_date", "status", "special"), all.y = T)
# work.plus2 <- merge(weighted_date, gm_av, by = c("mid_date", "status", "special"), all.x = T)

#plot with two y-axes
test.plot1 <- ggplot() +
  geom_rect(data = gm_av, aes(xmin = mid_date, xmax = lead(mid_date), ymin = 0,
            ymax = Inf, fill = status), alpha = 0.4) +
  geom_line(data = weighted_date, aes(y = work, x = mid_date, linetype = "CoMix"), size = 1) +
  geom_line(data = gm_av, aes(y = workplaces, x = mid_date, linetype = "Google Mobility"), size = 1) +
  geom_vline(data = gm_av, aes(xintercept = ifelse(is.na(special), NA, mid_date)), linetype = 4) +
  geom_text(data = gm_av, aes(x = ymd("2020-04-12"), y = +Inf, vjust = 2, label = "Easter")) +
  geom_text(data = gm_av, aes(x = ymd("2020-08-02"), y = 0, vjust = -2, label = "Summer Hols")) +
  geom_text(data = gm_av, aes(x = ymd("2020-12-20"), y = +Inf, vjust = 2, label = "Xmas/NYE")) +
  geom_text(data = gm_av, aes(x = ymd("2021-04-04"), y = 0, vjust = -2, label = "Easter")) +
  geom_text(data = gm_av, aes(x = ymd("2021-08-08"), y = +Inf, vjust = 2, label = "Summer Hols")) +
  geom_text(data = gm_av, aes(x = ymd("2021-12-26"), y = 0, vjust = -2, label = "Xmas/NYE")) +
  scale_x_date(labels = date_format("%B-%Y")) +
  scale_y_continuous(limits = c(0, 4), breaks = seq(0, 4, by = 0.5),
                     name = "Mean Number of ''Work'' Contacts",
                     sec.axis = sec_axis(trans = ~ .,
                                         name = "Google Mobility ''Workplaces'' Visits")) +
  labs(x = "Date", fill = "Status", linetype = "Data Type") +
  scale_fill_manual(values = c("No restrictions" = "#009E73",
                               "Some restrictions" = "#0072B2",
                               "Lockdown" = "#D55E00"),
                    labels = c("No restrictions", "Some restrictions",
                               "Lockdown")) +
  scale_linetype_manual(values = c("CoMix" = 1, "Google Mobility" = 2)) +
  guides(linetype = guide_legend(keywidth = 3)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

test.plot1

#import contact data
cnts <- qs::qread(file.path(data_path, "part_cnts_other_BE_longer.qs"))
cnts[, sample_type := ifelse(part_age == "[0,1)" | part_age == "[1,4)" | part_age == "[5,11)" | part_age == "[12,15)" | part_age == "[16,17)", "child", "adult")] 
cnts[, date := as.Date(parse_date_time(sday_id, orders = "ymd"))]

#filter out participants of a certain age
cnts <- cnts[sample_type == "adult"]
cnts <- cnts %>% filter(!is.na(part_age))

#order by date
cnts_date <- cnts[base::order(date)]
cnts_date <- cnts_date[date <= ymd("2022-03-02")]

#create data table with subset of variables
num <- cnts_date[, .(date, part_id, part_age, day_weight,
                     work = n_cnt_work, other = n_cnt_other)]
num[, t := as.numeric(date - ymd("2020-01-01"))]

#create study column
num[, study := "CoMix"]

#create second database which shifts the survey rounds and dates
num2 <- rlang::duplicate(num)
num2[, date := date + 7]

#merge the two 
num_merge <- rbind(num, num2) 

#create sequence of dates
date <- seq(as.Date("2020-03-02"), as.Date("2022-03-02"), by = "days")
lockdowns <- as.data.table(as.Date(date))
lockdowns$lockdown_status <- 0
colnames(lockdowns) <- c("date", "status")

#create time intervals for different types of restrictions
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

#add column for special dates
summer1 <- interval(ymd("2020-08-03"), ymd("2020-08-09"))
summer2 <- interval(ymd("2021-08-03"), ymd("2021-08-09"))
summer3 <- interval(ymd("2022-08-03"), ymd("2022-08-09"))
num_merge[, special := ifelse(date == ymd("2020-12-25"), "Xmas/NYE",
                       ifelse(date == ymd("2021-12-31"), "Xmas/NYE",
                       ifelse(date == ymd("2020-04-13"), "Easter",
                       ifelse(date == ymd("2021-04-05"), "Easter",
                       ifelse(date %within% summer1, "Summer Hol",
                       ifelse(date %within% summer2, "Summer Hol",
                       ifelse(date %within% summer3, "Summer Hol", NA)))))))]
num_merge <- num_merge[order(date)]

#get dates in week
week <- unique(as.data.table(as.Date(num_merge$date)))
colnames(week) <- "date"
week <- week[, week := isoweek(date)]

#insert missing dates
misdate <- as.data.table(seq(as.Date("2020-03-23"), as.Date("2022-03-02"), by = "days"))
colnames(misdate) <- "date"
misdate <- merge(misdate, lockdowns, by = "date")
num_merge <- merge(num_merge, misdate, by = c("date", "status"), all = T)

#get middate for fornight periods 
num_merge[, fortnight := paste(isoyear(date), "/", sprintf("%02d", ceiling(isoweek(date)/1)))]
num_merge[, start_date := min(date), by = .(fortnight)]
num_merge[, end_date := max(date), by = .(fortnight)]
num_merge[, mid_date := start_date + floor((end_date - start_date)/1) , by = .(fortnight)]

#get weighted means by week
weighted_date <- num_merge[, .(study, status, special,
                               work = weighted.mean(work, day_weight, na.rm = T),
                               other = weighted.mean(other, day_weight, na.rm = T)),
                           by = .(mid_date)]  
weighted_date <- unique(weighted_date)

#add column for special dates
summer1 <- interval(ymd("2020-08-03"), ymd("2020-08-09"))
summer2 <- interval(ymd("2021-08-03"), ymd("2021-08-09"))
summer3 <- interval(ymd("2022-08-03"), ymd("2022-08-09"))
easter1 <- interval(ymd("2020-04-12"), ymd("2020-04-13"))
easter2 <- interval(ymd("2021-04-04"), ymd("2021-04-05"))
xmas1 <- interval(ymd("2020-12-25"), ymd("2020-12-26"))
xmas2 <- interval(ymd("2021-12-25"), ymd("2021-12-26"))
weighted_date[, special := ifelse(mid_date %within% xmas1, "Xmas/NYE",
                           ifelse(mid_date %within% xmas2, "Xmas/NYE",
                           ifelse(mid_date %within% easter1, "Easter",
                           ifelse(mid_date %within% easter2, "Easter",
                           ifelse(mid_date %within% summer1, "Summer Hol",
                           ifelse(mid_date %within% summer2, "Summer Hol",
                           ifelse(mid_date %within% summer3, "Summer Hol", NA)))))))]

#import mobility data
mob <- qs::qread(file.path(data_path, "google_mob_BE.qs"))

#subset for same date range
mob_sub <- mob[date >= "2020-03-23" & date <= "2022-03-02"]

#duplicate google mobility data and rename columns
gm2 <- rlang::duplicate(mob_sub)
names(gm2) <- stringr::str_replace(names(gm2), "_percent_change_from_baseline", "")
names(gm2) <- stringr::str_replace(names(gm2), "_and", "")

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
mob_merge[, special := ifelse(date == ymd("2020-12-25"), "Xmas/NYE",
                       ifelse(date == ymd("2021-12-31"), "Xmas/NYE",
                       ifelse(date == ymd("2020-04-13"), "Easter",
                       ifelse(date == ymd("2021-04-05"), "Easter", 
                       ifelse(date %within% summer1, "Summer Hol",
                       ifelse(date %within% summer2, "Summer Hol", 
                       ifelse(date %within% summer3, "Summer Hol", NA)))))))]
mob_merge <- mob_merge[order(date)]

#get middate for fortnight periods 
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

#plot with two y-axes
test.plot2 <- ggplot() +
  geom_rect(data = gm_av, aes(xmin = mid_date, xmax = lead(mid_date), ymin = 0,
                              ymax = Inf, fill = status), alpha = 0.4) +
  geom_line(data = weighted_date, aes(y = other, x = mid_date, linetype = "CoMix"), size = 1) +
  geom_line(data = gm_av, aes(y = predictor, x = mid_date, linetype = "Google Mobility"), size = 1) +
  geom_vline(data = gm_av, aes(xintercept = ifelse(is.na(special), NA, mid_date)), linetype = 4) +
  geom_text(data = gm_av, aes(x = ymd("2020-04-12"), y = +Inf, vjust = 2, label = "Easter")) +
  geom_text(data = gm_av, aes(x = ymd("2020-08-02"), y = 0, vjust = -2, label = "Summer Hols")) +
  geom_text(data = gm_av, aes(x = ymd("2020-12-20"), y = +Inf, vjust = 2, label = "Xmas/NYE")) +
  geom_text(data = gm_av, aes(x = ymd("2021-04-04"), y = 0, vjust = -2, label = "Easter")) +
  geom_text(data = gm_av, aes(x = ymd("2021-08-08"), y = +Inf, vjust = 2, label = "Summer Hols")) +
  geom_text(data = gm_av, aes(x = ymd("2021-12-26"), y = 0, vjust = -2, label = "Xmas/NYE")) +
  scale_x_date(labels = date_format("%B-%Y")) +
  scale_y_continuous(limits = c(0, 4), breaks = seq(0, 4, by = 0.5),
                     name = "Mean Number of ''Other'' Contacts",
                     sec.axis = sec_axis(trans = ~ .,
                                         name = "Google Mobility ''Other'' Visits")) +
  labs(x = "Date", fill = "Status", linetype = "Data Type") +
  scale_fill_manual(values = c("No restrictions" = "#009E73",
                               "Some restrictions" = "#0072B2",
                               "Lockdown" = "#D55E00"),
                    labels = c("No restrictions", "Some restrictions",
                               "Lockdown")) +
  scale_linetype_manual(values = c("CoMix" = 1, "Google Mobility" = 2)) +
  guides(linetype = guide_legend(keywidth = 3)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

test.plot2

#plot work and other
ggarrange(test.plot1, test.plot2, common.legend = T,
          legend = "bottom", labels = "AUTO")
