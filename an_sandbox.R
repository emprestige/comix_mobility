##sandbox

#load libraries
library(ggplot2)
library(tidyverse)
library(cowplot) 
library(ggpubr)

#set data path
data_path <-"C:\\Users\\emiel\\Documents\\LSHTM\\Fellowship\\Project\\comix_mobility\\Data\\"

#import contact data
cnts <- qs::qread(file.path(data_path,"part_cnts.qs"))

#order by date
#cnts_date <- cnts[order(date)]
cnts_date <- cnts %>%
  arrange(date)

                  
#import mobility data
mob <- qs::qread(file.path(data_path, "google_mob.qs"))

#subset for same date range
mob_sub <- mob[date >= "2020-03-24" & date <= "2022-03-02"]

##visualisation - google mobility data

#retail and recreation
ggplot(data = mob, aes(date, retail_and_recreation_percent_change_from_baseline)) + 
 geom_line(group = 1) 

#grocery and pharmacy
ggplot(data = mob, aes(date, grocery_and_pharmacy_percent_change_from_baseline)) + 
  geom_line(group = 1)

#parks
ggplot(data = mob, aes(date, parks_percent_change_from_baseline)) + 
  geom_line(group = 1)

#transit stations
ggplot(data = mob, aes(date, transit_stations_percent_change_from_baseline)) + 
  geom_line(group = 1)

#workplaces
ggplot(data = mob, aes(date, workplaces_percent_change_from_baseline)) + 
  geom_line(group = 1)

#residential
ggplot(data = mob, aes(date, residential_percent_change_from_baseline)) + 
  geom_line(group = 1)

##calculate mean contacts 

#daily
home_d <- cnts_date[, mean(n_cnt_home), by = date]
work_d <- cnts_date[, mean(n_cnt_work), by = date]

#weekly
home_w <- cnts_date[, mean(n_cnt_home), by = paste(year(date), "/", week(date))]
work_w <- cnts_date[, mean(n_cnt_work), by = paste(year(date), "/", week(date))]

#fortnightly
home_f <- cnts_date[, mean(n_cnt_home), 
                    by = paste(year(date), "/", ceiling(week(date)/2))]
work_f <- cnts_date[, mean(n_cnt_work), 
                    by = paste(year(date), "/", ceiling(week(date)/2))]

#survey round
home_surv <- cnts[, mean(n_cnt_home), by = survey_round]
work_surv <- cnts[, mean(n_cnt_work), by = survey_round]

##calculate mean changes in baseline

#weekly
g_home_w  <- mob_sub[, mean(residential_percent_change_from_baseline), 
                  by = list(yw = paste(year(date), "/", week(date)))]
g_work_w  <- mob_sub[, mean(workplaces_percent_change_from_baseline),
                  by = list(yw = paste(year(date), "/", week(date)))]

#fortnightly
g_home_f <- mob_sub[, mean(residential_percent_change_from_baseline), 
                  by = list(yw = paste(year(date), "/", ceiling(week(date)/2)))]
g_work_f <- mob_sub[, mean(workplaces_percent_change_from_baseline),
                  by = list(yw = paste(year(date), "/", ceiling(week(date))/2))]

##visualisation - contact data 

#workplaces 
ggplot(data = work_d, aes(date, V1)) + geom_line(group = 1)

#homes
ggplot(data = home_d, aes(date, V1)) + geom_line(group = 1)

##visualisation - combination

#workplace daily
w1 <- ggplot(data = mob_sub, aes(date, workplaces_percent_change_from_baseline)) + 
  geom_line(group = 1)
w2 <- ggplot(data = work_d, aes(date, V1)) + geom_line(group = 1)
plot_grid(w1, w2)

#workplace weekly
w3 <- ggplot(data = g_work_w, aes(yw, V1)) + geom_line(group = 1)
w4 <- ggplot(data = work_w, aes(paste, V1)) + geom_line(group = 1)
plot_grid(w3, w4)

#workplace fortnightly
w5 <- ggplot(data = g_work_f, aes(yw, V1)) + geom_line(group = 1)
w6 <- ggplot(data = work_f, aes(paste, V1)) + geom_line(group = 1)
plot_grid(w5, w6)

#home daily
h1 <- ggplot(data = mob_sub, aes(date, residential_percent_change_from_baseline)) + 
  geom_line(group = 1)
h2 <- ggplot(data = home_d, aes(date, V1)) + geom_line(group = 1)
plot_grid(h1, h2)

#home weekly
h3 <- ggplot(data = g_home_w, aes(yw, V1)) + geom_line(group = 1)
h4 <- ggplot(data = home_w, aes(paste, V1)) + geom_line(group = 1)
plot_grid(h3, h4)

#home fortnightly
h5 <- ggplot(data = g_home_f, aes(yw, V1)) + geom_line(group = 1)
h6 <- ggplot(data = home_f, aes(paste, V1)) + geom_line(group = 1)
plot_grid(h5, h6)

##visualisation - recreating figures from lancet supplementary material

#get fraction changes
graph <- c("date", "week", "fortnight", "frac_change_res", "frac_change_work",
           "mean_res_w", "mean_work_w", "mean_res_f", "mean_work_f")
frac <- mob_sub[, week  := paste(year(date), "/", week(date))]
frac[, fortnight        := paste(year(date), "/", ceiling(week(date)/2))]
frac[, frac_change_res  := 1 + (residential_percent_change_from_baseline)/100]
frac[, frac_change_work := 1 + (workplaces_percent_change_from_baseline)/100]
frac[, mean_res_w       := mean(frac_change_res),  by = week]
frac[, mean_work_w      := mean(frac_change_work), by = week]
frac[, mean_res_f       := mean(frac_change_res),  by = fortnight]
frac[, mean_work_f      := mean(frac_change_work), by = fortnight]
frac <- frac[, ..graph]

#subset contacts
graph2 <- c("date", "week", "fortnight", "mean_cnt_home", "mean_cnt_work", 
            "mean_cnt_home_w", "mean_cnt_work_w", "mean_cnt_home_f", 
            "mean_cnt_work_f")
cnt_sub <- cnts_date[, week := paste(year(date), "/", week(date))]
cnt_sub[, fortnight         := paste(year(date), "/", ceiling(week(date)/2))]
cnt_sub[, mean_cnt_home     := mean(n_cnt_home), by = date]
cnt_sub[, mean_cnt_work     := mean(n_cnt_work), by = date]
cnt_sub[, mean_cnt_home_w   := mean(n_cnt_home), by = week]
cnt_sub[, mean_cnt_work_w   := mean(n_cnt_work), by = week]
cnt_sub[, mean_cnt_home_f   := mean(n_cnt_home), by = fortnight]
cnt_sub[, mean_cnt_work_f   := mean(n_cnt_work), by = fortnight]
cnt_sub <- unique(cnts_date[, ..graph2])

#merge 
mob_v_cnt   <- merge(frac, cnt_sub, by = c("date", "week", "fortnight"),
                     all.x = F)

#plot workplace daily
ggplot(data = mob_v_cnt, aes(frac_change_work, mean_cnt_work)) + geom_point()

#plot workplace weekly 
ggplot(data = mob_v_cnt, aes(mean_work_w, mean_cnt_work_w)) + geom_point()

#plot workplace fortnightly
ggplot(data = mob_v_cnt, aes(mean_work_f, mean_cnt_work_f)) + geom_point()

#plot home daily
ggplot(data = mob_v_cnt, aes(frac_change_res, mean_cnt_home)) + geom_point()

#plot home weekly
ggplot(data = mob_v_cnt, aes(mean_res_w, mean_cnt_home_w)) + geom_point()

#plot home fortnightly
ggplot(data = mob_v_cnt, aes(mean_res_f, mean_cnt_home_f)) + geom_point()

##using the same date range as lancet article 

#subset data
mob_v_cnt_sub <- mob_v_cnt[date >= "2020-03-24" & date <= "2020-12-31"]

#plot workplace daily
ggplot(data = mob_v_cnt_sub, aes(frac_change_work, mean_cnt_work)) + geom_point()

#plot workplace weekly 
ggplot(data = mob_v_cnt_sub, aes(mean_work_w, mean_cnt_work_w)) + geom_point()

#plot workplace fortnightly
ggplot(data = mob_v_cnt_sub, aes(mean_work_f, mean_cnt_work_f)) + geom_point()

#plot home daily
ggplot(data = mob_v_cnt_sub, aes(frac_change_res, mean_cnt_home)) + geom_point()

#plot home weekly
ggplot(data = mob_v_cnt_sub, aes(mean_res_w, mean_cnt_home_w)) + geom_point()

#plot home fortnightly
ggplot(data = mob_v_cnt_sub, aes(mean_res_f, mean_cnt_home_f)) + geom_point()