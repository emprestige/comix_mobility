##GAM practice

#load libraries
library(data.table)
library(ggplot2)
library(tidyverse)
library(mgcv)

#set data path
data_path <-"C:\\Users\\emiel\\Documents\\LSHTM\\Fellowship\\Project\\comix_mobility\\Data\\"

#import mobility data
mob <- qs::qread(file.path(data_path, "google_mob.qs"))

#subset for same date range
mob_sub <- mob[date >= "2020-03-24" & date <= "2022-03-02"]

#import contact data
cnts <- qs::qread(file.path(data_path,"part_cnts.qs"))

#order by date
cnts_date <- cnts[order(date)]

#get fornightly work data
cnt_sub <- cnts_date[, fortnight := paste(year(date), "/", ceiling(week(date)/2))]
work <- cnt_sub[, .(mean_cnt_work_f = mean(n_cnt_work)), by = fortnight]
frac_sub <- mob_sub[, fortnight  := paste(year(date), "/", ceiling(week(date)/2))]
workplaces <- frac_sub[, .(mean_work_f = mean(1 + (workplaces_percent_change_from_baseline)/100)), by = fortnight]
mob_v_cnt <- merge(work, workplaces, by = "fortnight", all.x = F)
colnames(mob_v_cnt) <- c("fortnight", "work", "workplaces")
  
#model using GAM
gam1 <- gam(work ~ s(workplaces), data = mob_v_cnt)

#predict using 'new' data
work_f = data.table(workplaces = seq(0, 1.25, by = 0.01));
work_f[, work := pmax(0.0, predict(gam1, work_f, type = "response"))]

#plot
plw = ggplot(mob_v_cnt) + 
  geom_point(aes(x = workplaces, y = work, col = "red")) + 
  geom_line(data = work_f, aes(x = workplaces, y = work)) +
  ylim(0, 3.5) + labs(x = "Google Mobility\n'workplaces' visits", 
                      y = "Work contacts") +
  theme(legend.position = "none")
plw


##can the same be done with home data??

#get fornightly home data
home <- cnt_sub[, .(mean_cnt_home_f = mean(n_cnt_home)), by = fortnight]
frac_sub <- mob_sub[, fortnight  := paste(year(date), "/", ceiling(week(date)/2))]
residences <- frac_sub[, .(mean_home_f = mean(1 + (residential_percent_change_from_baseline)/100)), by = fortnight]
mob_v_cnt2 <- merge(home, residences, by = "fortnight", all.x = F)
colnames(mob_v_cnt2) <- c("fortnight", "home", "residences")

#model using GAM
gam2 <- gam(home ~ s(residences), data = mob_v_cnt2)

#predict using 'new' data
home_f = data.table(residences = seq(0, 1.25, by = 0.01));
home_f[, home := pmax(0.0, predict(gam2, home_f, type = "response"))]

#plot
plw2 = ggplot(mob_v_cnt2) + 
  geom_point(aes(x = residences, y = home, col = "red")) + 
  geom_line(data = home_f, aes(x = residences, y = home)) +
  ylim(0, 3.5) + labs(x = "Google Mobility\n'residential' visits", 
                      y = "Home contacts") +
  theme(legend.position = "none")
plw2

## weighted function for 'transit stations', 'retail and recreation', and 
## 'grocery and pharmacy' visits

#get fortnightly 'other' data
other <- cnt_sub[, .(mean_cnt_other_f = mean(n_cnt_other)), by = fortnight]
transit <- frac_sub[, .(mean_transit_f = mean(1 + (transit_stations_percent_change_from_baseline)/100)), by = fortnight]
grocery_pharmacy <- frac_sub[, .(mean_gracery_f = mean(1 + (grocery_and_pharmacy_percent_change_from_baseline)/100)), by = fortnight]
retail_recreation <- frac_sub[, .(mean_retail_f = mean(1 + (retail_and_recreation_percent_change_from_baseline)/100)), by = fortnight]
mob_v_cnt3 <- merge(other, transit, by = "fortnight", all.x = F)
mob_v_cnt3 <- merge(mob_v_cnt3, grocery_pharmacy, by = "fortnight", all.x = F)
mob_v_cnt3 <- merge(mob_v_cnt3, retail_recreation, by = "fortnight", all.x = F)
colnames(mob_v_cnt3) <- c("fortnight", "other","transit", "grocery_pharmacy", "retail_recreation")

#create weighted predictor
mob_v_cnt3[, predictor := retail_recreation * 0.345 + transit * 0.445 + grocery_pharmacy * 0.210] # See "optimisation" below for how this was arrived at

#model using GAM
model = gam(other ~ s(predictor), family = gaussian, data = mob_v_cnt3)
other_f = data.table(predictor = seq(0, 1.25, by = 0.01));
other_f[, other := pmax(0.0, predict(model, other_f, type = "response"))]

plo = ggplot(mob_v_cnt3) + 
  geom_point(aes(x = predictor, y = other, col = "red")) + 
  geom_line(data = other_f, aes(x = predictor, y = other)) +
  xlim(0, 1.25) + ylim(0, 5) + labs(x = "Google Mobility weighted 'transit stations',\n'retail and recreation', and 'grocery and pharmacy' visits",
                                    y = "Other contacts") +
  theme(legend.position = "none")
plo
