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
  
#test out models
gam1 <- gam(work ~ s(workplaces), data = mob_v_cnt)
summary(gam1)
plot(gam1, pages = 1)
pred <- predict(gam1)
mob_v_cnt$pred <- pred

#predict using 'new' data
work_f = data.table(workplaces = seq(0, 1.25, by = 0.01));
work_f[, work := pmax(0.0, predict(gam1, work_f, type = "response"))]

#plot
#ggplot(data = mob_v_cnt, aes(workplaces, work)) + geom_point() + 
#  geom_smooth(aes(pred))

plw = ggplot(mob_v_cnt) + 
  geom_point(aes(x = workplaces, y = work), col = "red") + 
  geom_line(data = work_f, aes(x = workplaces, y = work)) +
  ylim(0, 3.5) + labs(x = "Google Mobility\n'workplaces' visits", 
                      y = "Work contacts", colour = "Study") +
  theme(legend.position = "none")
plw


##can the same be done with home data??

#get fornightly home data
cnt_sub <- cnts_date[, fortnight := paste(year(date), "/", ceiling(week(date)/2))]
home <- cnt_sub[, .(mean_cnt_home_f = mean(n_cnt_home)), by = fortnight]
frac_sub <- mob_sub[, fortnight  := paste(year(date), "/", ceiling(week(date)/2))]
residences <- frac_sub[, .(mean_home_f = mean(1 + (residential_percent_change_from_baseline)/100)), by = fortnight]
mob_v_cnt2 <- merge(home, residences, by = "fortnight", all.x = F)
colnames(mob_v_cnt2) <- c("fortnight", "home", "residences")

#test out models
gam2 <- gam(home ~ s(residences), data = mob_v_cnt2)
summary(gam2)
plot(gam2, pages = 1)
pred <- predict(gam2)
mob_v_cnt2$pred <- pred

#predict using 'new' data
home_f = data.table(residences = seq(0.8, 2, by = 0.01));
home_f[, home := pmax(0.0, predict(gam2, home_f, type = "response"))]

#plot
#ggplot(data = mob_v_cnt2, aes(residences, home)) + geom_point() + 
#  geom_smooth(aes(pred))

plw2 = ggplot(mob_v_cnt2) + 
  geom_point(aes(x = residences, y = home), col = "red") + 
  geom_line(data = home_f, aes(x = residences, y = home)) +
  ylim(0, 3.5) + labs(x = "Google Mobility\n'residential' visits", 
                      y = "Home contacts", colour = "Study") +
  theme(legend.position = "none")
plw2
