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

#get fornightly work date
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

work_f = data.table(workplaces = seq(0, 1.25, by = 0.01));
work_f[, work := pmax(0.0, predict(gam1, work_f, type = "response"))]

ggplot(data = mob_v_cnt, aes(workplaces, work)) + geom_point() + 
  geom_smooth(aes(pred))

plw = ggplot(mob_v_cnt) + 
  geom_point(aes(x = workplaces, y = work), col = "red") + 
  geom_line(data = work_f, aes(x = workplaces, y = work)) +
  ylim(0, 3.5) + labs(x = "Google Mobility\n'workplaces' visits", 
                      y = "Work contacts", colour = "Study") +
  theme(legend.position = "none")
plw
