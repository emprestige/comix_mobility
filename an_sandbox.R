##sandbox

#load libraries
library(ggplot2)
library(tidyverse)
library(cowplot) 

#set data path
data_path <-"C:\\Users\\emiel\\Documents\\LSHTM\\Fellowship\\Project\\comix_mobility\\Data\\"

#import contact data
cnts <- qs::qread(file.path(data_path,"part_cnts.qs"))
                  
#import mobility data
mob <- qs::qread(file.path(data_path, "google_mob.qs"))

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

##calculate mean contacts for each survey round 

#mean contacts for all, home, and work -- easily comparable to google mobility
all <-  cnts[, mean(n_cnt),      by = substr(part_wave_uid, 4, 6)]
home <- cnts[, mean(n_cnt_home), by = substr(part_wave_uid, 4, 6)]
work <- cnts[, mean(n_cnt_work), by = substr(part_wave_uid, 4, 6)]

##calculate mean baseline change for each week
mob_sub <- mob[date >= "2020-03-24" & date <= "2022-03-02"]
g_home <- mob_sub[, mean(residential_percent_change_from_baseline), 
                  by = list(yw = paste(year(date), week(date)))]
g_work <- mob_sub[, mean(workplaces_percent_change_from_baseline),
                  by = list(yw = paste(year(date), week(date)))]

##visualisation - contact data 

#workplaces 
ggplot(data = work, aes(substr, V1)) + geom_line(group = 1)

#homes
ggplot(data = home, aes(substr, V1)) + geom_line(group = 1)

##visualisation - combination
w1 <- ggplot(data = g_work, aes(yw, V1)) + geom_line(group = 1)
w2 <- ggplot(data = work, aes(substr, V1)) + geom_line(group = 1)

plot_grid(w1, w2)

h1 <- ggplot(data = g_home, aes(yw, V1)) + geom_line(group = 1)
h2 <- ggplot(data = home, aes(substr, V1)) + geom_line(group = 1)

plot_grid(h1, h2)
