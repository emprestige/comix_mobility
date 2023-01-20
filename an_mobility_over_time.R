##mobility over time 

#load libraries
library(data.table)
library(ggplot2)
library(tidyverse)
library(mgcv)
library(lubridate)
library(cowplot)
library(zoo)
library(matrixStats)

#set cowplot theme
theme_set(cowplot::theme_cowplot(font_size = 10) + theme(strip.background = element_blank()))

#set data path
data_path <-"C:\\Users\\emiel\\Documents\\LSHTM\\Fellowship\\Project\\comix_mobility\\Data\\"

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

#get averages (weekly to start?)
gm_av <- gm2[, .(retail = mean(retail_recreation), 
                 grocery = mean(grocery_pharmacy),
                 parks = mean(parks), 
                 transit = mean(transit_stations),
                 workplaces = mean(workplaces),
                 residential = mean(residential)),
             by = .(week = paste(isoyear(date), "/", isoweek(date)))]
gm_av <- gm_av[order(gm_av$week)]

#get labels for all plots
int <- seq(1, 103, 12)
my_list <- gm_av$week[int]

#plot retail
retail <- ggplot(gm_av) + 
  geom_line(aes(week, retail), group = 1) + 
  guides(x = guide_axis(angle = 90)) + scale_x_discrete(breaks = my_list)
retail 

#plot grocery
grocery <- ggplot(gm_av) +
  geom_line(aes(week, grocery), group = 1) + 
  guides(x = guide_axis(angle = 90)) + scale_x_discrete(breaks = my_list)
grocery

#plot parks
parks <- ggplot(gm_av) + 
  geom_line(aes(week, parks), group = 1) + 
  guides(x = guide_axis(angle = 90)) + scale_x_discrete(breaks = my_list)
parks

#plot transit
transit <- ggplot(gm_av) + 
  geom_line(aes(week, transit), group = 1) + 
  guides(x = guide_axis(angle = 90)) + scale_x_discrete(breaks = my_list)
transit

#workplaces
workplaces <- ggplot(gm_av) +
  geom_line(aes(week, workplaces), group = 1) + 
  guides(x = guide_axis(angle = 90)) + scale_x_discrete(breaks = my_list)
workplaces

#plot residential
residential <- ggplot(gm_av) +
  geom_line(aes(week, residential), group = 1) + 
  guides(x = guide_axis(angle = 90)) + scale_x_discrete(breaks = my_list)
residential

#plot together
plot_grid(retail, grocery, parks, transit, workplaces, residential)

#create smoothed average
mob1 <- gm2
mob2 <- rlang::duplicate(mob1)
mob2[, date := date + 14]
gm2 <- rbind(mob1, mob2)
gm2$date <- as.Date(gm2$date)
gm2 <- gm2[order(date)]

#get averages - fortnightly
gm_av <- gm2[, .(retail = mean(retail_recreation), 
                 grocery = mean(grocery_pharmacy),
                 parks = mean(parks), 
                 transit = mean(transit_stations),
                 workplaces = mean(workplaces),
                 residential = mean(residential)),
             by = .(fortnight = paste(isoyear(date), "/", ceiling(isoweek(date)/2)))]
gm_av <- gm_av[order(gm_av$fortnight)]

#get labels for all plots
int <- seq(1, 53, 6)
my_list <- gm_av$fortnight[int]

#plot retail
retail <- ggplot(gm_av) + 
  geom_line(aes(fortnight, retail), group = 1) + 
  guides(x = guide_axis(angle = 90)) + scale_x_discrete(breaks = my_list)
retail 

#plot grocery
grocery <- ggplot(gm_av) +
  geom_line(aes(fortnight, grocery), group = 1) + 
  guides(x = guide_axis(angle = 90)) + scale_x_discrete(breaks = my_list)
grocery

#plot parks
parks <- ggplot(gm_av) + 
  geom_line(aes(fortnight, parks), group = 1) + 
  guides(x = guide_axis(angle = 90)) + scale_x_discrete(breaks = my_list)
parks

#plot transit
transit <- ggplot(gm_av) + 
  geom_line(aes(fortnight, transit), group = 1) + 
  guides(x = guide_axis(angle = 90)) + scale_x_discrete(breaks = my_list)
transit

#workplaces
workplaces <- ggplot(gm_av) +
  geom_line(aes(fortnight, workplaces), group = 1) + 
  guides(x = guide_axis(angle = 90)) + scale_x_discrete(breaks = my_list)
workplaces

#plot residential
residential <- ggplot(gm_av) +
  geom_line(aes(fortnight, residential), group = 1) + 
  guides(x = guide_axis(angle = 90)) + scale_x_discrete(breaks = my_list)
residential

#plot together
plot_grid(retail, grocery, parks, transit, workplaces, residential)

#create smoothed average
mob1 <- gm2
mob2 <- rlang::duplicate(mob1)
mob2[, date := date + 21]
gm2 <- rbind(mob1, mob2)
gm2$date <- as.Date(gm2$date)
gm2 <- gm2[order(date)]

#get averages - 3 weeks
gm_av <- gm2[, .(retail = mean(retail_recreation), 
                 grocery = mean(grocery_pharmacy),
                 parks = mean(parks), 
                 transit = mean(transit_stations),
                 workplaces = mean(workplaces),
                 residential = mean(residential)),
             by = .(tri_weekly = paste(isoyear(date), "/", ceiling(isoweek(date)/3)))]
gm_av <- gm_av[order(gm_av$tri_weekly)]

#get labels for all plots
int <- seq(1, 36, 4)
my_list <- gm_av$tri_weekly[int]

#plot retail
retail <- ggplot(gm_av) + 
  geom_line(aes(tri_weekly, retail), group = 1) + 
  guides(x = guide_axis(angle = 90)) + scale_x_discrete(breaks = my_list)
retail 

#plot grocery
grocery <- ggplot(gm_av) +
  geom_line(aes(tri_weekly, grocery), group = 1) + 
  guides(x = guide_axis(angle = 90)) + scale_x_discrete(breaks = my_list)
grocery

#plot parks
parks <- ggplot(gm_av) + 
  geom_line(aes(tri_weekly, parks), group = 1) + 
  guides(x = guide_axis(angle = 90)) + scale_x_discrete(breaks = my_list)
parks

#plot transit
transit <- ggplot(gm_av) + 
  geom_line(aes(tri_weekly, transit), group = 1) + 
  guides(x = guide_axis(angle = 90)) + scale_x_discrete(breaks = my_list)
transit

#workplaces
workplaces <- ggplot(gm_av) +
  geom_line(aes(tri_weekly, workplaces), group = 1) + 
  guides(x = guide_axis(angle = 90)) + scale_x_discrete(breaks = my_list)
workplaces

#plot residential
residential <- ggplot(gm_av) +
  geom_line(aes(tri_weekly, residential), group = 1) + 
  guides(x = guide_axis(angle = 90)) + scale_x_discrete(breaks = my_list)
residential

#plot together
plot_grid(retail, grocery, parks, transit, workplaces, residential)
