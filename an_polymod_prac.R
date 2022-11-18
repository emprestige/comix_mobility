##POLYMOD practice

#load libraries
library(data.table)
library(ggplot2)
library(tidyverse)
library(mgcv)
library(lubridate)
library(cowplot)

#set cowplot theme
theme_set(cowplot::theme_cowplot(font_size = 10) + theme(strip.background = element_blank()))

#set data path
data_path <-"C:\\Users\\emiel\\Documents\\LSHTM\\Fellowship\\Project\\comix_mobility\\Data\\"

#import mobility data
mob <- qs::qread(file.path(data_path, "google_mob.qs"))

#subset for same date range
mob_sub <- mob[date >= "2020-03-23" & date <= "2022-03-02"]

#import contact data
cnts <- qs::qread(file.path(data_path,"part_cnts.qs"))

#filter out participants of a certain age
cnts <- cnts[part_age >= 18 & part_age <= 65]

#order by date
cnts_date <- cnts[order(date)]

#create data table with subset of variables
num <- cnts_date[, .(date, part_id, part_age, home = n_cnt_home, 
                work = n_cnt_work, school = n_cnt_school, other = n_cnt_other)]
num[, t := as.numeric(date - ymd("2020-01-01"))]

#duplicate google mobility data and rename columns
gm2 <- rlang::duplicate(mob_sub)
names(gm2) <- str_replace(names(gm2), "_percent_change_from_baseline", "")
names(gm2) <- str_replace(names(gm2), "_and", "")

#merge data tables 
num <- merge(num, gm2[, 9:15], by = "date", all.x = T)

#create study column
num[, study := "CoMix"]

#import edited polymod data
pnum <- qs::qread(file.path(data_path, "polymod.qs"))

#create study column
pnum[, study := "POLYMOD"]

#bind the rows together
num <- rbind(num, pnum, fill = TRUE)

#turn mobility data into decimals instead of percentages
num[, retail_recreation := (100 + retail_recreation) * 0.01]
num[, grocery_pharmacy  := (100 + grocery_pharmacy ) * 0.01]
num[, parks             := (100 + parks            ) * 0.01]
num[, transit_stations  := (100 + transit_stations ) * 0.01]
num[, workplaces        := (100 + workplaces       ) * 0.01]
num[, residential       := (100 + residential      ) * 0.01]

#remove participants of certain age from POLYMOD
num <- rbind(
  num[study == "CoMix"],
  num[study == "POLYMOD" & part_age >= 18 & part_age <= 65]
)

#get fortnightly work data
another <- num[, .(work = mean(work), workplaces = mean(workplaces),
                  transit = mean(transit_stations)), 
              by = .(week = ifelse(study == "CoMix", paste(year(date), "/", 
                                                     ceiling(week(date)/2)), 
                                   rep(0, length(date))), study)]

#model using GAM
model <- gam(work ~ s(workplaces), family = gaussian, data = another)
gam.check(model)

#predict using 'new' data
work_f <- data.table(workplaces = seq(0, 1.25, by = 0.01));
work_f[, work := pmax(0.0, predict(model, work_f, type = "response"))]

#plot
plw <- ggplot(another) + 
  geom_point(aes(x = workplaces, y = work, colour = study)) + 
  geom_line(data = work_f, aes(x = workplaces, y = work)) + ylim(0, 3.5) +
  labs(x = "Google Mobility\n'workplaces' visits", y = "Work contacts", 
       colour = "Study") +
  theme(legend.position = "none")
plw

#get fortnightly 'other' data
another <- num[, .(other = mean(other), retail = mean(retail_recreation), 
              grocery = mean(grocery_pharmacy), transit = mean(transit_stations)), 
              by = .(week = ifelse(study == "CoMix", paste(year(date), "/", 
                                                     ceiling(week(date)/2)), 
                                   rep(0, length(date))), study)]

#create predictor from weighting of variables
another[, predictor := retail * 0.345 + transit * 0.445 + grocery * 0.210] # See "optimisation" below for how this was arrived at

#model using GAM
model <- gam(other ~ s(predictor), family = gaussian, data = another)

#predict using 'new' data
other_f <- data.table(predictor = seq(0, 1.25, by = 0.01));
other_f[, other := pmax(0.0, predict(model, other_f, type = "response"))]

#plot
plo <- ggplot(another) + 
  geom_point(aes(x = predictor, y = other, colour = study)) + 
  geom_line(data = other_f, aes(x = predictor, y = other)) +
  xlim(0, 1.25) + ylim(0, 5) + 
  labs(x = "Google Mobility weighted 'transit stations',\n'retail and recreation', and 'grocery and pharmacy' visits", 
       y = "Other contacts", colour = "Study") +
  theme(legend.position = "none")
plo
