##compare rural and urban areas

#load libraries
library(data.table)
library(ggplot2)
library(tidyverse)
library(lubridate)
library(cowplot)

#set cowplot theme
theme_set(cowplot::theme_cowplot(font_size = 10) + theme(strip.background = element_blank()))

#set data path
data_path <-"C:\\Users\\emiel\\Documents\\LSHTM\\Fellowship\\Project\\comix_mobility\\Data\\"

#import contact data
cnts <- qs::qread(file.path(data_path, "part_cnts.qs"))

#filter out participants of a certain age
cnts <- cnts[part_age >= 18]

#order by date
cnts_date <- cnts[order(date)]

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
F2 <- interval(ymd("2021-07-19"), ymd("2022-03-02"))

#assign value to each type of restriction
lockdowns$status <- ifelse(ymd(lockdowns$date) %within% T1, 1, 
                           ifelse(ymd(lockdowns$date) %within% L1, 2, 
                           ifelse(ymd(lockdowns$date) %within% T2, 1, 
                           ifelse(ymd(lockdowns$date) %within% T3, 1, 
                           ifelse(ymd(lockdowns$date) %within% L2, 2, 
                           ifelse(ymd(lockdowns$date) %within% T4, 1, 
                           ifelse(ymd(lockdowns$date) %within% L3, 2, 
                           ifelse(ymd(lockdowns$date) %within% T5, 1, 0))))))))

#create factor
lockdown_fac <- factor(lockdowns$status, levels = c(0, 1, 2, 3),
                       labels = c("No restrictions", "Some restrictions",
                                  "Lockdown", "Pre-Pandemic"))
lockdowns$status <- lockdown_fac

#create second database which shifts the survey rounds and dates
num2 <- rlang::duplicate(cnts_date)
num2[, date := date + 7]
num2[, survey_round := survey_round + 1]

#merge the two 
num_merge <- rbind(cnts_date, num2) 

#calculate non home contacts
num_merge[, nonhome := n_cnt - n_cnt_home]

#merge contact data and lockdown information
cnts_l <- merge(num_merge, lockdowns, by = "date", all.y = F)

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
gm2[, study := "CoMix"]
gm2 <- gm2[, .(date, study, retail_recreation, grocery_pharmacy, parks, 
               transit_stations, workplaces, residential)]

#get means for google mobility data
gm <- gm2[, .(workplaces = mean(workplaces),
              residential = mean(residential),
              retail = mean(retail_recreation), 
              grocery = mean(grocery_pharmacy), 
              transit = mean(transit_stations),
              parks = mean(parks)),
          by = .(week = paste(year(date), "/", week(date)))]

#get means for different types of contacts
num_filt <- cnts_l[area_rural_urban_code == 1 | area_rural_urban_code == 2]
cnt <- num_filt[, .(status, work = weighted.mean(n_cnt_work, day_weight),
                    other = weighted.mean(n_cnt_other, day_weight),
                    shop = weighted.mean(n_cnt_shop, day_weight), 
                    healthcare = weighted.mean(n_cnt_health_facility, day_weight),
                  public_transport = weighted.mean(n_cnt_public_transport, day_weight),
                  supermarket = weighted.mean(n_cnt_supermarket, day_weight), 
                  bar_rest = weighted.mean(n_cnt_bar_rest, day_weight),
                  outside = weighted.mean(n_cnt_outside, day_weight),
                  nonhome = weighted.mean(nonhome, day_weight)),
                by = .(area = area_rural_urban_code, 
                       week = paste(year(date), "/", week(date)))]

#merge
mob_cnt <- merge(cnt, gm, by = c("week"))
mob_cnt <- unique(mob_cnt)
mob_cnt$area <- factor(mob_cnt$area, labels = c("Urban", "Rural"), levels = c(1, 2))

#plot work
# plw <- ggplot(mob_cnt) + 
#   geom_point(aes(x = workplaces, y = work, colour = status, shape = area)) + 
#   labs(x = "Google Mobility\n'workplaces' visits",
#        y = "Number of work contacts", colour = "Status",
#        shape = "Area Type") 
# plw

plw2 <- ggplot(mob_cnt, aes(workplaces, work, col = status)) + geom_point() +
  labs(x = "Google Mobility\n'workplaces' visits",
       y = "Number of work contacts", colour = "Status") +
  facet_wrap(~ area)
plw2

##other
mob_cnt[, predictor := retail * 0.345 + transit * 0.445 + grocery * 0.210]
# plo <- ggplot(mob_cnt) + 
#   geom_point(aes(x = predictor, y = other, colour = status, shape = area)) + 
#   labs(x = "Google Mobility weighted 'transit stations',\n'retail and recreation', and 'grocery and pharmacy' visits", 
#        y = "Number of 'other' contacts", 
#        colour = "Status", shape = "Area Type")
# plo

plo2 <- ggplot(mob_cnt, aes(predictor, other, col = status)) + geom_point() + 
  labs(x = "Google Mobility weighted 'transit stations',\n'retail and recreation', and 'grocery and pharmacy' visits", 
       y = "Number of 'other' contacts", colour = "Status") +
  facet_wrap(~ area)
plo2

##other comparisons

##public transit
# plt <- ggplot(mob_cnt) + 
#   geom_point(aes(x = transit, y = public_transport, colour = status, shape = area)) + 
#   labs(x = "Google Mobility\n'public transit' visits",
#        y = "Number of 'public transport' contacts", 
#        colour = "Status", shape = "Area Type") 
# plt

plt2 <- ggplot(mob_cnt, aes(transit, public_transport, col = status)) + 
  geom_point() + facet_wrap(~ area) +
  labs(x = "Google Mobility\n'public transit' visits",
       y = "Number of 'public transport' contacts", 
       colour = "Status", shape = "Area Type") 
plt2

##retail and recreation
mob_cnt[, shop_bar_rest := shop + bar_rest]
# plr <- ggplot(mob_cnt) +
#   geom_point(aes(x = retail, y = shop_bar_rest, colour = status, shape = area)) +
#   labs(x = "Google Mobility\n'retail and recreation' visits",
#        y = "Number of 'bar and restaurant'\nand 'shop' contacts",
#        colour = "Status", shape = "Area Type")
# plr

plr2 <- ggplot(mob_cnt, aes(retail, shop_bar_rest, col = status)) + geom_point() +
  labs(x = "Google Mobility\n'retail and recreation' visits",
       y = "Number of 'bar and restaurant'\nand 'shop' contacts",
       colour = "Status") + facet_wrap(~ area)
plr2

##grocery and pharmacy
# plg <- ggplot(mob_cnt) +
#   geom_point(aes(x = grocery, y = supermarket, colour = status, shape = area)) +
#   labs(x = "Google Mobility\n'grocery and pharmarcy' visits",
#        y = "Number of 'supermarket' contacts", 
#        colour = "Status", shape = "Area Type")
# plg

plg2 <- ggplot(mob_cnt, aes(grocery, supermarket, col = status)) +
  geom_point() + facet_wrap(~ area) +
  labs(x = "Google Mobility\n'grocery and pharmarcy' visits",
       y = "Number of 'supermarket' contacts", colour = "Status")
plg2

#combine supermarket and healthcare
mob_cnt[, supermarket_healthcare := supermarket + healthcare]
# plgg <- ggplot(mob_cnt) +
#   geom_point(aes(x = grocery, y = supermarket_healthcare, 
#                  colour = status, shape = area)) +
#   labs(x = "Google Mobility\n'grocery and pharmarcy' visits",
#        y = "Number of 'supermarket' and 'healthcare facility' contacts", 
#        colour = "Status", shape = "Area Type")
# plgg

plgg2 <- ggplot(mob_cnt, aes(grocery, supermarket_healthcare, col = status)) +
  geom_point() + facet_wrap(~ area) +
  labs(x = "Google Mobility\n'grocery and pharmarcy' visits",
       y = "Number of 'supermarket' and 'healthcare facility' contacts", 
       colour = "Status", shape = "Area Type")
plgg2

##parks
# plp <- ggplot(mob_cnt) +
#   geom_point(aes(x = parks, y = outside, colour = status, shape = area)) +
#   labs(x = "Google Mobility\n'parks' visits", colour = "Status",
#        y = "Number of 'outside' contacts", shape = "Area Type")
# plp

plp2 <- ggplot(mob_cnt, aes(parks, outside, colour = status)) + geom_point() +
  labs(x = "Google Mobility\n'parks' visits", colour = "Status",
       y = "Number of 'outside' contacts", shape = "Area Type") +
  facet_wrap(~ area)
plp2

##nonhome
# plh <- ggplot(mob_cnt, aes(residential, nonhome, colour = status)) +
#   geom_point() + labs(x = "Google Mobility\n time at 'residential' location",
#                       colour = "Status", shape = "Area Type",
#                       y = "Number of non-home contacts")
# plh

plh2 <- ggplot(mob_cnt, aes(residential, nonhome, colour = status)) +
  geom_point() + labs(x = "Google Mobility\n time at 'residential' location",
                      colour = "Status", y = "Number of non-home contacts") +
  facet_wrap(~ area)
plh2
