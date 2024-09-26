##scaling factors for the BE

#load libraries
library(data.table)
library(dplyr)
library(lubridate)
library(here)
library(stringr)

#set data path
data_path <- here("data")

#import mobility data
mob <- qs::qread(file.path(data_path, "google_mob_BE.qs"))

#subset for same date range
comix <- qs::qread(file.path(data_path, "participants_BE.qs"))
comix_longer <- qs::qread(file.path(data_path, "participants_BE_longer.qs"))
comix_longer <- comix_longer[, date := as.Date(parse_date_time(sday_id, orders = "ymd"))]
comix_longer <- comix_longer[date > "2021-03-04"]
comix_longer[, sday_id := NULL]
comix_longer[, weekday := ifelse(dayofweek == 0, "Monday",
                         ifelse(dayofweek == 1, "Tuesday",
                         ifelse(dayofweek == 2, "Wednesday",
                         ifelse(dayofweek == 3, "Thursday",
                         ifelse(dayofweek == 4, "Friday",
                         ifelse(dayofweek == 5, "Saturday", "Sunday"))))))]
comix_longer[, dayofweek := NULL]
comix_longer[, day := NULL]
comix_longer[, hh_id := NULL]
comix_names <- names(comix_longer)
comix <- comix[, ..comix_names]
comix_all <- rbind(comix, comix_longer)
dates <- comix_all$date
mob_sub1 <- mob[date %in% dates]

#duplicate google mobility data and rename columns
gm2 <- rlang::duplicate(mob_sub1)
names(gm2) <- str_replace(names(gm2), "_percent_change_from_baseline", "")
names(gm2) <- str_replace(names(gm2), "_and", "")

#turn mobility data into decimals instead of percentages
gm2[, retail_recreation := (100 + retail_recreation) * 0.01]
gm2[, grocery_pharmacy  := (100 + grocery_pharmacy ) * 0.01]
gm2[, parks             := (100 + parks            ) * 0.01]
gm2[, transit_stations  := (100 + transit_stations ) * 0.01]
gm2[, workplaces        := (100 + workplaces       ) * 0.01]
gm2[, residential       := (100 + residential      ) * 0.01]

#get middate for fornight periods 
gm2[, fortnight := paste(isoyear(date), "/", sprintf("%02d", ceiling(isoweek(date)/2)))]
gm2[, start_date := min(date), by = .(fortnight)]
gm2[, end_date := max(date), by = .(fortnight)]
gm2[, mid_date := start_date + floor((end_date - start_date)/2) , by = .(fortnight)]

#get means for google mobility data
gm <- gm2[, .(workplaces = mean(workplaces),
              residential = mean(residential),
              retail = mean(retail_recreation), 
              grocery = mean(grocery_pharmacy), 
              transit = mean(transit_stations),
              parks = mean(parks)), by = .(mid_date)]
gm[, study := "CoMix"]

#create predictor for 'other' contacts
gm[, predictor := retail * 0.333 + transit * 0.334 + grocery * 0.333]

#calculate scaling factor
ests <- rlang::duplicate(gm)
ests <- ests[, .(mid_date, work_mob = workplaces, other_mob = predictor)]
ests[, work_mob2 := work_mob**2]
ests[, other_mob2 := other_mob**2]
ests[, work_scaling_fac_lin := (-0.2273 + 1.3280*work_mob)/1.9529120]
ests[, work_scaling_fac_quad := (1.3169 -4.7718*work_mob + 5.7062*work_mob2)/1.9529120]
ests[, other_scaling_fac_lin := (-0.04875 + 1.03978*other_mob)/3.4837340]
ests[, other_scaling_fac_quad := (2.9441 - 9.2762*other_mob + 8.5566*other_mob2)/3.4837340]

ests1 <- ests

#now for new subset
mob_sub2 <- mob[date >= "2020-04-16" & date <= "2022-03-31"]

#duplicate google mobility data and rename columns
gm2 <- rlang::duplicate(mob_sub2)
names(gm2) <- str_replace(names(gm2), "_percent_change_from_baseline", "")
names(gm2) <- str_replace(names(gm2), "_and", "")

#turn mobility data into decimals instead of percentages
gm2[, retail_recreation := (100 + retail_recreation) * 0.01]
gm2[, grocery_pharmacy  := (100 + grocery_pharmacy ) * 0.01]
gm2[, parks             := (100 + parks            ) * 0.01]
gm2[, transit_stations  := (100 + transit_stations ) * 0.01]
gm2[, workplaces        := (100 + workplaces       ) * 0.01]
gm2[, residential       := (100 + residential      ) * 0.01]

#get middate for fornight periods 
gm2[, fortnight := paste(isoyear(date), "/", sprintf("%02d", ceiling(isoweek(date)/2)))]
gm2[, start_date := min(date), by = .(fortnight)]
gm2[, end_date := max(date), by = .(fortnight)]
gm2[, mid_date := start_date + floor((end_date - start_date)/2) , by = .(fortnight)]

#get means for google mobility data
gm <- gm2[, .(workplaces = mean(workplaces),
              residential = mean(residential),
              retail = mean(retail_recreation), 
              grocery = mean(grocery_pharmacy), 
              transit = mean(transit_stations),
              parks = mean(parks)), by = .(mid_date)]
gm[, study := "CoMix"]

#create predictor for 'other' contacts
gm[, predictor := retail * 0.333 + transit * 0.334 + grocery * 0.333]

#calculate scaling factor
ests <- rlang::duplicate(gm)
ests <- ests[, .(mid_date, work_mob = workplaces, other_mob = predictor)]
ests[, work_mob2 := work_mob**2]
ests[, other_mob2 := other_mob**2]
ests[, work_scaling_fac_lin := (-0.2273 + 1.3280*work_mob)/1.9529120]
ests[, work_scaling_fac_quad := (1.3169 -4.7718*work_mob + 5.7062*work_mob2)/1.9529120]
ests[, other_scaling_fac_lin := (-0.04875 + 1.03978*other_mob)/3.4837340]
ests[, other_scaling_fac_quad := (2.9441 - 9.2762*other_mob + 8.5566*other_mob2)/3.4837340]

ests2 <- ests

int <- interval(ymd("2020-08-15"), ymd("2020-12-07"))
ests3 <- rlang::duplicate(ests2)
ests3 <- ests3[mid_date %within% int, ]

ests4 <- full_join(ests1, ests3, by = colnames(ests1))
ests4 <- ests4[order(ests4$mid_date)]

#save scaling factors 
qs::qsave(ests4, file.path(data_path, "scaling_factors_BE_fortnightly_filtered_middate_all.qs"))
