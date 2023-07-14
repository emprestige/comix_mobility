##scaling factors for the UK 

#load libraries
library(data.table)
library(tidyverse)
library(lubridate)

#set data path
data_path <-"C:\\Users\\emiel\\Documents\\LSHTM\\Fellowship\\Project\\comix_mobility\\Data\\"

#import mobility data
mob <- qs::qread(file.path(data_path, "google_mob.qs"))

#subset for same date range
mob_sub <- mob[date >= "2020-03-23" & date <= "2021-03-31"]

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

#save scaling factors 
qs::qsave(ests, file.path(data_path, "scaling_factors_UK_fortnightly_filtered_middate.qs"))
