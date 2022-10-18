##sandbox

#load libraries
library(ggplot2)
library(tidyverse)
 
#set daa path
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

#survey rounds
#ans <- cnts[substr(part_wave_uid, 3, 4) == , m_all := mean(n_cnt)]
