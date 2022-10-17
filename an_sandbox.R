##sandbox

library(ggplot2)
 
data_path <-"C:\\Users\\emiel\\OneDrive\\Documents\\LSHTM\\Fellowship\\Project\\Data\\"

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

##visualisation - contact data

ggplot(data = )