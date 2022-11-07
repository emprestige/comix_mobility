##circuit breaker figure practice 

#load libraries
library(data.table)
library(ggplot2)
library(tidyverse)
library(lubridate)
library(zoo)
library(mgcv)
library(stringr)
library(cowplot)

#set data path
data_path <-"C:\\Users\\emiel\\Documents\\LSHTM\\Fellowship\\Project\\comix_mobility\\Data\\"

#set cowplot theme
theme_set(cowplot::theme_cowplot(font_size = 10) + theme(strip.background = element_blank()))

#load mobility data 
gm <- qs::qread(file.path(data_path, "google_mob_region.qs"))
gm[, goog_name := ifelse(sub_region_2 != "", paste0(sub_region_2, ", ", sub_region_1),
                    sub_region_1)];
gm <- gm[goog_name != ""]

#load region data
regions <- fread(file.path(data_path, "goog_regions.txt"))

#merge 
gm2 <- merge(gm, regions, by = "goog_name", all.x = T, allow.cartesian = T)
gm3 <- gm2[!duplicated(gm2[, .(goog_name, date, residential_percent_change_from_baseline,
                        workplaces_percent_change_from_baseline, 
                        transit_stations_percent_change_from_baseline,
                        grocery_and_pharmacy_percent_change_from_baseline,
                        parks_percent_change_from_baseline,
                        retail_and_recreation_percent_change_from_baseline)], fromLast = T)]
gm3[, country := str_sub(gss_code, 1, 1)]

#Northern Ireland
gm_ni <- gm3[country == "N"]
gm_ni <- gm_ni[, .(country, grocery_and_pharmacy_mean = weighted.mean(grocery_and_pharmacy_percent_change_from_baseline, population, na.rm = T),
                   residential_mean = weighted.mean(residential_percent_change_from_baseline, population, na.rm = T),
                   retail_and_recreation_mean = weighted.mean(retail_and_recreation_percent_change_from_baseline, population, na.rm = T),
                   transit_stations_mean = weighted.mean(transit_stations_percent_change_from_baseline, population, na.rm = T),
                   workplaces_mean = weighted.mean(workplaces_percent_change_from_baseline, population, na.rm = T)), keyby = date]
gm_ni <- unique(gm_ni)
gm_ni <- gm_ni[date %between% c(ymd("2020-09-26", "2020-11-14"))]
gm_ni$days <- c(-21:28)
#gm_ni <- gm_ni[,-1]

# #Scotland
# gm_s <- gm3[country == "S"]
# gm_s <- gm_s[, .(country, grocery_and_pharmacy_mean = weighted.mean(grocery_and_pharmacy_percent_change_from_baseline, population, na.rm = T),
#                    residential_mean = weighted.mean(residential_percent_change_from_baseline, population, na.rm = T),
#                    retail_and_recreation_mean = weighted.mean(retail_and_recreation_percent_change_from_baseline, population, na.rm = T),
#                    transit_stations_mean = weighted.mean(transit_stations_percent_change_from_baseline, population, na.rm = T),
#                    workplaces_mean = weighted.mean(workplaces_percent_change_from_baseline, population, na.rm = T)), keyby = date]
# gm_s <- unique(gm_s)
# gm_s <- gm_s[date %between% c(ymd("2020-09-19", "2020-11-07"))]
# gm_s$days <- c(-21:28)
# #gm_s <- gm_s[, -1]

#Wales
gm_w <- gm3[country == "W"]
gm_w <- gm_w[, .(country, grocery_and_pharmacy_mean = weighted.mean(grocery_and_pharmacy_percent_change_from_baseline, population, na.rm = T),
                 residential_mean = weighted.mean(residential_percent_change_from_baseline, population, na.rm = T),
                 retail_and_recreation_mean = weighted.mean(retail_and_recreation_percent_change_from_baseline, population, na.rm = T),
                 transit_stations_mean = weighted.mean(transit_stations_percent_change_from_baseline, population, na.rm = T),
                 workplaces_mean = weighted.mean(workplaces_percent_change_from_baseline, population, na.rm = T)), keyby = date]
gm_w <- unique(gm_w)
gm_w <- gm_w[date %between% c(ymd("2020-10-03", "2020-11-21"))]
gm_w$days <- c(-21:28)
#gm_w <- gm_w[, -1]

#England
gm_e <- gm3[country == "E"]
gm_e <- gm_e[, .(country, grocery_and_pharmacy_mean = weighted.mean(grocery_and_pharmacy_percent_change_from_baseline, population, na.rm = T),
                 residential_mean = weighted.mean(residential_percent_change_from_baseline, population, na.rm = T),
                 retail_and_recreation_mean = weighted.mean(retail_and_recreation_percent_change_from_baseline, population, na.rm = T),
                 transit_stations_mean = weighted.mean(transit_stations_percent_change_from_baseline, population, na.rm = T),
                 workplaces_mean = weighted.mean(workplaces_percent_change_from_baseline, population, na.rm = T)), keyby = date]
gm_e <- unique(gm_e)
gm_e <- gm_e[date %between% c(ymd("2020-10-15", "2020-12-03"))]
gm_e$days <- c(-21:28)
#gm_e <- gm_e[, -1]

#combine
gm_merge <- rbind(gm_ni, gm_w, gm_e, fill = T)
gm_wide <- melt(gm_merge, id.vars = c("country", "days"))

#grocery and pharmacy mean
gm_grocery <- gm_wide[variable == "grocery_and_pharmacy_mean"]
grocery <- ggplot(gm_grocery) + 
  geom_line(aes(x = days, y = value, colour = country)) +
  geom_vline(xintercept = 0, linetype = 2) + 
  labs(x = "Days from lockdown", y = "Google Mobility Index", 
       title = "Grocery and Pharmacy") +
  scale_colour_manual(values = c("red", "black", "blue"), name = "Country",
                      labels = c("England", "Northern Ireland", "Wales"))

#residential mean 
gm_residential <- gm_wide[variable == "residential_mean"]
residential <- ggplot(gm_residential) + 
  geom_line(aes(x = days, y = value, colour = country)) +
  geom_vline(xintercept = 0, linetype = 2) + 
  labs(x = "Days from lockdown", y = "Google Mobility Index", 
       title = "Residential") +
  scale_colour_manual(values = c("red", "black", "blue"), name = "Country",
                      labels = c("England", "Northern Ireland", "Wales"))

#retail and recreation mean
gm_retail <- gm_wide[variable == "retail_and_recreation_mean"]
retail <- ggplot(gm_retail) + 
  geom_line(aes(x = days, y = value, colour = country)) +
  geom_vline(xintercept = 0, linetype = 2) + 
  labs(x = "Days from lockdown", y = "Google Mobility Index", 
       title = "Retail and Recreation") +
  scale_colour_manual(values = c("red", "black", "blue"), name = "Country",
                      labels = c("England", "Northern Ireland", "Wales"))

#transit stations mean
gm_transit <- gm_wide[variable == "transit_stations_mean"]
transit <- ggplot(gm_transit) + 
  geom_line(aes(x = days, y = value, colour = country)) +
  geom_vline(xintercept = 0, linetype = 2) + 
  labs(x = "Days from lockdown", y = "Google Mobility Index", 
       title = "Transit Stations") +
  scale_colour_manual(values = c("red", "black", "blue"), name = "Country",
                      labels = c("England", "Northern Ireland", "Wales"))

#workplaces mean
gm_workplaces <- gm_wide[variable == "workplaces_mean"]
workplaces <- ggplot(gm_workplaces) + 
  geom_line(aes(x = days, y = value, colour = country)) +
  geom_vline(xintercept = 0, linetype = 2) + 
  labs(x = "Days from lockdown", y = "Google Mobility Index", 
       title = "Workplaces") +
  scale_colour_manual(values = c("red", "black", "blue"), name = "Country",
                      labels = c("England", "Northern Ireland", "Wales"))

#plot together
plot_grid(grocery, residential, retail, transit, workplaces)
