##GAM practice

#load libraries
library(ggplot2)
library(tidyverse)
library(cowplot) 
library(ggpubr)
library(mgcv)

#set data path
data_path <-"C:\\Users\\emiel\\Documents\\LSHTM\\Fellowship\\Project\\comix_mobility\\Data\\"

#import contact data
cnts <- qs::qread(file.path(data_path,"part_cnts.qs"))

#order by date
#cnts_date <- cnts[order(date)]
cnts_date <- cnts %>%
  arrange(date)

#import mobility data
mob <- qs::qread(file.path(data_path, "google_mob.qs"))

#subset for same date range
mob_sub <- mob[date >= "2020-03-24" & date <= "2022-03-02"]