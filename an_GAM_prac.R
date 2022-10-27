##GAM practice

#load libraries
library(data.table)
library(ggplot2)
library(tidyverse)
library(cowplot) 
library(ggpubr)
library(mgcv)
library(socialmixr)

#set data path
data_path <-"C:\\Users\\emiel\\Documents\\LSHTM\\Fellowship\\Project\\comix_mobility\\Data\\"

#import mobility data
mob <- qs::qread(file.path(data_path, "google_mob.qs"))

#subset for same date range
mob_sub <- mob[date >= "2020-03-24" & date <= "2022-03-02"]

##
#set data path 
dir_data_validate <- "C:\\Users\\emiel\\Filr\\Net Folders\\EPH Shared\\Comix_survey\\data\\validated\\"

#import participant and contact data 
pt <- qs::qread(file.path(dir_data_validate, "part_min.qs"))
ct <- qs::qread(file.path(dir_data_validate, "contacts.qs"))

#filter to just uk 
pt_uk <- pt[country == "uk"]
ct_uk <- ct[country == "uk"]

#filter down to one time point 
pt_uk1 <- pt_uk[date == "2020-04-19"]
ct_uk1 <- ct_uk[date == "2020-04-19"]
#pt_uk1[is.na(pt_uk1)] <- 0
#ct_uk1[is.na(ct_uk1)] <- 0

#new_survey <- survey(pt_uk1, ct_uk1)
#contact_matrix(new_survey)
