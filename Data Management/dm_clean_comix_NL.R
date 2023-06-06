##clean data for contact matrix 

#load libraries
library(data.table)
library(tidyverse)
library(socialmixr)

#set data path 
dir_data_validate <- "C:\\Users\\emiel\\Filr\\Net Folders\\EPH Shared\\Comix_survey\\data\\validated\\"

#import participant and contact data 
pt <- qs::qread(file.path(dir_data_validate, "part_min.qs"))
ct <- qs::qread(file.path(dir_data_validate, "contacts.qs"))

#first filter out unnecessary data (not from UK)
pt_nl <- pt[country == "nl"]
ct_nl <- ct[country == "nl"]

#filter out unnecessary variables 
ct_nmes <- c("part_id", "date", "cnt_age_est_min", "cnt_age_est_max", 
             "cnt_gender", "cnt_home", "cnt_work", "cnt_school",
             "cnt_public_transport", "cnt_other", "cnt_phys", "survey_round")
ct_nl <- ct_nl[, ..ct_nmes]

#save data
qs::qsave(pt_nl, "C:\\Users\\emiel\\Documents\\LSHTM\\Fellowship\\Project\\comix_mobility\\Data\\participants_NL.qs")
qs::qsave(ct_nl, "C:\\Users\\emiel\\Documents\\LSHTM\\Fellowship\\Project\\comix_mobility\\Data\\contact_NL.qs")
