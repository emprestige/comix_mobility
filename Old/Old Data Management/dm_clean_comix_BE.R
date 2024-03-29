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

#first filter out unnecessary data (not from BE)
pt_be <- pt[substr(part_wave_uid, 1, 2) == "be"]
ct_be <- ct[substr(part_wave_uid, 1, 2) == "be"]

#filter out unnecessary variables 
ct_nmes <- c("part_id", "date", "cnt_age_est_min", "cnt_age_est_max", 
             "cnt_gender", "cnt_home", "cnt_work", "cnt_school",
             "cnt_public_transport", "cnt_other", "cnt_phys", "survey_round")
ct_be <- ct_be[, ..ct_nmes]

#save data
qs::qsave(pt_be, "C:\\Users\\emiel\\Documents\\LSHTM\\Fellowship\\Project\\comix_mobility\\Data\\participants_BE.qs")
qs::qsave(ct_be, "C:\\Users\\emiel\\Documents\\LSHTM\\Fellowship\\Project\\comix_mobility\\Data\\contact_BE.qs")
