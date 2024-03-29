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
pt_uk <- pt[country == "uk"]
ct_uk <- ct[country == "uk"]

#filter out people who aren't adults 
#pt_uk <- pt_uk[part_age >= "18" & part_age <= "65"]

#get lower limits of age groups 
ages <- ct_uk %>% group_by(cnt_age_group) %>% tally()

#filter out unnecessary variables 
ct_nmes <- c("part_id", "date", "cnt_age_est_min", "cnt_age_est_max", 
             "cnt_gender", "cnt_home", "cnt_work", "cnt_school",
             "cnt_public_transport", "cnt_other", "cnt_phys", "survey_round")
ct_uk <- ct_uk[, ..ct_nmes]

#save data
qs::qsave(pt_uk, "C:\\Users\\emiel\\Documents\\LSHTM\\Fellowship\\Project\\comix_mobility\\Data\\participants.qs")
qs::qsave(ct_uk, "C:\\Users\\emiel\\Documents\\LSHTM\\Fellowship\\Project\\comix_mobility\\Data\\contact.qs")

#filter out bad rounds
ct_uk <- ct_uk[survey_round != "6"]
pt_uk <- pt_uk[survey_round != "6"]
ct_uk <- ct_uk[survey_round != "7"]
pt_uk <- pt_uk[survey_round != "7"]

#save data
qs::qsave(pt_uk, "C:\\Users\\emiel\\Documents\\LSHTM\\Fellowship\\Project\\comix_mobility\\Data\\participants_filt.qs")
qs::qsave(ct_uk, "C:\\Users\\emiel\\Documents\\LSHTM\\Fellowship\\Project\\comix_mobility\\Data\\contact_filt.qs")

##

pt <- qs::qread(file.path(dir_data_validate, "part.qs"))
pt_uk <- pt[country == "uk"]
pt_uk <- pt_uk[survey_round != "6"]
pt_uk <- pt_uk[survey_round != "7"]
qs::qsave(pt_uk, "C:\\Users\\emiel\\Documents\\LSHTM\\Fellowship\\Project\\comix_mobility\\Data\\participants_filt_full.qs")
