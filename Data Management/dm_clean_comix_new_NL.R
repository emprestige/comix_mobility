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

#separate mass contacts 
ct_mass <- ct[cnt_mass == "mass"]
ct_nonmass <- ct[cnt_mass == "individual"]

#filter out when there are more than 50 observations for work
ct_mass_groups_work <- ct_mass %>% 
  filter(cnt_work == 1) %>%
  group_by(part_id, date) %>%
  mutate(count = n()) %>%
  filter(count > 50) %>%
  sample_n(size = 50)

#put mass work contacts back in to main work contact frame
ct_mass_groups_work <- ct_mass_groups_work[, -68]
ct_work <- rbind(ct_nonmass, ct_mass_groups_work)

#filter out when there are more than 50 observations for other
ct_mass_groups_other <- ct_mass %>% 
  filter(cnt_other == 1) %>%
  group_by(part_id, date) %>%
  mutate(count = n()) %>%
  filter(count > 50) %>%
  sample_n(size = 50)

#put mass other contacts back in to main other contact frame
ct_mass_groups_other <- ct_mass_groups_other[, -68]
ct_other <- rbind(ct_nonmass, ct_mass_groups_other)

#first filter out unnecessary data (not from BE)
pt_nl <- pt[country == "nl"]
ct_work_nl <- ct_work[country == "nl"]
ct_other_nl <- ct_other[country == "nl"]

#filter out unnecessary variables 
ct_nmes <- c("part_id", "date", "cnt_age_est_min", "cnt_age_est_max", 
             "cnt_gender", "cnt_home", "cnt_work", "cnt_school",
             "cnt_public_transport", "cnt_other", "cnt_phys", "survey_round")
ct_work_nl <- ct_work_nl[, ..ct_nmes]
ct_other_nl <- ct_other_nl[, ..ct_nmes]

#save data
qs::qsave(pt_nl, "C:\\Users\\emiel\\Documents\\LSHTM\\Fellowship\\Project\\comix_mobility\\Data\\participants_NL.qs")
qs::qsave(ct_work_nl, "C:\\Users\\emiel\\Documents\\LSHTM\\Fellowship\\Project\\comix_mobility\\Data\\contact_work_NL.qs")
qs::qsave(ct_other_nl, "C:\\Users\\emiel\\Documents\\LSHTM\\Fellowship\\Project\\comix_mobility\\Data\\contact_other_NL.qs")
