##clean data for contact matrix 

#load libraries
library(data.table)
library(dplyr)
library(socialmixr)
library(here)

#set data path 
dir_data_validate <- here("data")

#import participant and contact data 
pt <- as.data.table(read.csv(file.path(dir_data_validate, "CoMix_nl_participant_extra.csv")))
pt_min <- as.data.table(read.csv(file.path(dir_data_validate, "CoMix_nl_participant_common.csv")))
ct_min <- as.data.table(read.csv(file.path(dir_data_validate, "CoMix_nl_contact_common.csv")))
ct <- as.data.table(read.csv(file.path(dir_data_validate, "CoMix_nl_contact_extra.csv")))
ct <- merge(ct, ct_min, by = "cont_id")
dates <- as.data.table(read.csv(file.path(dir_data_validate, "CoMix_nl_sday.csv")))
pt_dates <- merge(dates, pt_min, by = "part_id")

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
ct_mass_groups_work <- ct_mass_groups_work[, -42]
ct <- rbind(ct_nonmass, ct_mass_groups_work)

#filter out when there are more than 50 observations for other
ct_mass_groups_other <- ct_mass %>%
  filter(cnt_otherplace == 1) %>%
  group_by(part_id, date) 

#put mass other contacts back in to main other contact frame
ct_other <- rbind(ct_nonmass, ct_mass_groups_other)


#filter out unnecessary variables 
ct_nmes_work <- c("part_id", "date", "cnt_age_est_min", "cnt_age_est_max", 
             "cnt_gender", "cnt_home", "cnt_work", "cnt_school",
             "cnt_public_transport", "cnt_phys", "survey_round")
ct_nmes_other <- c("part_id", "date", "cnt_age_est_min", "cnt_age_est_max", 
                   "cnt_gender", "cnt_home", "cnt_otherplace", "cnt_school",
                   "cnt_public_transport", "cnt_phys", "survey_round")
                   
ct_work_nl <- ct[, ..ct_nmes_work]
ct_other_nl <- ct[, ..ct_nmes_other]

#save data
qs::qsave(pt_dates, file.path(data_path, "participants_NL_longer.qs"))
qs::qsave(ct_work_nl, file.path(data_path, "contact_work_NL_longer.qs"))
qs::qsave(ct_other_nl, file.path(data_path, "contact_other_NL_longer.qs"))
