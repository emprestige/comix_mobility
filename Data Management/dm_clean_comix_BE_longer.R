##clean data for contact matrix 

#load libraries
library(data.table)
library(dplyr)
library(socialmixr)
library(here)

#set data path 
dir_data_validate <- here("data")

#import participant and contact data 
pt <- as.data.table(read.csv(file.path(dir_data_validate, "CoMix_BE_participant_extra.csv")))
pt_min <- as.data.table(read.csv(file.path(dir_data_validate, "CoMix_BE_participant_common.csv")))
ct_min <- as.data.table(read.csv(file.path(dir_data_validate, "CoMix_BE_contact_common.csv")))
ct <- as.data.table(read.csv(file.path(dir_data_validate, "CoMix_BE_contact_extra.csv")))
ct <- merge(ct, ct_min, by = "cont_id")
dates <- as.data.table(read.csv(file.path(dir_data_validate, "CoMix_BE_sday.csv")))
pt_dates <- merge(dates, pt_min, by = "part_id")

cnts <- merge(dates, pt_min, by = "part_id")
cnts <- merge(cnts, pt, by = c("part_id", "wave"))
cnts <- merge(cnts, ct, by = "part_id")
ct_vars <- c(names(ct), "part_id", "sday_id")
ct <- cnts[, ..ct_vars]

cnts_mass <- cnts %>%
  group_by(part_id, sday_id) %>%
  mutate(count = n()) %>%
  filter(count > 50) %>%
  mutate(cnt_mass = "mass") 

cnts_nonmass <- cnts %>%
  group_by(part_id, sday_id) %>%
  mutate(count = n()) %>%
  filter(count <= 50) %>%
  mutate(cnt_mass = "individual") 

#filter out when there are more than 50 observations for work
ct_mass_groups_work <- cnts_mass %>%
  filter(cnt_work == 1) %>%
  group_by(part_id, sday_id) %>%
  mutate(count = n()) %>%
  filter(count > 50) %>%
  sample_n(size = 50)

#put mass work contacts back in to main work contact frame
ct_mass_groups_work <- ct_mass_groups_work[, -54]
ct_work <- as.data.table(rbind(cnts_nonmass, ct_mass_groups_work))

#filter out when there are more than 50 observations for other
ct_mass_groups_other <- cnts_mass %>%
  filter(cnt_otherplace == 1) %>%
  group_by(part_id, sday_id) %>%
  mutate(count = n()) %>%
  filter(count > 50) %>%
  sample_n(size = 50)

#put mass other contacts back in to main other contact frame
ct_mass_groups_other <- ct_mass_groups_other[, -54]
ct_other <- as.data.table(rbind(cnts_nonmass, ct_mass_groups_other))

#filter out unnecessary variables 
ct_nmes <- c("part_id", "sday_id", "cnt_age_est_min", "cnt_age_est_max", 
             "cnt_gender", "cnt_home", "cnt_work", "cnt_school",
             "cnt_otherplace")
ct_work_be <- ct_work[, ..ct_nmes]
ct_other_be <- ct_other[, ..ct_nmes]

#save data
qs::qsave(pt_dates, file.path(data_path, "participants_BE_longer.qs"))
qs::qsave(ct_work_be, file.path(data_path, "contact_work_BE_longer.qs"))
qs::qsave(ct_other_be, file.path(data_path, "contact_other_BE_longer.qs"))
