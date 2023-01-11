##summary information about participants

#load libraries
library(data.table)
library(tidyverse)

#set data path
data_path <-"C:\\Users\\emiel\\Documents\\LSHTM\\Fellowship\\Project\\comix_mobility\\Data\\"

#import participant and contact data
cnts <- qs::qread(file.path(data_path, "part_cnts.qs"))

#filter out participants of a certain age
cnts <- cnts[part_age >= 18 & part_age <= 65]

#fix age groups
cnts <- cnts %>%
  mutate(part_age_group = case_when(part_age >= 18 & part_age <= 29 ~ "18-29",
                                    part_age >= 30 & part_age <= 39 ~ "30-39",
                                    part_age >= 40 & part_age <= 49 ~ "40-49",
                                    part_age >= 50 & part_age <= 59 ~ "50-59",
                                    part_age >= 60 & part_age <= 69 ~ "60-69",
                                    part_age >= 70 ~ "70+"))


#gender
cnts %>% group_by(part_gender_nb) %>%
  tally()

#age group
cnts %>% group_by(part_age_group) %>%
  tally()

#employment status
cnts %>% group_by(part_employstatus) %>%
  tally()

#social class
cnts %>% group_by(part_social_group) %>%
  tally()

#area
cnts %>% group_by(area_2_name) %>%
  tally()

#household size group 
cnts %>% group_by(hh_size_group) %>%
  tally()

##not main variables

#household composition 
cnts %>% group_by(hh_type) %>%
  tally()

#occupation
cnts %>% group_by(part_occupation) %>%
  tally()

#high risk
cnts %>% group_by(part_high_risk) %>%
  tally()

#ethnicity
cnts %>% group_by(part_ethnicity) %>%
  tally()

#vaccinated
cnts %>% group_by(part_vacc) %>%
  tally()
