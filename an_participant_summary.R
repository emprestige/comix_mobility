##summary information about participants

#load libraries
library(data.table)
library(ggplot2)
library(tidyverse)
library(socialmixr)

#set data path
data_path <-"C:\\Users\\emiel\\Documents\\LSHTM\\Fellowship\\Project\\comix_mobility\\Data\\"

#import participant and contact data
pt <- qs::qread(file.path(data_path, "participants_filt.qs"))

#filter out participants of a certain age
pt <- pt[part_age >= 18 & part_age <= 65]

#fix age groups
pt <- pt %>%
  mutate(part_age_group = case_when(part_age >= 18 & part_age <= 29 ~ "18-29",
                                    part_age >= 30 & part_age <= 39 ~ "30-39",
                                    part_age >= 40 & part_age <= 49 ~ "40-49",
                                    part_age >= 50 & part_age <= 59 ~ "50-59",
                                    part_age >= 60 & part_age <= 69 ~ "60-69",
                                    part_age >= 70 ~ "70+"))


#area
pt %>% group_by(area_2_name) %>%
  tally()

#social class
pt %>% group_by(part_social_group) %>%
  tally()

#age group
pt %>% group_by(part_age_group) %>%
  tally()

#household size group 
pt %>% group_by(hh_size_group) %>%
  tally()
