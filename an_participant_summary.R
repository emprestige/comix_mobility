##summary information about participants

#load libraries
library(data.table)
library(tidyverse)

#set data path
data_path <-"C:\\Users\\emiel\\Documents\\LSHTM\\Fellowship\\Project\\comix_mobility\\Data\\"

#import participant and contact data
cnts <- qs::qread(file.path(data_path, "part_cnts.qs"))

#view original age groups
cnts %>% group_by(part_age_group) %>%
  summarise(n = n()) %>%
  mutate(freq = (n/sum(n)*100))
cnts %>% group_by(part_age_group) %>%
  summarise(min = min(part_age), max = max(part_age))


#filter out participants of a certain age
cnts <- cnts[sample_type == "adult"]

#
cnts <- cnts %>%
  mutate(part_age_group_new = case_when(part_age_group == "18-19" ~ "18-29",
                                        part_age_group == "18-29" ~ "18-29",
                                        part_age_group == "30-39" ~ "30-39",
                                        part_age_group == "40-49" ~ "40-49",
                                        part_age_group == "50-59" ~ "50-59",
                                        part_age_group == "60-69" ~ "60-69",
                                        part_age_group == "70-120" ~ "70+"))

#fix age groups
cnts <- cnts %>%
  mutate(part_age_group = case_when(part_age >= 18 & part_age <= 29 ~ "18-29",
                                    part_age >= 30 & part_age <= 39 ~ "30-39",
                                    part_age >= 40 & part_age <= 49 ~ "40-49",
                                    part_age >= 50 & part_age <= 59 ~ "50-59",
                                    part_age >= 60 & part_age <= 69 ~ "60-69",
                                    part_age >= 70 ~ "70+"))

##main variables 

#gender
cnts %>% group_by(part_gender_nb) %>%
  summarise(n = n()) %>%
  mutate(freq = (n/sum(n)*100))

#age group
cnts %>% group_by(part_age_group) %>%
  summarise(n = n()) %>%
  mutate(freq = (n/sum(n)*100))

cnts %>% group_by(part_age_group_new) %>%
  summarise(n = n()) %>%
  mutate(freq = (n/sum(n)*100))

cnts %>% group_by(part_age_group) %>%
  summarise(min = min(part_age), max = max(part_age))

#employment status
cnts %>% group_by(part_employstatus) %>%
  summarise(n = n()) %>%
  mutate(freq = (n/sum(n)*100))

#social class
cnts %>% group_by(part_social_group) %>%
  summarise(n = n()) %>%
  mutate(freq = (n/sum(n)*100))

#area
cnts %>% group_by(area_2_name) %>%
  summarise(n = n()) %>%
  mutate(freq = (n/sum(n)*100))

#household size group 
cnts %>% group_by(hh_size_group) %>%
  summarise(n = n()) %>%
  mutate(freq = (n/sum(n)*100))

##not main variables

#household composition 
cnts %>% group_by(hh_type) %>%
  summarise(n = n()) %>%
  mutate(freq = (n/sum(n)*100))

#occupation
cnts %>% group_by(part_occupation) %>%
  summarise(n = n()) %>%
  mutate(freq = (n/sum(n)*100))

#high risk
cnts %>% group_by(part_high_risk) %>%
  summarise(n = n()) %>%
  mutate(freq = (n/sum(n)*100))

#ethnicity
cnts %>% group_by(part_ethnicity) %>%
  summarise(n = n()) %>%
  mutate(freq = (n/sum(n)*100))

#vaccinated
cnts %>% group_by(part_vacc) %>%
  summarise(n = n()) %>%
  mutate(freq = (n/sum(n)*100))
