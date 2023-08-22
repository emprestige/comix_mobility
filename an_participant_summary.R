##summary information about participants

#load libraries
library(data.table)
library(tidyverse)

#set data path
data_path <-"C:\\Users\\emiel\\Documents\\LSHTM\\Fellowship\\Project\\comix_mobility\\Data\\"

#import participant and contact data
cnts <- qs::qread(file.path(data_path, "part_cnts.qs"))

#filter out participants of a certain age
cnts <- cnts[sample_type == "adult"]

# #fix age groups
# cnts <- cnts %>%
#   mutate(part_age_group = case_when(part_age >= 18 & part_age <= 29 ~ "18-29",
#                                     part_age >= 30 & part_age <= 39 ~ "30-39",
#                                     part_age >= 40 & part_age <= 49 ~ "40-49",
#                                     part_age >= 50 & part_age <= 59 ~ "50-59",
#                                     part_age >= 60 & part_age <= 69 ~ "60-69",
#                                     part_age >= 70 ~ "70+"))

#cnts[area_2_name == "Greater", area_2_name := "Greater London"]

#filter data for NA age group, order it by date 
cnts <- cnts %>%
  filter(!is.na(part_age_group))
cnts <- cnts[order(date)]

#check proportion for each day of the week
cnts %>% group_by(part_vacc) %>%
  summarise(n = n()) %>%
  mutate(freq = round(n/sum(n)*100, 1))

##main variables 

#gender
cnts %>% group_by(part_gender_nb) %>%
  summarise(n = n()) %>%
  mutate(freq = round(n/sum(n)*100, 1))

#age group
cnts %>% group_by(part_age_group) %>%
  summarise(n = n()) %>%
  mutate(freq = round(n/sum(n)*100, 1))

#employment status
cnts %>% group_by(part_employstatus) %>%
  summarise(n = n()) %>%
  mutate(freq = round(n/sum(n)*100, 1))

#social class
cnts %>% group_by(part_social_group) %>%
  summarise(n = n()) %>%
  mutate(freq = round(n/sum(n)*100, 1))

#area
cnts %>% group_by(area_3_name) %>%
  summarise(n = n()) %>%
  mutate(freq = round(n/sum(n)*100, 1))

#household size group 
cnts %>% group_by(hh_size_group) %>%
  summarise(n = n()) %>%
  mutate(freq = round(n/sum(n)*100, 1))

#employed yes or not 
cnts[, part_employed := ifelse(part_employstatus == "employed full-time (34 hours or more)", 
     T, ifelse(part_employstatus == "employed part-time (less than 34 hours)",
     T, ifelse(part_employstatus == "self employed", T, F)))]
cnts %>% group_by(part_employed) %>%
  summarise(n = n()) %>%
  mutate(freq = round(n/sum(n)*100, 1))

##not main variables

#household composition 
cnts %>% group_by(hh_type) %>%
  summarise(n = n()) %>%
  mutate(freq = round(n/sum(n)*100, 1))

#occupation
cnts %>% group_by(part_occupation) %>%
  summarise(n = n()) %>%
  mutate(freq = round(n/sum(n)*100, 1))

#high risk
cnts %>% group_by(part_high_risk) %>%
  summarise(n = n()) %>%
  mutate(freq = round(n/sum(n)*100, 1))

#ethnicity
cnts %>% group_by(part_ethnicity) %>%
  summarise(n = n()) %>%
  mutate(freq = round(n/sum(n)*100, 1))

#vaccinated
cnts %>% group_by(part_vacc) %>%
  summarise(n = n()) %>%
  mutate(freq = round(n/sum(n)*100, 1))

##

#select one observation per participant
cnts <- cnts %>%
  group_by(part_id) %>%
  filter(row_number(part_id) == 1)

##main variables 

#gender
cnts %>% group_by(part_gender_nb) %>%
  summarise(n = n()) %>%
  mutate(freq = round(n/sum(n)*100, 1))

#age group
cnts %>% group_by(part_age_group) %>%
  summarise(n = n()) %>%
  mutate(freq = round(n/sum(n)*100, 1))

#employment status
cnts %>% group_by(part_employstatus) %>%
  summarise(n = n()) %>%
  mutate(freq = round(n/sum(n)*100, 1))

#social class
cnts %>% group_by(part_social_group) %>%
  summarise(n = n()) %>%
  mutate(freq = round(n/sum(n)*100, 1))

#area
cnts %>% group_by(area_3_name) %>%
  summarise(n = n()) %>%
  mutate(freq = round(n/sum(n)*100, 1))

#household size group 
cnts %>% group_by(hh_size_group) %>%
  summarise(n = n()) %>%
  mutate(freq = round(n/sum(n)*100, 1))

#employed yes or no
cnts %>% group_by(part_employed) %>%
  summarise(n = n()) %>%
  mutate(freq = round(n/sum(n)*100, 1))

##not main variables

#household composition 
cnts %>% group_by(hh_type) %>%
  summarise(n = n()) %>%
  mutate(freq = round(n/sum(n)*100, 1))

#occupation
cnts %>% group_by(part_occupation) %>%
  summarise(n = n()) %>%
  mutate(freq = round(n/sum(n)*100, 1))

#high risk
cnts %>% group_by(part_high_risk) %>%
  summarise(n = n()) %>%
  mutate(freq = round(n/sum(n)*100, 1))

#ethnicity
cnts %>% group_by(part_ethnicity) %>%
  summarise(n = n()) %>%
  mutate(freq = round(n/sum(n)*100, 1))

#vaccinated
cnts %>% group_by(part_vacc) %>%
  summarise(n = n()) %>%
  mutate(freq = round(n/sum(n)*100, 1))
