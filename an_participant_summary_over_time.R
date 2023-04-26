##summary information about participants

#load libraries
library(data.table)
library(tidyverse)
library(lubridate)
library(cowplot)

#set data path
data_path <-"C:\\Users\\emiel\\Documents\\LSHTM\\Fellowship\\Project\\comix_mobility\\Data\\"

#set cowplot theme
theme_set(cowplot::theme_cowplot(font_size = 10) + theme(strip.background = element_blank()))

#import participant and contact data
cnts <- qs::qread(file.path(data_path, "part_cnts.qs"))
cnts <- cnts[order(date)]

#filter data for NA age group, order it by date
cnts <- cnts %>%
  filter(!is.na(part_age_group))

#filter out participants of a certain age
cnts <- cnts[sample_type == "adult"]

# #fix age groups
# cnts <- cnts %>%
#   mutate(part_age_group = case_when(part_age >= 18 & part_age <= 29 ~ "18-29",
#                                     part_age >= 30 & part_age <= 39 ~ "30-39",
#                                     part_age >= 40 & part_age <= 49 ~ "40-49",
#                                     part_age >= 50 & part_age <= 59 ~ "50-59",
#                                     part_age >= 60 & part_age <= 69 ~ "60-69",
#                                      part_age >= 70 ~ "70+"))

cnts[area_2_name == "Greater", area_2_name := "Greater London"]
cnts[, week := paste(isoyear(date), "/", sprintf("%02d", isoweek(date)))]

##main variables 
week <- names(table(cnts$week))
int <- seq(1, 100, 12)
my_list <- week[int]

#gender
cnts %>% group_by(part_gender_nb) %>%
  summarise(n = n()) %>%
  mutate(freq = (n/sum(n)*100))

sexes <- cnts[, .(n = .N), by = .(part_gender_nb, week)][, freq := prop.table(n), by = week]
ggplot(data = sexes, aes(x = week, y = freq, fill = part_gender_nb)) + geom_col() + 
  scale_x_discrete(breaks = my_list)

#age group
cnts %>% group_by(part_age_group) %>%
  summarise(n = n()) %>%
  mutate(freq = (n/sum(n)*100)) 

cnts %>% group_by(part_age_group) %>%
  summarise(min = min(part_age), max = max(part_age))

##
# cnts %>% group_by(part_age_group, week) %>%
#   summarise(n = n()) %>%
#   mutate(freq = (n/sum(n)*100)) %>%
#   filter(!is.na(part_age_group)) %>%
#   ggplot(aes(x = week, y = freq, fill = part_age_group)) + geom_col() + 
#   scale_x_discrete(breaks = my_list)

ages <- cnts[, .(n = .N), by = .(part_age_group, week)][, freq := prop.table(n), by = week]
ggplot(data = ages, aes(x = week, y = freq, fill = part_age_group)) + geom_col() + 
  scale_x_discrete(breaks = my_list)

ages2 <- cnts %>%
  filter(!is.na(part_age_group))
ages2 <- ages2[, .(n = .N), by = .(part_age_group, week)][, freq := prop.table(n), by = week]
ggplot(data = ages2, aes(x = week, y = freq, fill = part_age_group)) + geom_col() + 
  scale_x_discrete(breaks = my_list)

#employment status
cnts %>% group_by(part_employstatus) %>%
  summarise(n = n()) %>%
  mutate(freq = (n/sum(n)*100))

##
# cnts %>% group_by(part_employstatus, week) %>%
#   summarise(n = n()) %>%
#   mutate(freq = (n/sum(n)*100)) %>%
#   filter(!is.na(part_employstatus)) %>%
#   ggplot(aes(x = week, y = freq, fill = part_employstatus)) + geom_col() + 
#   scale_x_discrete(breaks = my_list)

employment <- cnts[, .(n = .N), by = .(part_employstatus, week)][, freq := prop.table(n), by = week]
ggplot(data = employment, aes(x = week, y = freq, fill = part_employstatus)) + geom_col() + 
  scale_x_discrete(breaks = my_list)

employment2 <- cnts %>%
  filter(!is.na(part_employstatus))
employment2 <- employment2[, .(n = .N), by = .(part_employstatus, week)][, freq := prop.table(n), by = week]
ggplot(data = employment2, aes(x = week, y = freq, fill = part_employstatus)) + geom_col() + 
  scale_x_discrete(breaks = my_list)

proportion <- cnts[, .(n = .N), by = .(part_attend_work_yesterday, week)][, freq := prop.table(n), by = week]
ggplot(data = proportion, aes(x = week, y = freq, fill = part_attend_work_yesterday)) + geom_col() + 
  scale_x_discrete(breaks = my_list)
proportion2 <- cnts %>%
  filter(!is.na(part_attend_work_yesterday))
proportion2 <- proportion2[, .(n = .N), by = .(part_attend_work_yesterday, week)][, freq := prop.table(n), by = week]
ggplot(data = proportion2, aes(x = week, y = freq, fill = part_attend_work_yesterday)) + geom_col() + 
  scale_x_discrete(breaks = my_list)

employed <- cnts
employed[, part_employed := ifelse(part_employstatus == "employed full-time (34 hours or more)", 
         T, ifelse(part_employstatus == "employed part-time (less than 34 hours)",
         T, ifelse(part_employstatus == "self employed", T, F)))]
employed_yn <- employed[, .(n = .N), by = .(part_employed, week)][, freq := prop.table(n), by = week]
ggplot(data = employed_yn, aes(x = week, y = freq, fill = part_employed)) + geom_col() +
  scale_x_discrete(breaks = my_list)
employed_yn2 <- employed %>%
  filter(!is.na(part_employed))
employed_yn2 <- employed_yn2[, .(n = .N), by = .(part_employed, week)][, freq := prop.table(n), by = week]
ggplot(data = employed_yn2, aes(x = week, y = freq, fill = part_employed)) + geom_col() +
  scale_x_discrete(breaks = my_list) 

#social class
cnts %>% group_by(part_social_group) %>%
  summarise(n = n()) %>%
  mutate(freq = (n/sum(n)*100))

##
# cnts %>% group_by(part_social_group, week) %>%
#   summarise(n = n()) %>%
#   mutate(freq = (n/sum(n)*100)) %>%
#   filter(!is.na(part_social_group)) %>%
#   ggplot(aes(x = week, y = freq, fill = part_social_group)) + geom_col() + 
#   scale_x_discrete(breaks = my_list)

social <- cnts[, .(n = .N), by = .(part_social_group, week)][, freq := prop.table(n), by = week]
ggplot(data = social, aes(x = week, y = freq, fill = part_social_group)) + geom_col() + 
  scale_x_discrete(breaks = my_list)

#area
cnts %>% group_by(area_2_name) %>%
  summarise(n = n()) %>%
  mutate(freq = (n/sum(n)*100))

area <- cnts[, .(n = .N), by = .(area_2_name, week)][, freq := prop.table(n), by = week]
ggplot(data = area, aes(x = week, y = freq, fill = area_2_name)) + geom_col() + 
  scale_x_discrete(breaks = my_list)

#household size group 
cnts %>% group_by(hh_size_group) %>%
  summarise(n = n()) %>%
  mutate(freq = (n/sum(n)*100))

sizes <- cnts[, .(n = .N), by = .(hh_size_group, week)][, freq := prop.table(n), by = week]
ggplot(data = sizes, aes(x = week, y = freq, fill = hh_size_group)) + geom_col() + 
  scale_x_discrete(breaks = my_list)

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
