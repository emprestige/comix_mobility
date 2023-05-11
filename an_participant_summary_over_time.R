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
cnts[, week := paste(isoyear(date), "/", sprintf("%02d", isoweek(date)))]

##main variables 
week <- names(table(cnts$week))
int <- seq(1, 100, 12)
my_list <- week[int]
my_list[104] <- "REF"

#gender
cnts %>% group_by(part_gender_nb) %>%
  summarise(n = n()) %>%
  mutate(freq = (n/sum(n)*100))

sexes <- cnts[, .(n = .N), by = .(part_gender_nb, week)][, freq := prop.table(n), by = week]
sexes <- sexes[, var := "grp1"]
sexes_dummy <- rlang::duplicate(sexes)
sexes_dummy <- sexes_dummy[, .(week = "REF", part_gender_nb = c("male", "female", "other"),
                               freq = c(0.4875, 0.5075, 0.005), var = "grp2")]
sexes <- full_join(sexes, sexes_dummy)
ggplot(data = sexes, aes(x = week, y = freq, fill = part_gender_nb)) + geom_col() + 
  scale_x_discrete(breaks = my_list) + facet_grid(~var, scales = "free_x", space = "free_x") +
  theme(strip.background = element_blank(), strip.text.x = element_blank())

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
ages <- ages[, var := "grp1"]
ages_dummy <- rlang::duplicate(ages)
ages_dummy <- ages_dummy[, .(week = "REF", part_age_group = c("18-29", "30-39", 
                        "40-49", "50-59", "60-69", "70-120"), freq = c(0.188, 0.172, 
                        0.16, 0.173, 0.136, 0.172), var = "grp2")]
ages <- full_join(ages, ages_dummy)
ggplot(data = ages, aes(x = week, y = freq, fill = part_age_group)) + geom_col() + 
  scale_x_discrete(breaks = my_list) + facet_grid(~var, scales = "free_x", space = "free_x") +
  theme(strip.background = element_blank(), strip.text.x = element_blank())

# ages2 <- cnts %>%
#   filter(!is.na(part_age_group))
# ages2 <- ages2[, .(n = .N), by = .(part_age_group, week)][, freq := prop.table(n), by = week]
# ggplot(data = ages2, aes(x = week, y = freq, fill = part_age_group)) + geom_col() + 
#   scale_x_discrete(breaks = my_list)

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

# employment <- cnts[, .(n = .N), by = .(part_employstatus, week)][, freq := prop.table(n), by = week]
# ggplot(data = employment, aes(x = week, y = freq, fill = part_employstatus)) + geom_col() + 
#   scale_x_discrete(breaks = my_list)
# 
# employment2 <- cnts %>%
#   filter(!is.na(part_employstatus))
# employment2 <- employment2[, .(n = .N), by = .(part_employstatus, week)][, freq := prop.table(n), by = week]
# ggplot(data = employment2, aes(x = week, y = freq, fill = part_employstatus)) + geom_col() + 
#   scale_x_discrete(breaks = my_list)
# 
# proportion <- cnts[, .(n = .N), by = .(part_attend_work_yesterday, week)][, freq := prop.table(n), by = week]
# ggplot(data = proportion, aes(x = week, y = freq, fill = part_attend_work_yesterday)) + geom_col() + 
#   scale_x_discrete(breaks = my_list)
# proportion2 <- cnts %>%
#   filter(!is.na(part_attend_work_yesterday))
# proportion2 <- proportion2[, .(n = .N), by = .(part_attend_work_yesterday, week)][, freq := prop.table(n), by = week]
# ggplot(data = proportion2, aes(x = week, y = freq, fill = part_attend_work_yesterday)) + geom_col() + 
#   scale_x_discrete(breaks = my_list)

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

employed_yn3 <- employed %>%
  filter(part_age <= 65)
employed_yn3 <- employed_yn3[, .(n = .N), by = .(part_employed, week)][, freq := prop.table(n), by = week]
employed_yn3 <- employed_yn3[, var := "grp1"]
employed_yn3_dummy <- rlang::duplicate(employed_yn3)
employed_yn3_dummy <- employed_yn3_dummy[, .(week = "REF", part_employed = c(FALSE, TRUE),
                                             freq = c(0.25, 0.75), var = "grp2")]
employed_yn3 <- full_join(employed_yn3, employed_yn3_dummy)
ggplot(data = employed_yn3, aes(x = week, y = freq, fill = part_employed)) + 
  geom_col() + scale_x_discrete(breaks = my_list) + 
  facet_grid(~var, scales = "free_x", space = "free_x") +
  theme(strip.background = element_blank(), strip.text.x = element_blank())

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
social <- social[, var := "grp1"]
social_dummy <- rlang::duplicate(social)
social_dummy <- social_dummy[, .(week = "REF", part_social_group = c("A - Upper middle class",
                             "B - Middle class", "C1 - Lower middle class", 
                             "C2 - Skilled working class", "D - Working class", 
                             "E - Lower level of subsistence"), freq = c(0.04,
                             0.23, 0.29, 0.21, 0.15, 0.08), var = "grp2")]
social <- full_join(social, social_dummy)
ggplot(data = social, aes(x = week, y = freq, fill = part_social_group)) + geom_col() + 
  scale_x_discrete(breaks = my_list) + facet_grid(~var, scales = "free_x", space = "free_x") +
  theme(strip.background = element_blank(), strip.text.x = element_blank())

#area
cnts %>% group_by(area_3_name) %>%
  summarise(n = n()) %>%
  mutate(freq = (n/sum(n)*100))

area <- cnts[, .(n = .N), by = .(area_3_name, week)][, freq := prop.table(n), by = week]
area <- area[, var := "grp1"]
area_dummy <- rlang::duplicate(area)
area_dummy <- area_dummy[, .(week = "REF", area_3_name = c("East Midlands", 
                         "East of England", "Greater London", "North East",
                         "North West", "Northern Ireland", "Scotland",
                         "South East", "South West", "Wales", "West Midlands",
                         "Yorkshire and The Humber"), freq = c(0.07, 0.09, 0.13,
                          0.04, 0.11, 0.03, 0.08, 0.14, 0.09, 0.05, 0.09, 0.08), 
                         var = "grp2")]
area <- full_join(area, area_dummy)
ggplot(data = area, aes(x = week, y = freq, fill = area_3_name)) + geom_col() + 
  scale_x_discrete(breaks = my_list) + facet_grid(~var, scales = "free_x", space = "free_x") +
  theme(strip.background = element_blank(), strip.text.x = element_blank())

#household size group 
cnts %>% group_by(hh_size_group) %>%
  summarise(n = n()) %>%
  mutate(freq = (n/sum(n)*100))

sizes <- cnts[, .(n = .N), by = .(hh_size_group, week)][, freq := prop.table(n), by = week]
sizes <- sizes[, var := "grp1"]
sizes_dummy <- rlang::duplicate(sizes)
sizes_dummy <- sizes_dummy[, .(week = "REF", hh_size_group = c("1", "2", "3-5", 
                           "6+"), freq = c(0.3, 0.35, 0.3, 0.05), var = "grp2")]
sizes <- full_join(sizes, sizes_dummy)
ggplot(data = sizes, aes(x = week, y = freq, fill = hh_size_group)) + geom_col() + 
  scale_x_discrete(breaks = my_list) + facet_grid(~var, scales = "free_x", space = "free_x") +
  theme(strip.background = element_blank(), strip.text.x = element_blank())

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
