##Create weights 

#import libraries
library(data.table)
library(readxl)
library(dplyr)
library(lubridate)

#set data path
data_path <-"C:\\Users\\emiel\\Documents\\LSHTM\\Fellowship\\Project\\comix_mobility\\Data\\"

#import contact data
cnts <- as.data.table(readxl::read_xlsx(file.path(data_path, "weight_test_data.xlsx")))
cnts <- cnts %>%
  mutate(part_age_group = case_when(part_age >= 18 & part_age <= 29 ~ "18-29",
                                    part_age >= 30 & part_age <= 39 ~ "30-39",
                                    part_age >= 40 & part_age <= 49 ~ "40-49",
                                    part_age >= 50 & part_age <= 59 ~ "50-59",
                                    part_age >= 60 & part_age <= 69 ~ "60-69",
                                    part_age >= 70 ~ "70+"))
cnts <- cnts %>%
  filter(!is.na(part_age_group))

pop <- as.data.table(readxl::read_xlsx(
  file.path(data_path, "WPP2019_INT_F03_1_POPULATION_BY_AGE_ANNUAL_BOTH_SEXES.xlsx"),
  skip = 16))

setnames(pop, 
         old = c("Region, subregion, country or area *", 
                 "Reference date (as of 1 July)"), 
         new = c("location", "year"))

pop <- pop[location == "United Kingdom" & year == 2020]

pop2 <- melt(pop, id.vars = c("location", "year"), 
             measure.vars = as.character(0:100), 
             variable.name = "age",
             value.name = "estimate")

pop2[, age := as.numeric(as.character(age))]
pop2[, estimate := as.numeric(as.character(estimate))]

pop2[between(age, 0, 4), part_age_group := "0-4"]
pop2[between(age, 5, 11), part_age_group := "5-11"]
pop2[between(age, 12, 17), part_age_group := "12-17"]
pop2[between(age, 18, 29), part_age_group := "18-29"]
pop2[between(age, 30, 39), part_age_group := "30-39"]
pop2[between(age, 40, 49), part_age_group := "40-49"]
pop2[between(age, 50, 59), part_age_group := "50-59"]
pop2[between(age, 60, 69), part_age_group := "60-69"]
pop2[between(age, 70, 120), part_age_group := "70+"]

pop2[part_age_group %in% c("0-4", "5-11", "12-17"), sample_type := "child"]
pop2[part_age_group %in% c("18-29", "30-39", "40-49", "50-59", "60-69", "70+"), 
     sample_type := "adult"]

pop2 <- pop2 %>% 
  filter(sample_type == "adult")

pop3 <- pop2[, .(pop_estimate2 = sum(estimate)), by = c("part_age_group")]
pop3 <- pop3[, pop_total2 := sum(pop_estimate2)]
pop3[, pop_proportion2 := pop_estimate2/pop_total2]

popcnts <- merge(cnts, pop3, by = c("part_age_group"))

weightlookup <- popcnts[, .(sample = .N), by = .(weekend, week, part_age_group)]
weightlookup[, sample_total := sum(sample), by = .(week)]
weightlookup[, sample_proportion := sample/sample_total]

pop5 <- popcnts[, .(week, part_age_group, weekend, 
                    pop_estimate = pop_estimate2, pop_proportion = pop_proportion2)]
pop6 <- unique(pop5)
weightlookup2 <- merge(weightlookup, pop6)
weightlookup2[, weight_raw := pop_estimate/sample]
weightlookup2[, weight_proportion := pop_proportion/sample_proportion]

#merge weights to cnts2
cnts3 <- merge(cnts2, weightlookup2, by = c("part_social_group", "weekend", "week",
                                            "part_age_group"))

#save
qs::qsave(cnts3, file.path(data_path, "cnts_weight_test_new.qs"))
