##Create weights 

#import libraries
library(data.table)
library(readxl)
library(dplyr)
library(lubridate)

#set data path
data_path <-"C:\\Users\\emiel\\Documents\\LSHTM\\Fellowship\\Project\\comix_mobility\\Data\\"

#import contact data
cnts <- qs::qread(file.path(data_path, "part_cnts_other.qs"))
cnts[, week := paste(isoyear(date), "/", sprintf("%02d", isoweek(date)))]

#filter out people without age group
cnts <- cnts %>%
  filter(!is.na(part_age_group))

#duplicate the dataset
cnts2 <- rlang::duplicate(cnts)

#define population proportions for social class
cnts2[, pop_proportion1 := ifelse(part_social_group == "A - Upper middle class",
        0.04, ifelse(part_social_group == "B - Middle class", 0.23, 
        ifelse(part_social_group == "C1 - Lower middle class", 0.29,
        ifelse(part_social_group == "C2 - Skilled working class", 0.21, 
        ifelse(part_social_group == "D - Working class", 0.15, 0.08)))))]

#calculate population estimate from the proportion 
cnts2[, pop_estimate1 := pop_proportion1*67866]

#rename gender column
cnts2[, part_gender := part_gender_nb]

#define those with no gender as gender = "other"
cnts2[is.na(part_gender), part_gender := "other"]

#import population estimates for each age 
pop <- as.data.table(readxl::read_xlsx(
  file.path(data_path, "WPP2019_INT_F03_1_POPULATION_BY_AGE_ANNUAL_BOTH_SEXES.xlsx"),
  skip = 16))

#rename column names
setnames(pop, old = c("Region, subregion, country or area *", 
         "Reference date (as of 1 July)"), new = c("location", "year"))

#define location and year
pop <- pop[location == "United Kingdom" & year == 2020]

#reshape data from wide to long format
pop2 <- melt(pop, id.vars = c("location", "year"), 
             measure.vars = as.character(0:100), variable.name = "age",
             value.name = "estimate")

#reformat variables
pop2[, age := as.numeric(as.character(age))]
pop2[, estimate := as.numeric(as.character(estimate))]

#define age groups using population estimates
pop2[between(age, 0, 4), part_age_group := "0-4"]
pop2[between(age, 5, 11), part_age_group := "5-11"]
pop2[between(age, 12, 17), part_age_group := "12-17"]
pop2[between(age, 18, 29), part_age_group := "18-29"]
pop2[between(age, 30, 39), part_age_group := "30-39"]
pop2[between(age, 40, 49), part_age_group := "40-49"]
pop2[between(age, 50, 59), part_age_group := "50-59"]
pop2[between(age, 60, 69), part_age_group := "60-69"]
pop2[between(age, 70, 120), part_age_group := "70-120"]

#definte sample type (adult or child)
pop2[part_age_group %in% c("0-4", "5-11", "12-17"), sample_type := "child"]
pop2[part_age_group %in% c("18-29", "30-39", "40-49", "50-59", "60-69", "70-120"), 
     sample_type := "adult"]

#remove smaples from children
pop2 <- pop2 %>% 
  filter(sample_type == "adult")

#calculate the population estimate for each age group
pop3 <- pop2[, .(pop_estimate2 = sum(estimate)), by = c("part_age_group")]

#calculate the population total
pop3 <- pop3[, pop_total2 := sum(pop_estimate2)]

#calculate the population proportion
pop3[, pop_proportion2 := pop_estimate2/pop_total2]

#merge the sample and population information
popcnts <- merge(cnts2, pop3, by = c("part_age_group"))

#calculate the population estimates by age group for each social group
#assuming that social group distribution is the same per age group 
popcnts[, pop_estimate3 := pop_estimate2*pop_proportion1]

#calculate the population totals for each social group 
popcnts[, pop_total3 := sum(pop_estimate3), by = c("part_social_group")]

#calculate the population prpoportions 
popcnts[, pop_proportion3 := pop_estimate3/pop_total3]

#first count the number of samples in each subgroup (social group, 
#week, and age group) 
weightlookup <- popcnts[, .(sample = .N), by = .(part_social_group, week, 
                                                 part_age_group)]

#calculate the sample total for each week
weightlookup[, sample_total := sum(sample), by = .(week)]

#calculate the sample proportion
weightlookup[, sample_proportion := sample/sample_total]

#create new data.table with only the needed information (week, social group,
#age group, population estimate, and population proportion)
pop5 <- popcnts[, .(week, part_social_group, part_age_group, 
                    pop_estimate = pop_estimate3, pop_proportion = pop_proportion3)]

#remove duplicates
pop6 <- unique(pop5)

#merge sample and population information
weightlookup2 <- merge(weightlookup, pop6)

#calculate weight as population estimate divided by the number of samples
weightlookup2[, weight_raw := pop_estimate/sample]

#calculate weight proportion
weightlookup2[, weight_proportion := pop_proportion/sample_proportion]

#merge weights to cnts2
cnts3 <- merge(cnts2, weightlookup2, by = c("part_social_group", "week",
                                            "part_age_group"))

#save
qs::qsave(cnts3, file.path(data_path, "cnts_weight_other.qs"))
