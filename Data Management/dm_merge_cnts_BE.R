##compare data for Belgium 

#load libraries
library(data.table)
library(dplyr)
library(lubridate)
library(here)

data_path <- here("data")

##work

#load data
cnts <- qs::qread(file.path(data_path, "part_cnts_work_BE.qs"))
cnts_longer <- qs::qread(file.path(data_path, "part_cnts_work_BE_longer.qs"))
cnts_longer[, sample_type := ifelse(part_age == "[0,1)" | part_age == "[1,6)" | part_age == "[6,12)" | part_age == "[12,18)", "child", "adult")] 
cnts_longer[, date := as.Date(parse_date_time(sday_id, orders = "ymd"))]

cnts_longer <- cnts_longer[date > "2021-03-04"]
cnts_longer[, sday_id := NULL]
cnts_longer[, weekday := ifelse(dayofweek == 0, "Monday",
                         ifelse(dayofweek == 1, "Tuesday",
                         ifelse(dayofweek == 2, "Wednesday",
                         ifelse(dayofweek == 3, "Thursday",
                         ifelse(dayofweek == 4, "Friday",
                         ifelse(dayofweek == 5, "Saturday", "Sunday"))))))]
cnts_longer[, dayofweek := NULL]
cnts_longer[, wave := NULL]
cnts_longer[, part_gender_nb := part_gender]
cnts_longer[, part_gender := NULL]

cnts_names <- names(cnts_longer)
cnts <- cnts[, ..cnts_names]

cnts_all <- rbind(cnts, cnts_longer)

qs::qsave(cnts_all, file.path(data_path, "part_cnts_work_BE_all.qs"))

##other

#load data
cnts <- qs::qread(file.path(data_path, "part_cnts_other_BE.qs"))
cnts_longer <- qs::qread(file.path(data_path, "part_cnts_other_BE_longer.qs"))
cnts_longer[, sample_type := ifelse(part_age == "[0,1)" | part_age == "[1,6)" | part_age == "[6,12)" | part_age == "[12,18)", "child", "adult")] 
cnts_longer[, date := as.Date(parse_date_time(sday_id, orders = "ymd"))]

cnts_longer <- cnts_longer[date > "2021-03-04"]
cnts_longer[, sday_id := NULL]
cnts_longer[, weekday := ifelse(dayofweek == 0, "Monday",
                         ifelse(dayofweek == 1, "Tuesday",
                         ifelse(dayofweek == 2, "Wednesday",
                         ifelse(dayofweek == 3, "Thursday",
                         ifelse(dayofweek == 4, "Friday",
                         ifelse(dayofweek == 5, "Saturday", "Sunday"))))))]
cnts_longer[, dayofweek := NULL]
cnts_longer[, wave := NULL]
cnts_longer[, part_gender_nb := part_gender]
cnts_longer[, part_gender := NULL]

cnts_names <- names(cnts_longer)
cnts <- cnts[, ..cnts_names]

cnts_all <- rbind(cnts, cnts_longer)

qs::qsave(cnts_all, file.path(data_path, "part_cnts_other_BE_all.qs"))