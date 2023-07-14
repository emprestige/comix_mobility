##estimate contact matrices from CoMix data for The Netherlands

#load libraries
library(data.table)
library(tidyverse)
library(lubridate)
library(socialmixr)

#set data path
data_path <-"C:\\Users\\emiel\\Documents\\LSHTM\\Fellowship\\Project\\comix_mobility\\Data\\"

#import participant and contact data
pt <- qs::qread(file.path(data_path, "participants_NL.qs"))
pt <- pt[part_id != "14756"]
pt <- pt[part_id != "14895"]
pt <- pt[part_id != "14800"]
pt <- pt[part_id != "14308"]
pt <- pt[part_id != "14913"]
ct <- qs::qread(file.path(data_path, "contact_NL.qs"))

#match column name for function
pt$dayofweek <- pt$weekday

#filter to relevant dates 
pt_date <- pt[date <= ymd("2021-03-31")]
ct_date <- ct[date <= ymd("2021-03-31")]

#define week variable
pt_date[, week := paste(isoyear(date), "/", sprintf("%02d", isoweek(date)))]
ct_date[, week := paste(isoyear(date), "/", sprintf("%02d", isoweek(date)))]

#separate data by weeks
pt_week <- pt_date %>% 
  group_split(week)
ct_week <- ct_date %>%
  group_split(week)

#get weeks for each contact matrix
week_dates <- list()
for (i in 1:length(ct_week)) {
  week_dates[i] <- ct_week[[i]]["week"][[1]]
}
week_dates <- t(data.frame(week_dates))
rownames(week_dates) <- 1:nrow(week_dates)

#create surveys for each week
new_survey <- list()
for (i in 1:length(ct_week)) {
  new_survey[[i]] <- survey(pt_week[[i]], ct_week[[i]])
}

#create contact matrices for each survey round 
weeks <- list()
for (i in 1:length(ct_week)) {
  weeks[[i]] <- contact_matrix(new_survey[[i]], weigh.dayofweek = T, 
                               age.limits = c(0, 5, 12, 18, 30, 40, 
                                              50, 60, 65, 70),
                               filter = list(cnt_work = 1))
}

#get dominant eigenvalues for each week
e_weeks <- list()
for(i in 1:length(ct_week)) {
  matrix <- weeks[[i]][["matrix"]]
  matrix[is.na(matrix)] <- 0
  e <- eigen(matrix)
  e_weeks[i] <- Re(e$values[1])
}

#format results
e_weeks_frame <- t(data.frame(e_weeks))
rownames(e_weeks_frame) <- 1:nrow(e_weeks_frame)
e_weeks_frame <- cbind(week_dates, e_weeks_frame)
colnames(e_weeks_frame) <- c("week", "dominant_eigenvalue")
e_weeks_frame <- as.data.table(e_weeks_frame)

#save dominant eigenvalues
qs::qsave(e_weeks_frame, file.path(data_path, "comix_eigens_work_NL.qs"))
