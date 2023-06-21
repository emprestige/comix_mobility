##estimate contact matrices from CoMix data for belgium

#load libraries
library(data.table)
library(tidyverse)
library(lubridate)
library(socialmixr)

#set data path
data_path <-"C:\\Users\\emiel\\Documents\\LSHTM\\Fellowship\\Project\\comix_mobility\\Data\\"

#import participant and contact data
pt <- qs::qread(file.path(data_path, "participants_BE_archive.qs"))
pt <- pt[part_id != "12313"]
pt <- pt[part_id != "12137"]
pt <- pt[part_id != "12732"]
pt <- pt[part_id != "13364"]
ct <- qs::qread(file.path(data_path, "contact_BE_archive.qs"))

#match column name for function
pt$dayofweek <- pt$weekday

#filter to relevant dates 
pt_date <- pt[date <= ymd("2021-03-31")]
ct_date <- ct[date <= ymd("2021-03-31")]

#define fortnight variable
pt_date[, fortnight := paste(isoyear(date), "/", sprintf("%02d", ceiling(isoweek(date)/2)))]
ct_date[, fortnight := paste(isoyear(date), "/", sprintf("%02d", ceiling(isoweek(date)/2)))]

#separate data by fortnights
pt_fortnight <- pt_date %>% 
  group_split(fortnight)
ct_fortnight <- ct_date %>%
  group_split(fortnight)

#get fortnights for each contact matrix
fortnight_dates <- list()
for (i in 1:length(ct_fortnight)) {
  fortnight_dates[i] <- ct_fortnight[[i]]["fortnight"][[1]]
}
fortnight_dates <- t(data.frame(fortnight_dates))
rownames(fortnight_dates) <- 1:nrow(fortnight_dates)

#create surveys for each fortnight
new_survey <- list()
for (i in 1:length(ct_fortnight)) {
  new_survey[[i]] <- survey(pt_fortnight[[i]], ct_fortnight[[i]])
}

#create contact matrices for each survey round 
fortnights <- list()
for (i in 1:length(ct_fortnight)) {
  fortnights[[i]] <- contact_matrix(new_survey[[i]], weigh.dayofweek = T, 
                               age.limits = c(0, 5, 12, 18, 30, 40, 
                                              50, 60, 65, 70),
                               filter = list(cnt_work = 1))
}


#get dominant eigenvalues for each fortnight
e_fortnights <- list()
for(i in 1:length(ct_fortnight)) {
  matrix <- fortnights[[i]][["matrix"]]
  matrix[is.na(matrix)] <- 0
  e <- eigen(matrix)
  e_fortnights[i] <- Re(e$values[1])
}

#format results
e_fortnights_frame <- t(data.frame(e_fortnights))
rownames(e_fortnights_frame) <- 1:nrow(e_fortnights_frame)
e_fortnights_frame <- cbind(fortnight_dates, e_fortnights_frame)
colnames(e_fortnights_frame) <- c("fortnight", "dominant_eigenvalue")
e_fortnights_frame <- as.data.table(e_fortnights_frame)

#save dominant eigenvalues
qs::qsave(e_fortnights_frame, file.path(data_path, "comix_eigens_work_BE_archive_fortnightly.qs"))
