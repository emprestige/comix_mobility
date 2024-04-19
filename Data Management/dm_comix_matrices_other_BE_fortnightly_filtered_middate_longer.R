##estimate contact matrices from CoMix data for BE

#load libraries
library(data.table)
library(dplyr)
library(lubridate)
library(socialmixr)
library(here)

#set data path
data_path <- here("data")

#import participant and contact data
pt <- qs::qread(file.path(data_path, "participants_BE_longer.qs"))
pt <- pt[, part_age_group := part_age]
pt <- pt[, part_age := NULL]
ct <- qs::qread(file.path(data_path, "contact_other_BE_longer.qs"))
pt <- pt[, date := as.Date(parse_date_time(sday_id, orders = "ymd"))]
ct <- ct[, date := as.Date(parse_date_time(sday_id, orders = "ymd"))]

# Convert age groups to numeric vectors
pt$part_age_group_numeric <- sapply(pt$part_age_group, function(group) {
  as.numeric(unlist(strsplit(gsub("\\[|\\)", "", group), ",")))
})

# Function to calculate midpoint of an age group
midpoint <- function(group) {
  mean(group)
}

# Calculate midpoints for each age group
part_ages <- sapply(pt$part_age_group_numeric, midpoint)

# Update pt with the calculated midpoints
pt <- pt[, part_age := part_ages]

# Remove the intermediate numeric vector column
pt[, part_age_group_numeric := NULL]

#match column name for function
pt <- pt[, dayofweek := ifelse(pt$dayofweek == 0, "Monday",
                        ifelse(pt$dayofweek == 1, "Tuesday",
                        ifelse(pt$dayofweek == 2, "Wednesday",
                        ifelse(pt$dayofweek == 3, "Thursday",
                        ifelse(pt$dayofweek == 4, "Friday",
                        ifelse(pt$dayofweek == 5, "Saturday", "Sunday"))))))]

#filter to relevant dates 
pt_date <- pt[date <= ymd("2022-03-31")]
ct_date <- ct[date <= ymd("2022-03-31")]

#define fortnight variable
pt_date[, fortnight := paste(isoyear(date), "/", sprintf("%02d", ceiling(isoweek(date)/2)))]
pt_date[, start_date := min(date), by = .(fortnight)]
pt_date[, end_date := max(date), by = .(fortnight)]
pt_date[, mid_date := start_date + floor((end_date - start_date)/2) , by = .(fortnight)]
ct_date[, fortnight := paste(isoyear(date), "/", sprintf("%02d", ceiling(isoweek(date)/2)))]
ct_date[, start_date := min(date), by = .(fortnight)]
ct_date[, end_date := max(date), by = .(fortnight)]
ct_date[, mid_date := start_date + floor((end_date - start_date)/2) , by = .(fortnight)]

#separate data by fortnights
pt_middate <- pt_date %>% 
  group_split(mid_date)
ct_middate <- ct_date %>% 
  group_split(mid_date)

#get weeks for each contact matrix
middate_dates <- list()
for (i in 1:length(ct_middate)) {
  middate_dates[i] <- ct_middate[[i]]["mid_date"][[1]]
}
middate_dates <- t(data.frame(middate_dates))
rownames(middate_dates) <- 1:nrow(middate_dates)

#create surveys for each middate
new_survey <- list()
for (i in 1:length(ct_middate)) {
  new_survey[[i]] <- survey(pt_middate[[i]], ct_middate[[i]])
}

#create contact matrices for each survey round 
middates <- list()
for (i in 1:length(ct_middate)) {
  middates[[i]] <- contact_matrix(new_survey[[i]], weigh.dayofweek = T, 
                                  age.limits = c(0, 1, 5, 12, 18, 30, 40, 
                                                 50, 60, 70),
                                  estimated.contact.age = "mean",
                                  filter = list(cnt_otherplace = 1))
}

#get dominant eigenvalues for each middate
e_middates <- list()
for(i in 1:length(ct_middate)) {
  matrix <- middates[[i]][["matrix"]]
  matrix[is.na(matrix)] <- 0
  e <- eigen(matrix)
  e_middates[i] <- Re(e$values[1])
}

#format results
e_middates_frame <- t(data.frame(e_middates))
rownames(e_middates_frame) <- 1:nrow(e_middates_frame)
e_middates_frame <- cbind(middate_dates, e_middates_frame)
colnames(e_middates_frame) <- c("mid_date", "dominant_eigenvalue")
e_middates_frame <- as.data.table(e_middates_frame)
e_middates_frame$dominant_eigenvalue <- sapply(e_middates_frame$dominant_eigenvalue, as.numeric)

#save dominant eigenvalues
qs::qsave(e_middates_frame, file.path(data_path, "comix_eigens_other_BE_fortnightly_filtered_middate_longer.qs"))
