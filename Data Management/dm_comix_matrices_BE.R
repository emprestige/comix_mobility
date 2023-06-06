##estimate contact matrices from CoMix data for belgium

#load libraries
library(data.table)
library(ggplot2)
library(tidyverse)
library(lubridate)
library(cowplot)
library(ggrepel)
library(socialmixr)

#set cowplot theme
theme_set(cowplot::theme_cowplot(font_size = 10) + theme(strip.background = element_blank()))

#set data path
data_path <-"C:\\Users\\emiel\\Documents\\LSHTM\\Fellowship\\Project\\comix_mobility\\Data\\"

#import participant and contact data
pt <- qs::qread(file.path(data_path, "participants_BE.qs"))
ct <- qs::qread(file.path(data_path, "contact_BE.qs"))

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

#create surveys for each week
new_survey <- list()
for (i in 1:9) {
  new_survey[[i]] <- survey(pt_week[[i]], ct_week[[i]])
}

#create contact matrices for each survey round 
weeks <- list()
for (i in 1:9) {
  weeks[[i]] <- contact_matrix(new_survey[[i]], weigh.dayofweek = T, 
                               age.limits = c(0, 5, 12, 18, 30, 40, 
                                              50, 60, 65, 70))
}

# #melt dataframes 
# dt <- list()
# for (i in 1:9) {
#   dt[[i]] <- melt(data.table(weeks[[i]]$matrix, keep.rownames = TRUE) ,
#                   id.vars = c("rn"))
#   dt[[i]]$rn <- factor(dt[[i]]$rn, levels = c("[0,5)", "[5,12)", "[12,18)",
#                                               "[18,30)", "[30,40)", "[40,50)",
#                                               "[50,60)", "[60,65)", "[65,70)",
#                                               "70+"))
# }

#get dominant eigenvalues for each week
e_weeks <- list()
for(i in 1:9) {
  matrix <- weeks[[i]][["matrix"]]
  matrix[is.na(matrix)] <- 0
  e <- eigen(matrix)
  e_weeks[i] <- e$values[1]
}

#save dominant eigenvalues
qs::qsave(e_weeks, file.path(data_path, "comix_eigens_BE.qs"))
