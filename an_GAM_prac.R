##GAM practice

#load libraries
library(data.table)
library(ggplot2)
library(tidyverse)
library(cowplot) 
library(ggpubr)
library(mgcv)
library(socialmixr)

#set data path
data_path <-"C:\\Users\\emiel\\Documents\\LSHTM\\Fellowship\\Project\\comix_mobility\\Data\\"

#import mobility data
mob <- qs::qread(file.path(data_path, "google_mob.qs"))

#subset for same date range
mob_sub <- mob[date >= "2020-03-24" & date <= "2022-03-02"]

#import participant and contact data
pt <- qs::qread(file.path(data_path, "participants.qs"))
ct <- qs::qread(file.path(data_path, "contact.qs"))

#filter down to one survey round
pt21 <- pt[survey_round == "21"]
ct21 <- ct[survey_round == "21"]

new_survey <- survey(pt21, ct21)
round21 <- contact_matrix(new_survey, age.limits = c(0, 5, 12, 18, 30, 40, 50, 60, 65, 70))

#plot contact matrix
dt = melt(data.table(round21$matrix, keep.rownames = TRUE) , id.vars = c("rn"))
ggplot(data = dt, aes(x = rn, y = variable, fill = value)) + geom_tile()
