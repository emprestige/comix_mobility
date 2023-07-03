##reproduction numbers 

#load libraries
library(data.table)
library(tidyverse)
library(lubridate)

#set data path
data_path <-"C:\\Users\\emiel\\Documents\\LSHTM\\Fellowship\\Project\\comix_mobility\\Data\\"

rt <- read.csv(file.path(data_path, "rt.csv"))
rt <- as.data.table(rt)
rt <- rt[country == "United Kingdom"]
rt <- rt[date >= "2020-03-23" & date <= "2021-03-31"]
rt2 <- rlang::duplicate(rt)
rt2 <- rt2[, .(mean = mean(mean)), by = .(fortnight = paste(isoyear(date), "/", 
                                   sprintf("%02d", ceiling(isoweek(date)/2))))]

#get evenly spaced dates
my_list <- seq(1, 28, length.out = 4)

#extract the correct values 
rt3 <- rt2[my_list]

#export them 
qs::qsave(rt2, file.path(data_path, "reproduction_numbers_UK.qs"))
