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
rt <- rt[date <= ymd("2021-03-31")]
rt2 <- rlang::duplicate(rt)
rt2 <- rt2[, .(mean = mean(mean)), by = .(fortnight = paste(isoyear(date), "/", 
                                   sprintf("%02d", ceiling(isoweek(date)/2))))]

#extract the correct values 
rt3 <- rt2[c(4:7), ]

#export them 
qs::qsave(rt3, file.path(data_path, "reproduction_numbers_UK.qs"))
