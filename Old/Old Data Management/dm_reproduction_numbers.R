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

#export them 
qs::qsave(rt2, file.path(data_path, "reproduction_numbers_UK.qs"))

# source("https://gist.githubusercontent.com/sbfnk/d2900c745312219e3e48e08adde47cde/raw/c98fbdd738eafa71af12d05af0d3e068cf5b607b/get_covid19_nowcasts.r")
# df <- get_covid19_nowcasts()
# df <- as.data.table(df)
# df_be <- df[country == "Belgium"]
# df_nl <- df[country == "Netherlands"]
#neither have estimates early enough
