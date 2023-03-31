##data management of stringency index

#import libraries
library(lubridate)
library(data.table)

#set data path
data_path <-"C:\\Users\\emiel\\Documents\\LSHTM\\Fellowship\\Project\\comix_mobility\\Data\\"

#load data
stringency <- read.csv(file.path(data_path, "owid-covid-data.csv"))
stringency <- as.data.table(stringency)

#filter to relevant information
stringency_uk <- stringency[location == "United Kingdom"]
stringency_filt <- stringency_uk[, .(date, stringency_index)]

#save
qs::qsave(stringency_filt, file.path(data_path, "stringency.qs"))
