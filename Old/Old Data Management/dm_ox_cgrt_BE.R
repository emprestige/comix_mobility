##data management of stringency index components

#import libraries
library(lubridate)
library(data.table)

#set data path
data_path <-"C:\\Users\\emiel\\Documents\\LSHTM\\Fellowship\\Project\\comix_mobility\\Data\\"

#load data
ox_cgrt <- read.csv(file.path(data_path, "OxCGRT_nat_latest.csv"))
ox_cgrt <- as.data.table(ox_cgrt)
ox_cgrt <- ox_cgrt[CountryName == "Belgium", ]

#format dates
ox_cgrt$Date <- ymd(ox_cgrt$Date)
ox_cgrt1 <- ox_cgrt[Date >= "2020-03-23" & Date <= "2022-03-02"]

#format variables
ox_cgrt1 <- ox_cgrt1[, .(date = Date, C1M_School.closing, C1M_Flag, 
                         C2M_Workplace.closing, C2M_Flag, 
                         C7M_Restrictions.on.internal.movement, 
                         C7M_Flag, H1_Public.information.campaigns, H1_Flag)]
ox_cgrt1 <- ox_cgrt1[, C1M_School.closing := ifelse(C1M_Flag == 1, C1M_School.closing, 0)]
ox_cgrt1 <- ox_cgrt1[, C2M_Workplace.closing := ifelse(C2M_Flag == 1, 
                                                C2M_Workplace.closing, 0)]
ox_cgrt1 <- ox_cgrt1[, C7M_Restrictions.on.internal.movement := ifelse(C7M_Flag == 1,
                       C7M_Restrictions.on.internal.movement, 0)]
ox_cgrt1 <- ox_cgrt1[, .(date, C1M_School.closing, C2M_Workplace.closing,  
                         C7M_Restrictions.on.internal.movement, 
                         H1_Public.information.campaigns)]

#set NAs to 0
ox_cgrt1[is.na(ox_cgrt1)] <- 0

#save
qs::qsave(ox_cgrt1, file.path(data_path, "stringency_component_BE.qs"))
