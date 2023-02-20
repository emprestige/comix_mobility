## data management of warwick data

#import libraries
library(socialmixr)
library(lubridate)
library(data.table)

#set data path
data_path <-"C:\\Users\\emiel\\Documents\\LSHTM\\Fellowship\\Project\\comix_mobility\\Data\\"

#load participant and contact data for UK
war_p <- read.csv(file.path(data_path, "Person_data.csv"))
war_c = read.csv(file.path(data_path, "Contact_data.csv"))
war <- merge(war_p, war_c, by.x = "P_ID", by.y = "C_PID", all.x = TRUE)
war <- as.data.table(war)

#format data 
war[, d_home      := C_Wheres_1 == 1]
war[, d_leisure   := C_Wheres_4 == 1]
war[, d_transport := C_Wheres_3 == 1]
war[, d_school    := P_age < 18 & C_Wheres_2 == 1]
war[, d_work      := P_age >= 18 & C_Wheres_2 == 1]
war[, d_other     := (C_Wheres_3 + C_Wheres_4 + C_Wheres_5 + C_Wheres_6) >= 1 & C_Wheres_1 == 0 & C_Wheres_2 == 0]
war[, d_phys      := C_Touch == 1]
#remove NAs
war = war[!is.na(d_home)]
war_date <- war
war_date$P_date_stamp_str <- as.Date(parse_date_time(war_date$P_date_stamp_str, c("dmy", "ymd")))

#sum contacts
wnum = war_date[, .(home = sum(d_home), work = sum(d_work), school = sum(d_school), 
                other = sum(d_other), phys = sum(d_phys)), 
                by = .(part_id = P_ID, part_age = P_age, date = ymd(P_date_stamp_str))]
wnum = wnum[, lapply(.SD, function(x) pmin(50, x)), by = .(part_id, part_age, date), 
            .SDcols = c("home", "work", "school", "other", "phys")]

#insert extra variables 
wnum[, t := as.numeric(date - ymd("2006-01-01"))]
wnum[, all := home + work + school + other]
wnum[, study := "Warwick"]

#save
qs::qsave(wnum, "C:\\Users\\emiel\\Documents\\LSHTM\\Fellowship\\Project\\comix_mobility\\Data\\warwick.qs")
