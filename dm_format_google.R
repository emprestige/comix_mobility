##data management for mobility data

library(data.table)

data_path <-"C:\\Users\\emiel\\OneDrive\\Documents\\LSHTM\\Fellowship\\Project\\comix_mobility\\Data\\"

mob_20 <- as.data.table(read.csv(file.path(data_path, "2020_GB_Region_Mobility_Report.csv")))
mob_21 <- as.data.table(read.csv(file.path(data_path, "2021_GB_Region_Mobility_Report.csv")))
mob_22 <- as.data.table(read.csv(file.path(data_path, "2022_GB_Region_Mobility_Report.csv")))

UK_20 <- mob_20[sub_region_1 == ""]
UK_21 <- mob_21[sub_region_1 == ""]
UK_22 <- mob_22[sub_region_1 == ""]

UK_mob <- rbind(UK_20, UK_21, UK_22)

qs::qsave(UK_mob, "C:\\Users\\emiel\\OneDrive\\Documents\\LSHTM\\Fellowship\\Project\\comix_mobility\\Data\\google_mob.qs")
