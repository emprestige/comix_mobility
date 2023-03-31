##data management for mobility data

library(data.table)

data_path <-"C:\\Users\\emiel\\Documents\\LSHTM\\Fellowship\\Project\\comix_mobility\\Data\\"

mob_20 <- fread(file.path(data_path, "2020_NL_Region_Mobility_Report.csv"))
mob_21 <- fread(file.path(data_path, "2021_NL_Region_Mobility_Report.csv"))
mob_22 <- fread(file.path(data_path, "2022_NL_Region_Mobility_Report.csv"))

region <- rbind(mob_20, mob_21, mob_22)

qs::qsave(region, "C:\\Users\\emiel\\Documents\\LSHTM\\Fellowship\\Project\\comix_mobility\\Data\\google_mob_region_NL.qs")

NL_20 <- mob_20[sub_region_1 == ""]
NL_21 <- mob_21[sub_region_1 == ""]
NL_22 <- mob_22[sub_region_1 == ""]

NL_mob <- rbind(NL_20, NL_21, NL_22)

qs::qsave(NL_mob, "C:\\Users\\emiel\\Documents\\LSHTM\\Fellowship\\Project\\comix_mobility\\Data\\google_mob_NL.qs")
