## data management of polymod data

#import libraries
library(socialmixr)
library(lubridate)

#import data
data(polymod)

#load participant and contact data for UK
poly_p = polymod$participants[country == "United Kingdom"]
poly_c = polymod$contacts
poly = merge(poly_p, poly_c, by = "part_id", all.x = TRUE)

#format data 
poly[, d_home      := cnt_home == 1]
poly[, d_leisure   := cnt_leisure == 1]
poly[, d_transport := cnt_transport == 1]
poly[, d_school    := ifelse(part_age < 18, cnt_school == 1 & cnt_home == 0, 
                             cnt_school == 1 & cnt_home == 0 & cnt_work == 0)]
poly[, d_work      := ifelse(part_age >= 18, cnt_work == 1 & cnt_home == 0,
                             cnt_work == 1 & cnt_home == 0 & cnt_school == 0)]
poly[, d_other     := (cnt_transport + cnt_leisure + cnt_otherplace) >= 1 & cnt_home == 0 & cnt_school == 0 & cnt_work == 0]
poly = poly[!is.na(d_home)] # only cuts out 6 contacts.

#sum contacts
pnum = poly[, .(home = sum(d_home), work = sum(d_work), school = sum(d_school), 
                other = sum(d_other), phys = sum(phys_contact)), 
            by = .(part_id, part_age, date = ymd(sday_id))]
pnum = pnum[, lapply(.SD, function(x) pmin(50, x)), by = .(part_id, part_age, date), 
            .SDcols = c("home", "work", "school", "other", "phys")]

#insert extra variables 
pnum[, t := as.numeric(date - ymd("2006-01-01"))]
pnum[, all := home + work + school + other]
# pnum[, retail_recreation := 0]
# pnum[, grocery_pharmacy := 0]
# pnum[, parks := 0]
# pnum[, transit_stations := 0]
# pnum[, workplaces := 0]
# pnum[, residential := 0]
pnum[, study := "POLYMOD"]

#save
qs::qsave(pnum, "C:\\Users\\emiel\\Documents\\LSHTM\\Fellowship\\Project\\comix_mobility\\Data\\polymod.qs")
