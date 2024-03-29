## data management of polymod data

#import libraries
library(socialmixr)
library(lubridate)

#import data
data(polymod)

#load participant and contact data for Belgium
poly_p = polymod$participants[country == "Belgium"]
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
poly[, d_other     := ifelse(cnt_home == 0 & cnt_work == 0 & cnt_school == 0, 1, 0)]
poly[, d_phys      := phys_contact == 1]
#remove NAs
poly = poly[!is.na(d_home)]

#sum contacts
pnum = poly[, .(home = sum(d_home), work = sum(d_work), school = sum(d_school), 
                other = sum(d_other), phys = sum(d_phys)), 
            by = .(part_id, part_age, date = ymd(sday_id))]
pnum = pnum[, lapply(.SD, function(x) pmin(50, x)), by = .(part_id, part_age, date), 
            .SDcols = c("home", "work", "school", "other", "phys")]

#insert extra variables 
pnum[, t := as.numeric(date - ymd("2006-01-01"))]
pnum[, all := home + work + school + other]
pnum[, study := "POLYMOD"]

#save
qs::qsave(pnum, "C:\\Users\\emiel\\Documents\\LSHTM\\Fellowship\\Project\\comix_mobility\\Data\\polymod_BE.qs")
