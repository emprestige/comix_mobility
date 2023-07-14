# Packages ----------------------------------------------------------------
library(data.table)
library(dplyr)
  
## Save participant data
dir_data_validate <- "C:\\Users\\emiel\\Filr\\Net Folders\\EPH Shared\\Comix_survey\\data\\validated\\"

pt <- qs::qread(file.path(dir_data_validate, "part.qs"))
pt_min <- qs::qread(file.path(dir_data_validate, "part_min.qs"))
ct <- qs::qread(file.path(dir_data_validate, "contacts.qs"))

#separate mass contacts 
ct_mass <- ct[cnt_mass == "mass"]
ct_nonmass <- ct[cnt_mass == "individual"]

#filter out when there are more than 50 observations for work
ct_mass_groups_work <- ct_mass %>% 
  filter(cnt_work == 1) %>%
  group_by(part_id, date) %>%
  mutate(count = n()) %>%
  filter(count > 50) %>%
  sample_n(size = 50)

#put mass work contacts back in to main work contact frame
ct_mass_groups_work <- ct_mass_groups_work[, -68]
ct_work <- rbind(ct_nonmass, ct_mass_groups_work)

#filter out when there are more than 50 observations for other
ct_mass_groups_other <- ct_mass %>% 
  filter(cnt_other == 1) %>%
  group_by(part_id, date) %>%
  mutate(count = n()) %>%
  filter(count > 50) %>%
  sample_n(size = 50)

#put mass other contacts back in to main other contact frame
ct_mass_groups_other <- ct_mass_groups_other[, -68]
ct_other <- rbind(ct_nonmass, ct_mass_groups_other)

#filter out bad survey rounds
pt <- pt[survey_round != "6"]
ct_work <- ct_work[survey_round != "6"]
ct_other <- ct_other[survey_round != "6"]
pt <- pt[survey_round != "7"]
ct_work <- ct_work[survey_round != "7"]
ct_other <- ct_other[survey_round != "7"]

# Map objects for labels --------------------------------------------------
cnt_main_vars <- c(
  "cnt_home", 
  "cnt_work",
  "cnt_school",
  "cnt_other",
  "cnt_phys"
)

cnt_other_vars <- c(
  "cnt_inside", 
  "cnt_outside", 
  "cnt_sport", 
  "cnt_outside_other",
  "cnt_other_place", 
  "cnt_other_house",
  "cnt_worship",
  "cnt_public_transport", 
  "cnt_supermarket",
  "cnt_shop",
  "cnt_bar_rest",
  "cnt_health_facility", 
  "cnt_salon",
  "cnt_public_market"
)

cnt_vars <- c(cnt_main_vars, cnt_other_vars)
all_vars <- c(cnt_vars, "part_wave_uid", "weekday")
ct_work <- ct_work[, ..all_vars]
ct_other <- ct_other[, ..all_vars]

sumna <- function(x) sum(x, na.rm = TRUE)

cp_n_cnts_work <- ct_work[, .(
  n_cnt = .N,
  n_cnt_home             = sumna(cnt_home),
  n_cnt_work             = sumna(cnt_work),
  n_cnt_school           = sumna(cnt_school),
  n_cnt_other            = sumna(cnt_other),
  n_cnt_phys             = sumna(cnt_phys),
  n_cnt_inside           = sumna(cnt_inside),
  n_cnt_outside          = sumna(cnt_outside),
  n_cnt_sport            = sumna(cnt_sport),
  n_cnt_outside_other    = sumna(cnt_outside_other),
  n_cnt_other_place      = sumna(cnt_other_place),
  n_cnt_other_house      = sumna(cnt_other_house),
  n_cnt_worship          = sumna(cnt_worship),
  n_cnt_public_transport = sumna(cnt_public_transport),
  n_cnt_supermarket      = sumna(cnt_supermarket),
  n_cnt_shop             = sumna(cnt_shop),
  n_cnt_bar_rest         = sumna(cnt_bar_rest),
  n_cnt_health_facility  = sumna(cnt_health_facility),
  n_cnt_salon            = sumna(cnt_salon)
),
by = part_wave_uid]

cp_n_cnts_other <- ct_other[, .(
  n_cnt = .N,
  n_cnt_home             = sumna(cnt_home),
  n_cnt_work             = sumna(cnt_work),
  n_cnt_school           = sumna(cnt_school),
  n_cnt_other            = sumna(cnt_other),
  n_cnt_phys             = sumna(cnt_phys),
  n_cnt_inside           = sumna(cnt_inside),
  n_cnt_outside          = sumna(cnt_outside),
  n_cnt_sport            = sumna(cnt_sport),
  n_cnt_outside_other    = sumna(cnt_outside_other),
  n_cnt_other_place      = sumna(cnt_other_place),
  n_cnt_other_house      = sumna(cnt_other_house),
  n_cnt_worship          = sumna(cnt_worship),
  n_cnt_public_transport = sumna(cnt_public_transport),
  n_cnt_supermarket      = sumna(cnt_supermarket),
  n_cnt_shop             = sumna(cnt_shop),
  n_cnt_bar_rest         = sumna(cnt_bar_rest),
  n_cnt_health_facility  = sumna(cnt_health_facility),
  n_cnt_salon            = sumna(cnt_salon)
),
by = part_wave_uid]

pt_cnt_work = merge(pt, cp_n_cnts_work, by = c("part_wave_uid"), all.x = TRUE)
pt_cnt_min_work <- merge(pt_min, cp_n_cnts_work, by = c("part_wave_uid"), all.x = TRUE)

pt_cnt_other = merge(pt, cp_n_cnts_other, by = c("part_wave_uid"), all.x = TRUE)
pt_cnt_min_other <- merge(pt_min, cp_n_cnts_other, by = c("part_wave_uid"), all.x = TRUE)

var_list_work <- names(cp_n_cnts_work)
for (j in var_list_work){
  set(pt_cnt_work, which(is.na(pt_cnt_work[[j]])),j,0)
  set(pt_cnt_min_work, which(is.na(pt_cnt_work[[j]])),j,0)
}

var_list_other <- names(cp_n_cnts_other)
for (j in var_list_other){
  set(pt_cnt_other, which(is.na(pt_cnt_other[[j]])),j,0)
  set(pt_cnt_min_other, which(is.na(pt_cnt_other[[j]])),j,0)
}

# count contacts but unique for home work school other --------------------
ct_p_work <- merge(ct_work, pt, by = c("part_wave_uid"), all.x = TRUE)
ct_p_other <- merge(ct_other, pt, by = c("part_wave_uid"), all.x = TRUE)

ct_p_work[, d_home   := cnt_home == 1]
ct_p_work[, d_school := ifelse(sample_type == "child", cnt_school == 1 & cnt_home == 0, cnt_school == 1 & cnt_home == 0 & cnt_work == 0)]
ct_p_work[, d_work := ifelse(sample_type == "adult", cnt_work == 1 & cnt_home == 0, cnt_work == 1 & cnt_home == 0 & cnt_school == 0)]
ct_p_work[, d_other  := cnt_other ]
ct_p_work[, d_phys  := cnt_phys]

ct_p_other[, d_home   := cnt_home == 1]
ct_p_other[, d_school := ifelse(sample_type == "child", cnt_school == 1 & cnt_home == 0, cnt_school == 1 & cnt_home == 0 & cnt_work == 0)]
ct_p_other[, d_work := ifelse(sample_type == "adult", cnt_work == 1 & cnt_home == 0, cnt_work == 1 & cnt_home == 0 & cnt_school == 0)]
ct_p_other[, d_other  := cnt_other ]
ct_p_other[, d_phys  := cnt_phys]

ct_p_cnts_work <- ct_p_work[, .(
    all = .N,
     n_cnt_unq_home             = sumna(d_home),
     n_cnt_unq_work             = sumna(d_work),
     n_cnt_unq_school           = sumna(d_school),
     n_cnt_unq_other            = sumna(d_other),
     n_cnt_unq_phys             = sumna(d_phys),
     n_cnt_unq_inside           = sumna(cnt_inside),
     n_cnt_unq_outside          = sumna(cnt_outside),
     n_cnt_unq_sport            = sumna(cnt_sport),
     n_cnt_unq_outside_other    = sumna(cnt_outside_other),
     n_cnt_unq_other_place      = sumna(cnt_other_place),
     n_cnt_unq_other_house      = sumna(cnt_other_house),
     n_cnt_unq_worship          = sumna(cnt_worship),
     n_cnt_unq_public_transport = sumna(cnt_public_transport),
     n_cnt_unq_supermarket      = sumna(cnt_supermarket),
     n_cnt_unq_shop             = sumna(cnt_shop),
     n_cnt_unq_bar_rest         = sumna(cnt_bar_rest),
     n_cnt_unq_health_facility  = sumna(cnt_health_facility),
     n_cnt_unq_salon            = sumna(cnt_salon)
),
by = part_wave_uid]

ct_p_cnts_other <- ct_p_other[, .(
  all = .N,
  n_cnt_unq_home             = sumna(d_home),
  n_cnt_unq_work             = sumna(d_work),
  n_cnt_unq_school           = sumna(d_school),
  n_cnt_unq_other            = sumna(d_other),
  n_cnt_unq_phys             = sumna(d_phys),
  n_cnt_unq_inside           = sumna(cnt_inside),
  n_cnt_unq_outside          = sumna(cnt_outside),
  n_cnt_unq_sport            = sumna(cnt_sport),
  n_cnt_unq_outside_other    = sumna(cnt_outside_other),
  n_cnt_unq_other_place      = sumna(cnt_other_place),
  n_cnt_unq_other_house      = sumna(cnt_other_house),
  n_cnt_unq_worship          = sumna(cnt_worship),
  n_cnt_unq_public_transport = sumna(cnt_public_transport),
  n_cnt_unq_supermarket      = sumna(cnt_supermarket),
  n_cnt_unq_shop             = sumna(cnt_shop),
  n_cnt_unq_bar_rest         = sumna(cnt_bar_rest),
  n_cnt_unq_health_facility  = sumna(cnt_health_facility),
  n_cnt_unq_salon            = sumna(cnt_salon)
),
by = part_wave_uid]

ct_p_cnts_work[, n_cnt_unq := n_cnt_unq_home + n_cnt_unq_work + n_cnt_unq_school + n_cnt_unq_other]
ct_p_cnts_work[, delta := all - n_cnt_unq]
ct_p_cnts_work[, n_cnt_unq := n_cnt_unq + delta]
ct_p_cnts_work[, n_cnt_unq_other := n_cnt_unq_other + delta]
ct_p_cnts_work[, delta := NULL]
ct_p_cnts_work[, all := NULL]

ct_p_cnts_work[, n_cnt_unq_workschool := n_cnt_unq_work + n_cnt_unq_school]

ct_p_cnts_other[, n_cnt_unq := n_cnt_unq_home + n_cnt_unq_work + n_cnt_unq_school + n_cnt_unq_other]
ct_p_cnts_other[, delta := all - n_cnt_unq]
ct_p_cnts_other[, n_cnt_unq := n_cnt_unq + delta]
ct_p_cnts_other[, n_cnt_unq_other := n_cnt_unq_other + delta]
ct_p_cnts_other[, delta := NULL]
ct_p_cnts_other[, all := NULL]

ct_p_cnts_other[, n_cnt_unq_workschool := n_cnt_unq_work + n_cnt_unq_school]

pt_cnt_work <- merge(pt_cnt_work, ct_p_cnts_work, by = "part_wave_uid", all = TRUE)
pt_cnt_min_work <- merge(pt_cnt_min_work, ct_p_cnts_work, by = "part_wave_uid", all = TRUE)

pt_cnt_other <- merge(pt_cnt_other, ct_p_cnts_other, by = "part_wave_uid", all = TRUE)
pt_cnt_min_other <- merge(pt_cnt_min_other, ct_p_cnts_other, by = "part_wave_uid", all = TRUE)

var_list_unq_work <- names(ct_p_cnts_work)
for (j in var_list_unq_work){
  set(pt_cnt_work, which(is.na(pt_cnt_work[[j]])),j,0)
  set(pt_cnt_min_work, which(is.na(pt_cnt_work[[j]])),j,0)
}

var_list_unq_other <- names(ct_p_cnts_other)
for (j in var_list_unq_other){
  set(pt_cnt_other, which(is.na(pt_cnt_other[[j]])),j,0)
  set(pt_cnt_min_other, which(is.na(pt_cnt_other[[j]])),j,0)
}

cnt_names_work <- grep("n_cnt", names(pt_cnt_min_work), value = TRUE)
cnt_names_work <- c("part_wave_uid", "panel", "part_id", "sample_type", 
                    "part_age", "part_age_group", "survey_round", "date", 
                    "weekday", "part_vacc", "part_employstatus", 
                    "part_attend_work_yesterday", "part_limit_work", 
                    "part_work_closed", "part_occupation", "part_gender_nb", 
                    "area_rural_urban_code", "part_ethnicity",
                    "area_rural_urban_label", "area_3_name", "hh_size_group",
                    "hh_type", "part_high_risk", "part_social_group", 
                    cnt_names_work)

cnt_names_other <- grep("n_cnt", names(pt_cnt_min_other), value = TRUE)
cnt_names_other <- c("part_wave_uid", "panel", "part_id", "sample_type", 
                     "part_age", "part_age_group", "survey_round", "date", 
                     "weekday", "part_vacc", "part_employstatus", 
                     "part_attend_work_yesterday", "part_limit_work", 
                     "part_work_closed", "part_occupation", "part_gender_nb",
                     "area_rural_urban_code", "part_ethnicity",
                     "area_rural_urban_label", "area_3_name", "hh_size_group",
                     "hh_type", "part_high_risk", "part_social_group", 
                     cnt_names_other)

#subset 
pt_cnt_work <- pt_cnt_work[, ..cnt_names_work]
pt_cnt_other <- pt_cnt_other[, ..cnt_names_other]

#create weighting based on weekday/weekend
pt_cnt_work[, day_weight := ifelse(weekday == "Saturday", 2/7, 
                                   ifelse(weekday == "Sunday", 2/7, 5/7))]
pt_cnt_other[, day_weight := ifelse(weekday == "Saturday", 2/7, 
                                    ifelse(weekday == "Sunday", 2/7, 5/7))]

#filter for just UK surveys
pt_cnt_work <- pt_cnt_work[substr(part_wave_uid, 1, 2) == "uk"]
pt_cnt_other <- pt_cnt_other[substr(part_wave_uid, 1, 2) == "uk"]

#add study name
pt_cnt_work <- pt_cnt_work[, study := "CoMix"]
pt_cnt_other <- pt_cnt_other[, study := "CoMix"]

#save as qs file
qs::qsave(pt_cnt_work, "C:\\Users\\emiel\\Documents\\LSHTM\\Fellowship\\Project\\comix_mobility\\Data\\part_cnts_work.qs")
qs::qsave(pt_cnt_other, "C:\\Users\\emiel\\Documents\\LSHTM\\Fellowship\\Project\\comix_mobility\\Data\\part_cnts_other.qs")
