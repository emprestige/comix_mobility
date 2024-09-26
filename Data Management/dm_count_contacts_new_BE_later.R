# Packages ----------------------------------------------------------------
library(data.table)
library(dplyr)
library(here)

## Save participant data
dir_data_validate <- here("data")

pt <- as.data.table(read.csv(file.path(dir_data_validate, "CoMix_2_participant_extra.csv")))
pt_min <- as.data.table(read.csv(file.path(dir_data_validate, "CoMix_2_participant_common.csv")))
ct_min <- as.data.table(read.csv(file.path(dir_data_validate, "CoMix_2_contact_common.csv")))
ct <- as.data.table(read.csv(file.path(dir_data_validate, "CoMix_2_contact_extra.csv")))
dates <- as.data.table(read.csv(file.path(dir_data_validate, "CoMix_2_sday.csv")))

# #separate mass contacts 
# ct_mass <- ct[cnt_mass == "mass"]
# ct_nonmass <- ct[cnt_mass == "individual"]
# 
# #filter out when there are more than 50 observations for work
# ct_mass_groups_work <- ct_mass %>% 
#   filter(cnt_work == 1) %>%
#   group_by(part_id, date) %>%
#   mutate(count = n()) %>%
#   filter(count > 50) %>%
#   sample_n(size = 50)

# #put mass work contacts back in to main work contact frame
# ct_mass_groups_work <- ct_mass_groups_work[, -69]
# ct_work <- rbind(ct_nonmass, ct_mass_groups_work)
# 
# #filter out when there are more than 50 observations for other
# ct_mass_groups_other <- ct_mass %>% 
#   filter(cnt_other == 1) %>%
#   group_by(part_id, date) %>%
#   mutate(count = n()) %>%
#   filter(count > 50) %>%
#   sample_n(size = 50)
 
# #put mass other contacts back in to main other contact frame
# ct_mass_groups_other <- ct_mass_groups_other[, -69]
# ct_other <- rbind(ct_nonmass, ct_mass_groups_other)
# 
# cnts <- merge(dates, pt_min, by = "part_id")
# cnts <- merge(cnts, ct_min, by = "part_id")

pt_dates <- merge(dates, pt_min, by = "part_id")

# Map objects for labels --------------------------------------------------
cnt_main_vars <- c(
  "cnt_home", 
  "cnt_work",
  "cnt_school",
  "cnt_otherplace"
)

# cnt_other_vars <- c(
#   "cnt_inside", 
#   "cnt_outside", 
#   "cnt_sport", 
#   "cnt_outside_other",
#   "cnt_other_place", 
#   "cnt_other_house",
#   "cnt_worship",
#   "cnt_public_transport", 
#   "cnt_supermarket",
#   "cnt_shop",
#   "cnt_bar_rest",
#   "cnt_health_facility", 
#   "cnt_salon",
#   "cnt_public_market"
# )

#cnt_vars <- c(cnt_main_vars, cnt_other_vars)
all_vars <- c(cnt_main_vars, "part_id")
ct_work <- rlang::duplicate(ct_min)
ct_other <- rlang::duplicate(ct_min)
ct_work <- ct_work[, ..all_vars, with = FALSE]
ct_other <- ct_other[, ..all_vars]

sumna <- function(x) sum(x, na.rm = TRUE)

cp_n_cnts_work <- ct_work[, .(
  n_cnt = .N,
  n_cnt_home             = sumna(cnt_home),
  n_cnt_work             = sumna(cnt_work),
  n_cnt_school           = sumna(cnt_school),
  n_cnt_other            = sumna(cnt_otherplace)
),
by = part_id]

cp_n_cnts_other <- ct_other[, .(
  n_cnt = .N,
  n_cnt_home             = sumna(cnt_home),
  n_cnt_work             = sumna(cnt_work),
  n_cnt_school           = sumna(cnt_school),
  n_cnt_other            = sumna(cnt_otherplace)
),
by = part_id]

pt_cnt_work = merge(pt_dates, cp_n_cnts_work, by = c("part_id"), all.x = TRUE)
#pt_cnt_min_work <- merge(pt_min, cp_n_cnts_work, by = c("part_wave_uid"), all.x = TRUE)

pt_cnt_other = merge(pt_dates, cp_n_cnts_other, by = c("part_id"), all.x = TRUE)
#pt_cnt_min_other <- merge(pt_min, cp_n_cnts_other, by = c("part_wave_uid"), all.x = TRUE)

var_list_work <- names(cp_n_cnts_work)
for (j in var_list_work){
  set(pt_cnt_work, which(is.na(pt_cnt_work[[j]])),j,0)
}

var_list_other <- names(cp_n_cnts_other)
for (j in var_list_other){
  set(pt_cnt_other, which(is.na(pt_cnt_other[[j]])),j,0)
}

# count contacts but unique for home work school other --------------------
ct_p_work <- merge(ct_work, pt_dates, by = c("part_id"), all.x = TRUE)
ct_p_work <- ct_p_work[, sample_type := ifelse(part_age == "Under 1" | part_age == "1-4" | part_age == "5-11" | part_age == "12-15" | part_age == "16-17", "child", "adult")] 
ct_p_other <- merge(ct_other, pt_dates, by = c("part_id"), all.x = TRUE)
ct_p_other <- ct_p_other[, sample_type := ifelse(part_age == "Under 1" | part_age == "1-4" | part_age == "5-11" | part_age == "12-15" | part_age == "16-17", "child", "adult")] 

ct_p_work[, d_home   := cnt_home == 1]
ct_p_work[, d_school := ifelse(sample_type == "child", cnt_school == 1 & cnt_home == 0, cnt_school == 1 & cnt_home == 0 & cnt_work == 0)]
ct_p_work[, d_work := ifelse(sample_type == "adult", cnt_work == 1 & cnt_home == 0, cnt_work == 1 & cnt_home == 0 & cnt_school == 0)]
ct_p_work[, d_other  := cnt_otherplace]

ct_p_other[, d_home   := cnt_home == 1]
ct_p_other[, d_school := ifelse(sample_type == "child", cnt_school == 1 & cnt_home == 0, cnt_school == 1 & cnt_home == 0 & cnt_work == 0)]
ct_p_other[, d_work := ifelse(sample_type == "adult", cnt_work == 1 & cnt_home == 0, cnt_work == 1 & cnt_home == 0 & cnt_school == 0)]
ct_p_other[, d_other  := cnt_otherplace]

ct_p_cnts_work <- ct_p_work[, .(
  all = .N,
  n_cnt_unq_home             = sumna(d_home),
  n_cnt_unq_work             = sumna(d_work),
  n_cnt_unq_school           = sumna(d_school),
  n_cnt_unq_other            = sumna(d_other)
),
by = part_id]

ct_p_cnts_other <- ct_p_other[, .(
  all = .N,
  n_cnt_unq_home             = sumna(d_home),
  n_cnt_unq_work             = sumna(d_work),
  n_cnt_unq_school           = sumna(d_school),
  n_cnt_unq_other            = sumna(d_other)
),
by = part_id]

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

pt_cnt_work <- merge(pt_cnt_work, ct_p_cnts_work, by = "part_id", all = TRUE)

pt_cnt_other <- merge(pt_cnt_other, ct_p_cnts_other, by = "part_id", all = TRUE)

var_list_unq_work <- names(ct_p_cnts_work)
for (j in var_list_unq_work){
  set(pt_cnt_work, which(is.na(pt_cnt_work[[j]])),j,0)
}

var_list_unq_other <- names(ct_p_cnts_other)
for (j in var_list_unq_other){
  set(pt_cnt_other, which(is.na(pt_cnt_other[[j]])),j,0)
}

cnt_names_work <- grep("n_cnt", names(pt_cnt_work), value = TRUE)
cnt_names_work <- c("part_id", "wave", "dayofweek", "part_age", "part_gender",
                    "sday_id", cnt_names_work)

cnt_names_other <- grep("n_cnt", names(pt_cnt_other), value = TRUE)
cnt_names_other <- c("part_id", "wave", "dayofweek", "part_age", "part_gender", 
                     "sday_id", cnt_names_other)

#subset 
pt_cnt_work <- pt_cnt_work[, ..cnt_names_work]
pt_cnt_other <- pt_cnt_other[, ..cnt_names_other]

#create weighting based on weekday/weekend
pt_cnt_work[, day_weight := ifelse(dayofweek == "5", 2/7, 
                                   ifelse(dayofweek == "6", 2/7, 5/7))]
pt_cnt_other[, day_weight := ifelse(dayofweek == "5", 2/7, 
                                    ifelse(dayofweek == "6", 2/7, 5/7))]

#filter for just UK surveys
pt_cnt_work <- pt_cnt_work[substr(part_id, 1, 2) == "be"]
pt_cnt_other <- pt_cnt_other[substr(part_id, 1, 2) == "be"]

#add study name
pt_cnt_work <- pt_cnt_work[, study := "CoMix"]
pt_cnt_other <- pt_cnt_other[, study := "CoMix"]

#save as qs file
qs::qsave(pt_cnt_work, file.path(dir_data_validate, "part_cnts_work_BE_later.qs"))
qs::qsave(pt_cnt_other, file.path(dir_data_validate, "part_cnts_other_BE_later.qs"))
