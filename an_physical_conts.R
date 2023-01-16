## get physical contacts and compare to BBC pandemic data

#load libraries
library(data.table)
library(ggplot2)
library(tidyverse)
library(socialmixr)
library(cowplot)

#set cowplot theme
theme_set(cowplot::theme_cowplot(font_size = 10) + theme(strip.background = element_blank()))

#set data path
data_path <-"C:\\Users\\emiel\\Documents\\LSHTM\\Fellowship\\Project\\comix_mobility\\Data\\"

#import participant and contact data
pt <- qs::qread(file.path(data_path, "participants_filt.qs"))
ct <- qs::qread(file.path(data_path, "contact_filt.qs"))

#match column name for function
pt$dayofweek <- pt$weekday

#separate data by survey rounds 
pt_rounds <- pt %>% 
  group_split(survey_round)
ct_rounds <- ct %>%
  group_split(survey_round)

#create surveys for each survey round 
new_survey <- list()
for (i in 1:99) {
  new_survey[[i]] <- survey(pt_rounds[[i]], ct_rounds[[i]])
}

#create contact matrices for each survey round 
rounds <- list()
for (i in 1:99) {
  rounds[[i]] <- contact_matrix(new_survey[[i]], weigh.dayofweek = T, 
                                age.limits = c(0, 5, 12, 18, 30, 40, 
                                               50, 60, 65, 70))
}

#filter to physical contacts
rounds_phys <- list()
for (i in 1:99) {
  rounds_phys[[i]] <- contact_matrix(new_survey[[i]], weigh.dayofweek = T, 
                                     filter = list(cnt_phys = 1),
                                     age.limits = c(0, 5, 12, 18, 30, 40, 
                                                    50, 60, 65, 70))
}

#melt dataframes 
dt <- list()
for (i in 1:99) {
  dt[[i]] <- melt(data.table(rounds[[i]]$matrix, keep.rownames = TRUE) ,
                  id.vars = c("rn"))
  dt[[i]]$rn <- factor(dt[[i]]$rn, levels = c("[0,5)", "[5,12)", "[12,18)",
                                              "[18,30)", "[30,40)", "[40,50)",
                                              "[50,60)", "[60,65)", "[65,70)",
                                              "70+"))
}

#physical contacts
dt_phys <- list()
for (i in 1:99) {
  dt_phys[[i]] <- melt(data.table(rounds_phys[[i]]$matrix, keep.rownames = TRUE),
                       id.vars = c("rn"))
  dt_phys[[i]]$rn <- factor(dt_phys[[i]]$rn, levels = c("[0,5)", "[5,12)", 
                                                        "[12,18)", "[18,30)",
                                                        "[30,40)", "[40,50)",
                                                        "[50,60)", "[60,65)",
                                                        "[65,70)", "70+"))
}

#create plots 
plots <- list()
for (i in 1:99) {
  plots[[i]] <- ggplot(data = dt[[i]], aes(x = rn, y = variable, fill = value)) + 
    geom_tile() + coord_equal() + labs(x = "Particpant Age Group", 
                                       y = "Contact Age Group", 
                                       fill = "Mean Number \n of Contacts")
}

#physical plots
plots_phys <- list()
for (i in 1:99) {
  plots_phys[[i]] <- ggplot(data = dt_phys[[i]],
                            aes(x = rn, y = variable, fill = value)) + 
    geom_tile() + coord_equal() + labs(x = "Particpant Age Group", 
                                       y = "Contact Age Group", 
                                       fill = "Mean Number of \nPhysical Contacts")
}

#plot some to check 
plot_grid(plots[[1]], plots_phys[[1]])
plot_grid(plots[[28]], plots_phys[[28]])
plot_grid(plots[[55]], plots_phys[[55]])
plot_grid(plots[[99]], plots_phys[[99]])
plot_grid(plots[[21]], plots_phys[[21]])

##
#import contact data
cnts <- qs::qread(file.path(data_path, "part_cnts.qs"))

#sort ages
cnts <- cnts[part_age != is.na(part_age)]
cnts <- cnts[part_age >= 18 & part_age <= 65]
cnts <- cnts %>%
  mutate(part_age_group = case_when(part_age >= 18 & part_age <= 29 ~ "18-29",
                                    part_age >= 30 & part_age <= 39 ~ "30-39",
                                    part_age >= 40 & part_age <= 49 ~ "40-49",
                                    part_age >= 50 & part_age <= 59 ~ "50-59",
                                    part_age >= 60 & part_age <= 69 ~ "60-69",
                                    part_age >= 70 ~ "70+"))

#get average contacts both all and physical specifically
avg_cnt <- cnts %>%
  group_by(part_age_group) %>%
  summarise(all = mean(n_cnt), physical = mean(n_cnt_phys)) %>%
  gather("contact_type", "contacts", -part_age_group)
avg_cnt$part_age_group <- factor(avg_cnt$part_age_group, 
                                 levels = c("0-4", "5-11", "12-17", "18-29",
                                            "30-39", "40-49", "50-59", "60-69",
                                            "70+"))

#plot
ggplot(avg_cnt) + geom_line(aes(part_age_group, contacts, group = contact_type, 
                                linetype = contact_type)) +
  geom_point(aes(part_age_group, contacts)) + 
  labs(x = "Age Group", y = "Average Number of Contacts", 
       linetype = "Type of Contact")

#import edited polymod data
pnum <- qs::qread(file.path(data_path, "polymod.qs"))

#add weighting to polymod data
pnum[, weekday := lubridate::wday(date, label = T, abbr = F)]
pnum[, day_weight := ifelse(weekday == "Saturday", 2/7, 
                            ifelse(weekday == "Sunday", 2/7, 5/7))]

#create age groups
pnum <- pnum[part_age != is.na(part_age)]
pnum <- pnum[part_age >= 18 & part_age <= 65]
pnum <- pnum %>%
  mutate(part_age_group = case_when(part_age >= 18 & part_age <= 29 ~ "18-29",
                                    part_age >= 30 & part_age <= 39 ~ "30-39",
                                    part_age >= 40 & part_age <= 49 ~ "40-49",
                                    part_age >= 50 & part_age <= 59 ~ "50-59",
                                    part_age >= 60 & part_age <= 69 ~ "60-69",
                                    part_age >= 70 ~ "70+"))

#set NA to 0 for physical contacts
pnum$phys[is.na(pnum$phys)] <- 0

#get average contacts both all and physical specifically
avg_cnt_poly <- pnum %>%
  group_by(part_age_group) %>%
  summarise(all = mean(all), physical = mean(phys)) %>%
  gather("contact_type", "contacts", -part_age_group)

#set both average data frames to data tables and define study name
avg_cnt <- as.data.table(avg_cnt)
avg_cnt[, study := "CoMix"]
avg_cnt_poly <- as.data.table(avg_cnt_poly)
avg_cnt_poly[, study := "POLYMOD"]

#merge
avgs <- rbind(avg_cnt, avg_cnt_poly)
groups <- c("contact_type", "study")

#plot
ggplot(avgs) + geom_line(aes(part_age_group, contacts, col = study,
                             group = interaction(study, contact_type), 
                             linetype = contact_type)) +
  geom_point(aes(part_age_group, contacts, col = study)) + 
  labs(x = "Age Group", y = "Average Number of Contacts",
       col = "Study", linetype = "Type of Contact")
