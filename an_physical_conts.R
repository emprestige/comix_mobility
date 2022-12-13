## get physical contacts and compare to BBC pandemic data

#load libraries
library(data.table)
library(ggplot2)
library(tidyverse)
library(socialmixr)

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
