## contact matrices practice 

#load libraries
library(data.table)
library(ggplot2)
library(tidyverse)
library(socialmixr)

#set data path
data_path <-"C:\\Users\\emiel\\Documents\\LSHTM\\Fellowship\\Project\\comix_mobility\\Data\\"

#import participant and contact data
pt <- qs::qread(file.path(data_path, "participants.qs"))
ct <- qs::qread(file.path(data_path, "contact.qs"))

#separate data by survey rounds 
pt_rounds <- pt %>% 
  group_split(survey_round)
ct_rounds <- ct %>%
  group_split(survey_round)

#create surveys for each survey round 
new_survey <- list()
for (i in 1:101) {
  new_survey[[i]] <- survey(pt_rounds[[i]], ct_rounds[[i]])
}

#create contact matrices for each survey round 
rounds <- list()
for (i in 1:101) {
  rounds[[i]] <- contact_matrix(new_survey[[i]], 
                                age.limits = c(0, 5, 12, 18, 30, 40, 50, 60, 65, 70))
}

#melt dataframes 
dt <- list()
for (i in 1:101) {
  dt[[i]] <- melt(data.table(rounds[[i]]$matrix, keep.rownames = TRUE) ,
                  id.vars = c("rn"))
}

#create plots 
plots <- list()
for (i in 1:101) {
  plots[[i]] <- ggplot(data = dt[[i]], aes(x = rn, y = variable, fill = value)) + 
    geom_tile()
}

#plot some to check 
plots[[1]]
plots[[28]]
plots[[55]]
plots[[101]]
plots[[21]]
