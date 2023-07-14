##scale the dominant eigenvalues by the reproduction numbers 

#load libraries
library(data.table)
library(ggplot2)
library(tidyverse)
library(lubridate)
library(cowplot)
library(ggforce)

#set cowplot theme
theme_set(cowplot::theme_cowplot(font_size = 10) + theme(strip.background = element_blank()))

#set data path
data_path <-"C:\\Users\\emiel\\Documents\\LSHTM\\Fellowship\\Project\\comix_mobility\\Data\\"

#import dominant eigenvalues from comix matrices 
eigens <- qs::qread(file.path(data_path, "comix_eigens_work_UK_fortnightly.qs"))

#import correct reproduction numbers 
reproduction_model <- qs::qread(file.path(data_path, "reproduction_numbers_UK.qs"))
colnames(reproduction_model) <- c("fortnight", "reproduction_number_model")

#combine the eigenvalues with the reproduction numbers 
eigen_reproduction <- merge(eigens, reproduction_model, by = "fortnight")

#calculate multiplicative factor 
multiplicative_factor <- eigen_reproduction[, .(fortnight,
                         multiplicative_factor = reproduction_number_model/
                           dominant_eigenvalue)]

#export 
qs::qsave(multiplicative_factor, file.path(data_path, "multiplicative_factors_UK.qs"))
