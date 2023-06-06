##using the scaling factors for Belgium

#load libraries
library(data.table)
library(ggplot2)
library(tidyverse)
library(lubridate)
library(cowplot)
library(ggrepel)

#set cowplot theme
theme_set(cowplot::theme_cowplot(font_size = 10) + theme(strip.background = element_blank()))

#set data path
data_path <-"C:\\Users\\emiel\\Documents\\LSHTM\\Fellowship\\Project\\comix_mobility\\Data\\"

#import contact matrices and scaling factors 
contact_matrices <- qs::qread(file.path(data_path, "contact_matrices_BE.qs"))
scaling_factors <- qs::qread(file.path(data_path, "scaling_factors_BE.qs"))

#extract matrices
work_matrix <- as.matrix(contact_matrices[[1]]$BEL)
other_matrix <- as.matrix(contact_matrices[[2]]$BEL)

#create new matrices which have been scaled by mobility 
work_mob_scaled <- list()
for(i in 1:nrow(scaling_factors)) {
  scalar <- scaling_factors[i, "work_mob"]$work_mob
  work_mob_scaled[[i]] <- scalar*work_matrix
}

#create new matrices which have been scaled by mobility squared
work_mob2_scaled <- list()
for(i in 1:nrow(scaling_factors)) {
  scalar <- scaling_factors[i, "work_mob2"]$work_mob2
  work_mob2_scaled[[i]] <- scalar*work_matrix
}

#create new matrices which have been scaled by linear scaling factor
work_lin_scaled <- list()
for(i in 1:nrow(scaling_factors)) {
  scalar <- scaling_factors[i, "work_scaling_fac_lin"]$work_scaling_fac_lin
  work_lin_scaled[[i]] <- scalar*work_matrix
}

#create new matrices which have been scaled by quadratic scaling factor
work_quad_scaled <- list()
for(i in 1:nrow(scaling_factors)) {
  scalar <- scaling_factors[i, "work_scaling_fac_quad"]$work_scaling_fac_quad
  work_quad_scaled[[i]] <- scalar*work_matrix
}

#get dominant eigenvalues for mobility scaled matrices
e_mob <- list()
for(i in 1:nrow(scaling_factors)) {
  matrix <- work_mob_scaled[[i]]
  e <- eigen(matrix)
  e_mob[i] <- e$values[1]
}

#get dominant eigenvalues for mobility squared scaled matrices
e_mob2 <- list()
for(i in 1:nrow(scaling_factors)) {
  matrix <- work_mob2_scaled[[i]]
  e <- eigen(matrix)
  e_mob2[i] <- e$values[1]
}

#get dominant eigenvalues for linear factor scaled matrices
e_lin <- list()
for(i in 1:nrow(scaling_factors)) {
  matrix <- work_lin_scaled[[i]]
  e <- eigen(matrix)
  e_lin[i] <- e$values[1]
}

#get dominant eigenvalues for quadratic factor scaled matrices
e_quad <- list()
for(i in 1:nrow(scaling_factors)) {
  matrix <- work_quad_scaled[[i]]
  e <- eigen(matrix)
  e_quad[i] <- e$values[1]
}

#import dominant eigenvalues from comix matrices 
eigens <- qs::qread(file.path(data_path, "comix_eigens_BE.qs"))
