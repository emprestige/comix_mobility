##calculate dominant eigenvalue for synthetic matrix

#load libraries
library(data.table)
library(ggplot2)
library(tidyverse)
library(lubridate)

#set data path
data_path <-"C:\\Users\\emiel\\Documents\\LSHTM\\Fellowship\\Project\\comix_mobility\\Data\\"

#import contact matrices and scaling factors 
contact_matrices <- qs::qread(file.path(data_path, "contact_matrices_UK.qs"))

#extract matrices
all_matrix <- as.matrix(contact_matrices[[3]]$GBR)

#get dominant eigenvalues 
eigen_synthetic <- Re(eigen(all_matrix)$values[1])