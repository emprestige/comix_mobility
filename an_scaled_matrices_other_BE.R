##using the scaling factors for Belgium

#load libraries
library(data.table)
library(ggplot2)
library(tidyverse)
library(lubridate)
library(cowplot)

#set cowplot theme
theme_set(cowplot::theme_cowplot(font_size = 10) + theme(strip.background = element_blank()))

#set data path
data_path <-"C:\\Users\\emiel\\Documents\\LSHTM\\Fellowship\\Project\\comix_mobility\\Data\\"

#import contact matrices and scaling factors 
contact_matrices <- qs::qread(file.path(data_path, "contact_matrices_BE.qs"))
scaling_factors <- qs::qread(file.path(data_path, "scaling_factors_BE.qs"))

#get dates for scaling factors 
scaling_factor_weeks <- as.data.table(scaling_factors$week)

#extract matrices
work_matrix <- as.matrix(contact_matrices[[1]]$BEL)
other_matrix <- as.matrix(contact_matrices[[2]]$BEL)

#create new matrices which have been scaled by mobility 
other_mob_scaled <- list()
for(i in 1:nrow(scaling_factors)) {
  scalar <- scaling_factors[i, "other_mob"]$other_mob
  other_mob_scaled[[i]] <- scalar*other_matrix
}

#create new matrices which have been scaled by mobility squared
other_mob2_scaled <- list()
for(i in 1:nrow(scaling_factors)) {
  scalar <- scaling_factors[i, "other_mob2"]$other_mob2
  other_mob2_scaled[[i]] <- scalar*other_matrix
}

#create new matrices which have been scaled by linear scaling factor
other_lin_scaled <- list()
for(i in 1:nrow(scaling_factors)) {
  scalar <- scaling_factors[i, "other_scaling_fac_lin"]$other_scaling_fac_lin
  other_lin_scaled[[i]] <- scalar*other_matrix
}

#create new matrices which have been scaled by quadratic scaling factor
other_quad_scaled <- list()
for(i in 1:nrow(scaling_factors)) {
  scalar <- scaling_factors[i, "other_scaling_fac_quad"]$other_scaling_fac_quad
  other_quad_scaled[[i]] <- scalar*other_matrix
}

#get dominant eigenvalues for mobility scaled matrices
e_mob <- list()
for(i in 1:nrow(scaling_factors)) {
  matrix <- other_mob_scaled[[i]]
  e <- eigen(matrix)
  e_mob[i] <- Re(e$values[1])
}
e_mob_frame <- t(data.frame(e_mob))
rownames(e_mob_frame) <- 1:nrow(e_mob_frame)
e_mob_frame <- cbind(scaling_factor_weeks, e_mob_frame)
colnames(e_mob_frame) <- c("week", "dominant_eigenvalue_mob")

#get dominant eigenvalues for mobility squared scaled matrices
e_mob2 <- list()
for(i in 1:nrow(scaling_factors)) {
  matrix <- other_mob2_scaled[[i]]
  e <- eigen(matrix)
  e_mob2[i] <- Re(e$values[1])
}
e_mob2_frame <- t(data.frame(e_mob2))
rownames(e_mob2_frame) <- 1:nrow(e_mob2_frame)
e_mob2_frame <- cbind(scaling_factor_weeks, e_mob2_frame)
colnames(e_mob2_frame) <- c("week", "dominant_eigenvalue_mob2")

#get dominant eigenvalues for linear factor scaled matrices
e_lin <- list()
for(i in 1:nrow(scaling_factors)) {
  matrix <- other_lin_scaled[[i]]
  e <- eigen(matrix)
  e_lin[i] <- Re(e$values[1])
}
e_lin_frame <- t(data.frame(e_lin))
rownames(e_lin_frame) <- 1:nrow(e_lin_frame)
e_lin_frame <- cbind(scaling_factor_weeks, e_lin_frame)
colnames(e_lin_frame) <- c("week", "dominant_eigenvalue_lin")

#get dominant eigenvalues for quadratic factor scaled matrices
e_quad <- list()
for(i in 1:nrow(scaling_factors)) {
  matrix <- other_quad_scaled[[i]]
  e <- eigen(matrix)
  e_quad[i] <- Re(e$values[1])
}
e_quad_frame <- t(data.frame(e_quad))
rownames(e_quad_frame) <- 1:nrow(e_quad_frame)
e_quad_frame <- cbind(scaling_factor_weeks, e_quad_frame)
colnames(e_quad_frame) <- c("week", "dominant_eigenvalue_quad")

#bind all of the estimates together
e_scaled <- as.data.table(cbind(e_mob_frame$week, 
                                e_mob_frame$dominant_eigenvalue_mob,
                                e_mob2_frame$dominant_eigenvalue_mob2,
                                e_lin_frame$dominant_eigenvalue_lin,
                                e_quad_frame$dominant_eigenvalue_quad))
colnames(e_scaled) <- c("week", "dominant_eigenvalue_mob", 
                        "dominant_eigenvalue_mob2",
                        "dominant_eigenvalue_lin",
                        "dominant_eigenvalue_quad")

#import dominant eigenvalues from comix matrices 
eigens <- qs::qread(file.path(data_path, "comix_eigens_other_BE.qs"))

#merge comix eigenvalues and scaled estimates 
eigens_all <- merge(eigens, e_scaled, by = "week")

#set reproduction numbers to numeric
eigens_all$dominant_eigenvalue <- sapply(eigens_all$dominant_eigenvalue, as.numeric)
eigens_all$dominant_eigenvalue_mob <- sapply(eigens_all$dominant_eigenvalue_mob, as.numeric)
eigens_all$dominant_eigenvalue_mob2 <- sapply(eigens_all$dominant_eigenvalue_mob2, as.numeric)
eigens_all$dominant_eigenvalue_lin <- sapply(eigens_all$dominant_eigenvalue_lin, as.numeric)
eigens_all$dominant_eigenvalue_quad <- sapply(eigens_all$dominant_eigenvalue_quad, as.numeric)

##plot comparisons

#scatter plot
ggplot(data = eigens_all, aes(x = dominant_eigenvalue)) + 
  geom_point(aes(y = dominant_eigenvalue_mob, col = "mob")) +
  geom_point(aes(y = dominant_eigenvalue_mob2, col = "mob2")) +
  geom_point(aes(y = dominant_eigenvalue_lin, col = "lin")) +
  geom_point(aes(y = dominant_eigenvalue_quad, col = "quad")) +
  labs(x = "Dominant Eigenvalue (CoMix)", y = "Dominant Eigenvalue (Estimates)",
       colour = "Scaling Factor") +
  scale_color_manual(breaks = c("mob", "mob2", "lin", "quad"),
                     values = c("purple", "red", "blue", "orange"),
                     labels = c("Mobility", "Mobility Squared", 
                                "Linear Model", "Quadratic Model"))

#line graph
ggplot(data = eigens_all) + 
  geom_line(aes(x = week, y = dominant_eigenvalue, col = "comix"), group = 1) +
  geom_line(aes(x = week, y = dominant_eigenvalue_mob, col = "mob"), group = 1) +
  geom_line(aes(x = week, y = dominant_eigenvalue_mob2, col = "mob2"), group = 1) +
  geom_line(aes(x = week, y = dominant_eigenvalue_lin, col = "lin"), group = 1) +
  geom_line(aes(x = week, y = dominant_eigenvalue_quad, col = "quad"), group = 1) +
  labs(x = "Week", y = "Dominant Eigenvalue", colour = "Estimate Type") +
  scale_color_manual(breaks = c("comix", "mob", "mob2", "lin", "quad"),
                     values = c("green", "purple", "red", "blue", "orange"),
                     labels = c("CoMix", "Mobility", "Mobility Squared", 
                                "Linear Model", "Quadratic Model"))
  
#calculate residuals
resid <- rlang::duplicate(eigens_all)
resid <- resid[, mob_resid := dominant_eigenvalue - dominant_eigenvalue_mob]
resid <- resid[, mob2_resid := dominant_eigenvalue - dominant_eigenvalue_mob2]
resid <- resid[, lin_resid := dominant_eigenvalue - dominant_eigenvalue_lin]
resid <- resid[, quad_resid := dominant_eigenvalue - dominant_eigenvalue_quad]

#plot residuals 
ggplot(data = resid, aes(x = week)) + 
  geom_point(aes(y = mob_resid, col = "mob")) +
  geom_point(aes(y = mob2_resid, col = "mob2")) +
  geom_point(aes(y = lin_resid, col = "lin")) +
  geom_point(aes(y = quad_resid, col = "quad")) +
  geom_hline(yintercept = 0, linetype = 2) +
  labs(x = "Week", y = "Residuals", colour = "Scaling Factor") +
  scale_color_manual(breaks = c("mob", "mob2", "lin", "quad"),
                     values = c("purple", "red", "blue", "orange"),
                     labels = c("Mobility", "Mobility Squared", 
                                "Linear Model", "Quadratic Model"))

#calculate mean squared error
mob_MSE <- mean((eigens_all$dominant_eigenvalue - eigens_all$dominant_eigenvalue_mob)^2)
mob2_MSE <- mean((eigens_all$dominant_eigenvalue - eigens_all$dominant_eigenvalue_mob2)^2)
lin_MSE <- mean((eigens_all$dominant_eigenvalue - eigens_all$dominant_eigenvalue_lin)^2)
quad_MSE <- mean((eigens_all$dominant_eigenvalue - eigens_all$dominant_eigenvalue_quad)^2)
