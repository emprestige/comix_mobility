##using the scaling factors for the UK

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

#import contact matrices and scaling factors 
contact_matrices <- qs::qread(file.path(data_path, "contact_matrices_UK.qs"))
scaling_factors <- qs::qread(file.path(data_path, "scaling_factors_UK_fortnightly.qs"))

#get dates for scaling factors 
scaling_factor_fortnights <- as.data.table(scaling_factors$fortnight)

#extract matrices
work_matrix <- as.matrix(contact_matrices[[1]]$GBR)
other_matrix <- as.matrix(contact_matrices[[2]]$GBR)

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
  e_mob[i] <- Re(e$values[1])
}
e_mob_frame <- t(data.frame(e_mob))
rownames(e_mob_frame) <- 1:nrow(e_mob_frame)
e_mob_frame <- cbind(scaling_factor_fortnights, e_mob_frame)
colnames(e_mob_frame) <- c("fortnight", "dominant_eigenvalue_mob")

#get dominant eigenvalues for mobility squared scaled matrices
e_mob2 <- list()
for(i in 1:nrow(scaling_factors)) {
  matrix <- work_mob2_scaled[[i]]
  e <- eigen(matrix)
  e_mob2[i] <- Re(e$values[1])
}
e_mob2_frame <- t(data.frame(e_mob2))
rownames(e_mob2_frame) <- 1:nrow(e_mob2_frame)
e_mob2_frame <- cbind(scaling_factor_fortnights, e_mob2_frame)
colnames(e_mob2_frame) <- c("fortnight", "dominant_eigenvalue_mob2")

#get dominant eigenvalues for linear factor scaled matrices
e_lin <- list()
for(i in 1:nrow(scaling_factors)) {
  matrix <- work_lin_scaled[[i]]
  e <- eigen(matrix)
  e_lin[i] <- Re(e$values[1])
}
e_lin_frame <- t(data.frame(e_lin))
rownames(e_lin_frame) <- 1:nrow(e_lin_frame)
e_lin_frame <- cbind(scaling_factor_fortnights, e_lin_frame)
colnames(e_lin_frame) <- c("fortnight", "dominant_eigenvalue_lin")

#get dominant eigenvalues for quadratic factor scaled matrices
e_quad <- list()
for(i in 1:nrow(scaling_factors)) {
  matrix <- work_quad_scaled[[i]]
  e <- eigen(matrix)
  e_quad[i] <- Re(e$values[1])
}
e_quad_frame <- t(data.frame(e_quad))
rownames(e_quad_frame) <- 1:nrow(e_quad_frame)
e_quad_frame <- cbind(scaling_factor_fortnights, e_quad_frame)
colnames(e_quad_frame) <- c("fortnight", "dominant_eigenvalue_quad")

#bind all of the estimates together
e_scaled <- as.data.table(cbind(e_mob_frame$fortnight, 
                                e_mob_frame$dominant_eigenvalue_mob,
                                e_mob2_frame$dominant_eigenvalue_mob2,
                                e_lin_frame$dominant_eigenvalue_lin,
                                e_quad_frame$dominant_eigenvalue_quad))
colnames(e_scaled) <- c("fortnight", "dominant_eigenvalue_mob", 
                        "dominant_eigenvalue_mob2",
                        "dominant_eigenvalue_lin",
                        "dominant_eigenvalue_quad")

#import dominant eigenvalues from comix matrices 
eigens <- qs::qread(file.path(data_path, "comix_eigens_work_UK_fortnightly.qs"))

#merge comix eigenvalues and scaled estimates 
eigens_all <- merge(eigens, e_scaled, by = "fortnight")

#set reproduction numbers to numeric
eigens_all$dominant_eigenvalue <- sapply(eigens_all$dominant_eigenvalue, as.numeric)
eigens_all$dominant_eigenvalue_mob <- sapply(eigens_all$dominant_eigenvalue_mob, as.numeric)
eigens_all$dominant_eigenvalue_mob2 <- sapply(eigens_all$dominant_eigenvalue_mob2, as.numeric)
eigens_all$dominant_eigenvalue_lin <- sapply(eigens_all$dominant_eigenvalue_lin, as.numeric)
eigens_all$dominant_eigenvalue_quad <- sapply(eigens_all$dominant_eigenvalue_quad, as.numeric)

##plot comparisons

#scatter plot
p1 <- ggplot(data = eigens_all, aes(x = dominant_eigenvalue)) + 
  geom_point(aes(y = dominant_eigenvalue_mob, col = "mob")) +
  labs(x = "Dominant Eigenvalue (CoMix)", y = "Dominant Eigenvalue (Estimates)",
       colour = "Scaling Factor") + xlim(0, 3) + ylim(0, 3) + 
  geom_abline(intercept = 0, slope = 1, linetype = 2) +
  scale_color_manual(breaks = "mob", values = "purple", labels = "Mobility")
p2 <- ggplot(data = eigens_all, aes(x = dominant_eigenvalue)) + 
  geom_point(aes(y = dominant_eigenvalue_mob2, col = "mob2")) +
  labs(x = "Dominant Eigenvalue (CoMix)", y = "Dominant Eigenvalue (Estimates)",
       colour = "Scaling Factor") + xlim(0, 3) + ylim(0, 3) +
  geom_abline(intercept = 0, slope = 1, linetype = 2) +
  scale_color_manual(breaks = "mob2", values = "red", labels = "Mobility Squared")
p3 <- ggplot(data = eigens_all, aes(x = dominant_eigenvalue)) + 
  geom_point(aes(y = dominant_eigenvalue_lin, col = "lin")) +
  labs(x = "Dominant Eigenvalue (CoMix)", y = "Dominant Eigenvalue (Estimates)",
       colour = "Scaling Factor") + xlim(0, 3) + ylim(0, 3) +
  geom_abline(intercept = 0, slope = 1, linetype = 2) +
  scale_color_manual(breaks = "lin", values = "blue", labels = "Linear Model")
p4 <- ggplot(data = eigens_all, aes(x = dominant_eigenvalue)) + 
  geom_point(aes(y = dominant_eigenvalue_quad, col = "quad")) +
  labs(x = "Dominant Eigenvalue (CoMix)", y = "Dominant Eigenvalue (Estimates)",
       colour = "Scaling Factor") + xlim(0, 3) + ylim(0, 3) +
  geom_abline(intercept = 0, slope = 1, linetype = 2) +
  scale_color_manual(breaks = "quad", values = "orange", labels = "Quadratic Model")
plot_grid(p1, p2, p3, p4)

#get labels for all plots
int <- seq(1, 27, 3)
my_list <- eigens_all$fortnight[int]

#line graph
ggplot(data = eigens_all) + 
  geom_line(aes(x = fortnight, y = dominant_eigenvalue, col = "comix"), group = 1) +
  geom_line(aes(x = fortnight, y = dominant_eigenvalue_mob, col = "mob"), group = 1) +
  geom_line(aes(x = fortnight, y = dominant_eigenvalue_mob2, col = "mob2"), group = 1) +
  geom_line(aes(x = fortnight, y = dominant_eigenvalue_lin, col = "lin"), group = 1) +
  geom_line(aes(x = fortnight, y = dominant_eigenvalue_quad, col = "quad"), group = 1) +
  labs(x = "Fortnight", y = "Dominant Eigenvalue",
       colour = "Estimate Type") + scale_x_discrete(breaks = my_list) +
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
ggplot(data = resid, aes(x = fortnight)) + 
  geom_point(aes(y = mob_resid, col = "mob")) +
  geom_point(aes(y = mob2_resid, col = "mob2")) +
  geom_point(aes(y = lin_resid, col = "lin")) +
  geom_point(aes(y = quad_resid, col = "quad")) +
  geom_hline(yintercept = 0, linetype = 2) + scale_x_discrete(breaks = my_list) +
  labs(x = "Fortnight", y = "Residuals", colour = "Scaling Factor") +
  scale_color_manual(breaks = c("mob", "mob2", "lin", "quad"),
                     values = c("purple", "red", "blue", "orange"),
                     labels = c("Mobility", "Mobility Squared", 
                                "Linear Model", "Quadratic Model"))

#calculate mean squared error
mob_MSE <- mean((eigens_all$dominant_eigenvalue - eigens_all$dominant_eigenvalue_mob)^2)
mob2_MSE <- mean((eigens_all$dominant_eigenvalue - eigens_all$dominant_eigenvalue_mob2)^2)
lin_MSE <- mean((eigens_all$dominant_eigenvalue - eigens_all$dominant_eigenvalue_lin)^2)
quad_MSE <- mean((eigens_all$dominant_eigenvalue - eigens_all$dominant_eigenvalue_quad)^2)

#now multiply the dominant eigenvalues by the scalar to calculate the reproduction number
reproduction_all <- rlang::duplicate(eigens_all)
reproduction_all <- reproduction_all[, .(fortnight,reproduction_number = dominant_eigenvalue*0.4724843,
                                         reproduction_number_mob = dominant_eigenvalue_mob*0.4724843,
                                         reproduction_number_mob2 = dominant_eigenvalue_mob2*0.4724843,
                                         reproduction_number_lin = dominant_eigenvalue_lin*0.4724843,
                                         reproduction_number_quad = dominant_eigenvalue_quad*0.4724843)]

#import correct reproduction numebers 
reproduction_model <- qs::qread(file.path(data_path, "reproduction_numbers_UK.qs"))
colnames(reproduction_model) <- c("fortnight", "reproduction_number_model")
reproduction_all <- merge(reproduction_all, reproduction_model, by = "fortnight")

##plot comparisons

#scatter plot
p5 <- ggplot(data = reproduction_all, aes(x = reproduction_number)) + 
  geom_point(aes(y = reproduction_number_mob, col = "mob")) +
  labs(x = "Reproduction Number (CoMix)", y = "Reproduction Number (Estimates)",
       colour = "Scaling Factor") + xlim(0, 1.5) + ylim(0, 1.5) + 
  geom_abline(intercept = 0, slope = 1, linetype = 2) +
  scale_color_manual(breaks = "mob", values = "purple", labels = "Mobility")
p6 <- ggplot(data = reproduction_all, aes(x = reproduction_number)) + 
  geom_point(aes(y = reproduction_number_mob2, col = "mob2")) +
  labs(x = "Reproduction Number (CoMix)", y = "Reproduction Number (Estimates)",
       colour = "Scaling Factor") + xlim(0, 1.5) + ylim(0, 1.5) +
  geom_abline(intercept = 0, slope = 1, linetype = 2) +
  scale_color_manual(breaks = "mob2", values = "red", labels = "Mobility Squared")
p7 <- ggplot(data = reproduction_all, aes(x = reproduction_number)) + 
  geom_point(aes(y = reproduction_number_lin, col = "lin")) +
  labs(x = "Reproduction Number (CoMix)", y = "Reproduction Number (Estimates)",
       colour = "Scaling Factor") + xlim(0, 1.5) + ylim(0, 1.5) +
  geom_abline(intercept = 0, slope = 1, linetype = 2) +
  scale_color_manual(breaks = "lin", values = "blue", labels = "Linear Model")
p8 <- ggplot(data = reproduction_all, aes(x = reproduction_number)) + 
  geom_point(aes(y = reproduction_number_quad, col = "quad")) +
  labs(x = "Reproduction Number (CoMix)", y = "Reproduction Number (Estimates)",
       colour = "Scaling Factor") + xlim(0, 1.5) + ylim(0, 1.5) +
  geom_abline(intercept = 0, slope = 1, linetype = 2) +
  scale_color_manual(breaks = "quad", values = "orange", labels = "Quadratic Model")
plot_grid(p5, p6, p7, p8)

#line graph
ggplot(data = reproduction_all) + 
  geom_line(aes(x = fortnight, y = reproduction_number, col = "comix"), group = 1) +
  geom_line(aes(x = fortnight, y = reproduction_number_mob, col = "mob"), group = 1) +
  geom_line(aes(x = fortnight, y = reproduction_number_mob2, col = "mob2"), group = 1) +
  geom_line(aes(x = fortnight, y = reproduction_number_lin, col = "lin"), group = 1) +
  geom_line(aes(x = fortnight, y = reproduction_number_quad, col = "quad"), group = 1) +
  labs(x = "Fortnight", y = "Reproduction Number",
       colour = "Estimate Type") + scale_x_discrete(breaks = my_list) +
  scale_color_manual(breaks = c("comix", "mob", "mob2", "lin", "quad"),
                     values = c("green", "purple", "red", "blue", "orange"),
                     labels = c("CoMix", "Mobility", "Mobility Squared", 
                                "Linear Model", "Quadratic Model"))

#calculate residuals
resid2 <- rlang::duplicate(reproduction_all)
resid2 <- resid2[, mob_resid2 := reproduction_number - reproduction_number_mob]
resid2 <- resid2[, mob2_resid2 := reproduction_number - reproduction_number_mob2]
resid2 <- resid2[, lin_resid2 := reproduction_number - reproduction_number_lin]
resid2 <- resid2[, quad_resid2 := reproduction_number - reproduction_number_quad]

#plot residuals 
ggplot(data = resid2, aes(x = fortnight)) + 
  geom_point(aes(y = mob_resid2, col = "mob")) +
  geom_point(aes(y = mob2_resid2, col = "mob2")) +
  geom_point(aes(y = lin_resid2, col = "lin")) +
  geom_point(aes(y = quad_resid2, col = "quad")) +
  geom_hline(yintercept = 0, linetype = 2) + scale_x_discrete(breaks = my_list) +
  labs(x = "Fortnight", y = "Residuals", colour = "Scaling Factor") +
  scale_color_manual(breaks = c("mob", "mob2", "lin", "quad"),
                     values = c("purple", "red", "blue", "orange"),
                     labels = c("Mobility", "Mobility Squared", 
                                "Linear Model", "Quadratic Model"))

#calculate mean squared error
mob_rootMSE <- sqrt(mean((reproduction_all$reproduction_number - reproduction_all$reproduction_number_mob)^2))
mob2_rootMSE <- sqrt(mean((reproduction_all$reproduction_number - reproduction_all$reproduction_number_mob2)^2))
lin_rootMSE <- sqrt(mean((reproduction_all$reproduction_number - reproduction_all$reproduction_number_lin)^2))
quad_rootMSE <- sqrt(mean((reproduction_all$reproduction_number - reproduction_all$reproduction_number_quad)^2))

#line graph
ggplot(data = reproduction_all) +
  geom_line(aes(x = fortnight, y = reproduction_number, col = "comix"), group = 1) +
  geom_line(aes(x = fortnight, y = reproduction_number_mob, col = "mob"), group = 1) +
  geom_line(aes(x = fortnight, y = reproduction_number_mob2, col = "mob2"), group = 1) +
  geom_line(aes(x = fortnight, y = reproduction_number_lin, col = "lin"), group = 1) +
  geom_line(aes(x = fortnight, y = reproduction_number_quad, col = "quad"), group = 1) +
  geom_line(aes(x = fortnight, y = reproduction_number_model, col = "model"), group = 1) +
  labs(x = "Fortnight", y = "Reproduction Number",
       colour = "Estimate Type") + scale_x_discrete(breaks = my_list) +
  scale_color_manual(breaks = c("comix", "mob", "mob2", "lin", "quad", "model"),
                     values = c("green", "purple", "red", "blue", "orange", "black"),
                     labels = c("CoMix", "Mobility", "Mobility Squared", 
                                "Linear Model", "Quadratic Model",
                                "Transmission Model"))
