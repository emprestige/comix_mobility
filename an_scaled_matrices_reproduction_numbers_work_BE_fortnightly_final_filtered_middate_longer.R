##using the scaling factors for the BE

#load libraries
library(data.table)
library(ggplot2)
library(dplyr)
library(lubridate)
library(cowplot)
library(scales)
library(here)

#set cowplot theme
theme_set(cowplot::theme_cowplot(font_size = 14) + theme(strip.background = element_blank()))

#set data path
data_path <- here("data")

#import contact matrices and scaling factors 
contact_matrices <- qs::qread(file.path(data_path, "contact_matrices_BE.qs"))
scaling_factors <- qs::qread(file.path(data_path, "scaling_factors_BE_fortnightly_filtered_middate_longer.qs"))

#get dates for scaling factors 
scaling_factor_middates <- as.data.table(scaling_factors$mid_date)

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
  e_mob[i] <- Re(e$values[1])
}
e_mob_frame <- t(data.frame(e_mob))
rownames(e_mob_frame) <- 1:nrow(e_mob_frame)
e_mob_frame <- cbind(scaling_factor_middates, e_mob_frame)
colnames(e_mob_frame) <- c("mid_date", "dominant_eigenvalue_mob")

#get dominant eigenvalues for mobility squared scaled matrices
e_mob2 <- list()
for(i in 1:nrow(scaling_factors)) {
  matrix <- work_mob2_scaled[[i]]
  e <- eigen(matrix)
  e_mob2[i] <- Re(e$values[1])
}
e_mob2_frame <- t(data.frame(e_mob2))
rownames(e_mob2_frame) <- 1:nrow(e_mob2_frame)
e_mob2_frame <- cbind(scaling_factor_middates, e_mob2_frame)
colnames(e_mob2_frame) <- c("mid_date", "dominant_eigenvalue_mob2")

#get dominant eigenvalues for linear factor scaled matrices
e_lin <- list()
for(i in 1:nrow(scaling_factors)) {
  matrix <- work_lin_scaled[[i]]
  e <- eigen(matrix)
  e_lin[i] <- Re(e$values[1])
}
e_lin_frame <- t(data.frame(e_lin))
rownames(e_lin_frame) <- 1:nrow(e_lin_frame)
e_lin_frame <- cbind(scaling_factor_middates, e_lin_frame)
colnames(e_lin_frame) <- c("mid_date", "dominant_eigenvalue_lin")

#get dominant eigenvalues for quadratic factor scaled matrices
e_quad <- list()
for(i in 1:nrow(scaling_factors)) {
  matrix <- work_quad_scaled[[i]]
  e <- eigen(matrix)
  e_quad[i] <- Re(e$values[1])
}
e_quad_frame <- t(data.frame(e_quad))
rownames(e_quad_frame) <- 1:nrow(e_quad_frame)
e_quad_frame <- cbind(scaling_factor_middates, e_quad_frame)
colnames(e_quad_frame) <- c("mid_date", "dominant_eigenvalue_quad")

#bind all of the estimates together
e_scaled <- as.data.table(cbind(e_mob_frame$mid_date, 
                                e_mob_frame$dominant_eigenvalue_mob,
                                e_mob2_frame$dominant_eigenvalue_mob2,
                                e_lin_frame$dominant_eigenvalue_lin,
                                e_quad_frame$dominant_eigenvalue_quad))
colnames(e_scaled) <- c("mid_date", "dominant_eigenvalue_mob", 
                        "dominant_eigenvalue_mob2",
                        "dominant_eigenvalue_lin",
                        "dominant_eigenvalue_quad")

#import dominant eigenvalues from comix matrices 
eigens <- qs::qread(file.path(data_path, "comix_eigens_work_BE_fortnightly_filtered_middate_longer.qs"))

#merge comix eigenvalues and scaled estimates 
eigens_all <- full_join(eigens, e_scaled, by = "mid_date")
eigens_all <- eigens_all[order(eigens_all$mid_date)]

#set eigenvalues to numeric and date value to date
eigens_all$mid_date <- as.Date(eigens_all$mid_date, origin = "1970-01-01")
eigens_all$dominant_eigenvalue_mob <- sapply(eigens_all$dominant_eigenvalue_mob, as.numeric)
eigens_all$dominant_eigenvalue_mob2 <- sapply(eigens_all$dominant_eigenvalue_mob2, as.numeric)
eigens_all$dominant_eigenvalue_lin <- sapply(eigens_all$dominant_eigenvalue_lin, as.numeric)
eigens_all$dominant_eigenvalue_quad <- sapply(eigens_all$dominant_eigenvalue_quad, as.numeric)

#now multiply the dominant eigenvalues by the scalar to calculate the reproduction number
reproduction_all <- rlang::duplicate(eigens_all)
reproduction_all <- reproduction_all[, .(mid_date,reproduction_number = dominant_eigenvalue*0.1581799,
                                         reproduction_number_mob = dominant_eigenvalue_mob*0.1581799,
                                         reproduction_number_mob2 = dominant_eigenvalue_mob2*0.1581799,
                                         reproduction_number_lin = dominant_eigenvalue_lin*0.1581799,
                                         reproduction_number_quad = dominant_eigenvalue_quad*0.1581799)]

##plot comparisons

#scatter plot
p5 <- ggplot(data = reproduction_all, aes(x = reproduction_number)) + 
  geom_point(aes(y = reproduction_number_mob, col = "mob"), size = 2) +
  labs(x = "Reproduction Number (CoMix)", y = "Reproduction Number\n(Estimates)",
       colour = "Scaling Factor") + xlim(0, 0.5) + ylim(0, 0.5) + 
  geom_abline(intercept = 0, slope = 1, linetype = 2, size = 0.8) +
  scale_color_manual(breaks = "mob", values = "#CC79A7", labels = "Mobility")
p6 <- ggplot(data = reproduction_all, aes(x = reproduction_number)) + 
  geom_point(aes(y = reproduction_number_mob2, col = "mob2"), size = 2) +
  labs(x = "Reproduction Number (CoMix)", y = "Reproduction Number\n(Estimates)",
       colour = "Scaling Factor") + xlim(0, 0.5) + ylim(0, 0.5) +
  geom_abline(intercept = 0, slope = 1, linetype = 2, size = 0.8) +
  scale_color_manual(breaks = "mob2", values = "#D55E00", labels = "Mobility Squared")
p7 <- ggplot(data = reproduction_all, aes(x = reproduction_number)) + 
  geom_point(aes(y = reproduction_number_lin, col = "lin"), size = 2) +
  labs(x = "Reproduction Number (CoMix)", y = "Reproduction Number\n(Estimates)",
       colour = "Scaling Factor") + xlim(0, 0.5) + ylim(0, 0.5) +
  geom_abline(intercept = 0, slope = 1, linetype = 2, size = 0.8) +
  scale_color_manual(breaks = "lin", values = "#0072B2", labels = "Linear Model")
p8 <- ggplot(data = reproduction_all, aes(x = reproduction_number)) + 
  geom_point(aes(y = reproduction_number_quad, col = "quad"), size = 2) +
  labs(x = "Reproduction Number (CoMix)", y = "Reproduction Number\n(Estimates)",
       colour = "Scaling Factor") + xlim(0, 0.5) + ylim(0, 0.5) +
  geom_abline(intercept = 0, slope = 1, linetype = 2, size = 0.8) +
  scale_color_manual(breaks = "quad", values = "#F0E442", labels = "Quadratic Model")
plot_grid(p5, p6, p7, p8)

#line graph
ggplot(data = reproduction_all) + 
  geom_line(aes(x = mid_date, y = reproduction_number, col = "comix"), group = 1, size = 0.8) +
  geom_line(aes(x = mid_date, y = reproduction_number_mob, col = "mob"), group = 1, size = 0.8) +
  geom_line(aes(x = mid_date, y = reproduction_number_mob2, col = "mob2"), group = 1, size = 0.8) +
  geom_line(aes(x = mid_date, y = reproduction_number_lin, col = "lin"), group = 1, size = 0.8) +
  geom_line(aes(x = mid_date, y = reproduction_number_quad, col = "quad"), group = 1, size = 0.8) +
  labs(x = "Date", y = "Reproduction Number", colour = "Estimate Type") + 
  scale_x_date(labels = date_format("%B-%Y")) +
  scale_color_manual(breaks = c("comix", "mob", "mob2", "lin", "quad"),
                     values = c("#009E73", "#CC79A7", "#D55E00", "#0072B2", "#F0E442"),
                     labels = c("CoMix", "Mobility", "Mobility Squared", 
                                "Linear Model", "Quadratic Model"))

#calculate residuals
resid2 <- rlang::duplicate(reproduction_all)
resid2 <- resid2 %>% drop_na()
resid2 <- resid2[, mob_resid2 := reproduction_number - reproduction_number_mob]
resid2 <- resid2[, mob2_resid2 := reproduction_number - reproduction_number_mob2]
resid2 <- resid2[, lin_resid2 := reproduction_number - reproduction_number_lin]
resid2 <- resid2[, quad_resid2 := reproduction_number - reproduction_number_quad]
resid3 <- full_join(resid2, reproduction_all)

#plot residuals 
ggplot(data = resid3, aes(x = mid_date)) + 
  geom_line(aes(y = mob_resid2, col = "mob"), size = 0.8) +
  geom_line(aes(y = mob2_resid2, col = "mob2"), size = 0.8) +
  geom_line(aes(y = lin_resid2, col = "lin"), size = 0.8) +
  geom_line(aes(y = quad_resid2, col = "quad"), size = 0.8) +
  geom_hline(yintercept = 0, linetype = 2, size = 0.8) + 
  scale_x_date(labels = date_format("%B-%Y")) +
  labs(x = "Date", y = "Residuals", colour = "Scaling Factor") +
  scale_color_manual(breaks = c("mob", "mob2", "lin", "quad"),
                     values = c("#CC79A7", "#D55E00", "#0072B2", "#F0E442"),
                     labels = c("Mobility", "Mobility Squared", 
                                "Linear Model", "Quadratic Model"))

#calculate mean squared error
mob_rootMSE <- sqrt(mean((resid2$reproduction_number - resid2$reproduction_number_mob)^2))
mob2_rootMSE <- sqrt(mean((resid2$reproduction_number - resid2$reproduction_number_mob2)^2))
lin_rootMSE <- sqrt(mean((resid2$reproduction_number - resid2$reproduction_number_lin)^2))
quad_rootMSE <- sqrt(mean((resid2$reproduction_number - resid2$reproduction_number_quad)^2))
