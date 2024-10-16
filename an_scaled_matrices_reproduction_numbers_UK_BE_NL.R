#load libraries
library(data.table)
library(ggplot2)
library(dplyr)
library(lubridate)
library(cowplot)
library(scales)
library(ggpubr)
library(here)
library(grid)
library(patchwork)

#set cowplot theme
theme_set(cowplot::theme_cowplot(font_size = 10) + theme(strip.background = element_blank(),
                                                         legend.box.margin = margin(l = 10, b = 10),
                                                         plot.margin = margin(l = 10, r = 10)))

#set data path
data_path <- here("data")

##UK

#import contact matrices and scaling factors 
contact_matrices <- qs::qread(file.path(data_path, "contact_matrices_UK.qs"))
scaling_factors <- qs::qread(file.path(data_path, "scaling_factors_UK_fortnightly_filtered_middate_longer.qs"))

#get dates for scaling factors 
scaling_factor_middates <- as.data.table(scaling_factors$mid_date)

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
colnames(e_lin_frame) <- c("mid_dates", "dominant_eigenvalue_lin")

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
eigens <- qs::qread(file.path(data_path, "comix_eigens_work_UK_fortnightly_filtered_middate_longer.qs"))

#merge comix eigenvalues and scaled estimates 
eigens_all <- merge(eigens, e_scaled, by = "mid_date")

#set eigenvalues to numeric and date value to date
eigens_all$mid_date <- as.Date(eigens_all$mid_date, origin = "1970-01-01")
eigens_all$dominant_eigenvalue <- sapply(eigens_all$dominant_eigenvalue, as.numeric)
eigens_all$dominant_eigenvalue_mob <- sapply(eigens_all$dominant_eigenvalue_mob, as.numeric)
eigens_all$dominant_eigenvalue_mob2 <- sapply(eigens_all$dominant_eigenvalue_mob2, as.numeric)
eigens_all$dominant_eigenvalue_lin <- sapply(eigens_all$dominant_eigenvalue_lin, as.numeric)
eigens_all$dominant_eigenvalue_quad <- sapply(eigens_all$dominant_eigenvalue_quad, as.numeric)

#calculate root mean squared error
mob_rootMSE <- sqrt(mean((eigens_all$dominant_eigenvalue - eigens_all$dominant_eigenvalue_mob)^2))
mob2_rootMSE <- sqrt(mean((eigens_all$dominant_eigenvalue - eigens_all$dominant_eigenvalue_mob2)^2))
lin_rootMSE <- sqrt(mean((eigens_all$dominant_eigenvalue - eigens_all$dominant_eigenvalue_lin)^2))
quad_rootMSE <- sqrt(mean((eigens_all$dominant_eigenvalue - eigens_all$dominant_eigenvalue_quad)^2))

#now multiply the dominant eigenvalues by the scalar to calculate the reproduction number
reproduction_all <- rlang::duplicate(eigens_all)
reproduction_all <- reproduction_all[, .(mid_date,reproduction_number = dominant_eigenvalue*0.1581799,
                                         reproduction_number_mob = dominant_eigenvalue_mob*0.1581799,
                                         reproduction_number_mob2 = dominant_eigenvalue_mob2*0.1581799,
                                         reproduction_number_lin = dominant_eigenvalue_lin*0.1581799,
                                         reproduction_number_quad = dominant_eigenvalue_quad*0.1581799)]

#plot
work_UK <- ggplot(data = reproduction_all) + 
  geom_line(aes(x = mid_date, y = reproduction_number, col = "comix"), group = 1, size = 0.8) +
  geom_line(aes(x = mid_date, y = reproduction_number_mob, col = "mob"), group = 1, size = 0.8) +
  geom_line(aes(x = mid_date, y = reproduction_number_mob2, col = "mob2"), group = 1, size = 0.8) +
  geom_line(aes(x = mid_date, y = reproduction_number_lin, col = "lin"), group = 1, size = 0.8) +
  geom_line(aes(x = mid_date, y = reproduction_number_quad, col = "quad"), group = 1, size = 0.8) +
  labs(x = "Date", y = "Reproduction Number (''Work'')", colour = "Estimate Type", title = "A") + 
  scale_x_date(labels = date_format("%b-%y"), breaks = "2 months") + ylim(0, NA) +
  scale_color_manual(breaks = c("comix", "mob", "mob2", "lin", "quad"),
                     values = c("#009E73", "#CC79A7", "#D55E00", "#0072B2", "#F0E442"),
                     labels = c("CoMix", "Mobility", "Mobility Squared", 
                                "Linear Model", "Quadratic Model")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#import contact matrices and scaling factors 
contact_matrices <- qs::qread(file.path(data_path, "contact_matrices_UK.qs"))
scaling_factors <- qs::qread(file.path(data_path, "scaling_factors_UK_fortnightly_filtered_middate_longer.qs"))

#get dates for scaling factors 
scaling_factor_middates <- as.data.table(scaling_factors$mid_date)

#extract matrices
work_matrix <- as.matrix(contact_matrices[[1]]$GBR)
other_matrix <- as.matrix(contact_matrices[[2]]$GBR)

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
e_mob_frame <- cbind(scaling_factor_middates, e_mob_frame)
colnames(e_mob_frame) <- c("mid_date", "dominant_eigenvalue_mob")

#get dominant eigenvalues for mobility squared scaled matrices
e_mob2 <- list()
for(i in 1:nrow(scaling_factors)) {
  matrix <- other_mob2_scaled[[i]]
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
  matrix <- other_lin_scaled[[i]]
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
  matrix <- other_quad_scaled[[i]]
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
eigens <- qs::qread(file.path(data_path, "comix_eigens_other_UK_fortnightly_filtered_middate_longer.qs"))

#merge comix eigenvalues and scaled estimates 
eigens_all <- merge(eigens, e_scaled, by = "mid_date")

#set eigenvalues to numeric and date value to date
eigens_all$mid_date <- as.Date(eigens_all$mid_date, origin = "1970-01-01")
eigens_all$dominant_eigenvalue <- sapply(eigens_all$dominant_eigenvalue, as.numeric)
eigens_all$dominant_eigenvalue_mob <- sapply(eigens_all$dominant_eigenvalue_mob, as.numeric)
eigens_all$dominant_eigenvalue_mob2 <- sapply(eigens_all$dominant_eigenvalue_mob2, as.numeric)
eigens_all$dominant_eigenvalue_lin <- sapply(eigens_all$dominant_eigenvalue_lin, as.numeric)
eigens_all$dominant_eigenvalue_quad <- sapply(eigens_all$dominant_eigenvalue_quad, as.numeric)

#calculate root mean squared error
mob_rootMSE <- sqrt(mean((eigens_all$dominant_eigenvalue - eigens_all$dominant_eigenvalue_mob)^2))
mob2_rootMSE <- sqrt(mean((eigens_all$dominant_eigenvalue - eigens_all$dominant_eigenvalue_mob2)^2))
lin_rootMSE <- sqrt(mean((eigens_all$dominant_eigenvalue - eigens_all$dominant_eigenvalue_lin)^2))
quad_rootMSE <- sqrt(mean((eigens_all$dominant_eigenvalue - eigens_all$dominant_eigenvalue_quad)^2))

#now multiply the dominant eigenvalues by the scalar to calculate the reproduction number
reproduction_all <- rlang::duplicate(eigens_all)
reproduction_all <- reproduction_all[, .(mid_date,reproduction_number = dominant_eigenvalue*0.1581799,
                                         reproduction_number_mob = dominant_eigenvalue_mob*0.1581799,
                                         reproduction_number_mob2 = dominant_eigenvalue_mob2*0.1581799,
                                         reproduction_number_lin = dominant_eigenvalue_lin*0.1581799,
                                         reproduction_number_quad = dominant_eigenvalue_quad*0.1581799)]

#plot
other_UK <- ggplot(data = reproduction_all) + 
  geom_line(aes(x = mid_date, y = reproduction_number, col = "comix"), group = 1, size = 0.8) +
  geom_line(aes(x = mid_date, y = reproduction_number_mob, col = "mob"), group = 1, size = 0.8) +
  geom_line(aes(x = mid_date, y = reproduction_number_mob2, col = "mob2"), group = 1, size = 0.8) +
  geom_line(aes(x = mid_date, y = reproduction_number_lin, col = "lin"), group = 1, size = 0.8) +
  geom_line(aes(x = mid_date, y = reproduction_number_quad, col = "quad"), group = 1, size = 0.8) +
  labs(x = "Date", y = "Reproduction Number (''Other'')", colour = "Estimate Type", title = "B") +
  scale_x_date(labels = date_format("%b-%y"), breaks = "2 months") + ylim(0, NA) +
  scale_color_manual(breaks = c("comix", "mob", "mob2", "lin", "quad"),
                     values = c("#009E73", "#CC79A7", "#D55E00", "#0072B2", "#F0E442"),
                     labels = c("CoMix", "Mobility", "Mobility Squared", 
                                "Linear Model", "Quadratic Model")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#plot work and other together
UK_preds <- ggarrange(work_UK + xlab(NULL) + ylab(NULL), 
                      other_UK + xlab(NULL) + ylab(NULL),
                      common.legend = T, legend = "bottom")

##BE

##using the scaling factors for the BE

#load libraries
library(data.table)
library(ggplot2)
library(tidyverse)
library(lubridate)
library(cowplot)
library(scales)
library(ggpubr)
library(here)

#set cowplot theme
theme_set(cowplot::theme_cowplot(font_size = 14) + theme(strip.background = element_blank(),
                                                         legend.box.margin = margin(l = 10, b = 10)))

#set data path
data_path <- here("data")

#import contact matrices and scaling factors 
contact_matrices <- qs::qread(file.path(data_path, "contact_matrices_BE.qs"))
scaling_factors <- qs::qread(file.path(data_path, "scaling_factors_BE_fortnightly_filtered_middate.qs"))

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
colnames(e_lin_frame) <- c("mid_dates", "dominant_eigenvalue_lin")

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
eigens <- qs::qread(file.path(data_path, "comix_eigens_work_BE_fortnightly_filtered_middate.qs"))

#merge comix eigenvalues and scaled estimates 
eigens_all <- full_join(eigens, e_scaled, by = "mid_date")
eigens_all <- eigens_all[order(eigens_all$mid_date)]

#set eigenvalues to numeric and date value to date
eigens_all$mid_date <- as.Date(eigens_all$mid_date, origin = "1970-01-01")
eigens_all$dominant_eigenvalue <- sapply(eigens_all$dominant_eigenvalue, as.numeric)
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

#plot
work_BE <- ggplot(data = reproduction_all) + 
  geom_line(aes(x = mid_date, y = reproduction_number, col = "comix"), group = 1, size = 0.8) +
  geom_line(aes(x = mid_date, y = reproduction_number_mob, col = "mob"), group = 1, size = 0.8) +
  geom_line(aes(x = mid_date, y = reproduction_number_mob2, col = "mob2"), group = 1, size = 0.8) +
  geom_line(aes(x = mid_date, y = reproduction_number_lin, col = "lin"), group = 1, size = 0.8) +
  geom_line(aes(x = mid_date, y = reproduction_number_quad, col = "quad"), group = 1, size = 0.8) +
  labs(x = "Date", y = "Reproduction Number (''Work'')", colour = "Estimate Type", title = "C") + 
  scale_x_date(labels = date_format("%b-%y"), breaks = "2 months") + ylim(0, NA) +
  scale_color_manual(breaks = c("comix", "mob", "mob2", "lin", "quad"),
                     values = c("#009E73", "#CC79A7", "#D55E00", "#0072B2", "#F0E442"),
                     labels = c("CoMix", "Mobility", "Mobility Squared", 
                                "Linear Model", "Quadratic Model"))

#import contact matrices and scaling factors 
contact_matrices <- qs::qread(file.path(data_path, "contact_matrices_BE.qs"))
scaling_factors <- qs::qread(file.path(data_path, "scaling_factors_BE_fortnightly_filtered_middate.qs"))

#get dates for scaling factors 
scaling_factor_middates <- as.data.table(scaling_factors$mid_date)

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
e_mob_frame <- cbind(scaling_factor_middates, e_mob_frame)
colnames(e_mob_frame) <- c("mid_date", "dominant_eigenvalue_mob")

#get dominant eigenvalues for mobility squared scaled matrices
e_mob2 <- list()
for(i in 1:nrow(scaling_factors)) {
  matrix <- other_mob2_scaled[[i]]
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
  matrix <- other_lin_scaled[[i]]
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
  matrix <- other_quad_scaled[[i]]
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
eigens <- qs::qread(file.path(data_path, "comix_eigens_other_BE_fortnightly_filtered_middate.qs"))

#merge comix eigenvalues and scaled estimates 
eigens_all <- full_join(eigens, e_scaled, by = "mid_date")
eigens_all <- eigens_all[order(eigens_all$mid_date)]

#set eigenvalues to numeric and date value to date
eigens_all$mid_date <- as.Date(eigens_all$mid_date, origin = "1970-01-01")
eigens_all$dominant_eigenvalue <- sapply(eigens_all$dominant_eigenvalue, as.numeric)
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
#plot
other_BE <- ggplot(data = reproduction_all) + 
  geom_line(aes(x = mid_date, y = reproduction_number, col = "comix"), group = 1, size = 0.8) +
  geom_line(aes(x = mid_date, y = reproduction_number_mob, col = "mob"), group = 1, size = 0.8) +
  geom_line(aes(x = mid_date, y = reproduction_number_mob2, col = "mob2"), group = 1, size = 0.8) +
  geom_line(aes(x = mid_date, y = reproduction_number_lin, col = "lin"), group = 1, size = 0.8) +
  geom_line(aes(x = mid_date, y = reproduction_number_quad, col = "quad"), group = 1, size = 0.8) +
  labs(x = "Date", y = "Reproduction Number (''Other'')", colour = "Estimate Type", title = "D") +
  scale_x_date(labels = date_format("%b-%y"), breaks = "2 months") + ylim(0, NA) +
  scale_color_manual(breaks = c("comix", "mob", "mob2", "lin", "quad"),
                     values = c("#009E73", "#CC79A7", "#D55E00", "#0072B2", "#F0E442"),
                     labels = c("CoMix", "Mobility", "Mobility Squared", 
                                "Linear Model", "Quadratic Model"))

#plot work and other together
ggarrange(work, other, common.legend = T, legend = "bottom")


#plot work and other together
BE_preds <- ggarrange(work_BE + xlab(NULL) + ylab(NULL), 
                      other_BE + xlab(NULL) + ylab(NULL),
                      common.legend = T, legend = "bottom")

##NL

#import contact matrices and scaling factors 
contact_matrices <- qs::qread(file.path(data_path, "contact_matrices_NL.qs"))
scaling_factors <- qs::qread(file.path(data_path, "scaling_factors_NL_fortnightly_filtered_middate.qs"))

#get dates for scaling factors 
scaling_factor_middates <- as.data.table(scaling_factors$mid_date)

#extract matrices
work_matrix <- as.matrix(contact_matrices[[1]]$NLD)
other_matrix <- as.matrix(contact_matrices[[2]]$NLD)

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
colnames(e_lin_frame) <- c("mid_dates", "dominant_eigenvalue_lin")

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
eigens <- qs::qread(file.path(data_path, "comix_eigens_work_NL_fortnightly_filtered_middate.qs"))

#merge comix eigenvalues and scaled estimates 
eigens_all <- full_join(eigens, e_scaled, by = "mid_date")
eigens_all <- eigens_all[order(eigens_all$mid_date)]

#set eigenvalues to numeric and date value to date
eigens_all$mid_date <- as.Date(eigens_all$mid_date, origin = "1970-01-01")
eigens_all$dominant_eigenvalue <- sapply(eigens_all$dominant_eigenvalue, as.numeric)
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

#plot
work_NL <- ggplot(data = reproduction_all) + 
  geom_line(aes(x = mid_date, y = reproduction_number, col = "comix"), group = 1, size = 0.8) +
  geom_line(aes(x = mid_date, y = reproduction_number_mob, col = "mob"), group = 1, size = 0.8) +
  geom_line(aes(x = mid_date, y = reproduction_number_mob2, col = "mob2"), group = 1, size = 0.8) +
  geom_line(aes(x = mid_date, y = reproduction_number_lin, col = "lin"), group = 1, size = 0.8) +
  geom_line(aes(x = mid_date, y = reproduction_number_quad, col = "quad"), group = 1, size = 0.8) +
  labs(x = "Date", y = "Reproduction Number (''Work'')", colour = "Estimate Type", title = "E") + 
  scale_x_date(labels = date_format("%b-%y"), breaks = "2 months") + ylim(0, NA) +
  scale_color_manual(breaks = c("comix", "mob", "mob2", "lin", "quad"),
                     values = c("#009E73", "#CC79A7", "#D55E00", "#0072B2", "#F0E442"),
                     labels = c("CoMix", "Mobility", "Mobility Squared", 
                                "Linear Model", "Quadratic Model")) 

#import contact matrices and scaling factors 
contact_matrices <- qs::qread(file.path(data_path, "contact_matrices_NL.qs"))
scaling_factors <- qs::qread(file.path(data_path, "scaling_factors_NL_fortnightly_filtered_middate.qs"))

#get dates for scaling factors 
scaling_factor_middates <- as.data.table(scaling_factors$mid_date)

#extract matrices
work_matrix <- as.matrix(contact_matrices[[1]]$NLD)
other_matrix <- as.matrix(contact_matrices[[2]]$NLD)

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
e_mob_frame <- cbind(scaling_factor_middates, e_mob_frame)
colnames(e_mob_frame) <- c("mid_date", "dominant_eigenvalue_mob")

#get dominant eigenvalues for mobility squared scaled matrices
e_mob2 <- list()
for(i in 1:nrow(scaling_factors)) {
  matrix <- other_mob2_scaled[[i]]
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
  matrix <- other_lin_scaled[[i]]
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
  matrix <- other_quad_scaled[[i]]
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
eigens <- qs::qread(file.path(data_path, "comix_eigens_other_NL_fortnightly_filtered_middate.qs"))

#merge comix eigenvalues and scaled estimates 
eigens_all <- full_join(eigens, e_scaled, by = "mid_date")
eigens_all <- eigens_all[order(eigens_all$mid_date)]

#set eigenvalues to numeric and date value to date
eigens_all$mid_date <- as.Date(eigens_all$mid_date, origin = "1970-01-01")
eigens_all$dominant_eigenvalue <- sapply(eigens_all$dominant_eigenvalue, as.numeric)
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
#plot
other_NL <- ggplot(data = reproduction_all) + 
  geom_line(aes(x = mid_date, y = reproduction_number, col = "comix"), group = 1, size = 0.8) +
  geom_line(aes(x = mid_date, y = reproduction_number_mob, col = "mob"), group = 1, size = 0.8) +
  geom_line(aes(x = mid_date, y = reproduction_number_mob2, col = "mob2"), group = 1, size = 0.8) +
  geom_line(aes(x = mid_date, y = reproduction_number_lin, col = "lin"), group = 1, size = 0.8) +
  geom_line(aes(x = mid_date, y = reproduction_number_quad, col = "quad"), group = 1, size = 0.8) +
  labs(x = "Date", y = "Reproduction Number (''Other'')", colour = "Estimate Type", title = "F") +
  scale_x_date(labels = date_format("%b-%y"), breaks = "2 months") + ylim(0, NA) +
  scale_color_manual(breaks = c("comix", "mob", "mob2", "lin", "quad"),
                     values = c("#009E73", "#CC79A7", "#D55E00", "#0072B2", "#F0E442"),
                     labels = c("CoMix", "Mobility", "Mobility Squared", 
                                "Linear Model", "Quadratic Model"))

#plot work and other together
NL_preds <- ggarrange(work_NL + xlab(NULL) + ylab(NULL), 
                      other_NL + xlab(NULL) + ylab(NULL),
                      common.legend = T, legend = "bottom")

#plot all together
# all_preds <- ggarrange(UK_preds, BE_preds, NL_preds, 
#                        ncol = 1, common.legend = T, legend = "bottom")
# 
# annotate_figure(all_preds, left = textGrob("Reproduction Number (''Work'')", 
#                                            rot = 90, vjust = 0.75, gp = gpar(cex = 1.3)),
#                 right = textGrob("Reproduction Number (''Other'')", 
#                                 rot = 270, vjust = 0.75, gp = gpar(cex = 1.3)),
#                 bottom = textGrob("Date", gp = gpar(cex = 1.3)))
# 
# #plot together a different way
# all_preds2 <- ggarrange(work_UK + xlab(NULL) + ylab(NULL), other_UK + xlab(NULL) + ylab(NULL),
#           work_BE + xlab(NULL) + ylab(NULL), other_BE + xlab(NULL) + ylab(NULL),
#           work_NL + xlab(NULL) + ylab(NULL), other_NL + xlab(NULL) + ylab(NULL), 
#           ncol = 2, nrow = 3, common.legend = T, legend = "none")
legend <- get_legend(work_BE, position = "top")
legend_plot <- as_ggplot(legend) 

cowplot::plot_grid(legend_plot,
cowplot::plot_grid(
  work_UK + guides(col="none") + labs(title = NULL, x = NULL, y = NULL) + theme_cowplot(font_size = 12) + theme(axis.text.x = element_text(angle = 45, hjust = 1)),
  work_BE + guides(col="none") + labs(title = NULL, x = NULL) + theme_cowplot(font_size = 12),
  work_NL + guides(col="none") + labs(title = NULL, y = NULL) + theme_cowplot(font_size = 12), 
  other_UK + guides(col="none") + labs(title = NULL, x = NULL, y = NULL) + theme_cowplot(font_size = 12) + theme(axis.text.x = element_text(angle = 45, hjust = 1)),
  other_BE + guides(col="none") + labs(title = NULL, x = NULL) + theme_cowplot(font_size = 12),
  other_NL + guides(col="none") + labs(title = NULL, y = NULL) + theme_cowplot(font_size = 12),
  nrow = 3, byrow = FALSE, align = "v", labels = LETTERS, label_x = 0.17, rel_heights = c(1, 0.87, 0.94)
), nrow = 2, rel_heights = c(0.1, 1))


design1 <- 
  "AAAA
   BBBB
   CCCC"
work_patch <- (work_UK + guides(col="none")) / (work_BE + guides(col="none")) / (work_NL + guides(col="none")) + 
  plot_layout(design = design1, guides = "collect", axes = "collect", nrow = 3, heights = c(4,4,4))
design2 <- 
  "AAAA
   BBBB
   CCCC"
other_patch <- (other_UK + guides(col="none"))/ (other_BE + guides(col="none")) / (other_NL + guides(col="none"))  +
  plot_layout(design = design2, guides = "collect", axes = "collect", nrow = 3, heights = c(4,4,4))
design3 <- 
  "AA##"
legend_patch <- as_ggplot(legend) 

legend_patch
work_patch
other_patch
design4 <- 
  "AAAA
   BBCC
   BBCC
   BBCC"
free(legend_patch) / (work_patch | other_patch) + plot_layout(nrow = 2, heights = c(1, 10))

# (work_UK + other_UK) / (work_BE + other_BE) / (work_NL + other_NL) + guide_area() + 
#   plot_layout(design = design, guides = "collect", axes = "collect")

# annotate_figure(all_preds2, left = textGrob("Reproduction Number (''Work'')", 
#                                            rot = 90, vjust = 0.75, gp = gpar(cex = 1.3)),
#                 right = textGrob("Reproduction Number (''Other'')", 
#                                  rot = 270, vjust = 0.75, gp = gpar(cex = 1.3)),
#                 bottom = textGrob("Date", vjust = -0.1, gp = gpar(cex = 1.3)),
#                 top = legendGrob(#title = "Estimate Type", 
#                                  gp = gpar(col = c("white", "#009E73", "#CC79A7", "#D55E00",
#                                                    "#0072B2", "#F0E442"),
#                                            lty = 1, lwd = 2),
#                                  labels = c("Estimate Type", "CoMix", "Mobility", 
#                                             "Mobility Squared", "Linear Model",
#                                             "Quadratic Model"),
#                                  ncol = 6))

