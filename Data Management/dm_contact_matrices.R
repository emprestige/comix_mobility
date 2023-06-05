##get synthetic contact matrices 

#load libraries
library(data.table)
library(tidyverse)
library(lubridate)

#set data path
data_path <-"C:\\Users\\emiel\\Documents\\LSHTM\\Fellowship\\Project\\comix_mobility\\Data\\"

#load in contact data
load(file.path(data_path, "contact_work.rdata"))
load(file.path(data_path, "contact_others.rdata"))

#work contacts from BE/NL/UK
contacts_work_BE <- contact_work["BEL"]
contacts_work_NL <- contact_work["NLD"]
contacts_work_UK <- contact_work["GBR"]

#other contacts from BE/NL/UK
contacts_other_BE <- contact_others["BEL"]
contacts_other_NL <- contact_others["NLD"]
contacts_other_UK <- contact_others["GBR"]

#work and other contacts from BE/NL/UK
contacts_BE <- list(contacts_work_BE, contacts_other_BE)
contacts_NL <- list(contacts_work_NL, contacts_other_NL)
contacts_UK <- list(contacts_work_UK, contacts_other_UK)

#save contact matrices 
qs::qsave(contacts_BE, file.path(data_path, "contact_matrices_BE.qs"))
qs::qsave(contacts_NL, file.path(data_path, "contact_matrices_NL.qs"))
qs::qsave(contacts_UK, file.path(data_path, "contact_matrices_UK.qs"))
