##circuit breaker figure practice 

#load libraries
library(data.table)
library(ggplot2)
library(tidyverse)
library(lubridate)
library(zoo)
library(mgcv)
library(stringr)
library(cowplot)

#set data path
data_path <-"C:\\Users\\emiel\\Documents\\LSHTM\\Fellowship\\Project\\comix_mobility\\Data\\"

#set cowplot theme
theme_set(cowplot::theme_cowplot(font_size = 10) + theme(strip.background = element_blank()))

#load mobility data 
gm <- qs::qread(file.path(data_path, "google_mob_region.qs"))
gm[, goog_name := ifelse(sub_region_2 != "", paste0(sub_region_2, ", ", sub_region_1),
                    sub_region_1)];
gm <- gm[goog_name != ""]

#load region data
regions <- fread(file.path(data_path, "goog_regions.txt"))

#merge 
gm2 <- merge(gm, regions, by = "goog_name", all.x = T)

#other places' lockdowns/circuit breakers
gm_melt <- function(gm) {
  
  g <- melt(gm, id.vars = 1:5)
  g[, variable := str_remove_all(variable, "_percent_change_from_baseline")]
  return(g)
  
}         
    