##tier figure practice 

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

#import regional data
gm <- qs::qread(file.path(data_path, "google_mob_region.qs"))

#subset by date
gm <- gm[date <= "2020-10-27"]

#define google regions
gp <- rbind(
  gm[sub_region_1 != "", .(place = unique(sub_region_1), type = "sub_region_1")],
  gm[sub_region_2 != "", .(place = unique(sub_region_2), type = "sub_region_2")]
)

#import tiers data
tiers <- fread(file.path(data_path, "tiers.txt"))

#create interactive match finder
imatch <- function(strings, table, table_attr) {

  match_table <- data.table(str = table, attr = table_attr);
  match_table[, match_stry := str_replace_all(str, "&", "and")];

  result = NULL;

  for (string in strings) {

    match_table[, score := 0];

    s2 <- str_replace_all(string, "&", "and");
    s2a <- str_split(s2, " | ")[[1]]
    s2b <- s2a[2];
    s2a <- s2a[1];

    match_table[str %ilike% paste0("^", s2a, "$"), score := score + 100];
    match_table[str %ilike% paste0("^", s2b, "$"), score := score + 100];
    words <- str_split(s2, " ")[[1]];

    for ( w in words) {

      match_table[str %ilike% w, score := score + str_length(w)];

    }

    best <- match_table[order(-score)][1:10, .(str, attr, score)];

    cat(paste0(string, ":\n"));
    cat(paste0(1:10, ": ", best$str, " (", best$attr, ")", collapse = "\n"))
    cat("\n0: No match\n");

    a <- as.numeric(readline(prompt = "Which match is best? "));

    result <- rbind(result, data.table(string = string, match = best$str[a], attr = best$attr[a]));

  }

  return(result)

}

#import match table
result <- fread(file.path(data_path, "tiers_match_table.txt"))

# #update it 
tiers[, reg12 := paste(reg1, reg2, sep = " | ")]
missing <- tiers[!reg12 %in% result[, string], reg12]
updates <- imatch(missing, gp$place, gp$type); 
result <- rbind(result, updates);
fwrite(result, file.path(data_path, "tiers_match_table.txt"))

#merge tiers and result
tiers <- merge(tiers, result, by.x = "reg12", by.y = "string", all = T)

#create tiers timeline 
changes <- tiers[, .(gp = match, type = attr, date = as.Date(date), tier)]
changes <- rbind(gp[, .(gp = place, type, date = ymd("2020-10-14"), tier = 1)], changes)

#remove first changes on same date
changes <- changes[!duplicated(changes[, .(gp, type, date)], fromLast = T)]

#add tier "0"
changes <- rbind(gp[, .(gp = place, type, date = ymd("2020-09-01"), tier = 0)], changes)

#put in order 
changes <- changes[order(date, gp, type)]

#create two subsets of google mobility
gm1 <- gm[sub_region_1 != "" & sub_region_2 == ""]
gm2 <- gm[sub_region_1 != "" & sub_region_2 != ""]

#merge changes into google mobility
gm1 <- merge(gm1, changes[type == "sub_region_1", .(gp, date, tier)], by.x = c("sub_region_1", "date"), by.y = c("gp", "date"), all.x = T);
gm2 <- merge(gm2, changes[type == "sub_region_2", .(gp, date, tier)], by.x = c("sub_region_2", "date"), by.y = c("gp", "date"), all.x = T);

gm <- rbindlist(list(gm[sub_region_1 == ""], gm1, gm2), fill = T);
gm <- gm[order(country_region, sub_region_1, sub_region_2, date)];

#remove "all UK"
gm <- gm[sub_region_1 != ""]

#fill out tiers
gm[, tier := na.locf(tier, na.rm = F), by = .(sub_region_1, sub_region_2)]

#remove pre tier 0
gm = gm[!is.na(tier)]

#restrict to England
regions <- fread(file.path(data_path, "goog_regions.txt"))
english_places <- regions[gss_code %like% "^E", .(goog_name)]
gm <- gm[sub_region_1 %in% english_places$goog_name]

#set up for analysis
gm[, full_region := paste(sub_region_1, "|", sub_region_2)]
gm[, weekday := factor(wday(date, label = T), ordered = F)]
gm[, weekday_t := wday(date)]
gm[, date_t := as.numeric(ymd(date) - ymd("2020-01-01"))]
gm[, week_t := week(date)]

indicators <- names(gm)[names(gm) %like% "_percent_change"]

#analysis
regions <- gm[, unique(full_region)]
gm2 <- rlang::duplicate(gm)
for (col in 9:14) set(gm2, j = col, value = as.double(gm2[[col]]))

#remove weekday effect from each trendline
results <- NULL
for (reg in regions) {
  
  cat(".")
  
  for (ind in indicators) {
    
    model <- try(gam(get(ind) ~ s(date_t) + s(weekday_t, bs = "cp", k = 7),
      knots = list(weekday_t = c(0, 7)), data = gm2[full_region == reg]), silent = T)
    
    if ("try-error" %in% class(model)) {
      
      next; 
      
    }
    
    pp <- predict(model, type = "terms")
    results <- rbind(results, 
        gm2[full_region == reg & !is.na(get(ind)),
            .(sub_region_1, sub_region_2, date, date_t, weekday, tier, 
              indicator = ind, original = get(ind), 
              unweekday = get(ind) - pp[, "s(weekday_t)"])]
        )
    
  }
  
}

#remove national trend from trendlines
for (ind in indicators) {
  
  cat(".")
  model <- gam(unweekday ~ s(date_t), data = results[indicator == ind])
  results[indicator == ind, detrend := unweekday - predict(model, newdata = .SD)];

}

#factorise
results[, full_region := factor(paste(sub_region_1, sub_region_2))]
results[, tier := factor(paste("T", tier))]
results[, indicator := factor(indicator)]

# - detrended, compared to tier 0
baselineD <- results[tier == "T 0", .(baseline = mean(tail(detrend, 7))),
                     by = .(full_region, indicator)]
resultsD <- merge(results, baselineD, by = c("full_region", "indicator"))
resD <- resultsD[tier != "T 0", .(detrend = mean(detrend), 
                                  baseline = mean(baseline),
                                  change = mean(detrend - baseline)),
                 by = .(full_region, sub_region_1, indicator, tier)]

#create factor of google mobility indicator
resultsD[, indic := str_remove_all(indicator, "_percent_change_from_baseline")]
resultsD[, indic := str_replace_all(indic, "_", " ")]
resultsD[, indic := str_to_sentence(indic)]
resultsD[, indic := factor(indic)]

#get tier by tier data for each indicator, detrended
individual <- resultsD[tier != "T 0", .(detrend = mean(detrend), 
              baseline = mean(original), change = mean(detrend - baseline)), 
              by = .(full_region, sub_region_1, indic, tier)]

#get summary data
summary <- individual[, .(change = mean(change), d = var(change)/.N), 
                      keyby = .(indic, tier)]
summary[, T1 := first(change), by = indic]
summary[, T1d := first(d), by = indic]
summary <- summary[, .(change = change - T1, sd = sqrt(d + T1d)), keyby = .(indic, tier)]
summary[tier == "T 1", sd := 0]

#create violin plots 
ggplot(individual) +
  geom_violin(aes(x = tier, y = change, fill = as.factor(tier)), alpha = 0.25, colour = NA) +
  geom_jitter(aes(x = tier, y = change, colour = as.factor(tier)), width = 0.4, height = 0, size = 0.75) +
  geom_violin(aes(x = tier, y = change), colour = "#000000", size = 0.3, fill = NA) +
  geom_label(data = summary, aes(x = tier, y = change, label = round(change, 2)), alpha = 0.75) +
  facet_wrap(~indic, scales = "free") +
  scale_colour_manual(values = c("#4499cc", "#ee99ee", "#ee4433"), aesthetics = c("colour", "fill")) +
  theme(legend.position = "none") +
  labs(x = "Tier", y = "Change in Google Mobility index", fill = "Tier", colour = "Tier")
