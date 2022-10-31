####


num <- cnts_date[, t := as.numeric(date - ymd("2020-01-01"))]
gm2 = rlang::duplicate(mob)
names(gm2) = str_replace(names(gm2), "_percent_change_from_baseline", "")
names(gm2) = str_replace(names(gm2), "_and", "")
colnames(num$n_cnt_home)
num = merge(num, gm2[, 8:14], by = "date", all.x = T)


pnum <- qs::qread(file.path(data_path, "polymod.qs"))
pnum[, study := "POLYMOD"]

num = rbind(num, pnum, fill = TRUE)

num[, retail_recreation := (100 + retail_recreation) * 0.01]
num[, grocery_pharmacy  := (100 + grocery_pharmacy ) * 0.01]
num[, parks             := (100 + parks            ) * 0.01]
num[, transit_stations  := (100 + transit_stations ) * 0.01]
num[, workplaces        := (100 + workplaces       ) * 0.01]
num[, residential       := (100 + residential      ) * 0.01]

# Un-oversample young people from POLYMOD
num = rbind(
  num[study == "CoMix"],
  num[study == "POLYMOD" & part_age <= 20][seq(0, .N, by = 2)],
  num[study == "POLYMOD" & part_age > 20]
)


another = num[, .(work = mean(work), workplaces = mean(workplaces), transit = mean(transit_stations)), 
              by = .(week = ifelse(study == "CoMix", week(date) %/% 2, rep(0, length(date))), study)]
model = gam(work ~ s(workplaces), family = gaussian, data = another)
work_f = data.table(workplaces = seq(0, 1.25, by = 0.01));
work_f[, work := pmax(0.0, predict(model, work_f, type = "response"))]

plw = ggplot(another) + 
  geom_point(aes(x = workplaces, y = work, colour = study)) + 
  geom_line(data = work_f, aes(x = workplaces, y = work)) +
  ylim(0, 3.5) +
  labs(x = "Google Mobility\n'workplaces' visits", y = "Work contacts", colour = "Study") +
  theme(legend.position = "none")
