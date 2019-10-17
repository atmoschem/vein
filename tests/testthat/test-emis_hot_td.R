context("emis_dist")

euros <- c("V", "V", "IV", "III", "II", "I", "PRE", "PRE")

efh <- ef_ldv_speed(v = "PC", t = "4S", cc = "<=1400", f = "G",

                    eu = euros, p = "CO", speed = Speed(34))

lkm <- units::as_units(18:11, "km")*1000

veh_month <- c(rep(8, 1), rep(10, 5), 9, rep(10, 5))

veh <- age_ldv(1:10, agemax = 8)
a <- emis_hot_td(veh = veh,

                 lkm = lkm,

                 ef = EmissionFactors(as.numeric(efh[, 1:8])),

                 verbose = TRUE)

