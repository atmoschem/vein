context("emis_cold_td")

euros <- c("V", "V", "IV", "III", "II", "I", "PRE", "PRE")

dt <- matrix(rep(2:25,5), ncol = 12, nrow = 10) # 12 months, 10 rows

row.names(dt) <- paste0("Simple_Feature_", 1:10)

efc <- ef_ldv_cold(ta = dt,
                   cc = "<=1400",
                   f ="G",
                   eu = euros,
                   p = "CO",
                   speed = Speed(34))

efh <- ef_ldv_speed(v = "PC",
                    t = "4S",
                    cc = "<=1400",
                    f = "G",
                    eu = euros,
                    p = "CO",
                    speed = Speed(34))

lkm <- units::as_units(18:11, "km")*1000

cold_lkm <- cold_mileage(ltrip = units::as_units(20, "km"),
                         ta = celsius(dt))

names(cold_lkm) <- paste0("Month_", 1:12)

veh_month <- c(rep(8, 1), rep(10, 5), 9, rep(10, 5))

veh <- age_ldv(1:10, agemax = 8)


emis_cold_td(veh = veh,
             lkm = lkm,
             ef = efh,
             efcold = efc[1:10, ],
             beta = cold_lkm[,1],
             verbose = TRUE)

test_that("emis_cold works", {
  expect_equal(round(emis_cold_td(veh = veh,
                                  lkm = lkm,
                                  ef = efh,
                                  efcold = efc[1:10, ],
                                  beta = cold_lkm[,1],
                                  verbose = TRUE)$emissions[1]),
               Emissions(108))
  expect_output(emis_cold_td(veh = veh, lkm = lkm, ef = efh, efcold = efc[1:10, ],
                             beta = cold_lkm[,1], verbose = TRUE),
                "S.?")
  expect_message(emis_cold_td(veh = veh, lkm = lkm, ef = efh, efcold = efc[1:10, ],
                              beta = cold_lkm[,1], verbose = TRUE),
                 "E.?")
  expect_equal(round(emis_cold_td(veh = veh, lkm = lkm, ef = efh, efcold = efc[1:10, ],
                                  beta = cold_lkm[,1], verbose = TRUE,
                                  params = list(paste0("data_", 1:10),
                                                "moredata"))$emissions[1]),
               Emissions(108))
  expect_equal(round(emis_cold_td(veh = veh, lkm = lkm, ef = efh, efcold = efc,
                                  beta = cold_lkm, pro_month = veh_month, verbose = T)$emissions[1]),
               Emissions(7))
  efhh <- as.data.frame(
    matrix(seq_along(veh), nrow =  nrow(veh), ncol = ncol(veh)))
  expect_equal(round(emis_cold_td(veh = veh,
                                  lkm = lkm,
                                  ef = EmissionFactors(efhh),
                                  efcold = efc,
                                  fortran = T,
                                  beta = cold_lkm,
                                  pro_month = veh_month,
                                  verbose = T)$emissions[1]),
               Emissions(30))
  expect_equal(round(emis_cold_td(veh = veh, lkm = lkm, ef = EmissionFactors(efhh), efcold = efc, fortran = T,
                                  beta = cold_lkm, pro_month = veh_month, verbose = T)$emissions[1]),
               Emissions(30))
  expect_message(round(emis_cold_td(veh = veh, lkm = lkm, ef = efh, efcold = efc,
                                    beta = cold_lkm,
                                    pro_month = veh_month,
                                    verbose = T)$emissions[1]),
                 "E.?")
  expect_message(round(emis_cold_td(veh = veh, lkm = lkm, ef = efh, efcold = efc,
                                    beta = cold_lkm,
                                    pro_month = veh_month,
                                    verbose = T)$emissions[1]),
                 "A.?")
  expect_output(round(emis_cold_td(veh = veh, lkm = lkm, ef = efh, efcold = efc,
                                   beta = cold_lkm,
                                   pro_month = veh_month,
                                   verbose = T)$emissions[1]),
                "S.?")
expect_equal(
    round(emis_cold_td(veh = veh,
                       lkm = lkm,
                       ef = efh,
                       efcold = efc,
                       beta = cold_lkm,
                       pro_month = veh_month,
                       verbose = FALSE,
                       params = list(paste0("data_", 1:10), "moredata"))$emissions[1]),
    Emissions(7))
})



test_that("emis_cold stops", {
  expect_error(
    emis_cold_td(veh = veh, lkm = 1, ef = efh, efcold = efc,
                 beta = cold_lkm, pro_month = veh_month, verbose = FALSE,
                 params = list(paste0("data_", 1:10), "moredata")),
    ".?")
  a <- units::set_units(1, "m")
  expect_error(
    emis_cold_td(veh = veh, lkm = a, ef = efh, efcold = efc,
                 beta = cold_lkm, pro_month = veh_month, verbose = FALSE,
                 params = list(paste0("data_", 1:10), "moredata")),
    ".?")
  a <- units::set_units(1, "km")
  expect_error(
    emis_cold_td(veh = veh, lkm = a, ef = efh, efcold = efc,
                 beta = cold_lkm, pro_month = veh_month, verbose = FALSE,
                 params = list(paste0("data_", 1:10), "moredata")),
    ".?")
  a <- units::set_units(1, "km")
  expect_error(
    emis_cold_td(veh = veh, lkm = lkm, ef = remove_units(efh), efcold = efc,
                 beta = cold_lkm, pro_month = veh_month, verbose = FALSE,
                 params = list(paste0("data_", 1:10), "moredata")),
    ".?")
  expect_error(
    emis_cold_td(veh = veh, lkm = lkm, ef = celsius(remove_units(efh)), efcold = efc,
                 beta = cold_lkm, pro_month = veh_month, verbose = FALSE,
                 params = list(paste0("data_", 1:10), "moredata")),
    ".?")
  expect_error(
    emis_cold_td(veh = veh, lkm = lkm, ef = remove_units(efh$V1), efcold = efc,
                 beta = cold_lkm, pro_month = veh_month, verbose = FALSE,
                 params = list(paste0("data_", 1:10), "moredata")),
    ".?")
  expect_error(
    emis_cold_td(veh = veh, lkm = lkm, ef = celsius(remove_units(efh$V1)), efcold = efc,
                 beta = cold_lkm, pro_month = veh_month, verbose = FALSE,
                 params = list(paste0("data_", 1:10), "moredata")),
    ".?")
})
veh_month <- c(rep(8, 1), rep(10, 5), 9, rep(10, 5))
df_month <- as.data.frame(matrix(veh_month, ncol = 12) )

test_that("emis_cold works", {
  expect_equal(round(emis_cold_td(veh = veh,
                                  lkm = lkm,
                                  ef = efh,
                                  efcold = efc,
                                  beta = cold_lkm,
                                  pro_month = df_month,
                                  verbose = T)$emissions[1]),
               Emissions(7))

  expect_equal(round(emis_cold_td(veh = veh,
                                  lkm = lkm,
                                  ef = EmissionFactors(as.numeric(efh[, 1:ncol(veh)])),
                                  efcold = efc,
                                  beta = cold_lkm,
                                  pro_month = df_month,
                                  verbose = T)$emissions[1]),
               Emissions(7))

  expect_equal(round(emis_cold_td(veh = veh,
                                  lkm = lkm,
                                  ef = EmissionFactors(as.numeric(efh[, 1:ncol(veh)])),
                                  efcold = efc,
                                  beta = cold_lkm,
                                  pro_month = df_month,
                                  verbose = T,
                                  fortran = TRUE)$emissions[1]),
               Emissions(7))

  expect_equal(round(emis_cold_td(veh = veh,
                                  lkm = lkm,
                                  ef = EmissionFactors(as.numeric(efh[, 1:ncol(veh)])),
                                  efcold = efc,
                                  beta = cold_lkm,
                                  pro_month = df_month,
                                  verbose = T,
                                  fortran = TRUE)$emissions[1]),
               Emissions(7))

  expect_error(emis_cold_td(veh = veh,
                            lkm = lkm,
                            ef = efh,
                            efcold = efc,
                            fortran = TRUE,
                            beta = cold_lkm,
                            pro_month = df_month,
                            verbose = T),
               ".?")

  efhh <- as.data.frame(
    matrix(seq_along(veh), nrow =  nrow(veh), ncol = ncol(veh)))

  expect_equal(round(emis_cold_td(veh = veh,
                                  lkm = lkm,
                                  ef = EmissionFactors(efhh),
                                  efcold = efc,
                                  beta = cold_lkm,
                                  pro_month = df_month,
                                  verbose = T)$emissions[1]),
               Emissions(30))
  expect_equal(round(emis_cold_td(veh = veh,
                                  lkm = lkm,
                                  ef = EmissionFactors(efhh),
                                  efcold = efc,
                                  fortran = TRUE,
                                  beta = cold_lkm,
                                  pro_month = df_month,
                                  verbose = T)$emissions[1]),
               Emissions(30))
})


test_that("emis_cold does not work", {
  expect_error(round(emis_cold_td(veh = veh,
                                  lkm = lkm,
                                  ef = efh,
                                  efcold = remove_units(efc),
                                  beta = cold_lkm,
                                  pro_month = df_month,
                                  verbose = T)$emissions[1]),
               ".?")

  efhh <- as.data.frame(
    matrix(seq_along(veh), nrow =  nrow(veh), ncol = ncol(veh)))
  expect_error(round(emis_cold_td(veh = veh,
                                  lkm = lkm,
                                  ef = EmissionFactors(efhh),
                                  efcold = remove_units(efc),
                                  beta = cold_lkm,
                                  pro_month = df_month,
                                  verbose = T)$emissions[1]),
               ".?")
})

