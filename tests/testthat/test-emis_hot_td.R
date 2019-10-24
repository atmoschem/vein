context("emis_hot_td")

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

# caso simple ####
test_that("emis_hot_td works", {
  expect_equal(round(emis_hot_td(veh = veh,
                                 lkm = lkm,
                                 ef = EmissionFactors(as.numeric(efh[, 1:8])),
                                 verbose = TRUE)$emissions[1]),
               Emissions(395))

  expect_equal(round(emis_hot_td(veh = veh,
                                 lkm = lkm,
                                 ef = EmissionFactors(as.numeric(efh[, 1:8])),
                                 verbose = TRUE,
                                 params = list(paste0("data_", 1:10),
                                               "moredata"))$emissions[1]),
               Emissions(395))

  expect_message(round(emis_hot_td(veh = veh,
                                   lkm = lkm,
                                   ef = EmissionFactors(as.numeric(efh[, 1:8])),
                                   verbose = TRUE)$emissions[1]),
                 "E.?")

  expect_output(emis_hot_td(veh = veh,
                            lkm = lkm,
                            ef = EmissionFactors(as.numeric(efh[, 1:8])),
                            verbose = TRUE),
                "S.?")
  expect_message(round(emis_hot_td(veh = veh,
                                   lkm = lkm,
                                   ef = EmissionFactors(as.numeric(efh[, 1:8])),
                                   verbose = TRUE,
                                   params = list(paste0("data_", 1:10),
                                                 "moredata"))$emissions[1]),
                 "A.?")

})



# Caso perfil mensual ####
test_that("emis_hot_td works", {
  expect_equal(round(emis_hot_td(veh = veh,
                                 lkm = lkm,
                                 ef = EmissionFactors(as.numeric(efh[, 1:8])),
                                 verbose = TRUE,
                                 pro_month = 1:12)$emissions[1]),
               Emissions(5))
  expect_equal(round(emis_hot_td(veh = veh,
                                 lkm = lkm,
                                 ef = EmissionFactors(as.numeric(efh[, 1:8])),
                                 verbose = TRUE,
                                 pro_month = 1:12,
                                 params = list(paste0("data_", 1:10),
                                               "moredata"))$emissions[1]),
               Emissions(5))


  expect_message(emis_hot_td(veh = veh,
                             lkm = lkm,
                             ef = EmissionFactors(as.numeric(efh[, 1:8])),
                             verbose = TRUE,
                             pro_month = 1:12),
                 "E.?")

  expect_message(emis_hot_td(veh = veh,
                             lkm = lkm,
                             ef = EmissionFactors(as.numeric(efh[, 1:8])),
                             verbose = TRUE,
                             pro_month = 1:12),
                 "E.?")

})


# Caso ef es data.frame ####
efs <- EmissionFactors(matrix(1, nrow = 12, ncol = ncol(veh)))
efs2 <- EmissionFactors(matrix(1, nrow = nrow(veh), ncol = ncol(veh)))
test_that("emis_hot_td works", {
  expect_error(emis_hot_td(veh = veh,
                                 lkm = lkm,
                                 ef = efs,
                                 verbose = TRUE),
               "N.?")
  expect_equal(round(emis_hot_td(veh = veh,
                           lkm = lkm,
                           ef = efs2,
                           verbose = TRUE)$emissions[1]),
               Emissions(1624))

  expect_message(round(emis_hot_td(veh = veh,
                                 lkm = lkm,
                                 ef = efs2,
                                 verbose = TRUE)$emissions[1]),
               "E.?")
  expect_message(round(emis_hot_td(veh = veh,
                                   lkm = lkm,
                                   ef = efs2,
                                   verbose = TRUE)$emissions[1]),
                 ".?")

  expect_equal(round(emis_hot_td(veh = veh,
                                 lkm = lkm,
                                 ef = efs2,
                                 verbose = TRUE,
                                 params = list(paste0("data_", 1:nrow(veh)),
                                               "moredata"))$emissions[1]),
               Emissions(1624))
  expect_message(emis_hot_td(veh = veh,
                                 lkm = lkm,
                                 ef = efs2,
                                 verbose = TRUE,
                                 params = list(paste0("data_", 1:nrow(veh)),
                                               "moredata")),
               "A.?")
})






