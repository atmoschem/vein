context("emis_hot_td")

data(net)
netsf <- sf::st_as_sf(net)[1:10, ]
euros <- c("V", "V", "IV", "III", "II", "I", "PRE", "PRE")
efh <- ef_ldv_speed(v = "PC", t = "4S", cc = "<=1400", f = "G",
                    eu = euros, p = "CO", speed = Speed(34))
veh <- age_ldv(1:10, agemax = 8)
veh2 <- age_ldv(1:10, agemax = 8, net = netsf[1:10, ])

lkm <- units::as_units(18:11, "km")*1000
veh_month <- c(rep(8, 1), rep(10, 5), 9, rep(10, 5))
data(net)
vehsf <- age_ldv(1:10, agemax = 10, net = net[1:10, ])
a <- emis_hot_td(veh = veh,
                 lkm = lkm,
                 ef = EmissionFactors(as.numeric(efh[, 1:8])),
                 verbose = TRUE)
system.time(a <- emis_hot_td(veh = veh,
                 lkm = lkm,
                 ef = EmissionFactors(as.numeric(efh[, 1:8])),
                 verbose = TRUE,
                 fortran = TRUE)
)
system.time(
a <- emis_hot_td(veh = veh,
                 lkm = lkm,
                 ef = EmissionFactors(as.numeric(efh[, 1:8])),
                 verbose = TRUE,
                 fortran = TRUE,
                 nt = check_nt()/2)
)
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
                                 fortran = TRUE)$emissions[1]),
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
  expect_message(round(emis_hot_td(veh = veh,
                                   lkm = lkm,
                                   ef = EmissionFactors(as.numeric(efh[, 1:8])),
                                   verbose = TRUE,
                                   fortran = TRUE)$emissions[1]),
                 "E.?")

  expect_output(emis_hot_td(veh = veh,
                            lkm = lkm,
                            ef = EmissionFactors(as.numeric(efh[, 1:8])),
                            verbose = TRUE),
                "S.?")
  expect_output(emis_hot_td(veh = veh2,
                            lkm = lkm,
                            ef = EmissionFactors(as.numeric(efh[, 1:8])),
                            verbose = TRUE),
                ".?")
  expect_message(round(emis_hot_td(veh = veh,
                                   lkm = lkm,
                                   ef = EmissionFactors(as.numeric(efh[, 1:8])),
                                   verbose = TRUE,
                                   params = list(paste0("data_", 1:10),
                                                 "moredata"))$emissions[1]),
                 "A.?")

  expect_error(emis_hot_td(veh = veh,
                           lkm = 2,
                           ef = EmissionFactors(as.numeric(efh[, 1:8])),
                           verbose = TRUE),
               "l.?")

  expect_error(emis_hot_td(veh = veh,
                           lkm = units::set_units(2, "m"),
                           ef = EmissionFactors(as.numeric(efh[, 1:8])),
                           verbose = TRUE),
               ".?")

  expect_error(emis_hot_td(veh = veh,
                           lkm = units::set_units(2, "km"),
                           ef = EmissionFactors(as.numeric(efh[, 1:8])),
                           verbose = TRUE),
               ".?")
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

  expect_error(emis_hot_td(veh = veh,
                           lkm = lkm,
                           ef = 1,
                           verbose = TRUE,
                           pro_month = 1:12),
               ".?")

  expect_error(emis_hot_td(veh = veh,
                           lkm = lkm,
                           ef = 1,
                           verbose = TRUE),
               ".?")

  expect_equal(emis_hot_td(veh = age_ldv(1:10, agemax = 8),
                           lkm = lkm,
                           ef = EmissionFactors(as.numeric(efh[, 1:8])),
                           verbose = TRUE)$rows[1],
               "1")
  expect_message(emis_hot_td(veh = age_ldv(1:10, agemax = 8),
                             lkm = lkm,
                             ef = EmissionFactors(as.numeric(efh[, 1:8])),
                             verbose = TRUE)$rows[1],
                 ".?")
  expect_output(emis_hot_td(veh = age_ldv(1:10, agemax = 8),
                            lkm = lkm,
                            ef = EmissionFactors(as.numeric(efh[, 1:8])),
                            verbose = TRUE)$rows[1],
                ".?")

  expect_error(emis_hot_td(veh = veh,
                           lkm = lkm,
                           ef = EmissionFactors(as.numeric(efh[, 1:8])),
                           verbose = TRUE,
                           pro_month = data.frame(1:12)),
               ".?")

  expect_equal(emis_hot_td(veh = veh,
                           lkm = lkm,
                           ef = EmissionFactors(as.numeric(efh[, 1:8])),
                           verbose = TRUE,
                           pro_month = data.frame(matrix(1, ncol = 12)))$rows[1],
               "1")
  expect_message(emis_hot_td(veh = veh,
                           lkm = lkm,
                           ef = EmissionFactors(as.numeric(efh[, 1:8])),
                           verbose = TRUE,
                           pro_month = data.frame(matrix(1, ncol = 12))),
               ".?")

  expect_output(emis_hot_td(veh = veh,
                             lkm = lkm,
                             ef = EmissionFactors(as.numeric(efh[, 1:8])),
                             verbose = TRUE,
                             pro_month = data.frame(matrix(1, ncol = 12))),
                 ".?")

})


# Caso ef es data.frame ####
efm <- matrix(1, nrow = 1, ncol = ncol(veh))
efs <- EmissionFactors(matrix(1, nrow = 12, ncol = ncol(veh)))
efs2 <- EmissionFactors(matrix(1, nrow = nrow(veh), ncol = ncol(veh)))
test_that("emis_hot_td works", {
  expect_error(emis_hot_td(veh = veh,
                           lkm = lkm,
                           ef = efm,
                           verbose = TRUE),
               ".?")

  expect_equal(emis_hot_td(veh = veh,
                           lkm = lkm,
                           ef = EmissionFactors(efm),
                           verbose = TRUE)$rows[1],
               "1")

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




veh_month <- as.data.frame(matrix(veh_month, ncol = 12) )


# Caso ef es data.frame y perfil mensual tambien ####
efm <- matrix(1, nrow = 1, ncol = ncol(veh))
efs <- EmissionFactors(matrix(1, nrow = 12, ncol = ncol(veh)))
efs2 <- EmissionFactors(matrix(1, nrow = nrow(veh), ncol = ncol(veh)))

test_that("emis_hot_td works", {
  expect_error(emis_hot_td(veh = veh,
                           lkm = lkm,
                           ef = efm,
                           pro_month = veh_month,
                           verbose = TRUE),
               ".?")

  expect_equal(round(emis_hot_td(veh = veh,
                           lkm = lkm,
                           ef = EmissionFactors(efm),
                           pro_month = veh_month,
                           verbose = TRUE)$emissions[1]),
               Emissions(111))

  expect_error(round(emis_hot_td(veh = veh[1, ],
                                 lkm = lkm,
                                 ef = EmissionFactors(rbind(efm, efm)),
                                 pro_month = veh_month,
                                 verbose = TRUE)$emissions[1]),
               ".?")

  expect_error(round(emis_hot_td(veh = veh[1, ],
                                 lkm = lkm,
                                 ef = EmissionFactors(cbind(efm, efm)),
                                 pro_month = veh_month,
                                 verbose = TRUE)$emissions[1]),
               ".?")

  expect_error(round(emis_hot_td(veh = veh[1, ],
                                 lkm = c(lkm,lkm),
                                 ef = EmissionFactors(efm),
                                 pro_month = veh_month,
                                 verbose = TRUE)$emissions[1]),
               ".?")

  expect_error(round(emis_hot_td(veh = veh[1, ],
                                 lkm = c(lkm,lkm),
                                 ef = EmissionFactors(efm),
                                 pro_month = rbind(veh_month, veh_month),
                                 verbose = TRUE)$emissions[1]),
               ".?")

  expect_error(emis_hot_td(veh = veh,
                           lkm = lkm,
                           pro_month = veh_month,
                           ef = efs,
                           fortran = T,
                           verbose = TRUE),
               ".?")

  expect_equal(round(emis_hot_td(veh = veh,
                                 lkm = lkm,
                                 ef = efs2,
                                 pro_month = matrix(as.numeric(veh_month),
                                                    nrow = nrow(veh),
                                                    ncol = 12,
                                                    byrow = TRUE),
                                 verbose = TRUE)$emissions[1]),
               Emissions(111))

  expect_equal(round(emis_hot_td(veh = veh,
                                 lkm = lkm,
                                 ef = efs2,
                                 pro_month = matrix(as.numeric(veh_month),
                                                    nrow = nrow(veh),
                                                    ncol = 12,
                                                    byrow = TRUE),
                                 fortran = T,
                                 verbose = TRUE)$emissions[1]),
               Emissions(111))
  expect_equal(round(emis_hot_td(veh = veh,
                                 lkm = lkm,
                                 ef = efs2,
                                 pro_month = matrix(as.numeric(veh_month),
                                                    nrow = nrow(veh),
                                                    ncol = 12,
                                                    byrow = TRUE),
                                 fortran = T,
                                 verbose = TRUE,
                                 nt = 1)$emissions[1]),
               Emissions(111))

  expect_error(round(emis_hot_td(veh = veh,
                                 lkm = lkm,
                                 ef = efs2,
                                 pro_month = matrix(as.numeric(veh_month),
                                                    nrow = nrow(veh),
                                                    ncol = 12,
                                                    byrow = TRUE),
                                 fortran = T,
                                 verbose = TRUE,
                                 nt = 1000)$emissions[1]),
               ".?")


    expect_equal(round(emis_hot_td(veh = veh,
                                 lkm = lkm,
                                 ef = efs2,
                                 pro_month = as.numeric(veh_month),
                                 fortran = T,
                                 verbose = TRUE,
                                 nt = 1)$emissions[1]),
               Emissions(111))

    expect_equal(round(emis_hot_td(veh = veh,
                                   lkm = lkm,
                                   ef = efs2,
                                   pro_month = as.numeric(veh_month),
                                   fortran = T,
                                   verbose = TRUE,
                                   nt = 1)$emissions[1]),
                 Emissions(111))

    expect_equal(round(emis_hot_td(veh = veh[1, ],
                                 lkm = lkm,
                                 ef = efs2[1, ],
                                 pro_month = veh_month,
                                 fortran = T,
                                 verbose = TRUE)$emissions[1]),
               Emissions(111))

    expect_equal(round(emis_hot_td(veh = veh[1, ],
                                   lkm = lkm,
                                   ef = efs2[1, ],
                                   pro_month = veh_month,
                                   fortran = T,
                                   verbose = TRUE,
                                   nt = 1)$emissions[1]),
                 Emissions(111))

    expect_error(round(emis_hot_td(veh = veh[1, ],
                                   lkm = lkm,
                                   ef = efs2,
                                   pro_month = veh_month,
                                   verbose = TRUE)$emissions[1]),
                 ".?")

        expect_message(round(emis_hot_td(veh = veh,
                                 lkm = lkm,
                                 ef = efs2,
                                 pro_month = matrix(as.numeric(veh_month),
                                                    nrow = nrow(veh),
                                                    ncol = 12,
                                                    byrow = TRUE),
                                 fortran = T,
                                 verbose = TRUE)$emissions[1]),
               ".?")
})

