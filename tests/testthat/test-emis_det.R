context("emis_det")

data(fkm)
pckm <- fkm[[1]](1:24); pckma <- cumsum(pckm)
km <- units::set_units(cumsum(pckma[1:11]), km)

# caso simple ####
test_that("emis_det works", {
  expect_equal(emis_det(po = "CO",
                        cc = "<=1400",
                        eu = "III",
                        km = km)[10],
               1.91)
  expect_error(emis_det(po = "CO",
                        cc = "<=1400",
                        eu = "III",
                        km = 1)[10],
               ".?")
  # mileage in m is converted to km
  expect_equal(emis_det(po = "CO",
                        cc = "<=1400",
                        eu = "III",
                        km = units::set_units(as.numeric(km) * 1000, "m")),
               emis_det(po = "CO",
                        cc = "<=1400",
                        eu = "III",
                        km = km))
  expect_error(emis_det(po = "CO",
                        cc = "<=1400",
                        eu = "III", speed = 10,
                        km = units::set_units(1:11, "m")),
               ".?")
  expect_error(emis_det(po = "CO",
                        cc = "<=1400",
                        eu = "III", speed = units::set_units(10, "g"),
                        km = units::set_units(1:11, "m")),
               ".?")
  expect_message(emis_det(po = "CO",
                        cc = "<=1400",
                        eu = "III",
                        verbose = T,
                        km = km)[10],
               "A.?")
  expect_equal(round(emis_det(po = "CO",
                        cc = "<=1400",
                        eu = "III",
                        km = km[1])),
               1)

  # one mileage for several euro standards is recycled
  expect_equal(length(emis_det(po = "CO",
                               cc = "<=1400",
                               eu = rep("III", 2),
                               km = km[1])),
               2)
  expect_equal(emis_det(po = "CO",
                        cc = "<=1400",
                        eu = rep("III", 2),
                        km = km[1:2],
                        speed = Speed(1:2))$km1[1],
               1)
})

# caso data.frame euro ####
dfeu <- data.frame(matrix("III", nrow = 11))
test_that("emis_det works", {
  expect_error(emis_det(po = "CO",
                        cc = "<=1400",
                        eu = dfeu,
                        km = km)[10],
               "L.?")

  expect_equal(emis_det(po = "CO",
                        cc = "<=1400",
                        eu = dfeu,
                        km = km[1])[1,1],
               1)

  expect_message(emis_det(po = "CO",
                        cc = "<=1400",
                        eu = dfeu,
                        km = km[1],
                        verbose = TRUE)[1,1],
               '.?')
})

test_that("emis_det handles VIc, later standards, numeric cc and clamps at 1", {
  e <- emis_det(po = "CO", cc = 1600, eu = c("VIc", "VId", "VII", "PRE"),
                km = km[c(1, 1, 1, 11)])
  expect_true(all(is.finite(e)))
  expect_true(all(e >= 1))
  expect_equal(e[4], 1)
  expect_equal(emis_det(po = "CO", cc = 2000, eu = "III", km = km[11]),
               emis_det(po = "CO", cc = "1400_2000", eu = "III", km = km[11]))
})

test_that("deterioration is applied per age column with sweep", {
  eu <- c("IV", "IV", "III", "III", "II")
  kms <- km[c(1, 3, 5, 8, 11)]
  det <- emis_det(po = "CO", cc = "<=1400", eu = eu, km = kms)
  veh <- data.frame(matrix(1:15, nrow = 3))
  vd <- sweep(veh, 2, det, "*")
  for (j in 1:5) expect_equal(vd[[j]], veh[[j]] * det[j])
})
