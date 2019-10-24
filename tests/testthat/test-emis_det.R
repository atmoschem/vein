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
  expect_error(emis_det(po = "CO",
                        cc = "<=1400",
                        eu = "III",
                        km = units::set_units(1:11, "m")),
               ".?")
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

  expect_error(emis_det(po = "CO",
                              cc = "<=1400",
                              eu = rep("III", 2),
                              km = km[1]),
               ".?")
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
