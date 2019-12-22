context("ef_ldv_cold")

test_that("ef_ldv_cold eu is not data.frame", {
  expect_equal(ef_ldv_cold(ta = 15,
                           cc = "<=1400",
                           f = "G",
                           eu = "I",
                           p = "CO")(10),
               2.754)
  expect_equal(ef_ldv_cold(ta = 15,
                           cc = "<=1400",
                           f = "G",
                           eu = "I",
                           p = "CO", speed = Speed(0)),
               EmissionFactors(1.974))
  expect_error(ef_ldv_cold(ta = data.frame(15),
                           cc = "<=1400",
                           f = "G",
                           eu = "I",
                           p = "CO"),
              "w.?")

  expect_output(ef_ldv_cold(ta = 15,
                           cc = "<=1400",
                           f = "G",
                           eu = "I",
                           p = "CO",
                           show.equation = TRUE)(10),
               "a.?")

  expect_output(ef_ldv_cold(ta = 15,
                            cc = "<=1400",
                            f = "G",
                            eu = "I",
                            p = "CO",
                            show.equation = TRUE)(10),
                "E.?")

  expect_equal(ef_ldv_cold(ta = data.frame(15),
                           cc = "<=1400",
                           f = "G",
                           eu = "I",
                           p = "CO",
                           speed = Speed(10))$I_1,
               EmissionFactors(2.754))
  expect_equal(ef_ldv_cold(ta = data.frame(15),
                           cc = "<=1400",
                           f = "G",
                           eu = c("I", "II"),
                           p = "CO",
                           speed = Speed(10))$I_1,
               EmissionFactors(2.754))
})

test_that("ef_ldv_cold eu is data.frame", {
  expect_equal(ef_ldv_cold(ta = 15,
                           cc = "<=1400",
                           f = "G",
                           eu = data.frame("I"),
                           p = "CO",
                           speed = Speed(10))$V1,
               EmissionFactors(2.754))


  expect_error(ef_ldv_cold(ta = 15,
                           cc = "<=1400",
                           f = "G",
                           eu = data.frame("I"),
                           p = "CO",
                           speed = 10),
               "s.?")

  expect_error(ef_ldv_cold(ta = 15,
                           cc = "<=1400",
                           f = "G",
                           eu = data.frame("I"),
                           p = "CO",
                           speed = units::set_units(10, "km/g")),
               "U.?")


  expect_equal(ef_ldv_cold(ta = data.frame(15),
                           cc = "<=1400",
                           f = "G",
                           eu = data.frame("I"),
                           p = "CO",
                           speed = Speed(10))$V1,
               EmissionFactors(2.754))
  expect_equal(ef_ldv_cold(ta = matrix(15),
                           cc = "<=1400",
                           f = "G",
                           eu = data.frame("I"),
                           p = "CO",
                           speed = Speed(10))$V1,
               EmissionFactors(2.754))
})

test_that("ef_ldv_cold is.numeric(ta) & length(ta) == 1 & length(eu) > 1", {
  expect_equal(ef_ldv_cold(ta = 15,
                           cc = "<=1400",
                           f = "G",
                           eu = c("I", "II"),
                           p = "CO",
                           speed = Speed(10))$I,
               EmissionFactors(2.754))

  expect_error(ef_ldv_cold(ta = 15,
                           cc = "<=1400",
                           f = "G",
                           eu = c("I", "II"),
                           p = "CO",
                           speed = 10),
               "s.?")

  expect_error(ef_ldv_cold(ta = 15,
                           cc = "<=1400",
                           f = "G",
                           eu = c("I", "II"),
                           p = "CO",
                           speed = units::set_units(10, "km/g")),
               "U.?")


  expect_equal(ef_ldv_cold(ta = data.frame(15),
                           cc = "<=1400",
                           f = "G",
                           eu = c("I", "II"),
                           p = "CO",
                           speed = Speed(10))$I_1,
               EmissionFactors(2.754))
  expect_equal(ef_ldv_cold(ta = matrix(15),
                           cc = "<=1400",
                           f = "G",
                           eu = c("I", "II"),
                           p = "CO",
                           speed = Speed(10))$I_1,
               EmissionFactors(2.754))
  expect_error(ef_ldv_cold(ta = data.frame(15),
                           cc = "<=1400",
                           f = "G",
                           eu = c("I", "II"),
                           p = "CO"),
               ".?")
  expect_error(ef_ldv_cold(ta = data.frame(a = celsius(1:10)),
                           cc = "<=1400",
                           f = "G",
                           eu = data.frame(a = "I", c = "II"),
                           p = "CO"),
               ".?")
})

