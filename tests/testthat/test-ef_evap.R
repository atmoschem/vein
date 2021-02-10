context("ef_evap")

test_that("ef_evap works", {
  expect_equal(ef_evap(ef = "erhotc",
                       v = "PC",
                       cc = "<=1400",
                       dt = "0_15",
                       ca = "no",
                       show = FALSE),
               units::as_units(2.39, "g"))
  expect_error(ef_evap(ef = "erh",
                       v = "PC",
                       cc = "<=1400",
                       dt = "0_15",
                       ca = "no",
                       show = FALSE),
               ".?")
  expect_equal(ef_evap(ef = "erhotc",
                       v = "PC",
                       cc = "<=1400",
                       dt = "0_15",
                       ca = "no",
                       show = TRUE)$g,
               units::as_units(2.39, "g"))
  expect_error(ef_evap(ef = "erhotc",
                       v = "PC",
                       cc = "<=1400",
                       dt = "0_15",
                       ltrip = 1,
                       kmday = 1,
                       ca = "no"),
               "Y.?")
  expect_error(ef_evap(ef = "erhotc",
                       v = "PC",
                       cc = "<=1400",
                       dt = "0_15",
                       kmday = 1:2,
                       ca = "no"),
               ".?")
  expect_error(ef_evap(ef = "erhotc",
                       v = "PC",
                       cc = "<=1400",
                       dt = "0_15",
                       kmday = 1,
                       ca = "no"),
               ".?")
  expect_error(ef_evap(ef = "erhotc",
                       v = "PC",
                       cc = "<=1400",
                       dt = "0_15",
                       kmday = units::set_units(1, "m"),
                       ca = "no"),
               ".?")
  expect_equal(ef_evap(ef = "erhotc",
                       v = "PC",
                       cc = "<=1400",
                       dt = "0_15",
                       kmday = units::set_units(1, "km"),
                       ca = "no")$g,
               EmissionFactors(2.39))

  expect_equal(ef_evap(ef = "erhotc",
                       v = "PC",
                       cc = "<=1400",
                       dt = 30,
                       kmday = units::set_units(1, "km"),
                       ca = "no")$g,
               EmissionFactors(5.42))

  expect_equal(ef_evap(ef = c("erhotc", "erhotc"),
                       v = "PC",
                       cc = "<=1400",
                       dt = 30:31,
                       kmday = units::set_units(1, "km"),
                       ca = "no")$g[1],
               EmissionFactors(5.42))

  expect_equal(ef_evap(ef = c("erhotc", "erhotc"),
                       v = "PC",
                       cc = "<=1400",
                       dt = 30,
                       kmday = units::set_units(1, "km"),
                       ca = "no")$g[1],
               EmissionFactors(5.42))

  expect_equal(ef_evap(ef = c("erhotc", "erhotc"),
                       v = "PC",
                       cc = "<=1400",
                       dt = 30:31,
                       kmday = units::set_units(1, "km"),
                       ca = "no")$g[1],
               EmissionFactors(5.42))
  expect_equal(ef_evap(ef = c("erhotc", "erhotc"),
                       v = "PC",
                       cc = "<=1400",
                       dt = 30:31,
                       kmday = units::set_units(1, "km"),
                       ca = "no",
                       show = TRUE)$g[1],
               EmissionFactors(5.42))

  expect_error(ef_evap(ef = "ed",
                       v = "PC",
                       cc = "<=1400",
                       dt = factor(1),
                       ltrip = units::set_units(1, "km"),
                       ca = "no"),
               ".?")

  expect_error(ef_evap(ef = "erhotc",
                       v = "PC",
                       cc = "<=1400",
                       dt = "0_15",
                       ltrip = 1,
                       ca = "no"),
               "l.?")
  expect_error(ef_evap(ef = "ed",
                       v = "PC",
                       cc = "<=1400",
                       dt = "0_15",
                       ltrip = 1,
                       ca = "no"),
               "e.?")
  expect_equal(ef_evap(ef = "erhotc",
                       v = "PC",
                       cc = "<=1400",
                       dt = "0_15",
                       ltrip = units::set_units(1, "km"),
                       ca = "no"),
               units::as_units(2.39, "g/km"))
  expect_error(ef_evap(ef = "ed",
                       v = "PC",
                       cc = "<=1400",
                       dt = "0_15",
                       ltrip = 1:2,
                       ca = "no"),
               ".?")
  expect_error(ef_evap(ef = "ed",
                       v = "PC",
                       cc = "<=1400",
                       dt = "0_15",
                       ltrip = units::set_units(1, "m"),
                       ca = "no"),
               ".?")
})

dt <- matrix(rep(1:24,5), ncol = 12) # 12 months
test_that("ef_evap ta matrix", {
  expect_equal(ef_evap(ef = "erhotc",
                       v = "PC",
                       cc = "<=1400",
                       dt = dt,
                       ca = "no",
                       show = FALSE)$ef[1],
               units::as_units(1.67, "g"))
  expect_error(ef_evap(ef = "erhotc",
                       v = c("PC", "LCV"),
                       cc = "<=1400",
                       dt = dt,
                       ca = "no",
                       show = FALSE)$ef[1],
               "W.?")
  expect_equal(ef_evap(ef = "erhotc",
                       v = "PC",
                       cc = "<=1400",
                       dt = matrix(rep(1:24), ncol = 1),
                       ca = "no",
                       show = FALSE)$V1[1],
               units::as_units(1.67, "g"))
})
