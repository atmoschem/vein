context("ef_evap fixes")

test_that("kmday converts the diurnal factor (g/day) and returns g/km", {
  ed <- ef_evap(ef = "ed", v = "PC", cc = "<=1400", dt = "0_15", ca = "no")
  r <- ef_evap(ef = "ed", v = "PC", cc = "<=1400", dt = "0_15", ca = "no",
    kmday = units::set_units(30, "km"))
  expect_equal(as.numeric(r), as.numeric(ed) / 30)
  expect_equal(as.character(units(r)), "g/km")
  expect_true(is.data.frame(
    ef_evap(ef = "ed", v = "PC", cc = "<=1400", dt = "0_15", ca = "no",
      kmday = units::set_units(30, "km"), show = TRUE)))
  expect_error(ef_evap(ef = "eshotc", v = "PC", cc = "<=1400", dt = "0_15",
    ca = "no", kmday = units::set_units(30, "km")), "ed")
})

test_that("one-column data.frame dt returns only the selected pollutant", {
  r <- ef_evap(ef = "eshotc", v = "PC", cc = "<=1400",
    dt = data.frame(m1 = c(1, 20)), ca = "no")
  expect_equal(ncol(r), 1)
  expect_equal(as.numeric(r[, 1]),
    as.numeric(ef_evap(ef = "eshotc", v = "PC", cc = "<=1400",
      dt = c("-5_10", "10_25"), ca = "no")))
})

test_that("numeric temperatures go to the class with the nearest mid-point", {
  cls <- c("-5_10", "0_15", "10_25", "20_35")
  g <- sapply(cls, function(x) as.numeric(ef_evap(ef = "eshotc", v = "PC",
    cc = "<=1400", dt = x,
    ca = "no")))
  tt <- c(0, 4.9, 5, 8, 12.4, 12.5, 18, 22.4, 22.5, 30)
  ex <- c(1, 1, 2, 2, 2, 3, 3, 3, 4, 4)
  r <- as.numeric(ef_evap(ef = "eshotc", v = "PC", cc = "<=1400", dt = tt,
    ca = "no"))
  expect_equal(r, unname(g[ex]))
})
