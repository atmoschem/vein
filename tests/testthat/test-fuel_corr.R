context("test-fuel_corr")


test_that("fuel_corr works", {
  expect_equal(fuel_corr(euro = "PRE")$LDVG$CO[[1]],
               1)
})

test_that("fuel_corr works", {
  expect_equal(fuel_corr(euro = "I")$LDVG$CO[[1]],
               1)
})

test_that("fuel_corr works", {
  expect_equal(fuel_corr(euro = "II")$LDVG$CO[[1]],
               1)
})

test_that("fuel_corr works", {
  expect_equal(fuel_corr(euro = "III")$LDVG$CO[[1]],
               1)
})

test_that("fuel_corr works", {
  expect_equal(fuel_corr(euro = "IV")$LDVG$CO[[1]],
               1)
})

test_that("fuel_corr works", {
  expect_equal(fuel_corr(euro = "V")$LDVG$CO[[1]],
               1)
})

test_that("fuel_corr works", {
  expect_equal(fuel_corr(euro = "VI")$LDVG$CO[[1]],
               1)
})

test_that("fuel_corr works", {
  expect_equal(fuel_corr(euro = "VIc")$LDVG$CO[[1]],
               1)
})


test_that("fuel_corr works", {
  expect_equal(fuel_corr(euro = c("PRE", "I"))$value[1],
               1)
})
