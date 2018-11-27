context("test-fuel_corr")


test_that("fuel_corr works", {
  expect_equal(fuel_corr(euro = "I")$LDVG$CO[[1]],
               1)
})
