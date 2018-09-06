context("ef_ive")

test_that("ef_ive works", {
  expect_equal(as.numeric(ef_ive(mileage = 10, pol = "CO_gkm")),
               22.255)
})
