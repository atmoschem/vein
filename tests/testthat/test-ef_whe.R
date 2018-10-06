context("ef_whe")


test_that("ef_whe works", {
  expect_equal(as.numeric(ef_whe(10, 0.5, 1)),
               5.5)
})
