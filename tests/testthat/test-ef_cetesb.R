context("ef_cetesb")

test_that("ef_cetesb works", {
  expect_equal(as.numeric(ef_cetesb("CO", "PC_G")[1]), 0.141)
})
