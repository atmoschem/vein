context("ef_whe")


test_that("ef_whe works", {
  expect_equal(as.numeric(ef_whe(10, 0.5, 1)),
               5.5)
})

test_that("ef_whe stops", {
  expect_error(ef_whe(phe = 1, ef = 1),
               "No.?\\(?")
})

test_that("ef_whe stops", {
  expect_error(ef_whe(efhe = 1, ef = 1),
               "No.?\\(?")
})

test_that("ef_whe stops", {
  expect_error(ef_whe(efhe = 1, phe = 1),
               "No.?\\(?")
})
