context("test-dmonth")

test_that("dmonth works", {
  expect_equal(dmonth(2022,5), 31)
})

test_that("dmonth error", {
  expect_error(dmonth(2022,"l"), ".")
})

