context("test-celsius")

test_that("celsius works", {
  expect_equal(as.numeric(celsius(1)),1)
})

test_that("celsius works", {
  expect_equal(celsius(matrix(1, ncol = 10))$V1[1],
               1*units::as_units("degC"))
})

test_that("celsius works", {
  expect_equal(celsius(as.data.frame(matrix(1, ncol = 10)))$V1[1],
  1*units::as_units("degC"))
})
