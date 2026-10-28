context("EmissionFactorsList fixes")

test_that("data.frame input gives a list, not a data.frame", {
  x <- EmissionFactorsList(data.frame(a = c(1, 2), b = c(3, 4)))
  expect_false(is.data.frame(x))
  expect_true(inherits(x, "EmissionFactorsList"))
  expect_equal(length(x), 2)
  expect_equal(x[[2]][[1]](30), 3)
})
