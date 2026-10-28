context("temp_fact and ef_fun fixes")

test_that("temp_fact without q gives its own message", {
  expect_error(temp_fact(pro = 1), "No traffic data")
})

test_that("ef_fun default midpoint is the mean age", {
  ef <- c(rep(0.5, 20), rep(1, 20))
  a <- ef_fun(ef = ef, verbose = FALSE)
  b <- ef_fun(ef = ef, x0 = mean(1:40), verbose = FALSE)
  expect_equal(a, b)
  # with the old default (x0 = mean(ef) = 0.75) age 10 was already at 91% of L
  expect_lt(as.numeric(a[10]), 0.5)
})
