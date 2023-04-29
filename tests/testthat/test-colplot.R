context("colplot")

test_that("colplot works", {
  expect_equal(
    colplot(
      EmissionFactors(data.frame(a = 1:5,
                                 b = 1:5))
      )$rect$w, NULL)
})

test_that("colplot works", {
  expect_equal(
    colplot(
      EmissionFactors(data.frame(a = 1:5))
    )$rect$w, NULL)
})

