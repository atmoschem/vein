context("colplot")

test_that("colplot works", {
  expect_equal(
    round(colplot(
      EmissionFactors(
        data.frame(a = 1:5)
        )
      )$rect$w), 0)
})

