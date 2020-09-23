context("colplot")

test_that("colplot works", {
  expect_equal(
    colplot(
      EmissionFactors(
        data.frame(a = 1:5)
        )
      )$mfrow[1], 1)
})

