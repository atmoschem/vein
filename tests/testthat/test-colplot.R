context("colplot")

test_that("colplot works", {
  expect_equal(
    colplot(
      EmissionFactors(
        data.frame(a = 1:5)
        )
      )$mfrow[1], 1)
  expect_equal(
    colplot(
      EmissionFactors(
        data.frame(a = 1:5,
                   b = 1:5)
        )
      )$mfrow[1], 1)
  expect_equal(
    colplot(
      EmissionFactors(data.frame(a = 1:5)
                      ),
      theme = "clean")$mfrow[1], 1)
  expect_equal(
    colplot(
      EmissionFactors(data.frame(a = 1:5,
                                 b = 1:5)
                      ),
      theme = "clean")$mfrow[1], 1)
})

