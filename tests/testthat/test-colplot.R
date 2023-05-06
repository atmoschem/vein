context("colplot")

a <-   colplot(
  EmissionFactors(data.frame(a = 1:5,
                             b = 1:5))
)
a
test_that("colplot works", {
  expect_equal(
    round(a$rect$w), 0)
})

test_that("colplot works", {
  expect_equal(
    round(colplot(
      EmissionFactors(data.frame(a = 1:5))
    )$rect$w, 0), 0)
})

