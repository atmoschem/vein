context("test-speed")

test_that("Speed works", {
  expect_equal(Speed(1), 1*units::as_units("km h-1"))
})
