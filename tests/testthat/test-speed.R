context("test-speed")

test_that("Speed works", {
  expect_equal(Speed(1), 1*units::as_units("km h-1"))
})

test_that("Speed works", {
  expect_equal(Speed(matrix(1))$V1,
               1*units::as_units("km h-1"))
})

test_that("Speed works", {
  expect_error(Speed(list(1)),
               "L.?")
})

test_that("Speed works", {
  expect_output(print(Speed(matrix(1, ncol = 11, nrow = 11))),
               ".?.")
  expect_output(print(Speed(matrix(1, ncol = 11, nrow = 5))),
                ".?.")
  expect_output(print(Speed(matrix(1, ncol = 5, nrow = 11))),
                ".?.")
  expect_output(print(Speed(matrix(1, ncol = 5, nrow = 5))),
                ".?.")
  expect_equal(summary(Speed(matrix(1, ncol = 11, nrow = 11)))[[1]],
                1)
  expect_output(summary(Speed(matrix(1, ncol = 11, nrow = 11)))[[1]],
               "S.?")
  # expect_equal(plot(Speed(matrix(1, ncol = 11, nrow = 11))),
                # NULL)
})

