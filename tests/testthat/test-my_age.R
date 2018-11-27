context("test-my_age")

test_that("my_age works", {
  expect_equal(my_age(x = 1, y = 1, name = "PC"),
               Vehicles(data.frame(PC_1 = 1)))
})
