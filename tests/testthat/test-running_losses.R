context("test-running_losses")

test_that("multiplication works", {
  expect_equal(running_losses(x = 1:10, carb = 0, p = 1, erhot = 1, erwarmc =1,
                              erhotfi = 1)[1], Evaporative(1))
})
