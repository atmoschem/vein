context("running_losses")


test_that("running_losses works", {
  expect_warning(running_losses(1),
               ".?")
})

