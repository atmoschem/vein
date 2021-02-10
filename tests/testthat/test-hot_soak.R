context("hot_soak")


test_that("hot_soak works", {
  expect_warning(hot_soak(1),
                 "'hot_soak' is deprecated.?")
})

