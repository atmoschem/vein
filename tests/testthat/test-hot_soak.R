context("test-hot_soak")


test_that("hot_soak works", {
  expect_equal(hot_soak(x = 1:10, carb = 0, p = 1, eshot = 1, eswarmc =1,
                        eshotfi = 1)[1], Emissions(1))
})
