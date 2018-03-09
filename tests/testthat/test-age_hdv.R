context("age_hdv")

test_that("age_hdv works", {
  expect_equal(age_hdv(x = 10, name = "LT_B5", agemax = 2),
               Vehicles(data.frame(LT_B5_1 = 5,
                                   LT_B5_2 = 5)))
})
