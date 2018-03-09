context("age_ldv")

test_that("age_ldv works", {
  expect_equal(age_ldv(x = 10, name = "PC", agemax = 2),
               Vehicles(data.frame(PC_1 = 5,
                                   PC_2 = 5)))
})
