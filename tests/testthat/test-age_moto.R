context("age_moto")

test_that("age_moto works", {
  expect_equal(age_moto(x = 10, name = "MC", agemax = 2),
               Vehicles(data.frame(MC_1 = 5,
                                   MC_2 = 5)))
})
