context("age")

vehLIA <- rep(1, 25)

test_that("age works", {
  expect_equal(round(age(x = vehLIA, type = "gompertz")[1]),
               Vehicles(1))
  expect_message(age(x = vehLIA, type = "gompertz", verbose = TRUE),
                "N.?")
  expect_equal(round(age(x = vehLIA, type = "double_logistic")[1]),
               Vehicles(1))
  expect_message(age(x = vehLIA, type = "double_logistic", verbose = TRUE),
                 "N.?")
  expect_equal(round(age(x = vehLIA, type = "weibull")[1]),
               Vehicles(1))
  expect_message(age(x = vehLIA, type = "weibull", verbose = TRUE),
                 "N.?")
  expect_equal(round(age(x = vehLIA, type = "weibull2")[1]),
               Vehicles(1))
  expect_message(age(x = vehLIA, type = "weibull2", verbose = TRUE),
                 "N.?")

})


