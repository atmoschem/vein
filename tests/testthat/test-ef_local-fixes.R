context("ef_local fixes")

ef <- data.frame(Age = 1:5, Year = 2017:2013, Pollutant = "CO",
                 PC_G = c(1, 2, 3, 4, 5))

test_that("project = 'constant' also works with full = TRUE", {
  v <- ef_local(p = "CO", veh = "PC_G", year = 2019, agemax = 5, ef = ef,
                verbose = FALSE)
  f <- ef_local(p = "CO", veh = "PC_G", year = 2019, agemax = 5, ef = ef,
                full = TRUE, verbose = FALSE)
  expect_equal(as.numeric(v), c(1, 1, 1, 2, 3))
  expect_equal(as.numeric(f$CO), as.numeric(v))
  expect_equal(f$Age, 1:5)
  expect_equal(f$Year, 2019:2015)
})

test_that("unknown pollutant gives a message", {
  expect_error(ef_local(p = "XX", veh = "PC_G", ef = ef, verbose = FALSE),
               "CO")
})
