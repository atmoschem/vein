context("units fixes")

data(net)
nn <- sf::st_as_sf(net)[1:3, ]
lm <- sf::st_length(nn)

test_that("add_lkm and add_miles convert units objects", {
  expect_equal(add_lkm(lm), units::set_units(lm, "km"))
  expect_equal(add_lkm(data.frame(a = lm))$a, units::set_units(lm, "km"))
  expect_equal(as.numeric(add_miles(units::set_units(1.609344, "km"))), 1)
  expect_error(add_lkm("a"), "x")
})

test_that("celsius converts units objects", {
  expect_equal(as.numeric(celsius(units::set_units(273.15, "K"))), 0)
  expect_error(celsius("a"), "x")
})

test_that("cold_mileage converts temperature and length units", {
  a <- cold_mileage(units::set_units(5, "km"), celsius(c(0, 10)))
  expect_equal(a, c(0.52015, 0.44200))
  expect_equal(cold_mileage(units::set_units(5, "km"),
                            units::set_units(c(273.15, 283.15), "K")), a)
  expect_equal(cold_mileage(units::set_units(5000, "m"), celsius(c(0, 10))), a)
  b <- cold_mileage(units::set_units(5, "km"), celsius(matrix(c(0, 10), nrow = 1)))
  expect_equal(dim(b), c(1, 2))
  expect_equal(unname(unlist(b)), a)
})

test_that("vkm converts lkm units and supports one street", {
  expect_equal(vkm(veh = c(1, 1, 1), lkm = lm, profile = matrix(1, 1, 1))$h1,
               as.numeric(units::set_units(lm, "km")))
  expect_equal(dim(vkm(veh = 10, lkm = 1, profile = matrix(1, 24, 7))),
               c(1, 168))
  expect_equal(dim(vkm(veh = 10, lkm = 1, profile = matrix(1, 24, 7),
                       as_df = FALSE)),
               c(1, 24, 7))
  expect_equal(dim(vkm(veh = 1:3, lkm = 1, profile = matrix(1, 24, 7),
                       as_df = FALSE)),
               c(3, 24, 7))
})

test_that("Vehicles with time", {
  expect_equal(as.character(units(Vehicles(data.frame(a = 1), time = "1/h")$a)),
               "veh/h")
  expect_equal(as.character(units(Vehicles(matrix(1), time = "1/h")$V1)),
               "veh/h")
  expect_error(Vehicles(TRUE), "class")
  expect_error(Vehicles(units::set_units(1, "veh*veh")), "veh")
  expect_output(print(Vehicles(data.frame(a = numeric(0)))), "a")
})

test_that("Speed converts units and prints", {
  expect_equal(as.numeric(suppressMessages(Speed(units::set_units(10, "m/s")))), 36)
  expect_output(print(Speed(matrix(1, nrow = 3, ncol = 10))), "V10")
  expect_false(any(grepl("NA", capture.output(print(Speed(matrix(1, nrow = 3, ncol = 10)))))))
})

test_that("remove_units drops units from arrays", {
  a <- array(1:8, c(2, 2, 2)) * units::as_units("g")
  r <- remove_units(a)
  expect_false(inherits(r, "units"))
  expect_equal(dim(r), c(2, 2, 2))
})
