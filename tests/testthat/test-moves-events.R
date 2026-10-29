event_fixture <- function(table = "start") {
  rates <- expand.grid(modelYearID = c(2010L, 2020L), hourID = 1:2,
                       temperature = c(50, 68), sourceTypeID = 21L,
                       fuelTypeID = 1L, KEEP.OUT.ATTRS = FALSE)
  rates$MOVESRunID <- 1L
  rates$MOVESScenarioID <- "events"
  rates$yearID <- 2025L
  rates$pollutantID <- if (table == "start") 3L else 1L
  rates$processID <- if (table == "start") 2L else 11L
  rates$relHumidity <- 50
  rates$ratePerStart <- if (table == "start") rates$modelYearID / 1000 else NULL
  rates$ratePerVehicle <- if (table == "start") NULL else rates$modelYearID / 1000
  runs <- data.frame(MOVESRunID = 1L, massUnits = "g", distanceUnits = "mi",
                     masterVersion = "MOVES6.0.0", defaultDatabaseUsed = "fixture",
                     stringsAsFactors = FALSE)
  vehicles <- data.frame(sourceTypeID = 21L, sourceTypeName = "Passenger Car",
                         fuelTypeID = 1L, fuelTypeDesc = "Gasoline",
                         stringsAsFactors = FALSE)
  list(rates = rates, runs = runs, vehicles = vehicles)
}

test_that("start factors are selected in g/start without speed", {
  a <- event_fixture("start")
  d <- moves_start_factors(a$rates, a$runs, a$vehicles)
  expect_identical(d$basis, "start")
  expect_true("gperstart" %in% names(d$rates))
  out <- ef_moves_start("NOx", 2020, factors = d,
                        temperature = 20, select = list(hourID = 1))
  expect_equal(out$gperstart, 2.02)
  expect_identical(attr(out, "units"), "g/start")
  expect_equal(nrow(ef_moves_start("NOx", 2020, factors = d)), 4)
  expect_error(ef_moves_start("CO", 2020, factors = d), "No MOVES start")
  expect_error(ef_moves("NOx", 2020, speed = 50, factors = d), "moves_factors")
})

test_that("evaporative factors combine permeation/leaks and venting profiles", {
  a <- event_fixture("evap")
  base <- a$rates
  base$processID <- 11L
  leaks <- base
  leaks$processID <- 13L
  leaks$ratePerVehicle <- leaks$ratePerVehicle * 2
  profile <- base[base$hourID == 1, ]
  profile$processID <- 12L
  profile$temperatureProfileID <- 7L
  d <- moves_evap_factors(rbind(base, leaks), profile, a$runs, a$vehicles)
  expect_identical(d$basis, "evap")
  expect_equal(sort(unique(d$rates$processID)), c(11L, 12L, 13L))
  out <- ef_moves_evap("THC", 2020, factors = d, temperature = 20)
  expect_equal(sort(unique(out$processID)), c(11L, 12L, 13L))
  expect_equal(attr(out, "units"), "g/vehicle/h")
  expect_equal(out$gpervehicle[out$processID == 13L],
               2 * out$gpervehicle[out$processID == 11L])
  expect_equal(unique(out$basis[out$processID == 12L]), "profile")
  only <- ef_moves_evap("THC", 2020, process = "leaks", factors = d)
  expect_equal(unique(only$processID), 13L)
  expect_error(ef_moves_evap("THC", 2020, process = "boil", factors = d), "arg")
  expect_error(ef_moves("THC", 2020, speed = 50, factors = d), "moves_factors")
})
