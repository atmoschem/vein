test_that("the packaged EPA reference covers all on-road vehicle types", {
  x <- moves_factors()
  v <- moves_vehicles(x)
  r <- x$rates
  expect_equal(nrow(r), 86304)
  expect_equal(nrow(v), 38)
  expect_equal(sort(unique(v$sourceTypeID)),
               c(11L, 21L, 31L, 32L, 41L, 42L, 43L, 51L, 52L, 53L, 54L, 61L, 62L))
  expect_equal(sort(unique(v$fuelTypeID)), c(1L, 2L, 3L, 5L, 9L))
  expect_true(all(c("Motorcycle", "Passenger Car", "Passenger Truck",
                    "Light Commercial Truck", "Other Buses", "Transit Bus",
                    "School Bus", "Refuse Truck", "Single Unit Short-haul Truck",
                    "Single Unit Long-haul Truck", "Motor Home",
                    "Combination Short-haul Truck",
                    "Combination Long-haul Truck") %in% v$sourceTypeName))
  expect_equal(sort(unique(r$pollutantID)), c(2L, 3L))
  expect_equal(sort(unique(r$temperature_C)), c(10, 20, 30))
  expect_equal(unique(r$locationID), "zone:261610")
  expect_true(all(table(r$pollutantID) > 0))
  expect_true(all(is.finite(r$gkm)) && all(r$gkm >= 0))
  expect_equal(unique(x$runs$masterVersion), "MOVES6.0.0")
  expect_equal(unique(x$runs$defaultDatabaseUsed), "movesdb20260729")
  expect_equal(x$runs$expectedDONEFiles, x$runs$retrievedDONEFiles)
  # Every listed combination must have usable rates in at least one model year.
  expect_equal(nrow(unique(r[c("sourceTypeID", "fuelTypeID")])), nrow(v))
})

test_that("vehicle and fuel names select the same factors as numeric IDs", {
  a <- ef_moves("NOx", 2020, speed = c(20, 40, 60),
                source_type_id = 62, fuel_type_id = 2,
                select = list(hourID = 1))
  b <- ef_moves("NOx", 2020, speed = c(20, 40, 60),
                source_type_id = "Combination Long-haul Truck",
                fuel_type_id = "Diesel Fuel", select = list(hourID = 1))
  c <- ef_moves("NOx", 2020, speed = c(20, 40, 60),
                source_type_id = "combination long-haul truck",
                fuel_type_id = "diesel fuel", select = list(hourID = 1))
  expect_equal(a, b)
  expect_equal(a, c)
  expect_true(all(as.numeric(a) > 0))
})

test_that("all exported reference factors have the expected speed coverage", {
  x <- moves_factors()
  r <- x$rates
  groups <- unique(r[c("sourceTypeID", "fuelTypeID", "pollutantID",
                       "modelYearID", "hourID")])
  expect_equal(nrow(groups) * 16L, nrow(r))
  expect_true(all(table(paste(r$sourceTypeID, r$fuelTypeID,
                              r$pollutantID, r$modelYearID,
                              r$hourID)) == 16L))
})

test_that("both R evaluators reproduce exported EPA rates for every combination", {
  x <- moves_factors()
  v <- moves_vehicles(x)
  bins <- utils::read.csv(system.file("extdata", "moves", "avgspeedbin.csv", package = "vein"))
  nominal <- bins$avgBinSpeed
  within <- nominal
  within[1] <- 1.25
  for (i in seq_len(nrow(v))) {
    source <- v$sourceTypeID[i]
    fuel <- v$fuelTypeID[i]
    raw <- x$rates[x$rates$sourceTypeID == source & x$rates$fuelTypeID == fuel, ]
    year <- max(raw$modelYearID)
    for (p in c(2L, 3L)) {
      group <- raw[raw$pollutantID == p & raw$modelYearID == year, ]
      hour <- group$hourID[1]
      expected <- group$ratePerDistance[match(bins$avgSpeedBinID, group$avgSpeedBinID)] / 1.609344
      f <- ef_moves(p, year, factors = x,
                    source_type_id = source, fuel_type_id = fuel,
                    select = list(hourID = hour))
      linear <- ef_moves(p, year, factors = x,
                         source_type_id = source, fuel_type_id = fuel,
                         select = list(hourID = hour), method = "linear")
      expect_equal(f(within * 1.609344), expected, tolerance = 1e-12,
                   info = paste(source, fuel, p, year, hour))
      expect_equal(linear(nominal * 1.609344), expected, tolerance = 1e-12,
                   info = paste(source, fuel, p, year, hour))
    }
  }
})

test_that("packaged start and evaporative data cover all on-road types", {
  starts <- moves_start_factors()
  evap <- moves_evap_factors()
  expect_equal(nrow(starts$rates), 5394)
  expect_equal(nrow(evap$rates), 3186)
  expect_equal(sort(unique(starts$rates$sourceTypeID)),
               c(11L, 21L, 31L, 32L, 41L, 42L, 43L, 51L, 52L, 53L, 54L, 61L, 62L))
  expect_equal(sort(unique(starts$rates$pollutantID)), c(2L, 3L))
  expect_equal(sort(unique(starts$rates$fuelTypeID)), c(1L, 2L, 3L, 5L, 9L))
  expect_equal(sort(unique(evap$rates$processID)), c(11L, 12L, 13L))
  expect_equal(sort(unique(evap$rates$fuelTypeID)), c(1L, 5L))
  expect_true(all(evap$rates$temperatureProfileID[evap$rates$processID == 12L] > 0))
  expect_true(all(is.na(evap$rates$temperatureProfileID[evap$rates$processID != 12L])))
  expect_equal(unique(starts$runs$masterVersion), "MOVES6.0.0")
  expect_equal(unique(evap$runs$masterVersion), "MOVES6.0.0")
})

test_that("frozen start and evaporative values remain reproducible", {
  start <- ef_moves_start("NOx", 2020, temperature = 20,
                          source_type_id = "Passenger Car",
                          fuel_type_id = "Gasoline", select = list(hourID = 1))
  expect_equal(start$gperstart, 0.12535, tolerance = 1e-12)
  start_co <- ef_moves_start("CO", 2020, temperature = 20,
                             source_type_id = "Passenger Car",
                             fuel_type_id = "Gasoline", select = list(hourID = 1))
  expect_equal(start_co$gperstart, 2.16096, tolerance = 1e-12)
  evap <- ef_moves_evap("THC", 2020, temperature = 20,
                        source_type_id = "Passenger Car",
                        fuel_type_id = "Gasoline", select = list(hourID = 1))
  expect_equal(evap$gpervehicle[evap$processID == 11L], 0.00734352, tolerance = 1e-12)
  expect_equal(evap$gpervehicle[evap$processID == 13L], 0.00728413, tolerance = 1e-12)
  expect_equal(evap$gpervehicle[evap$processID == 12L], 0.00264464, tolerance = 1e-12)
  expect_equal(evap$temperatureProfileID[evap$processID == 12L], 2616100100)
})

test_that("frozen reference values remain reproducible", {
  # Independent values recorded from raw CSV (g/mile), gasoline passenger car,
  # model year 2020, 20 C, RH 50%, speed bins 1, 3, 6, 11 and 16.
  raw <- c(0.0224853, 0.018784, 0.0173561, 0.0151238, 0.0195563)
  speed <- c(1.25, 10, 25, 50, 75) * 1.609344
  result <- ef_moves("NOx", 2020, speed = speed, temperature = 20, rel_humidity = 50)
  expect_equal(as.numeric(result), raw / 1.609344, tolerance = 1e-12)
  expect_error(ef_moves("NOx", 2020, speed = 50), "ambiguous")
  expect_error(ef_moves("NOx", 2020, speed = 50, temperature = 25), "No MOVES rates")
})
