# Synthetic unit fixtures below are deliberately not scientific reference data.
portable_moves_fixture <- function() {
  rates <- expand.grid(avgSpeedBinID = 1:16, modelYearID = c(2010L, 2020L))
  rates$MOVESRunID <- 1L
  rates$MOVESScenarioID <- "test"
  rates$yearID <- 2025L
  rates$hourID <- 1L
  rates$pollutantID <- 3L
  rates$processID <- 1L
  rates$sourceTypeID <- 21L
  rates$fuelTypeID <- 1L
  rates$roadTypeID <- 5L
  rates$temperature <- 68
  rates$relHumidity <- 50
  # Make the values easy to verify independently: 1:16 g/km for 2020,
  # twice that for 2010, including a real zero at bin 1.
  rates$ratePerDistance <- (rates$avgSpeedBinID - 1) * 1.609344 *
    ifelse(rates$modelYearID == 2010L, 2, 1)
  runs <- data.frame(MOVESRunID = 1L, massUnits = "g", distanceUnits = "mi",
                     masterVersion = "MOVES6.0.0", defaultDatabaseUsed = "fixture",
                     stringsAsFactors = FALSE)
  list(rates = rates, runs = runs)
}

test_that("vehicle/fuel combinations are discoverable with and without names", {
  a <- portable_moves_fixture()
  d <- moves_factors(a$rates, a$runs)
  v <- moves_vehicles(d)
  expect_equal(nrow(v), 1)
  expect_identical(names(v), c("sourceTypeID", "sourceTypeName", "fuelTypeID", "fuelTypeDesc"))
  expect_true(is.na(v$sourceTypeName))
  named <- data.frame(sourceTypeID = 21L, sourceTypeName = "Passenger Car",
                      fuelTypeID = 1L, fuelTypeDesc = "Gasoline")
  d <- moves_factors(a$rates, a$runs, named)
  expect_equal(moves_vehicles(d), named)
  expect_equal(ef_moves("NOx", 2020, speed = 50, factors = d,
                        source_type_id = "passenger car",
                        fuel_type_id = "GASOLINE"),
               ef_moves("NOx", 2020, speed = 50, factors = d))
  expect_error(ef_moves("NOx", 2020, speed = 50, factors = d,
                        source_type_id = "Truck"), "Unknown source_type_id")
  expect_error(moves_factors(a$rates, a$runs, rbind(named, named[1, ])), "duplicate")
})

test_that("portable factors convert units per run and preserve their provenance", {
  a <- portable_moves_fixture()
  b <- a$rates
  b$MOVESRunID <- 2L
  b$ratePerDistance <- b$ratePerDistance / 1.609344 / 1000
  run2 <- a$runs
  run2$MOVESRunID <- 2L
  run2$massUnits <- "Kilograms"
  run2$distanceUnits <- "Kilometers"
  d <- moves_factors(rbind(a$rates, b), rbind(a$runs, run2))
  expect_equal(d$rates$gkm[1:32], d$rates$gkm[33:64])
  expect_equal(d$rates$temperature_C, rep(20, 64))
  expect_identical(d$runs, rbind(a$runs, run2))
  expect_equal(sum(d$rates$gkm == 0), 4)
  lower <- a$rates
  names(lower) <- tolower(names(lower))
  expect_equal(moves_factors(lower, a$runs)$rates$gkm, d$rates$gkm[1:32])
  expect_equal(a$rates, portable_moves_fixture()$rates)
})

test_that("native bin lookup is ordered, unit aware and keeps zeros and missing speeds", {
  a <- portable_moves_fixture()
  d <- moves_factors(a$rates, a$runs)
  mph <- units::set_units(c(50, 0.5, 2.5, 7.5, 72.5, 75, NA), "mile/h")
  ef <- ef_moves("NOx", 2020, speed = mph, factors = d)
  expect_equal(as.numeric(ef), c(10, 0, 1, 2, 15, 15, NA))
  expect_true(units::ud_are_convertible(units::deparse_unit(ef), "g/km"))
  f <- ef_moves(3, 2020, factors = d)
  expect_equal(f(c(50, 0.5, 2.5, 7.5, 72.5, 75, NA) * 1.609344), as.numeric(ef))
  expect_error(f(0), "positive")
  expect_error(f(-1), "positive")
  expect_error(f(121), "75 mph")
  expect_error(f(Inf), "positive")
  expect_equal(f(numeric()), numeric())
  expect_equal(f(NA), NA_real_)
  expect_equal(f(c(NA_real_, NA_real_)), c(NA_real_, NA_real_))
})

test_that("linear interpolation uses native neighbors without extrapolation", {
  a <- portable_moves_fixture()
  d <- moves_factors(a$rates, a$runs)
  f <- ef_moves("NOx", 2020, factors = d, method = "linear")
  expect_equal(f(c(2.5, 3.75, 5, 7.5, 75) * 1.609344), c(0, 0.5, 1, 1.5, 15))
  expect_error(f(1), "starts at 2.5")
  # Removing bin 3 must not allow interpolation over its gap.
  a$rates <- a$rates[a$rates$avgSpeedBinID != 3, ]
  d <- moves_factors(a$rates, a$runs)
  f <- ef_moves("NOx", 2020, factors = d, method = "linear")
  expect_equal(f(5 * 1.609344), 1)
  expect_equal(f(15 * 1.609344 - 1e-12), 3)
  expect_error(f(7.5 * 1.609344), "adjacent")
  expect_error(ef_moves("NOx", 2020, speed = 10 * 1.609344, factors = d), "Missing MOVES")
})

test_that("different hours and meteorology require an explicit selection", {
  a <- portable_moves_fixture()
  b <- a$rates
  b$hourID <- 2L
  b$temperature <- 86
  b$ratePerDistance <- b$ratePerDistance * 3
  d <- moves_factors(rbind(a$rates, b), a$runs)
  expect_error(ef_moves("NOx", 2020, factors = d), "ambiguous.*hourID")
  cold <- ef_moves("NOx", 2020, factors = d, temperature = 20)
  hot <- ef_moves("NOx", 2020, factors = d, select = list(hourID = 2))
  expect_equal(hot(50), cold(50) * 3)
  expect_error(ef_moves("NOx", 2020, factors = d, temperature = 25), "No MOVES rates")
  expect_error(ef_moves("NOx", 2020, factors = d, select = list(hourID = c(1, 2))), "one")
  expect_error(ef_moves("NOx", 2020, factors = d, select = list(misspelling = 1)), "Invalid selection")
  expect_error(ef_moves("NOx", 2020, factors = d, select = list(avgSpeedBinID = 1)), "Invalid selection")
  expect_error(ef_moves("NOx", 2020, factors = d, select = list(modelYearID = 2020)), "duplicates")
})

test_that("categorical selections never fill model years or mix runs", {
  a <- portable_moves_fixture()
  b <- a$rates
  b$MOVESRunID <- 2L
  run2 <- a$runs
  run2$MOVESRunID <- 2L
  d <- moves_factors(rbind(a$rates, b), rbind(a$runs, run2))
  expect_error(ef_moves("NOx", 2020, factors = d), "MOVESRunID")
  expect_error(ef_moves("NOx", 2019, factors = d), "No MOVES rates")
  expect_error(ef_moves("CO", 2020, factors = d), "No MOVES rates")
  expect_error(ef_moves("NOx", 2020, factors = d, fuel_type_id = 2), "No MOVES rates")
  expect_error(ef_moves("bad", 2020, factors = d), "Unknown pollutant")
  f <- ef_moves("NOx", 2020, factors = d, select = list(MOVESRunID = 2))
  expect_identical(attr(f, "moves_metadata")$runs$MOVESRunID, 2L)
})

test_that("virtual speed links preserve location and never combine different zones", {
  a <- portable_moves_fixture()
  a$runs$domain <- "DEFAULT"
  a$rates$linkID <- 261610500 + a$rates$avgSpeedBinID
  b <- a$rates
  b$linkID <- 170310500 + b$avgSpeedBinID
  d <- moves_factors(rbind(a$rates, b), a$runs)
  expect_error(ef_moves("NOx", 2020, factors = d), "ambiguous.*locationID")
  f <- ef_moves("NOx", 2020, factors = d, select = list(locationID = "zone:261610"))
  expect_equal(f(50 * 1.609344), 10)
  expect_equal(attr(f, "moves_metadata")$scenario$locationID, "zone:261610")
  b$linkID[1] <- 123456789
  expect_error(moves_factors(b, a$runs), "Virtual linkID")
  # Without a known Default/County domain, a real link must remain a dimension.
  a$runs$domain <- "PROJECT"
  d <- moves_factors(a$rates, a$runs)
  expect_error(ef_moves("NOx", 2020, factors = d), "locationID")
})

test_that("invalid units, metadata and non-distance processes fail before use", {
  a <- portable_moves_fixture()
  expect_error(moves_factors(a$rates), "both")
  bad <- a$runs
  bad$massUnits <- "Joules"
  expect_error(moves_factors(a$rates, bad), "Unknown MOVES mass")
  bad <- a$runs
  bad$MOVESRunID <- 2L
  expect_error(moves_factors(a$rates, bad), "Every rate")
  expect_error(moves_factors(a$rates, rbind(a$runs, a$runs)), "duplicate MOVESRunID")
  bad <- a$runs
  bad$masterVersion <- NA_character_
  expect_error(moves_factors(a$rates, bad), "masterVersion")
  for (value in list(NA_real_, Inf, -1)) {
    bad <- a$rates
    bad$ratePerDistance[1] <- value
    expect_error(moves_factors(bad, a$runs), "ratePerDistance")
  }
  bad <- a$rates
  bad$processID <- 2L
  expect_error(moves_factors(bad, a$runs), "running exhaust")
  bad <- a$rates
  bad$pollutantID <- 91L
  expect_error(moves_factors(bad, a$runs), "Energy")
  bad <- a$rates
  bad$avgSpeedBinID[1] <- 0L
  expect_error(moves_factors(bad, a$runs), "1:16")
  expect_error(moves_factors(rbind(a$rates, a$rates[1, ]), a$runs), "Duplicate rate keys")
})

test_that("CSV and RDS data plus speed closures need no database connection", {
  a <- portable_moves_fixture()
  f <- tempfile(fileext = ".csv.gz")
  path <- tempfile(fileext = ".rds")
  on.exit(unlink(c(f, path)))
  con <- gzfile(f, "wt")
  utils::write.csv(a$rates, con, row.names = FALSE)
  close(con)
  d <- moves_factors(f, a$runs)
  curve <- ef_moves("NOx", 2020, factors = d)
  saveRDS(list(data = d, curve = curve), path)
  restored <- readRDS(path)
  expect_equal(restored$curve(50), curve(50))
  expect_equal(restored$data, d)
  expect_lt(file.info(path)$size, 20000)
})

test_that("MOVES curves integrate with vein emis in g/km with correct link and age order", {
  a <- portable_moves_fixture()
  d <- moves_factors(a$rates, a$runs)
  curves <- EmissionFactorsList(lapply(c(2020, 2010), function(y) {
    ef_moves("NOx", y, factors = d)
  }))
  veh <- data.frame(new = c(2, 3), old = c(4, 5))
  lkm <- units::set_units(c(1, 2), "km")
  speed <- data.frame(H1 = c(5, 10) * 1.609344)
  out <- emis(veh, lkm, ef = curves, speed = speed, profile = matrix(1, 1, 1),
              simplify = TRUE, verbose = FALSE)
  # bin 2: 1 and 2 g/km. bin 3: 2 and 4 g/km. Multiply by veh and km.
  expect_equal(as.numeric(out[, , 1]), as.numeric(matrix(c(2, 12, 8, 40), 2, 2)))
})
