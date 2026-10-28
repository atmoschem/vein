test_that("the packaged EPA reference has the documented complete coverage", {
  x <- moves_factors()
  r <- x$rates
  expect_equal(nrow(r), 2496)
  expect_equal(sort(unique(r$modelYearID)), 2000:2025)
  expect_equal(sort(unique(r$pollutantID)), c(2L, 3L))
  expect_equal(sort(unique(r$temperature_C)), c(10, 20, 30))
  expect_equal(unique(r$locationID), "zone:261610")
  expect_true(all(table(r$pollutantID, r$modelYearID, r$hourID) == 16))
  expect_equal(unique(x$runs$masterVersion), "MOVES6.0.0")
  expect_equal(unique(x$runs$defaultDatabaseUsed), "movesdb20260729")
  expect_equal(x$runs$expectedDONEFiles, x$runs$retrievedDONEFiles)
})

test_that("both R evaluators reproduce every exported EPA reference rate", {
  x <- moves_factors()
  bins <- utils::read.csv(system.file("extdata", "moves", "avgspeedbin.csv", package = "vein"))
  nominal <- bins$avgBinSpeed
  # Bin 1 describes speeds below 2.5 mph despite its nominal 2.5 mph value.
  within <- nominal
  within[1] <- 1.25
  groups <- unique(x$rates[c("pollutantID", "modelYearID", "hourID")])
  for (i in seq_len(nrow(groups))) {
    p <- groups$pollutantID[i]
    y <- groups$modelYearID[i]
    h <- groups$hourID[i]
    raw <- x$rates[x$rates$pollutantID == p & x$rates$modelYearID == y & x$rates$hourID == h, ]
    expected <- raw$ratePerDistance[match(bins$avgSpeedBinID, raw$avgSpeedBinID)] / 1.609344
    f <- ef_moves(p, y, factors = x, select = list(hourID = h))
    linear <- ef_moves(p, y, factors = x, select = list(hourID = h), method = "linear")
    expect_equal(f(within * 1.609344), expected, tolerance = 1e-12)
    expect_equal(linear(nominal * 1.609344), expected, tolerance = 1e-12)
  }
})

test_that("frozen NOx output values and meteorological selections remain reproducible", {
  # Independent values recorded from MOVESRunID 3's raw CSV (g/mile), model
  # year 2020, 20 C, RH 50%, speed bins 1, 3, 6, 11 and 16.
  raw <- c(0.0224853, 0.018784, 0.0173561, 0.0151238, 0.0195563)
  speed <- c(1.25, 10, 25, 50, 75) * 1.609344
  result <- ef_moves("NOx", 2020, speed = speed, temperature = 20, rel_humidity = 50)
  expect_equal(as.numeric(result), raw / 1.609344, tolerance = 1e-12)
  expect_error(ef_moves("NOx", 2020, speed = 50), "ambiguous")
  expect_error(ef_moves("NOx", 2020, speed = 50, temperature = 25), "No MOVES rates")
})
