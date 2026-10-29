context("ef_wear fixes")

# EMEP/EEA 1.A.3.b.vi, equations 5 (tyre) and 8 (brake)
st <- function(V) ifelse(V < 40, 1.39, ifelse(V <= 90, -0.00974 * V + 1.78, 0.902))
sb <- function(V) ifelse(V < 40, 1.67, ifelse(V <= 95, -0.0270 * V + 2.75, 0.185))
V <- c(10, 39.9, 40, 60, 90, 92, 95, 100, 130)

test_that("tyre speed correction is not inverted", {
  ef <- as.numeric(ef_wear("tyre", "PC", "TSP", speed = data.frame(s = V))[, 1])
  expect_equal(ef, 0.0107 * st(V))
  # low speeds emit more than high speeds
  expect_gt(ef[1], ef[length(ef)])
})

test_that("brake speed correction is not inverted and uses 95 km/h", {
  ef <- as.numeric(ef_wear("brake", "PC", "TSP", speed = data.frame(s = V))[, 1])
  expect_equal(ef, 0.0075 * sb(V))
  expect_equal(ef[V == 92], 0.0075 * (-0.0270 * 92 + 2.75))
})

test_that("each speed column is corrected with its own speeds", {
  sp <- data.frame(a = c(40, 40), b = c(100, 100))
  ef <- ef_wear("tyre", "PC", "TSP", speed = sp)
  expect_equal(as.numeric(ef[, 1]), rep(0.0107 * st(40), 2))
  expect_equal(as.numeric(ef[, 2]), rep(0.0107 * st(100), 2))
})

test_that("HDV brake uses the passenger car factor", {
  ef <- as.numeric(ef_wear("brake", "HDV", "TSP", speed = data.frame(s = 30), load = 0.5))
  expect_equal(ef, 3.13 * (1 + 0.79 * 0.5) * 0.0075 * 1.67)
  ef <- as.numeric(ef_wear("tyre", "TRUCKS", "TSP", speed = data.frame(s = 30), axle = 4, load = 1))
  expect_equal(ef, 4 / 2 * (1.41 + 1.38) * 0.0107 * 1.39)
})

test_that("speed with units is converted to km/h", {
  a <- ef_wear("tyre", "PC", "TSP", speed = units::set_units(60, "km/h"))
  b <- ef_wear("tyre", "PC", "TSP", speed = units::set_units(60 / 3.6, "m/s"))
  expect_equal(as.numeric(a[, 1]), as.numeric(b[, 1]))
})

test_that("road wear does not need speed and bad arguments stop", {
  expect_equal(as.numeric(ef_wear("road", "PC", "PM10")), 0.015 * 0.5)
  expect_error(ef_wear("tyre", "PC", "PM5", speed = 30), "pol")
  expect_error(ef_wear("tyre", "CAR", "PM10", speed = 30), "type")
  expect_error(ef_wear("tyre", "PC", "PM10"), "speed")
})
