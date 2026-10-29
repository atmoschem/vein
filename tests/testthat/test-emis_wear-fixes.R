context("emis_wear fixes")

veh <- data.frame(a1 = c(10, 20), a2 = c(1, 2))
lkm <- units::set_units(c(1, 2), "km")
profile <- matrix(c(1, 2, 3, 4), nrow = 2)   # 2 hours x 2 days
speed <- data.frame(h1 = c(10, 100), h2 = c(50, 50),
  h3 = c(100, 10), h4 = c(20, 95))

test_that("emis_wear does not apply the speed correction again", {
  ef <- ef_wear("tyre", "PC", "TSP", speed = speed)
  e <- emis_wear(veh = veh, lkm = lkm, ef = ef, speed = speed,
    profile = profile)
  expect_equal(dim(e), c(2, 2, 2, 2))
  efm <- as.matrix(remove_units(ef))
  for (j in 1:2) for (i in 1:2) for (k in 1:2) {
    col <- 2 * (j - 1) + i   # day offset
    expect_equal(as.numeric(e[, k, i, j]),
      veh[, k] * profile[i, j] * as.numeric(lkm) * efm[, col])
  }
})

test_that("brake and tyre give their own curves through ef_wear", {
  efb <- ef_wear("brake", "PC", "TSP", speed = speed)
  e <- emis_wear(veh = veh, lkm = lkm, ef = efb, what = "break",
    speed = speed, profile = profile)
  # day 2, hour 2, street 2: speed 95 km/h -> brake factor 0.185
  expect_equal(as.numeric(e[2, 1, 2, 2]), 20 * 4 * 2 * 0.0075 * 0.185)
})

test_that("a single road wear factor is used for all hours", {
  e <- emis_wear(veh = veh, lkm = lkm, ef = ef_wear("road", "PC", "TSP"),
    what = "road", profile = profile)
  expect_equal(as.numeric(e[1, 1, 2, 2]), 10 * 4 * 1 * 0.015)
})

test_that("number of ef columns must be hours x days", {
  ef <- ef_wear("tyre", "PC", "TSP", speed = speed[, 1:3])
  expect_error(emis_wear(veh = veh, lkm = lkm, ef = ef, profile = profile),
    "Number")
})
