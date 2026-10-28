context("Emissions, emis_order and emis_evap fixes")

test_that("Emissions converts units without time", {
  expect_equal(as.numeric(suppressMessages(Emissions(units::set_units(1:3, "kg")))), c(1000, 2000, 3000))
  expect_equal(as.character(units(suppressMessages(Emissions(units::set_units(1, "kg/d"), time = "h")))), "g/h")
  expect_error(Emissions(units::set_units(1, "km")), "units of x")
  expect_error(Emissions(TRUE), "Class")
})

test_that("emis_order accepts matrix", {
  m <- matrix(1:20, 2, 10)
  r <- emis_order(m, lt_emissions = "2020-02-19 00:00",
                  start_utc_time = "2020-02-20 00:00",
                  desired_length = 5, tz_lt = "America/Sao_Paulo", verbose = FALSE)
  # 2020-02-20 00:00 UTC = 2020-02-19 21:00 in Sao Paulo, the 22th hour, 2nd column
  expect_equal(names(r), paste0("V", 2:6))
  expect_error(emis_order(m, lt_emissions = "2020-02-19 00:00",
                          start_utc_time = "2020-02-20 00:00",
                          desired_length = 5, tz_lt = "America/Sao_Paulo",
                          verbose = FALSE, seconds = 1800),
               "seconds")
})

test_that("emis_evap data.frame branches", {
  veh <- Vehicles(data.frame(a = 1:3, b = 4:6))
  x <- units::as_units(c(10, 20), "km")
  hf <- EmissionFactors(data.frame(a = c(1, 1, 1), b = c(2, 2, 2)))
  e <- emis_evap(veh = veh, x = x, hotfi = hf)
  expect_equal(nrow(e), 6)
  expect_equal(sum(as.numeric(e$emissions)), sum((1:3) * 10 * 1 + (4:6) * 20 * 2))
  hf12 <- EmissionFactors(data.frame(a = rep(1, 36), b = rep(2, 36)))
  e12 <- emis_evap(veh = veh, x = x, hotfi = hf12, hotc = hf12, warmc = hf12,
                   carb = 0.3, p = 0.5, pro_month = rep(1, 12))
  expect_equal(nrow(e12), 6 * 12)
  # all factors are equal, so the sum is the same as with carb = 0
  expect_equal(sum(as.numeric(e12$emissions)), sum((1:3) * 10 * 1 + (4:6) * 20 * 2))
})

test_that("emis_evap2 labels and ages", {
  veh <- data.frame(a = 1, b = 2, c = 3)
  ef <- c(1, 1, 1)
  r <- suppressMessages(emis_evap2(veh, "PC", "<=1400", "G", aged = 1:3,
                                   nd4 = 1, nd3 = 1, nd2 = 1, nd1 = 1,
                                   hs_nd4 = ef, hs_nd3 = ef, hs_nd2 = ef, hs_nd1 = ef,
                                   rl_nd4 = ef, rl_nd3 = ef, rl_nd2 = ef, rl_nd1 = ef,
                                   d_nd4 = ef, d_nd3 = ef, d_nd2 = ef, d_nd1 = ef))
  expect_equal(nrow(r), 36)
  expect_equal(sort(unique(r$Temperature)), sort(c("20_35", "10_25", "0_15", "-5_10")))
  expect_equal(r$g[1:3], c(1, 2, 3))
})
