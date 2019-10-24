context("emis_evap")

data(net)
a <- Vehicles(1:10)
lkm <- units::as_units(1:10, "km")
ef <- EmissionFactors(1:10)
netsf <- sf::st_as_sf(net)[1:10, "ldv"]

test_that("emis_evap works", {
  expect_equal(emis_evap(veh = a, x = lkm, hotfi = ef)[1],
               Emissions(1))
  expect_message(emis_evap(veh = a, x = lkm, hotfi = ef, verbose = TRUE),
                 "E.?")
  expect_equal(emis_evap(veh = netsf$ldv,
                         x = lkm,
                         hotfi = ef,
                         pro_month = 1:12)[1],
               Emissions(4350))
  expect_equal(emis_evap(veh = netsf$ldv,
                         x = lkm,
                         hotfi = ef,
                         pro_month = as.data.frame(1:12))[1],
               Emissions(4350))
})


test_that("emis_evap works", {
  expect_error(emis_evap(veh = a,
                         x = lkm,
                         ed = 1)[1],
               "u.?")
  expect_warning(emis_evap(veh = a,
                           x = lkm,
                           ed = Emissions(1))[1],
                 "u.?")
  expect_equal(emis_evap(veh = netsf,
                           x = lkm,
                           ed = data.frame(Emissions(1)))$emissions[1],
                 Emissions(4350))
  expect_message(emis_evap(veh = netsf,
                         x = lkm,
                         ed = data.frame(Emissions(1)),
                         verbose = TRUE),
               "E.?")
  expect_output(emis_evap(veh = netsf,
                           x = lkm,
                           ed = data.frame(Emissions(1)),
                           verbose = TRUE),
                 "S.?")
})


dfa <- Vehicles(data.frame(a = 1:10, b = 1:10))
dfef <- EmissionFactors(data.frame(a = 1:10, b = 1:10))
dfefm <- EmissionFactors(data.frame(a = 1:120, b = 1:120))

test_that("emis_evap works", {
  expect_error(emis_evap(veh = dfa,
                         x = lkm,
                         ed = 1)[1],
               "as.?")
  expect_warning(emis_evap(veh = a,
                           x = lkm,
                           ed = Emissions(1))[1],
                 "u.?")
  expect_equal(emis_evap(veh = dfa,
                         x = lkm,
                         ed = dfef)$emissions[1],
               Emissions(1))
  expect_message(emis_evap(veh = dfa,
                         x = lkm,
                         ed = dfef,
                         verbose = TRUE)$emissions[1],
                "E.?")
  expect_output(print(emis_evap(veh = dfa,
                           x = lkm,
                           ed = dfef,
                           verbose = TRUE)),
                 ".?")
  expect_equal(round(emis_evap(veh = dfa,
                         x = lkm,
                         ed = dfefm,
                         pro_month = 1:12,
                         verbose = TRUE)$emissions[1]),
               Emissions(0))
  expect_message(emis_evap(veh = dfa,
                               x = lkm,
                               ed = dfefm,
                               pro_month = 1:12,
                               verbose = TRUE)$emissions[1],
               ".?")

  expect_error(emis_evap(veh = dfa,
                           x = lkm,
                           ed = dfefm,
                           pro_month = 1,
                           verbose = TRUE),
                 ".?")
  expect_equal(round(emis_evap(veh = dfa,
                               x = lkm,
                               ed = dfefm,
                               pro_month = data.frame(matrix(1:12, ncol = 12)),
                               verbose = TRUE)$emissions[1]),
               Emissions(0))
})

#carb > 0
test_that("emis_evap works", {
  expect_error(emis_evap(veh = a,
                         x = lkm,
                         carb = 0.5)[1],
               ".?")
  expect_error(emis_evap(veh = dfa,
                         x = lkm,
                         carb = 0.5)[1],
               ".?")
  expect_message(emis_evap(veh = a,
                         x = lkm,
                         p = 0.1,
                         hotc = 10,
                         warmc = 10,
                         hotfi = 0.1,
                         carb = 0.5, verbose = TRUE)[1],
               ".?")
  expect_equal(emis_evap(veh = a,
                           x = lkm,
                           p = 0.1,
                           hotc = 10,
                           warmc = 10,
                           hotfi = 0.1,
                           carb = 0.5, verbose = TRUE)[1],
                 Emissions(5.05))


})

test_that("emis_evap works", {
  expect_error(emis_evap(veh = dfa,
                         x = lkm,
                         carb = 0.5)[1],
               ".?")
  expect_message(emis_evap(veh = dfa,
                           x = lkm,
                           p = 0.1,
                           hotc = data.frame(matrix(10, ncol = ncol(dfa))),
                           warmc = data.frame(matrix(10, ncol = ncol(dfa))),
                           hotfi = data.frame(matrix(10, ncol = ncol(dfa))),
                           carb = 0.5, verbose = TRUE)[1],
                 ".?")
  expect_equal(emis_evap(veh = dfa,
                           x = lkm,
                           p = 0.1,
                           hotc = data.frame(matrix(10, ncol = ncol(dfa))),
                           warmc = data.frame(matrix(10, ncol = ncol(dfa))),
                           hotfi = data.frame(matrix(10, ncol = ncol(dfa))),
                           carb = 0.5, verbose = TRUE)$emissions[1],
                 Emissions(10))


})
