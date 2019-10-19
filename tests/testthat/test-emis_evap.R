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

