context("emis_evap")

(a <- Vehicles(1:10))
(lkm <- units::as_units(1:10, "km"))
(ef <- EmissionFactors(1:10))
(ev <- emis_evap(veh = a, x = lkm, hotfi = ef))

test_that("adt works", {
  expect_equal(emis_evap(veh = a, x = lkm, hotfi = ef)[1],
               1*units::as_units("g"))
})
