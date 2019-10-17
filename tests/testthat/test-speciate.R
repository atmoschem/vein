context("speciate")

pm <- 1:100

dfa <- speciate(pm, spec = "e_eth", veh = "veh", fuel = "G", eu = "Exhaust")
dfb <- speciate(pm, spec = "e_tol", veh = "veh", fuel = "G", eu = "Exhaust")

test_that("adt works", {
  expect_equal(round(speciate(pm, spec = "bcom",
                              veh = "PC", fuel = "G", eu = "I")$BC[1]),
               0*units::as_units("g"))
  expect_equal(round(speciate(pm, spec = "e_tol", veh = "veh", fuel = "G",
                              eu = "Exhaust")$e_tol[1]),
               0)
  expect_equal(round(speciate(pm, spec = "tyre",
                              veh = "PC", fuel = "G", eu = "I")$PM10[1]),
               1*units::as_units("g"))
  expect_equal(round(speciate(pm, spec = "brake",
                              veh = "PC", fuel = "G", eu = "I")$PM10[1]),
               1*units::as_units("g"))
  expect_equal(round(speciate(pm, spec = "road",
                              veh = "PC", fuel = "G", eu = "I")$PM10[1]),
               0*units::as_units("g"))
  expect_equal(round(speciate(pm, spec = "iag",
                              veh = "veh", fuel = "G", eu = "Exhaust")$e_eth[1]),
               0)
})

