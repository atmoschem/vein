context("speciate")

pm <- 1:100
dfa <- speciate(pm, spec = "e_eth", veh = "veh", fuel = "G", eu = "Exhaust")
dfb <- speciate(pm, spec = "e_tol", veh = "veh", fuel = "G", eu = "Exhaust")

test_that("speciate works", {
  expect_equal(round(speciate(pm, spec = "bcom",
                              veh = "PC", fuel = "G", eu = "I")$BC[1]),
               0*units::as_units("g"))
  expect_output(speciate(pm, spec = "bcom",
                         veh = "PC", fuel = "G", eu = "I",
                         show = TRUE),
                ".?")
  expect_equal(speciate(pm, spec = "bcom",
                        veh = "PC", fuel = "G", eu = "I",
                        list = TRUE)[[1]][1],
               Emissions(0.25))
})




test_that("speciate works", {
  expect_equal(round(speciate(pm, spec = "e_tol", veh = "veh", fuel = "G",
                              eu = "Exhaust")$e_tol[1]),
               0)
  expect_output(speciate(pm, spec = "e_tol", veh = "veh", fuel = "G",
                         eu = "Exhaust",
                         show = TRUE),
                ".?")
  expect_equal(as.numeric(round(speciate(data.frame(pm = pm),
                                         spec = "e_tol",
                                         veh = "veh",
                                         fuel = "G",
                                         eu = "Exhaust",
                                         list = TRUE)[[1]][1,1])),
               0)
})



test_that("speciate works", {
  expect_equal(round(speciate(pm, spec = "tyre",
                              veh = "PC", fuel = "G", eu = "I")$PM10[1]),
               Emissions(1))
  expect_output(speciate(pm, spec = "tyre",
                         veh = "PC", fuel = "G", eu = "I",
                         show = TRUE),
                ".?")
  expect_equal(as.numeric(round(speciate(data.frame(pm = pm),
                                         veh = "PC", fuel = "G", eu = "I",
                                         list = TRUE)[[1]][1])),
               0)
})



test_that("speciate works", {
  expect_equal(round(speciate(pm, spec = "brake",
                              veh = "PC", fuel = "G", eu = "I")$PM10[1]),
               1*units::as_units("g"))
  expect_output(speciate(pm, spec = "brake",
                         veh = "PC", fuel = "G", eu = "I", show = TRUE),
                ".?")
  expect_equal(as.numeric(round(speciate(data.frame(pm = pm), spec = "brake",
                                         veh = "PC", fuel = "G", eu = "I",
                                         list = TRUE)[[1]][1])),
               1)
})


test_that("speciate works", {
  expect_equal(round(speciate(pm, spec = "road",
                              veh = "PC", fuel = "G", eu = "I")$PM10[1]),
               0*units::as_units("g"))
  expect_output(speciate(pm, spec = "road",
                         veh = "PC", fuel = "G", eu = "I", show = TRUE),
                ".?")
  expect_equal(as.numeric(round(speciate(data.frame(pm = pm), spec = "road",
                                         veh = "PC", fuel = "G", eu = "I",
                                         list = TRUE)[[1]][1])),
               0)
})


test_that("speciate works", {
  expect_equal(round(speciate(pm, spec = "iag",
                              veh = "veh", fuel = "G", eu = "Exhaust")$e_eth[1]),
               0)
  expect_output(speciate(pm, spec = "iag",
                         veh = "veh", fuel = "G", eu = "Exhaust",show = TRUE),
                ".?")
  expect_equal(as.numeric(round(speciate(data.frame(pm), spec = "iag",
                                         veh = "veh", fuel = "G",
                                         eu = "Exhaust",
                                         list = TRUE)[[1]][1,1])),
               0)
})
