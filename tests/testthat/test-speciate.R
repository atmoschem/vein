context("speciate")

pm <- 1:100

test_that("speciate works", {
  expect_equal(round(speciate(pm, spec = "bcom",
                              veh = "PC", fuel = "G", eu = "I")$BC[1]),
               0*units::as_units("g"))
  expect_equal(speciate(pm, spec = "bcom",
                        veh = "PC", fuel = "G", eu = "I",
                        list = TRUE)[[1]][1],
               Emissions(0.25))
})



test_that("speciate works", {
  expect_equal(round(speciate(pm, spec = "tyre",
                              veh = "PC", fuel = "G", eu = "I")$PM10[1]),
               Emissions(1))
})



test_that("speciate works", {
  expect_equal(round(speciate(pm, spec = "brake",
                              veh = "PC", fuel = "G", eu = "I")$PM10[1]),
               1*units::as_units("g"))
  expect_equal(as.numeric(round(speciate(data.frame(pm = pm), spec = "brake",
                                         veh = "PC", fuel = "G", eu = "I",
                                         list = TRUE)[[1]][1])),
               1)
})


test_that("speciate works", {
  expect_equal(round(speciate(pm, spec = "road",
                              veh = "PC", fuel = "G", eu = "I")$PM10[1]),
               0*units::as_units("g"))
  expect_equal(as.numeric(round(speciate(data.frame(pm = pm), spec = "road",
                                         veh = "PC", fuel = "G", eu = "I",
                                         list = TRUE)[[1]][1])),
               0)
})


test_that("speciate works", {
  expect_equal(round(speciate(pm, spec = "iag",
                              veh = "veh", fuel = "G", eu = "Exhaust")[[1]][1]),
               0)
  expect_equal(as.numeric(round(speciate(data.frame(pm), spec = "iag",
                                         veh = "veh", fuel = "G",
                                         eu = "Exhaust",
                                         list = TRUE)[[1]][1,1])),
               0)
})

test_that("speciate works", {
  expect_equal(as.numeric(round(speciate(data.frame(pm), spec = "iag",
                                         veh = "veh", fuel = "G",
                                         eu = "Exhaust",
                                         list = TRUE)[[1]][1,1])),
               0)
})


test_that("speciate works", {
  expect_equal(round(speciate(pm, spec = "nmhc",
                              veh = "LDV", fuel = "G", eu = "I", list = F)$x[1]),
               0)
  expect_equal(as.numeric(round(speciate(data.frame(pm), spec = "nmhc",
                                         veh = "LDV", fuel = "G", eu = "I",
                                         list = TRUE)[[1]][1,1])),
               0)
})


test_that("speciate works", {
  expect_equal(speciate(pm, spec = "nox",
                              veh = "PC", fuel = "G", eu = "I")$NO2[1],
               Emissions(0.04))
})
data(net)
g <- make_grid(net, 2)
g$PM = 10
test_that("speciate works", {
  expect_equal(as.numeric(round(speciate(g, spec = "pmiag")$E_SO4I[1])),
               0)
})


test_that("speciate works", {
  expect_equal(as.numeric(round(speciate(g, spec = "pmiag")$E_SO4I[1])),
               0)
})
#

