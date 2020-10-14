context("speciate")

pm <- 1:100

test_that("speciate works", {
  expect_equal(round(speciate(pm, spec = "bcom",
                              veh = "PC", fuel = "G", eu = "I", show = TRUE)$BC[1]),
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
  expect_equal(round(speciate(pm, spec = "tyre",
                              veh = "PC", fuel = "G", eu = "I", show = TRUE)$PM10[1]),
               Emissions(1))
  expect_output(speciate(pm, spec = "tyre",
                         veh = "PC", fuel = "G", eu = "I",
                         show = TRUE),
                ".?")
  expect_equal(as.numeric(round(speciate(data.frame(pm = pm),
                                         veh = "PC", fuel = "G", eu = "I",
                                         list = TRUE, show = TRUE)[[1]][1])),
               0)
})



test_that("speciate works", {
  expect_equal(round(speciate(pm, spec = "brake",
                              veh = "PC", fuel = "G", eu = "I", show = TRUE)$PM10[1]),
               1*units::as_units("g"))
  expect_output(speciate(pm, spec = "brake",
                         veh = "PC", fuel = "G", eu = "I", show = TRUE),
                ".?")
  expect_equal(as.numeric(round(speciate(data.frame(pm = pm), spec = "brake",
                                         veh = "PC", fuel = "G", eu = "I",
                                         list = TRUE, show = TRUE)[[1]][1])),
               1)
})


test_that("speciate works", {
  expect_equal(round(speciate(pm, spec = "road",
                              veh = "PC", fuel = "G", eu = "I", show = TRUE)$PM10[1]),
               0*units::as_units("g"))
  expect_output(speciate(pm, spec = "road",
                         veh = "PC", fuel = "G", eu = "I", show = TRUE),
                ".?")
  expect_equal(as.numeric(round(speciate(data.frame(pm = pm), spec = "road",
                                         veh = "PC", fuel = "G", eu = "I",
                                         list = TRUE, show = TRUE)[[1]][1])),
               0)
})


test_that("speciate works", {
  expect_equal(round(speciate(pm, spec = "iag",
                              veh = "veh", fuel = "G", eu = "Exhaust", show = TRUE)[[1]][1]),
               0)
  expect_output(speciate(pm, spec = "iag",
                         veh = "veh", fuel = "G", eu = "Exhaust",show = TRUE),
                ".?")
  expect_equal(as.numeric(round(speciate(data.frame(pm), spec = "iag",
                                         veh = "veh", fuel = "G",
                                         eu = "Exhaust",
                                         list = TRUE, show = TRUE)[[1]][1,1])),
               0)
})

test_that("speciate works", {
  expect_output(speciate(pm, spec = "iag",
                         veh = "veh", fuel = "G", eu = "Exhaust",show = TRUE),
                ".?")
  expect_equal(as.numeric(round(speciate(data.frame(pm), spec = "iag",
                                         veh = "veh", fuel = "G",
                                         eu = "Exhaust",
                                         list = TRUE, show = TRUE)[[1]][1,1])),
               0)
})


test_that("speciate works", {
  expect_equal(round(speciate(pm, spec = "nmhc",
                              veh = "LDV", fuel = "G", eu = "I", show = TRUE)$m_p_xylene[1]),
               0)
  expect_output(speciate(pm, spec = "nmhc",
                         veh = "LDV", fuel = "G", eu = "I", show = TRUE),
                ".?")
  expect_equal(as.numeric(round(speciate(data.frame(pm), spec = "nmhc",
                                         veh = "LDV", fuel = "G", eu = "I",
                                         list = TRUE, show = TRUE)[[1]][1,1])),
               0)
})


test_that("speciate works", {
  expect_equal(speciate(pm, spec = "nox",
                              veh = "PC", fuel = "G", eu = "I", show = TRUE)$NO2[1],
               Emissions(0.04))
  expect_output(speciate(pm, spec = "nox",
                        veh = "PC", fuel = "G", eu = "I", show = TRUE)$NO2[1],
               ".?")
})
data(net)
g <- make_grid(net, 2)
g$PM = 10
test_that("speciate works", {
  expect_equal(as.numeric(round(speciate(g, spec = "pmiag",show = TRUE)$e_so4i[1])),
               0)
  expect_output(as.numeric(round(speciate(g, spec = "pmiag",show = TRUE)$e_so4i[1])),
               ".?")
})


test_that("speciate works", {
  expect_equal(as.numeric(round(speciate(g, spec = "pmiag",show = TRUE)$e_so4i[1])),
               0)
  expect_output(as.numeric(round(speciate(g, spec = "pmiag",show = TRUE)$e_so4i[1])),
                ".?")
})
#
