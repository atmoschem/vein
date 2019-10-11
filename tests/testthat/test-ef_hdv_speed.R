context("ef_hdv_speed")

test_that("ef_hdv_speed works", {
  expect_equal(ef_hdv_speed(v = "Trucks",t = "RT", g = "<=7.5", e = "II", gr = 0,
                            l = 0.5, p = "HC", show.equation = FALSE)(30), 0.207398745)
})

test_that("ef_hdv_speed works", {
  expect_equal(round(ef_hdv_speed(v = "Trucks",
                            t = "RT",
                            g = "<=7.5",
                            e = data.frame(eu = "II"),
                            gr = 0,
                            l = 0.5,
                            p = "HC",
                            speed = Speed(0))[1,1]),
               EmissionFactors(1))
})

test_that("ef_hdv_speed works", {
  expect_equal(ef_hdv_speed(v = "Trucks",t = "RT", g = "<=7.5", e = "II", gr = 0,
                            l = 0.5, p = "Hg", show.equation = FALSE)(30), 5.350296e-07)
})

test_that("ef_hdv_speed works", {
  expect_equal(round(ef_hdv_speed(v = "Trucks",t = "RT", g = "<=7.5", e = "II", gr = 0,
                            l = 0.5, p = "CO2", show.equation = FALSE)(30)), 317)
})

test_that("ef_hdv_speed works", {
  expect_equal(round(ef_hdv_speed(v = "Trucks",t = "RT", g = "<=7.5", e = "II", gr = 0,
                                  l = 0.5, p = "napthalene", show.equation = FALSE)(30)), 0)
})

test_that("ef_hdv_speed works", {
  expect_equal(round(ef_hdv_speed(v = "Trucks",t = "RT", g = "<=7.5", e = "II", gr = 0,
                                  l = 0.5, p = "PCB", show.equation = FALSE)(30)), 0)
})

test_that("ef_hdv_speed works", {
  expect_equal(round(ef_hdv_speed(v = "Trucks",t = "RT", g = "<=7.5", e = "II", gr = 0,
                                  l = 0.5, p = "heptane", show.equation = FALSE)(30)), 0)
})

test_that("ef_hdv_speed works", {
  expect_equal(round(ef_hdv_speed(v = "Trucks",t = "RT", g = "<=7.5", e = "II", gr = 0,
                                  l = 0.5, p = "N_rural", show.equation = FALSE)(30)), 3.19e+14)
})

test_that("ef_hdv_speed stops", {
  expect_error(ef_hdv_speed(v = "Trucks",t = "RT", g = "<=7.5", e = "II", gr = 0,
                                  l = 0.5, p = "N_rural", speed = 1),
               "speed.?")
})

test_that("ef_hdv_speed stops", {
  expect_error(ef_hdv_speed(v = "Trucks",t = "RT", g = "<=7.5", e = "II", gr = 0,
                            l = 0.5, p = "N_rural", speed = units::set_units(1, "t/km")),
               "Units.?")
})

test_that("ef_hdv_speed works", {
  expect_equal(ef_hdv_speed(v = "Trucks",t = "RT", g = "<=7.5", e = "II", gr = 0,
                            l = 0.5, p = "HC", show.equation = FALSE)(30), 0.207398745)
})

test_that("ef_hdv_speed prints", {
  expect_output(ef_hdv_speed(v = "Trucks",t = "RT", g = "<=7.5", e = "II", gr = 0,
                            l = 0.5, p = "HC", show.equation = TRUE)(30),
               "a.?")
  expect_output(ef_hdv_speed(v = "Trucks",t = "RT", g = "<=7.5", e = "II", gr = 0,
                             l = 0.5, p = "HC", show.equation = TRUE)(30),
                "E.?")
})

test_that("ef_hdv_speed works", {
  expect_equal(round(ef_hdv_speed(v = "Trucks",t = "RT", g = "<=7.5", e = "II", gr = 0,
                            l = 0.5, p = "SO2", x = 10)(30)), 0)
})

test_that("ef_hdv_speed works", {
  expect_equal(round(ef_hdv_speed(v = "Trucks",t = "RT", g = "<=7.5", e = "II", gr = 0,
                                  l = 0.5, p = "SO2", x = 10, speed = Speed(30))), 0)
})

