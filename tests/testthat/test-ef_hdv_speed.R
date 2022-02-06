context("ef_hdv_speed")

test_that("ef_hdv_speed works", {
  expect_equal(round(ef_hdv_speed(v = "Trucks",
                            t = "RT",
                            g = "<=7.5",
                            e = "II",
                            gr = 0,
                            l = 0.5,
                            p = "CO",
                            speed = Speed(30))),
                EmissionFactors(1))
})


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
  expect_equal(round(ef_hdv_speed(v = "Trucks",t = "RT", g = "<=7.5", e = "II", gr = 0,
                            l = 0.5, p = "CO2", show.equation = FALSE)(30)), 317)
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
  expect_equal(round(ef_hdv_speed(v = "Trucks",
                                  t = "RT",
                                  g = "<=7.5",
                                  e = "II",
                                  gr = 0,
                                  l = 0.5,
                                  p = "SO2",
                                  x = 10,
                                  speed = Speed(30))),
               EmissionFactors(0))
})

test_that("ef_hdv_speed stops", {
  expect_error(round(ef_hdv_speed(v = "Trucks",t = "RT", g = "<=7.5", e = "II", gr = 0,
                                  l = 0.5, p = "SO2", x = 10, speed = 30)),
               "sp?")
})

test_that("ef_hdv_speed works", {
  expect_equal(round(ef_hdv_speed(v = "Trucks",
                                  t = "RT",
                                  g = "<=7.5",
                                  e = c("II", "III"),
                                  gr = 0,
                                  l = 0.5,
                                  p = "SO2",
                                  x = 10,
                                  speed = Speed(30)))$II_1[1],
               EmissionFactors(0))
})

test_that("ef_hdv_speed works", {
  expect_error(ef_hdv_speed(v = "Trucks",
                                  t = "RT",
                                  g = "<=7.5",
                                  e = c("II", "III"),
                                  gr = 0,
                                  l = 0.5,
                                  p = "SO2",
                                  x = 10),
               "i.?")
})

test_that("ef_hdv_speed works", {
  expect_equal(round(ef_hdv_speed(v = "Trucks",
                            t = "RT",
                            g = "<=7.5",
                            e = c("II", "III"),
                            gr = 0,
                            l = 0.5,
                            p = "HC",
                            speed = Speed(10))$II_1[1]),
               EmissionFactors(0))
})


test_that("ef_hdv_speed stops", {
  expect_error(ef_hdv_speed(v = "Trucks",
                            t = "RT",
                            g = "<=7.5",
                            e = data.frame("II"),
                            gr = 0,
                            l = 0.5,
                            p = "SO2",
                            x = 10),
               "Add.?")
})

test_that("ef_hdv_speed stops", {
  expect_equal(round(ef_hdv_speed(v = "Trucks",
                            t = "RT",
                            g = "<=7.5",
                            e = data.frame("II", "III"),
                            gr = 0,
                            l = 0.5,
                            p = "SO2",
                            x = 10,
                            speed = Speed(0))$V1[1]),
               EmissionFactors(0))
})

