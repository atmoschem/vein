context("ef_nitro")

test_that("ef_nitro works", {
  expect_equal( ef_nitro(v = "PC",
                         t = "Hot",
                         cond = "Urban",
                         cc = "<=1400",
                         f = "G",
                         eu = "III",
                         p = "NH3",
                         S = 10,
                         show.equation = FALSE)(10),
                0.001637825)
})


test_that("ef_nitro works", {
  expect_equal(ef_nitro(v = "PC",
                        t = "Hot",
                        cond = "Urban",
                        f = "G",
                        cc = "<=1400",
                        eu = "III",
                        p = "NH3",
                        S = 10,
                        cumileage = units::set_units(25000, "km")),
               EmissionFactors(0.001700025))
})

test_that("ef_nitro stops", {
  expect_error(ef_nitro(v = "PC",
                        t = "Hot",
                        cond = "Urban",
                        f = "G",
                        cc = "<=1400",
                        eu = data.frame(euro = c("III", "III")),
                        p = "NH3",
                        S = 10),
               "Add?")
})

test_that("ef_nitro works", {
  expect_error(ef_nitro(v = "PC",
                        t = "Hot",
                        cond = "Urban",
                        f = "G",
                        cc = "<=1400",
                        eu = "III",
                        p = "NH3",
                        S = 10,
                        cumileage = units::set_units(25000, "m")),
               "Un?")
})

test_that("ef_nitro works", {
  expect_error(ef_nitro(v = "PC",
                        t = "Hot",
                        cond = "Urban",
                        f = "G",
                        cc = "<=1400",
                        eu = "III",
                        p = "NH3",
                        S = 10,
                        cumileage = 25000),
               "cu?")
})


test_that("ef_nitro works", {
  expect_equal(ef_nitro(v = "PC",
                        t = "Hot",
                        cond = "Urban",
                        f = "G",
                        cc = "<=1400",
                        eu = data.frame(euro = c("III", "III")),
                        p = "NH3",
                        S = 10,
                        cumileage = units::set_units(25000, "km")),
               EmissionFactors(data.frame(V1 = rep(0.001700025, 2))))
})




test_that("ef_nitro works", {
  expect_output(ef_nitro(v = "PC", t = "Hot", cond = "Urban", cc = "<=1400", f = "G",
                         eu = "III", p = "NH3", S = 10,
                         show.equation = TRUE)(10),
                "a.?")
})


test_that("ef_nitro works", {
  expect_equal(ef_nitro(v = "PC",
                        t = "Hot",
                        cond = "Urban",
                        f = "G",
                        cc = "<=1400",
                        eu = data.frame(euro = c("III", "III"),
                                        euro2 = c("III", "III")),
                        p = "NH3",
                        S = 10,
                        cumileage = units::set_units(25000, "km"))[,1],
               EmissionFactors(rep(0.001700025, 2)))
})


test_that("ef_nitro works", {
  expect_equal(ef_nitro(v = "PC",
                        t = "Hot",
                        cond = "Urban",
                        f = "G",
                        cc = "<=1400",
                        eu = c("III", "III"),
                        p = "NH3",
                        S = 10,
                        cumileage = units::set_units(25000, "km"))[1,1],
               EmissionFactors(0.001700025))
})

test_that("ef_nitro works", {
  expect_equal(ef_nitro(v = "PC",
                        t = "Hot",
                        cond = "Urban",
                        f = "G",
                        cc = "<=1400",
                        eu = c("III", "III"),
                        p = "NH3",
                        S = 10)[[1]](10),
               0.001637825)
})
