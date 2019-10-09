context("ef_nitro")

test_that("ef_nitro works", {
  expect_equal( ef_nitro(v = "PC", t = "Hot", cond = "Urban", cc = "<=1400", f = "G",
                         eu = "III", p = "NH3", S = 10,
                         show.equation = FALSE)(10),
                0.001637825)
})

efe10 <- ef_nitro(v = "PC", t = "Hot", cond = "Urban", f = "G", cc = "<=1400",
                  eu = "III", p = "NH3", S = 10, cumileage = units::set_units(25000, "km"))

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
