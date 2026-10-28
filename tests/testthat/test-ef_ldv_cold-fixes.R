context("ef_ldv_cold fixes")

test_that("k multiplies the cold quotient", {
  a <- ef_ldv_cold(ta = 10, cc = "<=1400", f = "G", eu = "I", p = "CO")(20)
  b <- ef_ldv_cold(ta = 10, cc = "<=1400", f = "G", eu = "I", p = "CO", k = 2)(20)
  expect_equal(b, 2 * a)
  b <- ef_ldv_cold(ta = 10, cc = "<=1400", f = "G", eu = c("I", "I"), p = "CO",
                   k = 2, speed = Speed(20))
  expect_equal(as.numeric(b[[1]]), 2 * a)
})

test_that("a numeric vector of temperatures is used (it returned NULL)", {
  r <- ef_ldv_cold(ta = c(10, 20), cc = "<=1400", f = "G", eu = "I", p = "CO",
                   speed = Speed(20))
  expect_false(is.null(r))
  expect_equal(r$ta, c(10, 20))
  expect_equal(as.numeric(r[[1]]),
               c(ef_ldv_cold(ta = 10, cc = "<=1400", f = "G", eu = "I", p = "CO")(20),
                 ef_ldv_cold(ta = 20, cc = "<=1400", f = "G", eu = "I", p = "CO")(20)))
  expect_error(ef_ldv_cold(ta = c(10, 20), cc = "<=1400", f = "G", eu = "I",
                           p = "CO"), "speed")
})

test_that("missing emission factors give an informative error", {
  expect_error(ef_ldv_cold(ta = 10, cc = "<=1400", f = "G", eu = "XX", p = "CO"),
               "No cold-start emission factor")
})

# data fixes in sysdata$cold and sysdata$ldv_det (fix_cold_det in buildvein)
cold <- vein:::sysdata$cold
skip_if(any(cold$EURO == "Vic"), "sysdata not rebuilt with fix_cold_det yet")

test_that("all cold equations parse and Euro labels exist", {
  ok <- vapply(as.character(cold$Y), function(y) {
    !inherits(try(parse(text = y), silent = TRUE), "try-error")
  }, logical(1))
  expect_true(all(ok))
  for (eu in c("PRE", "I", "II", "III", "IV", "V", "VI", "VIc", "VId", "VII")) {
    for (f in c("G", "D", "LPG")) {
      expect_silent(ef_ldv_cold(ta = 10, cc = "<=1400", f = f, eu = eu, p = "CO")(20))
    }
  }
})

test_that("diesel quotients follow EMEP/EEA Table 3-44 for all Euro", {
  for (eu in c("PRE", "III", "V", "VI")) {
    cf <- function(p, ta) ef_ldv_cold(ta = ta, cc = "<=1400", f = "D", eu = eu, p = p)(20)
    expect_equal(cf("CO", 10), 1.9 - 0.03 * 10)
    expect_equal(cf("NOx", 10), 1.3 - 0.013 * 10)
    expect_equal(cf("HC", 10), 3.1 - 0.09 * 10)
    expect_equal(cf("HC", 30), 0.5)
    expect_equal(cf("PM", 20), 3.1 - 0.1 * 20)
    expect_equal(cf("PM", 27), 0.5)
    expect_equal(cf("FC", 20), 1.34 - 0.008 * 20)
  }
})

test_that("petrol Euro 2+ bc factors reduce the cold excess", {
  r1 <- ef_ldv_cold(ta = 10, cc = "<=1400", f = "G", eu = "I", p = "CO")(20)
  expect_equal(r1, 0.156 * 20 - 0.155 * 10 + 3.519)
  expect_equal(ef_ldv_cold(ta = 10, cc = "<=1400", f = "G", eu = "II", p = "CO")(20),
               1 + 0.72 * (r1 - 1))
  expect_equal(ef_ldv_cold(ta = 10, cc = "<=1400", f = "G", eu = "IV", p = "CO")(20),
               1 + 0.18 * (r1 - 1))
  # Table 3-40: CO uses the low speed equation up to 33 km/h
  expect_equal(ef_ldv_cold(ta = 5, cc = "<=1400", f = "G", eu = "I", p = "CO")(30),
               0.156 * 30 - 0.155 * 5 + 3.519)
  # LPG Euro 1+ uses the petrol methodology
  expect_equal(ef_ldv_cold(ta = 10, cc = "<=1400", f = "LPG", eu = "IV", p = "CO")(20),
               1 + 0.18 * (r1 - 1))
})

test_that("ldv_det has no padded Euro labels", {
  det <- vein:::sysdata$ldv_det
  expect_true(all(det$EURO == trimws(det$EURO)))
  km <- units::set_units(c(60000, 60000), "km")
  expect_false(anyNA(emis_det(po = "CO", cc = "<=1400", eu = c("VIc", "VId"), km = km)))
})
