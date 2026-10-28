context("ef_nitro fixes")

args <- list(v = "PC", t = "Hot", cond = "Urban", f = "G", cc = "<=1400",
             p = "NH3", S = 10)

test_that("fcorr is applied", {
  a <- do.call(ef_nitro, c(args, eu = "III"))(10000)
  b <- do.call(ef_nitro, c(args, eu = "III", list(fcorr = rep(0.5, 8))))(10000)
  expect_equal(b, a * 0.5)
  km <- units::set_units(10000, "km")
  a <- do.call(ef_nitro, c(args, list(eu = c("III", "IV"), cumileage = km)))
  b <- do.call(ef_nitro, c(args, list(eu = c("III", "IV"), cumileage = km,
                                       fcorr = c(1, 1, 1, 0.5, 0.2, 1, 1, 1))))
  expect_equal(as.numeric(b[[1]]), as.numeric(a[[1]]) * 0.5)
  expect_equal(as.numeric(b[[2]]), as.numeric(a[[2]]) * 0.2)
})

test_that("one cumileage per euro works", {
  km <- units::set_units(c(10000, 50000), "km")
  r <- do.call(ef_nitro, c(args, list(eu = c("III", "IV"), cumileage = km)))
  expect_equal(nrow(r), 1)
  f3 <- do.call(ef_nitro, c(args, eu = "III"))
  f4 <- do.call(ef_nitro, c(args, eu = "IV"))
  expect_equal(as.numeric(r[[1]]), f3(10000))
  expect_equal(as.numeric(r[[2]]), f4(50000))
})

test_that("a grid of cumileage is evaluated for every euro", {
  km <- units::set_units(c(10000, 50000, 90000), "km")
  r <- do.call(ef_nitro, c(args, list(eu = c("III", "IV"), cumileage = km)))
  expect_equal(nrow(r), 3)
  f3 <- do.call(ef_nitro, c(args, eu = "III"))
  expect_equal(as.numeric(r[[1]]), f3(c(10000, 50000, 90000)))
  expect_equal(r$cumileage, as.numeric(km))
})
