context("emis fixes")

set.seed(1)
ns <- 4
na <- 3
veh <- as.data.frame(matrix(round(runif(ns * na, 1, 100)), ns, na))
lkm <- units::set_units(c(1, 2, 3, 4), "km")
efn <- EmissionFactors(c(1, 10, 100))
prof <- matrix(runif(6), nrow = 3, ncol = 2) # 3 hours x 2 days

ref4 <- array(NA_real_, c(ns, na, 3, 2))
for (i in 1:ns) for (j in 1:na) for (h in 1:3) for (d in 1:2) {
  ref4[i, j, h, d] <- veh[i, j] * as.numeric(lkm)[i] * as.numeric(efn)[j] * prof[h, d]
}
ref3 <- array(ref4, c(ns, na, 6))
nt_ok <- check_nt() > 2

test_that("emis R and fortran give the same 4D results", {
  expect_equal(unclass(emis(veh, lkm, efn, profile = prof)), ref4,
    check.attributes = FALSE)
  expect_equal(unclass(emis(veh, lkm, efn, profile = prof, fortran = TRUE)), ref4,
    check.attributes = FALSE)
  expect_equal(unclass(emis(veh, lkm, efn, profile = as.data.frame(prof))), ref4,
    check.attributes = FALSE)
  expect_equal(unclass(emis(veh, lkm, efn, profile = as.data.frame(prof), fortran = TRUE)), ref4,
    check.attributes = FALSE)
  skip_if_not(nt_ok)
  expect_equal(unclass(emis(veh, lkm, efn, profile = prof, fortran = TRUE, nt = 2)), ref4,
    check.attributes = FALSE)
})

test_that("emis R and fortran give the same 3D results", {
  expect_equal(unclass(emis(veh, lkm, efn, profile = prof, simplify = TRUE)), ref3,
    check.attributes = FALSE)
  expect_equal(unclass(emis(veh, lkm, efn, profile = prof, simplify = TRUE, fortran = TRUE)), ref3,
    check.attributes = FALSE)
  skip_if_not(nt_ok)
  expect_equal(unclass(emis(veh, lkm, efn, profile = prof, simplify = TRUE, fortran = TRUE, nt = 2)), ref3,
    check.attributes = FALSE)
})

test_that("emis top-down R and fortran are equal", {
  ref2 <- as.matrix(veh) * as.numeric(lkm) %o% rep(1, na)
  ref2 <- sweep(ref2, 2, as.numeric(efn), "*")
  expect_equal(as.matrix(remove_units(emis(veh, lkm, efn))), ref2, check.attributes = FALSE)
  expect_equal(as.matrix(remove_units(emis(veh, lkm, efn, fortran = TRUE))), ref2, check.attributes = FALSE)
  skip_if_not(nt_ok)
  expect_equal(as.matrix(remove_units(emis(veh, lkm, efn, fortran = TRUE, nt = 2))), ref2, check.attributes = FALSE)
})

test_that("emis with EmissionFactorsList uses the speed of each hour and day", {
  lef <- EmissionFactorsList(c(function(V) 1 * V, function(V) 2 * V, function(V) 3 * V))
  sp <- as.data.frame(matrix(1:(ns * 6), ns, 6))
  refl <- array(NA_real_, c(ns, na, 3, 2))
  for (i in 1:ns) for (j in 1:na) for (h in 1:3) for (d in 1:2) {
    refl[i, j, h, d] <- veh[i, j] * as.numeric(lkm)[i] * j * sp[i, h + 3 * (d - 1)] * prof[h, d]
  }
  expect_equal(unclass(emis(veh, lkm, lef, speed = sp, profile = prof)), refl,
    check.attributes = FALSE)
  expect_equal(unclass(emis(veh, lkm, lef, speed = sp, profile = prof, simplify = TRUE)),
    array(refl, c(ns, na, 6)), check.attributes = FALSE)
  # list of vehicles
  lv <- list(veh, veh * 2)
  refv <- array(NA_real_, c(ns, na, 2))
  for (i in 1:ns) for (j in 1:na) for (h in 1:2) {
    refv[i, j, h] <- lv[[h]][i, j] * as.numeric(lkm)[i] * j * sp[i, h]
  }
  expect_equal(unclass(emis(lv, lkm, lef, speed = sp[, 1:2])), refv, check.attributes = FALSE)
})

test_that("emis keeps the street dimension with one street", {
  expect_equal(dim(emis(veh[1, ], lkm[1], efn, profile = prof)), c(1, 3, 3, 2))
  expect_equal(dim(emis(veh[1, ], lkm[1], efn, profile = prof, fortran = TRUE)), c(1, 3, 3, 2))
  expect_equal(dim(emis(veh[1, ], lkm[1], efn, profile = prof, simplify = TRUE)), c(1, 3, 6))
  lef <- EmissionFactorsList(list(function(V) 1, function(V) 1, function(V) 1))
  expect_equal(dim(emis(veh[1, ], lkm[1], lef, speed = data.frame(1, 1, 1, 1, 1, 1),
    profile = prof)), c(1, 3, 3, 2))
})

test_that("emis uses agemax in R and fortran", {
  expect_equal(dim(emis(veh, lkm, efn, profile = prof, agemax = 2)), c(4, 2, 3, 2))
  expect_equal(dim(emis(veh, lkm, efn, profile = prof, agemax = 2, fortran = TRUE)), c(4, 2, 3, 2))
  expect_error(emis(veh, lkm, efn, profile = prof, agemax = 5), "agemax")
})

test_that("emis converts units of lkm and ef", {
  e1 <- emis(veh, lkm, efn, profile = prof)
  e2 <- emis(veh, units::set_units(lkm, "m"), efn, profile = prof)
  e3 <- emis(veh, lkm, units::set_units(efn, "mg/km"), profile = prof)
  expect_equal(unclass(e2), unclass(e1))
  expect_equal(unclass(e3), unclass(e1))
  e4 <- emis(veh, units::set_units(as.numeric(lkm), "mi"), efn, profile = prof)
  expect_equal(unclass(e4), unclass(e1) * 1.609344, tolerance = 1e-6)
  expect_error(emis(veh, units::set_units(1:4, "h"), efn, profile = prof), "Units")
})

test_that("emis with missing profile and list of functions is top-down", {
  lef <- list(function(V) V, function(V) V, function(V) V)
  e <- emis(veh, lkm, lef, speed = 1:4)
  expect_equal(as.numeric(e[, 2]), veh[, 2] * as.numeric(lkm) * 1:4)
})

test_that("vein_check_size stops before integer overflow", {
  expect_error(vein_check_size(1e6, 41, 24, 7), "fortran = FALSE")
  expect_silent(vein_check_size(10, 41, 24, 7))
})
