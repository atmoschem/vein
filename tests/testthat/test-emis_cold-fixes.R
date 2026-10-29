context("emis_cold fixes")

veh <- data.frame(a = c(1, 2), b = c(3, 4), c = c(5, 6))
lkm <- units::set_units(c(1, 2), "km")
ef <- EmissionFactorsList(list(function(V) 1, function(V) 2, function(V) 3))
efc <- EmissionFactorsList(list(function(V) 2, function(V) 3, function(V) 4))
beta <- matrix(c(0.1, 0.2, 0.3, 0.4), 2, 2)
prof <- matrix(c(1, 2, 3, 4), 2, 2)
sp <- data.frame(1, 1, 1, 1)

ref <- array(NA_real_, c(2, 3, 2, 2))
for (i in 1:2) for (k in 1:3) for (h in 1:2) for (d in 1:2) {
  ref[i, k, h, d] <- beta[h, d] * veh[i, k] * prof[h, d] * as.numeric(lkm)[i] * k * ((k + 1) - 1)
}

test_that("emis_cold 4D and 3D results", {
  expect_equal(unclass(emis_cold(veh, lkm, ef, efc, beta, speed = sp, profile = prof)),
    ref, check.attributes = FALSE)
  expect_equal(unclass(emis_cold(veh, lkm, ef, efc, beta, speed = sp, profile = prof,
    simplify = TRUE)),
  array(ref, c(2, 3, 4)), check.attributes = FALSE)
})

test_that("emis_cold keeps the street dimension with one street", {
  r <- emis_cold(veh[1, ], lkm[1], ef, efc, beta, speed = sp, profile = prof)
  expect_equal(dim(r), c(1, 3, 2, 2))
  expect_equal(unclass(r)[1, , , ], ref[1, , , ])
})

test_that("emis_cold pads efcold as ef", {
  efc2 <- EmissionFactorsList(list(function(V) 2, function(V) 3))
  r <- emis_cold(veh, lkm, ef, efc2, beta, speed = sp, profile = prof)
  # third category uses the last efcold (3), cold factor 2 instead of 3
  expect_equal(unclass(r)[, 3, , ], ref[, 3, , ] * 2 / 3)
})

test_that("emis_cold converts lkm units", {
  expect_equal(unclass(emis_cold(veh, units::set_units(lkm, "m"), ef, efc, beta,
    speed = sp, profile = prof)),
  ref, check.attributes = FALSE)
})
