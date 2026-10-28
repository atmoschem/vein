context("emis_hot_td fixes")

nr <- 3
nc <- 4
veh <- age_veh(c(10, 20, 30), "ldv", agemax = nc)
row.names(veh) <- paste0("R", 1:nr)
lkm <- units::as_units(c(10, 20, 30, 40), "km")
efv <- EmissionFactors(c(1, 2, 3, 4))
efdf <- EmissionFactors(as.data.frame(matrix(1:(nr * nc), nr, nc)))
ef12 <- EmissionFactors(as.data.frame(matrix(1:(12 * nr * nc), 12 * nr, nc)))
pm <- c(8, rep(10, 5), 9, rep(10, 5))
pmn <- pm / sum(pm)
pmdf <- as.data.frame(matrix(pm, nr, 12, byrow = TRUE))
pmdf[2, ] <- pmdf[2, ] * c(2, rep(1, 11))
pmdfn <- t(apply(pmdf, 1, function(x) x / sum(x)))

V <- as.matrix(remove_units(veh))
L <- as.numeric(lkm)
EFD <- as.matrix(remove_units(efdf))
E12 <- as.matrix(remove_units(ef12))
mk <- function(f) {
  out <- data.frame(e = numeric(0), rows = character(0), age = integer(0), month = integer(0))
  for (k in 1:12) for (j in 1:nc) for (i in 1:nr) {
    out[nrow(out) + 1, ] <- list(f(i, j, k), row.names(veh)[i], j, k)
  }
  out
}
refs <- list(
  dfdf = mk(function(i, j, k) V[i, j] * L[j] * EFD[i, j] * pmdfn[i, k]),
  dfnum = mk(function(i, j, k) V[i, j] * L[j] * EFD[i, j] * pmn[k]),
  e12df = mk(function(i, j, k) V[i, j] * L[j] * E12[(k - 1) * nr + i, j] * pmdfn[i, k]),
  e12num = mk(function(i, j, k) V[i, j] * L[j] * E12[(k - 1) * nr + i, j] * pmn[k]),
  numdf = mk(function(i, j, k) V[i, j] * L[j] * as.numeric(efv)[j] * pmdfn[i, k]),
  numnum = mk(function(i, j, k) V[i, j] * L[j] * as.numeric(efv)[j] * pmn[k])
)
same <- function(r, ref) {
  expect_equal(as.numeric(r$emissions), ref$e)
  expect_equal(as.character(r$rows), ref$rows)
  expect_equal(as.integer(r$age), ref$age)
  expect_equal(as.integer(r$month), ref$month)
}
nt_ok <- check_nt() > 2

test_that("emis_hot_td: all monthly branches, R and fortran, values and labels", {
  for (f in c(FALSE, TRUE)) {
    same(emis_hot_td(veh, lkm, efdf, pro_month = pmdf, fortran = f), refs$dfdf)
    same(emis_hot_td(veh, lkm, efdf, pro_month = pm, fortran = f), refs$dfnum)
    same(emis_hot_td(veh, lkm, ef12, pro_month = pmdf, fortran = f), refs$e12df)
    same(emis_hot_td(veh, lkm, ef12, pro_month = pm, fortran = f), refs$e12num)
    same(emis_hot_td(veh, lkm, efv, pro_month = pmdf, fortran = f), refs$numdf)
    same(emis_hot_td(veh, lkm, efv, pro_month = pm, fortran = f), refs$numnum)
  }
  skip_if_not(nt_ok)
  same(emis_hot_td(veh, lkm, efdf, pro_month = pmdf, fortran = TRUE, nt = 2), refs$dfdf)
  same(emis_hot_td(veh, lkm, efdf, pro_month = pm, fortran = TRUE, nt = 2), refs$dfnum)
  same(emis_hot_td(veh, lkm, ef12, pro_month = pmdf, fortran = TRUE, nt = 2), refs$e12df)
  same(emis_hot_td(veh, lkm, ef12, pro_month = pm, fortran = TRUE, nt = 2), refs$e12num)
  same(emis_hot_td(veh, lkm, efv, pro_month = pmdf, fortran = TRUE, nt = 2), refs$numdf)
  same(emis_hot_td(veh, lkm, efv, pro_month = pm, fortran = TRUE, nt = 2), refs$numnum)
})

test_that("emis_hot_td: one-row data.frame pro_month is replicated by row", {
  p1 <- as.data.frame(matrix(pm, 1, 12))
  same(emis_hot_td(veh, lkm, efv, pro_month = p1), refs$numnum)
  same(emis_hot_td(veh, lkm, efdf, pro_month = p1), refs$dfnum)
  same(emis_hot_td(veh, lkm, efv, pro_month = p1, fortran = TRUE), refs$numnum)
})

test_that("emis_hot_td converts units of ef and lkm", {
  a <- emis_hot_td(veh, lkm, efv)
  b <- emis_hot_td(veh, lkm, units::set_units(efv, "mg/km"))
  d <- emis_hot_td(veh, units::set_units(lkm, "m"), efv)
  expect_equal(as.numeric(b$emissions), as.numeric(a$emissions))
  expect_equal(as.numeric(d$emissions), as.numeric(a$emissions))
  expect_error(emis_hot_td(veh, lkm, units::set_units(1:4, "g/h")), "g/km")
})

test_that("emis_hot_td labels rows with the row names of veh", {
  a <- emis_hot_td(veh, lkm, efv)
  b <- emis_hot_td(veh, lkm, efv, fortran = TRUE)
  expect_equal(a$rows, rep(row.names(veh), nc))
  expect_equal(b$rows, rep(row.names(veh), nc))
  expect_equal(as.numeric(a$emissions), as.numeric(b$emissions))
})
