context("emis_cold_td fixes")

euros <- c("V", "IV", "III")
nr <- 3
nc <- 3
# temperature varies by row and month, so efcold differs in each row/month
dt <- matrix(c(5, 10, 15), ncol = 12, nrow = nr) + rep(0:11, each = nr) * 0.5
efc <- ef_ldv_cold(ta = dt, cc = "<=1400", f = "G", eu = euros, p = "CO",
                   speed = Speed(34))
lkm <- units::as_units(c(10, 20, 30), "km")
veh <- age_veh(c(10, 20, 30), "ldv", agemax = nc)
beta <- as.data.frame(matrix(0.1 * (1:(nr * 12)), nr, 12))
pm <- c(8, rep(10, 5), 9, rep(10, 5))
ef1 <- EmissionFactors(c(1.5, 2, 3))
efdf <- EmissionFactors(as.data.frame(matrix(rep(as.numeric(ef1), each = nr), nr, nc)))
pmdf <- as.data.frame(matrix(pm, nr, 12, byrow = TRUE))

pmn <- pm / sum(pm)
efcm <- as.data.frame(lapply(efc[, 1:nc], as.numeric))
ref <- c()
for (k in 1:12) for (j in 1:nc) for (i in 1:nr) {
  ref <- c(ref, beta[i, k] * as.numeric(lkm)[j] * veh[i, j] * pmn[k] *
             as.numeric(ef1)[j] * efcm[(k - 1) * nr + i, j])
}
nt_ok <- check_nt() > 2

test_that("emis_cold_td: all monthly branches, R and fortran, equal the reference", {
  for (f in c(FALSE, TRUE)) {
    expect_equal(as.numeric(emis_cold_td(veh, lkm, ef1, efc, beta, pro_month = pm, fortran = f)$emissions), ref)
    expect_equal(as.numeric(emis_cold_td(veh, lkm, ef1, efc, beta, pro_month = pmdf, fortran = f)$emissions), ref)
    expect_equal(as.numeric(emis_cold_td(veh, lkm, efdf, efc, beta, pro_month = pm, fortran = f)$emissions), ref)
    expect_equal(as.numeric(emis_cold_td(veh, lkm, efdf, efc, beta, pro_month = pmdf, fortran = f)$emissions), ref)
  }
  skip_if_not(nt_ok)
  expect_equal(as.numeric(emis_cold_td(veh, lkm, ef1, efc, beta, pro_month = pm, fortran = TRUE, nt = 2)$emissions), ref)
  expect_equal(as.numeric(emis_cold_td(veh, lkm, ef1, efc, beta, pro_month = pmdf, fortran = TRUE, nt = 2)$emissions), ref)
  expect_equal(as.numeric(emis_cold_td(veh, lkm, efdf, efc, beta, pro_month = pm, fortran = TRUE, nt = 2)$emissions), ref)
  expect_equal(as.numeric(emis_cold_td(veh, lkm, efdf, efc, beta, pro_month = pmdf, fortran = TRUE, nt = 2)$emissions), ref)
})

test_that("emis_cold_td: one-row pro_month is replicated by row", {
  expect_equal(as.numeric(emis_cold_td(veh, lkm, ef1, efc, beta,
                                       pro_month = pmdf[1, ])$emissions), ref)
  expect_equal(as.numeric(emis_cold_td(veh, lkm, efdf, efc, beta,
                                       pro_month = pmdf[1, ])$emissions), ref)
})

test_that("emis_cold_td: without monthly profile", {
  ref0 <- c()
  for (j in 1:nc) for (i in 1:nr) {
    ref0 <- c(ref0, beta[i, 1] * as.numeric(lkm)[j] * veh[i, j] * as.numeric(ef1)[j] * efcm[i, j])
  }
  expect_equal(as.numeric(emis_cold_td(veh, lkm, ef1, efc[1:nr, ], beta[, 1])$emissions), ref0)
  expect_equal(as.numeric(emis_cold_td(veh, lkm, ef1, efc[1:nr, ], beta[, 1], fortran = TRUE)$emissions), ref0)
  expect_error(emis_cold_td(veh, lkm, ef1, efc[1:nr, ], beta), "beta")
})

test_that("emis_cold_td: units are converted and sf is accepted", {
  a <- emis_cold_td(veh, lkm, ef1, efc, beta, pro_month = pm)
  b <- emis_cold_td(veh, units::set_units(lkm, "m"), units::set_units(ef1, "mg/km"),
                    efc, beta, pro_month = pm)
  expect_equal(as.numeric(b$emissions), as.numeric(a$emissions))
  data(net)
  vsf <- sf::st_sf(veh, geometry = sf::st_geometry(sf::st_as_sf(net))[1:3])
  expect_equal(as.numeric(emis_cold_td(vsf, lkm, ef1, efc, beta, pro_month = pm)$emissions), ref)
})
