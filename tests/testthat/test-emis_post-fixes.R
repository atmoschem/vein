context("emis_post fixes")

set.seed(2)
a3 <- EmissionsArray(array(runif(3 * 2 * 4), c(3, 2, 4)))
a4 <- EmissionsArray(array(runif(3 * 2 * 2 * 2), c(3, 2, 2, 2)))

test_that("emis_post streets_narrow applies k and numbers the streets", {
  n3 <- emis_post(a3, pollutant = "CO", by = "streets_narrow", k = 10)
  expect_equal(sum(as.numeric(n3$g)), 10 * sum(a3))
  expect_equal(n3$street, rep(1:3, 4))
  expect_equal(as.numeric(n3$g), as.vector(apply(unclass(a3), c(1, 3), sum)) * 10)
  n4 <- emis_post(a4, pollutant = "CO", by = "streets_narrow", k = 10)
  expect_equal(sum(as.numeric(n4$g)), 10 * sum(a4))
})

test_that("emis_post streets and veh conserve mass", {
  w3 <- emis_post(a3, pollutant = "CO", by = "streets", k = 2)
  expect_equal(dim(w3), c(3, 4))
  expect_equal(sum(as.numeric(unlist(w3))), 2 * sum(a3))
  w4 <- emis_post(a4, pollutant = "CO", by = "streets")
  expect_equal(dim(w4), c(3, 4))
  expect_equal(as.numeric(unlist(w4)), as.vector(apply(unclass(a4), c(1, 3, 4), sum)))
  v4 <- suppressWarnings(emis_post(a4, pollutant = "CO", by = "veh"))
  expect_equal(as.numeric(v4$g), as.vector(apply(unclass(a4), c(2, 3, 4), sum)))
  v3 <- suppressWarnings(emis_post(a3, pollutant = "CO", by = "veh"))
  expect_equal(as.numeric(v3$g), as.vector(apply(unclass(a3), c(2, 3), sum)))
})

test_that("emis_post works with one street or one category", {
  a41 <- EmissionsArray(array(1, c(1, 2, 2, 2)))
  v <- emis_post(a41, pollutant = "CO", by = "veh", veh = "PC", size = "s",
                 fuel = "G", type_emi = "e")
  expect_equal(as.numeric(v$g), rep(1, 8))
  a42 <- EmissionsArray(array(1, c(3, 1, 2, 2)))
  v <- emis_post(a42, pollutant = "CO", by = "veh", veh = "PC", size = "s",
                 fuel = "G", type_emi = "e")
  expect_equal(as.numeric(v$g), rep(3, 4))
  s <- emis_post(a41, pollutant = "CO", by = "streets")
  expect_equal(dim(s), c(1, 4))
  a31 <- EmissionsArray(array(1, c(1, 2, 3)))
  expect_equal(dim(emis_post(a31, pollutant = "CO", by = "streets")), c(1, 3))
})

test_that("emis_post rejects arrays that are not 3D or 4D", {
  expect_error(emis_post(EmissionsArray(matrix(1, 2, 2)), pollutant = "CO", by = "streets"),
               "emis_post only reads")
})
