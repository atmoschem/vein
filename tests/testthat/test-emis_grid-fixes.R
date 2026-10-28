context("emis_grid fixes")

data(net)
netp <- sf::st_transform(sf::st_as_sf(net), 31983)[1:50, c("ldv", "hdv")]
g <- make_grid(netp, 1000, crs = 31983)
gsub <- g[1:floor(nrow(g) / 2), ]

# fraction of each street inside the half grid
u <- sf::st_union(gsub)
fr <- sapply(seq_len(nrow(netp)), function(i) {
  x <- suppressWarnings(sf::st_intersection(sf::st_geometry(netp)[i], u))
  if (length(x) == 0) 0 else as.numeric(sum(sf::st_length(x))) / as.numeric(sf::st_length(netp[i, ]))
})

gsum <- function(x, n) sum(as.numeric(x[[n]]))

test_that("emis_grid conserves the mass of each column", {
  gx <- suppressMessages(emis_grid(netp, g, flux = FALSE))
  expect_equal(gsum(gx, "ldv"), sum(as.numeric(netp$ldv)))
  expect_equal(gsum(gx, "hdv"), sum(as.numeric(netp$hdv)))
  # flux * area
  gf <- suppressMessages(emis_grid(netp, g))
  area <- as.numeric(units::set_units(sf::st_area(g), "km^2"))
  expect_equal(sum(as.numeric(gf$ldv) * area), sum(as.numeric(netp$ldv)))
})

test_that("emis_grid honours k", {
  gx <- suppressMessages(emis_grid(netp, g, flux = FALSE, k = 10))
  expect_equal(gsum(gx, "ldv"), 10 * sum(as.numeric(netp$ldv)))
  expect_equal(gsum(gx, "hdv"), 10 * sum(as.numeric(netp$hdv)))
})

test_that("emis_grid does not push emissions outside the grid inside", {
  gp <- suppressMessages(emis_grid(netp, gsub, flux = FALSE))
  expect_equal(gsum(gp, "ldv"), sum(as.numeric(netp$ldv) * fr), tolerance = 1e-6)
  expect_equal(gsum(gp, "hdv"), sum(as.numeric(netp$hdv) * fr), tolerance = 1e-6)
})

test_that("emis_grid keeps units by column and parses sr", {
  n2 <- netp
  n2$ldv <- units::set_units(as.numeric(n2$ldv), "g/h")
  n2$hdv <- units::set_units(as.numeric(n2$hdv), "kg/h")
  gx <- suppressMessages(emis_grid(n2, g, flux = FALSE, sr = "EPSG:31983"))
  expect_equal(as.character(units(gx$ldv)), "g/h")
  expect_equal(as.character(units(gx$hdv)), "kg/h")
  gy <- suppressMessages(emis_grid(n2, g, flux = FALSE, sr = 31983))
  expect_equal(gx$ldv, gy$ldv)
})

test_that("emis_grid transforms spobj to the crs of g", {
  n4326 <- sf::st_transform(netp, 4326)
  gx <- suppressMessages(emis_grid(n4326, g, flux = FALSE))
  expect_equal(gsum(gx, "ldv"), sum(as.numeric(netp$ldv)), tolerance = 1e-6)
})

test_that("emis_grid points and polygons conserve mass", {
  pts <- suppressWarnings(sf::st_centroid(netp))
  gx <- suppressMessages(emis_grid(pts, g, type = "points", flux = FALSE))
  expect_equal(gsum(gx, "ldv"), sum(as.numeric(netp$ldv)))
  buf <- sf::st_buffer(sf::st_centroid(sf::st_as_sfc(sf::st_bbox(g))), dist = 700)
  pol <- sf::st_sf(a = 100, geometry = buf)
  gp <- suppressMessages(emis_grid(pol, g, type = "polygons", flux = FALSE))
  expect_equal(gsum(gp, "a"), 100, tolerance = 1e-6)
})
