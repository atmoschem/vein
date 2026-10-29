context("spatial helpers fixes")

data(net)
nsf <- sf::st_as_sf(net)

test_that("emis_dist with osm distributes all gy", {
  n2 <- nsf[1:20, ]
  n2$highway <- rep(c("motorway", "primary", "residential", "secondary"), 5)
  r <- expect_warning(emis_dist(gy = 1000, spobj = n2, osm = c(5, 3, 2, 1, 1)), "normalized")
  expect_equal(sum(r$emission), 1000)
  expect_true(all(r$highway != "residential"))
  r2 <- suppressWarnings(emis_dist(gy = 1000, spobj = n2, osm = c(5, 3, 2, 1, 1),
    pro = matrix(1, 24, 1)))
  expect_equal(sum(sf::st_set_geometry(r2, NULL)[, 1:24]), 1000)
  n2$highway <- rep(c("motorway", "trunk", "primary", "secondary", "tertiary"), 4)
  r3 <- emis_dist(gy = 1000, spobj = n2, osm = c(5, 3, 2, 1, 1))
  expect_equal(sum(r3$emission), 1000)
  expect_equal(sum(r3$emission[r3$highway == "motorway"]), 1000 * 5 / 12)
})

test_that("emis_dist without osm", {
  r <- emis_dist(gy = 1000, spobj = nsf[1:10, ])
  expect_equal(names(r), c("emission", "geometry"))
  expect_equal(sum(r$emission), 1000)
})

np <- sf::st_transform(nsf, 31983)[1:20, c("ldv", "hdv")]
g <- make_grid(np, 1000, crs = 31983)

test_that("split_emis keeps names of add_column", {
  g$A <- "a"
  g$B <- "b"
  x <- split_emis(np, g, add_column = c("A", "B"), verbose = FALSE)
  expect_equal(names(x), c("ldv", "hdv", "A", "B", "id", "geometry"))
  expect_equal(sum(as.numeric(x$ldv)), sum(as.numeric(np$ldv)))
  x1 <- split_emis(np[, "ldv"], g, verbose = FALSE)
  expect_equal(names(x1), c("ldv", "id", "geometry"))
})

test_that("grid_emis bottom-up uses char = 'V' by default and conserves mass", {
  gg <- g
  gg$V1 <- 1
  gg$V2 <- 2
  r <- grid_emis(np, gg)
  expect_equal(sum(as.numeric(r$V1)), sum(gg$V1))
  expect_equal(sum(as.numeric(r$V2)), sum(gg$V2))
})

test_that("grid_emis top-down with different crs", {
  gg <- sf::st_transform(g, 4326)
  gg$emission <- 1
  r <- grid_emis(np, gg, top_down = TRUE)
  expect_equal(sum(r$emission), nrow(gg))
})

test_that("GriddedEmissionsArray default times with sf", {
  gg <- g[, "id"]
  gg$V1 <- 1
  gg$V2 <- 2
  nc <- nrow(gg) / 11
  a <- GriddedEmissionsArray(gg, rows = 11, cols = nc)
  expect_equal(dim(a), c(nc, 11, 2))
  b <- GriddedEmissionsArray(gg, rows = 11, cols = nc, times = 1, rotate = "cols")
  expect_equal(dim(b), c(11, nc, 1))
  m <- GriddedEmissionsArray(as.matrix(sf::st_set_geometry(gg, NULL)[, c("V1", "V2")]),
    rows = 11, cols = nc)
  expect_equal(dim(m), c(nc, 11, 2))
})
