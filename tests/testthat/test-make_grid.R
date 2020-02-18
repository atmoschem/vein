context("emis_grid")

data(net)
g <- make_grid(net, 1/102.47/2)
names(net)
netsf <- sf::st_as_sf(net)
netg <- emis_grid(spobj = netsf[, c("ldv", "hdv")], g = g, sr= 31983)

test_that("emis_grid works", {
  expect_equal(make_grid(net, 1/102.47/2)$id[1],
               1)
})

test_that("emis_grid works", {
  expect_equal(make_grid(net, 1/102.47/2, crs = 31983)$id[1],
               1)
})

b <- sf::st_bbox(net)
test_that("emis_grid works", {
  expect_equal(make_grid(b, 1/102.47/2, crs = 31983)$id[1],
               1)
})

test_that("emis_grid works", {
  expect_equal(make_grid(b, 1/102.47/2, crs = "+init=epsg:31983")$id[1],
               1)
})

test_that("emis_grid works", {
  expect_message(make_grid(net, 1/102.47/2, polygon = T),
                 "argument.?")
})
