context("emis_grid")

data(net)
g <- make_grid(net, 1/102.47/2)
names(net)
netsf <- sf::st_as_sf(net)
netg <- emis_grid(spobj = netsf[, c("ldv", "hdv")], g = g, sr= 31983)
round(sum(netg$ldv)) == round(as.numeric(sum(net$ldv)))
round(sum(netg$hdv)) == round(as.numeric(sum(net$hdv)))

test_that("emis_grid works", {
  expect_equal(make_grid(net, 1/102.47/2)$id[1],
               1)
})

test_that("emis_grid works", {
  expect_equal(make_grid(net, 1/102.47/2, crs = 31983)$id[1],
               1)
})

wrf <- paste(system.file("extdata", package = "eixport"),
             "/wrfinput_d02", sep="")
test_that("emis_grid works", {
  expect_equal(make_grid(wrf)$id[1],
               1)
})

b <- sf::st_bbox(net)
test_that("emis_grid works", {
  expect_equal(make_grid(b, 1/102.47/2, crs = 31983)$id[1],
               1)
})

test_that("emis_grid works", {
  expect_message(make_grid(net, 1/102.47/2, polygon = T),
                 "argument.?")
})
