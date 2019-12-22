context("emis_grid")

data(net)
g <- make_grid(net, 1/102.47/2) #500m in degrees
names(net)
netsf <- sf::st_as_sf(net)
netg <- emis_grid(spobj = netsf[, c("ldv", "hdv")], g = g, sr= 31983)
round(sum(netg$ldv)) == round(as.numeric(sum(net$ldv)))
round(sum(netg$hdv)) == round(as.numeric(sum(net$hdv)))

test_that("emis_grid works", {
  expect_equal(round(sum(emis_grid(spobj = netsf[, c("ldv", "hdv")],
                         g = g,
                         sr= 31983)$ldv, na.rm = T)),
               round(as.numeric(sum(net$ldv))))
})

test_that("emis_grid works", {
  expect_equal(round(sum(emis_grid(spobj = netsf[, c("ldv", "hdv")],
                                   g = g,
                                   sr= "+init=epsg:4326")$ldv, na.rm = T)),
               round(as.numeric(sum(net$ldv))))
})

sff2 <- suppressMessages(suppressWarnings(sf::st_centroid(netsf[, c("ldv")])))
test_that("emis_grid works", {
  expect_equal(round(sum(emis_grid(spobj = sff2,
                                   g = g,
                                   type = "points")$ldv, na.rm = T)),
               round(as.numeric(sum(net$ldv))))
})

test_that("emis_grid works", {
  netp <- sf::st_sf(a = 1, geometry = sf::st_as_sfc(sf::st_bbox(sf::st_as_sf(net))))
  expect_equal(round(sum(emis_grid(spobj = netp,
                                   g = g,
                                   type = "polygon")$ldv, na.rm = T)),
               0)
})

