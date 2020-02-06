context("emis_grid")

data(net)
g <- make_grid(net, 1/102.47/2) #500m in degrees
names(net)
netsf <- sf::st_as_sf(net)
netsf$CO <- netsf$ldv*Emissions(1)
netg <- emis_grid(spobj = netsf[, c("CO")], g = g, sr= 31983)


test_that("emis_grid works", {
  expect_equal(round(sum(emis_grid(spobj = netsf[, c("CO")],
                         g = g,
                         sr= 31983)$CO[1], na.rm = T)),
               round(as.numeric(6503)))
})

test_that("emis_grid works", {
  expect_equal(round(sum(emis_grid(spobj = netsf[, c("CO")],
                                   g = g,
                                   sr= "+init=epsg:4326")$CO[1], na.rm = T)),
               round(as.numeric(6503)))
})

sff2 <- suppressMessages(suppressWarnings(sf::st_centroid(netsf[, c("CO")])))
test_that("emis_grid works", {
  expect_equal(round(sum(emis_grid(spobj = sff2,
                                   g = g,
                                   type = "points")$CO[1], na.rm = T)),
               round(as.numeric(0)))
})

test_that("emis_grid works", {
  netp <- sf::st_sf(a = 1, geometry = sf::st_as_sfc(sf::st_bbox(sf::st_as_sf(net))))
  netp$a <- Emissions(netp$a) # must include units otherwise does not work)
  expect_equal(round(sum(emis_grid(spobj = netp,
                                   g = g,
                                   type = "polygon")$a[1], na.rm = T)),
               0)
})

# flux FALSE
test_that("emis_grid works", {
  expect_equal(round(sum(emis_grid(spobj = netsf[, c("CO")],
                                   g = g,
                                   sr= "+init=epsg:4326",
                                   flux = FALSE)$CO[1], na.rm = T)),
               round(as.numeric(1750)))
})

sff2 <- suppressMessages(suppressWarnings(sf::st_centroid(netsf[, c("CO")])))
test_that("emis_grid works", {
  expect_equal(round(sum(emis_grid(spobj = sff2,
                                   g = g,
                                   type = "points",
                                   flux = FALSE)$CO[1], na.rm = T)),
               round(as.numeric(0)))
})

test_that("emis_grid works", {
  netp <- sf::st_sf(a = 1, geometry = sf::st_as_sfc(sf::st_bbox(sf::st_as_sf(net))))
  netp$a <- Emissions(netp$a) # must include units otherwise does not work)
  expect_equal(round(sum(emis_grid(spobj = netp,
                                   g = g,
                                   type = "polygon",
                                   flux = FALSE)$a[1], na.rm = T)),
               0)
})

