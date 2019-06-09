context("split_emis")
data(net)
g <- make_grid(net, 1/102.47/2) #500m in degrees
netsf <- sf::st_as_sf(net)[, "ldv"]
x <- split_emis(netsf, g)
dim(x)

test_that("split_emis works", {
  expect_equal(as.numeric(sum(split_emis(netsf, g)$`gnet[, ncolnet]`)), 1946955)
})

