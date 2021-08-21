context("split_emis")
data(net)
net <- sf::st_transform(net, 31983)
g <- make_grid(net, 500) #500m in degrees
netsf <- sf::st_as_sf(net)[, "ldv"]
x <- split_emis(netsf, g)
dim(x)

test_that("split_emis works", {
  expect_equal(round(as.numeric(sum(x$ldv))), 1946955)
})

g$A <- rep(letters, length = 20)[1:nrow(g)]
g$B <- rev(g$A)
netsf <- sf::st_as_sf(net)[, c("ldv", "hdv")]

test_that("split_emis works", {
  expect_equal(round(as.numeric(sum(split_emis(netsf,
                                         g,
                                         add_column = c("A", "B"))$ldv
                              ))), 1946955)
})


test_that("split_emis message", {
  expect_equal(round(as.numeric(split_emis(net = netsf, distance = 1000, verbose = TRUE)$ldv[1])),
                 1603)
})

